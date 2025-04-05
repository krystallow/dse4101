# Load in libraries 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(corrplot) 
library(tidyr)
library(grf)
library(glmnet)
library(causalweight)
library(randomForest)
library(caret)
library(tidymodels)
library(ranger)
library(tableone)
library(patchwork)

# Load in data
students_cleaned <- read.csv("../data/cleaned_dataset.csv")

# Assign D, X and Y 
D <- students_cleaned$Treatment

X <- data.frame(students_cleaned$Gender, 
                students_cleaned$Age, 
                students_cleaned$City, 
                students_cleaned$Academic.Pressure, 
                students_cleaned$CGPA, 
                students_cleaned$Study.Satisfaction, 
                students_cleaned$Dietary.Habits, 
                students_cleaned$Work.Study.Hours, 
                students_cleaned$Financial.Stress, 
                students_cleaned$Depression, 
                students_cleaned$Suicidal_Thoughts, 
                students_cleaned$Family_History, 
                students_cleaned$Sleep.Duration)

# Explicitly set column names
colnames(X) <- c("Gender", "Age", "City", "Academic Pressure", "CGPA", "Study Satisfaction", 
                 "Dietary Habits", "Work Study Hours", "Financial Stress", "Depression", 
                 "Suicidal Thoughts", "Family History", "Sleep Duration")

# Converting the factor cols
X_df <- data.frame(lapply(X, function(col) {
  if (is.factor(col)) {
    return(as.numeric(as.factor(col)))  # Convert factor levels to numeric
  } else {
    return(as.numeric(col))  # Keep numeric variables as numeric
  }
}))

Y <- students_cleaned$Anxiety.Level
str(D)
str(Y)
str(X_df)
str(X)

X_vars <- c("Gender", "Age", "Academic.Pressure", "CGPA", "Study.Satisfaction",
            "Work.Study.Hours", "Financial.Stress", "Sleep.Duration",
            "Suicidal_Thoughts", "Family_History", "Major_City", "STEM_Degree", "Dietary_Score")

# Convert factor to numeric 
for (v in X_vars) {
  if (is.factor(students[[v]])) {
    students[[v]] <- as.numeric(as.character(students[[v]]))
  }
}

# Keep only complete cases
complete_idx <- complete.cases(students[, c(X_vars, "Anxiety.Level", "Treatment")])
students_complete <- students[complete_idx, ]
cat("Number of complete observations:", nrow(students_complete), "\n")

X <- model.matrix(~ . - 1, data = students_complete[, X_vars])  # one-hot encoded, no intercept
Y <- students_complete$Anxiety.Level
D <- students_complete$Treatment


#################
# Causal Forest #
#################
cf <- causal_forest(X = X, Y = Y, W = D, honesty = TRUE, seed = 54321)
cf_pred <- predict(cf, estimate.variance = TRUE)
tau_hat_cf <- cf_pred$predictions
tau_se_cf <- sqrt(cf_pred$variance.estimates)

blp_cf <- best_linear_projection(cf, A = D)
cat("Causal Forest - Best Linear Projection Summary:\n")
print(blp_cf)

cf_mse <- mean((Y - predict(cf)$predictions)^2)
cat("Causal Forest - MSE:", round(cf_mse, 4), "\n")

ci_width_cf <- mean(2 * 1.96 * tau_se_cf)
cat("Causal Forest - Average 95% Confidence Interval Width:", round(ci_width_cf, 4), "\n")

var_imp <- variable_importance(cf)
var_names <- colnames(X)
importance_df <- data.frame(Variable = var_names, Importance = var_imp)
importance_df <- importance_df[order(-importance_df$Importance), ]

cat("Top Variables Influencing CATE (Causal Forest):\n")
print(head(importance_df, 10))

hist(tau_hat_cf, breaks = 30, main = "Distribution of CATE Estimates (Causal Forest)",
     border = "white",
     xlab = "Estimated Treatment Effect", col = "lightblue")

ggplot(head(importance_df, 10), aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Important Features (Causal Forest)",
       x = "Variable", y = "Importance Score") +
  theme_minimal()

## Cross Validation for robustness check ## > Investigate if skewness of CATE estimates are justified

# Setup k-fold cross-validation (k=5)
k <- 5
folds <- sample(1:k, nrow(X), replace = TRUE)
tau_hat_cv <- numeric(nrow(X))
tau_se_cv <- numeric(nrow(X))

# Cross-validation loop
for (i in 1:k) {
  # Split data
  train_idx <- which(folds != i)
  test_idx <- which(folds == i)
  
  # Train on fold
  cf_fold <- causal_forest(
    X = X[train_idx, ], 
    Y = Y[train_idx], 
    W = D[train_idx],
    honesty = TRUE,
    honesty.fraction = 0.7,  # More aggressive honesty for CV
    min.node.size = 15,       # Larger nodes reduce overfitting
    num.trees = 2000          # More trees for stability
  )
  
  # Predict on outside fold
  pred_fold <- predict(cf_fold, newdata = X[test_idx, ], estimate.variance = TRUE)
  tau_hat_cv[test_idx] <- pred_fold$predictions
  tau_se_cv[test_idx] <- sqrt(pred_fold$variance.estimates)
}

# Compare distributions (original vs cross-validated)
df_compare <- data.frame(
  Method = rep(c("Full Sample", "Cross-Validated"), each = nrow(X)),
  CATE = c(tau_hat_cf, tau_hat_cv)
)

ggplot(df_compare, aes(x = CATE, fill = Method)) +
  geom_density(alpha = 0.6) +
  labs(title = "CATE Distribution: Full Sample vs Cross-Validated",
       x = "CATE Estimate",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Diagnostic metrics
cat("Robustness Check \n")
cat(sprintf("Correlation between full and CV estimates: %.3f\n", 
            cor(tau_hat_cf, tau_hat_cv)))
cat(sprintf("Mean absolute difference: %.3f\n", 
            mean(abs(tau_hat_cf - tau_hat_cv))))
cat(sprintf("Fraction of sign changes: %.3f\n", 
            mean(sign(tau_hat_cf) != sign(tau_hat_cv))))

#######
# DML #
#######
X_vars <- c("Gender", "Age", "Academic.Pressure", "CGPA", "Study.Satisfaction",
            "Work.Study.Hours", "Financial.Stress", "Sleep.Duration", "Depression", 
            "Suicidal.Thoughts", "Family.History", "Major.City", "STEM.Degree", "Dietary_Score")

for (v in X_vars) {
  if (is.factor(students[[v]])) {
    students[[v]] <- as.numeric(as.character(students[[v]]))
  }
}

complete_idx <- complete.cases(students[, c(X_vars, "Anxiety_Level", "Treatment")])
students_complete <- students[complete_idx, ]

X <- students_complete[, X_vars]
Y <- students_complete$Anxiety_Level
D <- students_complete$Treatment


set.seed(54321)
folds <- createFolds(Y, k = 2)

residual_Y <- rep(NA, length(Y))
residual_D <- rep(NA, length(D))

for (i in 1:2) {
  train_idx <- unlist(folds[-i])
  test_idx <- unlist(folds[i])
  
  rf_y <- randomForest(X[train_idx, ], Y[train_idx])
  m_hat <- predict(rf_y, X[test_idx, ])
  residual_Y[test_idx] <- Y[test_idx] - m_hat
  
  rf_d <- randomForest(x = X[train_idx, ], y = as.factor(D[train_idx]), ntree = 100)
  p_hat <- predict(rf_d, X[test_idx, ], type = "prob")[, 2]  # probability of treated
  residual_D[test_idx] <- D[test_idx] - p_hat
}


cate_model <- randomForest(x = X, y = residual_Y / (residual_D + 1e-6))  # prevent divide by zero
cate_pred <- predict(cate_model, X)


dml_mse <- mean(residual_Y^2)
dml_ci_width <- 2 * 1.96 * sd(residual_Y / (residual_D + 1e-6))

cat("\n DML-CATE Model Evaluation:\n")
cat("MSE:", round(dml_mse, 4), "\n")
cat("95% CI Width (approx):", round(dml_ci_width, 4), "\n")


hist(cate_pred, breaks = 30, main = "Distribution of CATE Estimates (DML - Residualized)",
     xlab = "Estimated Treatment Effect", col = "orange")

# Feature Importance Plot
imp <- importance(cate_model)
varImp_df <- data.frame(Variable = rownames(imp), Importance = imp[, 1]) %>%
  arrange(desc(Importance))

ggplot(head(varImp_df, 10), aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 10 Important Features (DML-CATE)",
       x = "Variable", y = "Importance Score") +
  theme_minimal()




#######
# FWL #
#######
X_vars <- c("Gender", "Age", "Academic.Pressure", "CGPA", "Study.Satisfaction",
            "Work.Study.Hours", "Financial.Stress", "Sleep.Duration", "Depression", 
            "Suicidal.Thoughts", "Family.History", "Major.City", "STEM.Degree", "Dietary_Score")

for (v in X_vars) {
  if (is.factor(students[[v]])) {
    students[[v]] <- as.numeric(as.character(students[[v]]))
  }
}

complete_idx <- complete.cases(students[, c(X_vars, "Anxiety_Level", "Treatment")])
students_complete <- students[complete_idx, ]

X <- model.matrix(~ . - 1, data = students_complete[, X_vars])
Y <- students_complete$Anxiety_Level
D <- students_complete$Treatment


set.seed(54321)
folds <- createFolds(Y, k = 2)
residual_Y <- rep(NA, length(Y))
residual_D <- rep(NA, length(D))

for (i in 1:2) {
  train_idx <- unlist(folds[-i])
  test_idx <- unlist(folds[i])
  
  lasso_y <- cv.glmnet(X[train_idx, ], Y[train_idx], alpha = 1)
  y_hat <- predict(lasso_y, X[test_idx, ], s = "lambda.min")
  residual_Y[test_idx] <- Y[test_idx] - y_hat
  
  lasso_d <- cv.glmnet(X[train_idx, ], D[train_idx], alpha = 1)
  d_hat <- predict(lasso_d, X[test_idx, ], s = "lambda.min")
  residual_D[test_idx] <- D[test_idx] - d_hat
}


cate_model <- lm(I(residual_Y / (residual_D + 1e-6)) ~ X)
cate_pred <- predict(cate_model)


cate_mse <- mean(residual_Y^2)
cate_ci_width <- 2 * 1.96 * sd(residual_Y / (residual_D + 1e-6))
cate_summary <- summary(cate_model)

cat("\n FWL-CATE Model Evaluation with Cross-Fitting:\n")
cat("MSE:", round(cate_mse, 4), "\n")
cat("95% CI Width (approx):", round(cate_ci_width, 4), "\n")

cat("\n Linear Regression Summary (CATE Model):\n")
print(cate_summary)


hist(cate_pred, breaks = 30, main = "Distribution of CATE Estimates (FWL-CATE)",
     xlab = "Estimated Treatment Effect", col = "skyblue")


coefs <- coef(cate_model)
imp_df <- data.frame(Variable = names(coefs)[-1], Importance = coefs[-1]) %>%
  arrange(desc(abs(Importance)))

ggplot(head(imp_df, 10), aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Important Features (FWL-CATE)",
       x = "Variable", y = "Coefficient (Importance)") +
  theme_minimal()



#############
# X-Learner #
#############
## Install causalToolbox from Github
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("soerenkuenzel/causalToolbox")
# library(causalToolbox) > i had issues downloading causalToolbox but there is a function X_RF for X-Learners

X_vars <- c("Gender", "Age", "Academic.Pressure", "CGPA", "Study.Satisfaction",
            "Work.Study.Hours", "Financial.Stress", "Sleep.Duration", "Depression", 
            "Suicidal.Thoughts", "Family.History", "Major.City", "STEM.Degree", "Dietary_Score")

for (v in X_vars) {
  if (is.factor(students[[v]])) {
    students[[v]] <- as.numeric(as.character(students[[v]]))
  }
}

complete_idx <- complete.cases(students[, c(X_vars, "Anxiety_Level", "Treatment")])
students_complete <- students[complete_idx, ]

X <- students_complete[, X_vars]
Y <- students_complete$Anxiety_Level
D <- students_complete$Treatment



# Split data by treatment
X_treated <- X[D == 1, ]
Y_treated <- Y[D == 1]
X_control <- X[D == 0, ]
Y_control <- Y[D == 0]

if (is.matrix(X)) X <- as.data.frame(X)
if (is.matrix(Y)) Y <- as.data.frame(Y)[,1]
full_data <- X %>% mutate(Y = Y, D = D)

prop_model <- glm(D ~ . - Y, data = full_data, family = binomial)
p_hat <- predict(prop_model, type = "response")

summary(p_hat)  # check distribution 
hist(p_hat, breaks = 30, main = "Propensity Score Distribution")

treated_data <- full_data %>% filter(D == 1)
control_data <- full_data %>% filter(D == 0)

# Stage 1: Potential Outcome Models
mu1_model <- ranger(Y ~ . - D, data = treated_data)
mu0_model <- ranger(Y ~ . - D, data = control_data)

mu1_hat <- predict(mu1_model, data = X)$predictions
mu0_hat <- predict(mu0_model, data = X)$predictions

# Stage 2: Impute Treatment Effects
tau1 <- treated_data$Y - predict(mu0_model, data = treated_data)$predictions # Treatment effect for treated
tau0 <- predict(mu1_model, data = control_data)$predictions - control_data$Y # Treatment effect for controls

# Stage 3: Train CATE Models
tau1_model <- ranger(tau1 ~ . - Y - D, data = treated_data)
tau0_model <- ranger(tau0 ~ . - Y - D, data = control_data)

tau1_model <- ranger(tau1 ~ ., data = X[D == 1, ], 
                     importance = "permutation")  # or "impurity"
tau0_model <- ranger(tau0 ~ ., data = X[D == 0, ], 
                     importance = "permutation")

# Stage 4: p(X) CATE > Kunzel weighting, same as 4231 lecture notes
tau1_hat <- predict(tau1_model, data = X)$predictions
tau0_hat <- predict(tau0_model, data = X)$predictions

# CATE as weighted sum using p(X)
cate_hat <- p_hat * tau0_hat + (1 - p_hat) * tau1_hat  # Künzel weighting

# Distribution of CATE estimates 
hist(cate_hat, 
     breaks = 30,
     main = "Distribution of CATE Estimates (X-Learner)",
     xlab = "Estimated Treatment Effect", 
     col = "lightgreen",  
     border = "white") 

# Cross-fitted MSE 
mse_components <- list(
  mu1 = mean((Y[D == 1] - mu1_hat[D == 1])^2),  # Treated outcome model
  mu0 = mean((Y[D == 0] - mu0_hat[D == 0])^2),  # Control outcome model
  tau1 = mean((tau1 - tau1_hat[D == 1])^2),     # Treated CATE model
  tau0 = mean((tau0 - tau0_hat[D == 0])^2)      # Control CATE model
)

# Overall pseudo-outcome MSE
psi <- (Y - ifelse(D == 1, mu0_hat, mu1_hat)) / (D - p_hat + 1e-6)
mse_final <- mean((psi - cate_hat)^2)

# Standard error of CATE estimates (using bootstrap) > get CI 
library(boot)
boot_fn <- function(data, indices) {
  mean(data[indices])
}
boot_results <- boot(cate_hat, boot_fn, R = 500)
ci_width <- diff(boot.ci(boot_results, type = "perc")$percent[4:5])

## Best Linear Projection for X-Learner 
# Fit BLP to X-Learner CATEs
blp_xlearner <- lm(cate_hat ~ ., data = X)  # Regress CATEs on covariates

# Get coefficients with p-values
library(broom)
blp_results <- tidy(blp_xlearner, conf.int = TRUE) %>%
  filter(term != "(Intercept)")  # Remove intercept

# Top significant modifiers
blp_results %>% 
  arrange(p.value) %>% 
  head(10)


## Feature importance 
# Get importance from both CATE models (treated and control)
imp_treated <- tau1_model$variable.importance
imp_control <- tau0_model$variable.importance

mean_p_hat <- mean(p_hat)
combined_imp <- mean_p_hat * imp_control + (1 - mean_p_hat) * imp_treated

# importance dataframe
imp_df <- data.frame(
  Variable = names(combined_imp),
  Importance = combined_imp,
  Treated_Imp = imp_treated,
  Control_Imp = imp_control,
  row.names = NULL
) %>% 
  arrange(desc(Importance)) %>%
  mutate(Variable = factor(Variable, levels = Variable))

ggplot(head(imp_df, 10), aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(aes(fill = "Weighted Average"), width = 0.7) +
  geom_point(aes(y = Treated_Imp, color = "Treated Arm"), size = 3) +
  geom_point(aes(y = Control_Imp, color = "Control Arm"), size = 3) +
  scale_fill_manual(values = "steelblue") +
  scale_color_manual(values = c("orange", "lightgreen")) +
  coord_flip() +
  labs(title = "X-Learner Feature Importance",
       subtitle = "Blue: Weighted Average | Orange: Treated | Green: Control",
       x = "Feature",
       y = "Importance Score") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())



#####################################
# Check high and low CATE subgroups #
#####################################
cate_median <- median(cate_hat)
full_data <- X %>% mutate(CATE_group = ifelse(cate_hat >= cate_median, "High", "Low"))

# Based on importance scores from models
covariates <- c("Work.Study.Hours", "Depression", "Financial.Stress", "Academic.Pressure")

# Compare covariate balance across CATE groups
table1 <- CreateTableOne(vars = covariates, strata = "CATE_group", data = full_data, test = TRUE)
print(table1, smd = TRUE)  # show standardised mean differences (SMD)


#####################
# Subgroup Analysis #
#####################
library(ggplot2)
library(dplyr)
library(patchwork)
library(grf)

tree <- get_tree(cf, 1) 

var_imp <- variable_importance(cf)
top_vars <- colnames(X)[order(-var_imp)][1:3]

# Create data frame for plotting
plot_data <- data.frame(
  CATE = tau_hat_cf,
  Work.Study.Hours = X[,top_vars[1]], 
  Academic.Pressure = X[,top_vars[2]], 
  Financial.Pressure = X[,top_vars[3]]
)

# Convert continuous variables to categories
plot_data <- plot_data %>%
  mutate(
    Work.Study.Hours = cut(Work.Study.Hours, 
                           breaks = c(-Inf, 4, 7, 10, Inf),
                           labels = c("0-4", "5-7", "8-10", "11+")),
    Academic.Pressure = cut(Academic.Pressure,
                            breaks = c(-Inf, 2, 3, 4, Inf),
                            labels = c("Low", "Medium", "High", "Very High")),
    Financial.Pressure = cut(Financial.Pressure,
                             breaks = c(-Inf, 2, 3, 4, Inf),
                             labels = c("Low", "Medium", "High", "Very High"))
  )

create_plot <- function(data, x_var, y_var, title, split_value, importance_score) {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(x_var))) +
    geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.shape = 16, 
                 coef = 1.5, width = 0.7) +
    stat_summary(
      fun = median,
      geom = "text",
      aes(label = sprintf("%.2f", ..y..)),
      vjust = -0.8,
      color = "black",
      size = 4,
      fontface = "bold"
    ) +
    scale_fill_viridis_d(
      option = "D", 
      direction = -1,
      begin = 0.2,
      end = 0.9
    ) +
    labs(
      title = title,
      subtitle = sprintf("Split at %s | Importance: %.3f", split_value, importance_score),
      x = gsub("\\.", " ", x_var),
      y = "CATE"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, 
                                size = 14 + importance_score * 5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 11),
      legend.position = "none",
      axis.text.x = element_text(
        angle = ifelse(x_var == "Work.Study.Hours", 45, 0),
        hjust = ifelse(x_var == "Work.Study.Hours", 1, 0.5)
      ),
      panel.grid.major.x = element_blank()
    ) +
    geom_vline(
      xintercept = ifelse(x_var == "Work.Study.Hours", 7.5,
                          ifelse(x_var == "Academic.Pressure", 3.5, 2.5)),
      linetype = "dashed", 
      color = "darkred",
      linewidth = 1.2
    )
}

p_work <- create_plot(synth_data, "Work.Study.Hours", "CATE_Work",
                      "Work Study Hours", "7", 0.427)

p_academic <- create_plot(synth_data, "Academic.Pressure", "CATE_Academic",
                          "Academic Pressure", "4", 0.233)

p_financial <- create_plot(synth_data, "Financial.Pressure", "CATE_Financial",
                           "Financial Pressure", "3", 0.196)

# Combine plots
combined_plot <- (p_work / (p_academic + p_financial)) +
  plot_annotation(
    title = "CATE Distribution for Subgroups",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
    )
  ) +
  plot_layout(heights = c(1.5, 1)) # Make Work.Study.Hours plot the biggest


combined_plot