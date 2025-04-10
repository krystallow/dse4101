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
print(blp_cf)

cf_mse <- mean((Y - predict(cf)$predictions)^2)
round(cf_mse, 4)

ci_width_cf <- mean(2 * 1.96 * tau_se_cf)
round(ci_width_cf, 4)

var_imp <- variable_importance(cf)
var_names <- colnames(X)
importance_df <- data.frame(Variable = var_names, Importance = var_imp)
importance_df <- importance_df[order(-importance_df$Importance), ]

head(importance_df, 10)

hist(tau_hat_cf, breaks = 30, main = "Distribution of CATE Estimates (Causal Forest)",
     xlab = "Estimated Treatment Effect", col = "lightblue")

tree <- get_tree(cf, 1)  # Extracts the first tree from the Causal Forest
print(tree)

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

mean(abs(tau_hat_cf - tau_hat_cv)
