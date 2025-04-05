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