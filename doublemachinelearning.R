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
