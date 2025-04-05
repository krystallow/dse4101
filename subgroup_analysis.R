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