# Load in libraries 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(corrplot) 

# Load in data
students <- read.csv("../data/Analyzing Mental Health Trends and Predictors Among Students.csv")

#################
# Data Cleaning #
#################
# Assign binary treatment based on postgraduate or undergraduate degrees
students$Treatment <- ifelse(grepl("M.|PhD", students$Degree), 1, 0)

# Only accept valid cities, since I utilised survey data, there were some invalid entries within the `City` Column
valid_cities <- c("Chennai", "Vadodara", "Ahmedabad", "Lucknow", "Pune", "Rajkot", "Meerut", "Ludhiana", "Nagpur", 
                  "Srinagar", "Vasai-Virar", "Nashik", "Thane", "Kalyan", "Kolkata", "Visakhapatnam", "Agra", 
                  "Kanpur", "Delhi", "Jaipur", "Bangalore", "Patna", "Bhopal", "Surat", "Mumbai", "Faridabad", 
                  "Ghaziabad", "Hyderabad", "Varanasi", "Indore")

students <- students %>% 
  filter(City %in% valid_cities) %>%
  filter(Profession == "Student") %>% # remove observations that are not studnets
  filter(Degree != "Class 12") # remove high school students, only accepting undergraduate and postgraduate students

students <- drop_na(students) # remove na entries

# Replace survey responses with binary values
students <- students %>%
  mutate(
    Suicidal_Thoughts = ifelse(`Have.you.ever.had.suicidal.thoughts..` == "Yes", 1, 0),
    Family_History = ifelse(`Family.History.of.Mental.Illness` == "Yes", 1, 0)
  )

# Convert to proper data type
students_cleaned <- students %>%
  mutate(
    Gender = as.factor(Gender),
    City = as.factor(City),
    Dietary.Habits = as.factor(Dietary.Habits),
    Degree = as.factor(Degree),
    Treatment = as.numeric(Treatment) 
  ) %>%
  select(-`Have.you.ever.had.suicidal.thoughts..`, -`Family.History.of.Mental.Illness`) # remove unnecessary column

# Save cleaned data
write.csv(students, file = "../data/cleaned_dataset.csv", row.names = FALSE)

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

Y <- students_cleaned$Anxiety_Level
str(D)
str(Y)
str(X_df)
str(X)


#####################
# Assumption Checks #
#####################

# Check for multi-collinearity 
cor_matrix <- cor(X_df)
print(cor_matrix)

# Change to long format for plotting
cor_matrix_melted <- melt(cor_matrix)

# Plot the correlation heatmap
ggplot(data = cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme_minimal() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Correlation Heatmap")

# Check for overlap assumption
ps <- glm(D ~ ., data = cbind(D, X_df), family = binomial(logit))$fitted

hist(ps[D==1])
hist(ps[D==0])

summary(ps[D==1])
summary(ps[D==0])

# Plot for overlap assumption: Overlapping histogram for distribution 
ggplot(data.frame(ps = ps, D = D), aes(x = ps, fill = as.factor(D))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Propensity Score Distribution", x = "Propensity Score", fill = "Treatment Status") +
  theme_minimal()

# Box plot for distribution 
ggplot(data.frame(ps = ps, D = D), aes (x = ps, fill = as.factor(D))) + 
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Propensity Scores by Treatment Status",
       x = "Treatment Status", 
       y = "Propensity Score") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) 


##############################
# Visualise Outcome Variable #
##############################
ggplot(students, aes(x = Anxiety.Level, fill = factor(Treatment))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Anxiety Level by Treatment",
       x = "Anxiety Level", fill = "Treatment") +
  theme_minimal()
