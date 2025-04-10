# DSE4101 Capstone Project in Data Science and Economics I  
## Anxiety in Academia: Causal Insights on Mental Health Disparities in Higher Education  

---

### 📌 Table of Contents  
- [Project Overview](#project-overview)  
- [Repository Structure](#repository-structure)  
- [Installation and Setup](#installation-and-setup)  

---

### Project Overview 
This study examines the impact of postgraduate degree attainment on student mental health to answer a critical question: How does this educational milestone affect psychological well-being, and who are most vulnerable? The primary methodological approach involves implementing Causal Forest to estimate Conditional Average Treatment Effect (CATE) and extract key features driving treatment heterogeneity. Additionally, Double Machine Learning (DML), Frisch-Waugh-Lovell (FWL) Partialling Out, and X-Learner will serve as supplementary methods to validate results and evaluate the robustness of the study. By analysing the differences in subgroup responses to academic pressures, this study seeks to contribute to the growing field of mental health research in higher education and provide actionable insights for university policymakers.

---
### Repository Structure  
```plaintext
├── /data/                   # Raw dataset
├── data_preprocessing_and_exploration.R  # EDA + cleaning (run first)
├── combined.R               # Master script (all models + evaluation)
├── # Individual models
│   ├── causalforest.R       # Causal forest 
│   ├── fwl_partiallingout.R # fwl
│   ├── doublemachinelearning.R # dml
│   └── xlearner.R           # X-learner
└── subgroup_analysis.R      # CATE subgroup investigation
```
---

### Installation and Setup
1. Clone the repository
2. Install R packages:
```r
install.packages(c(
  "dplyr",         
  "ggplot2",       
  "tidyverse",    
  "reshape2",      
  "corrplot",     
  "tidyr",         
  "grf",          
  "glmnet",       
  "causalweight", 
  "randomForest", 
  "caret",         
  "tidymodels",  
  "ranger",        
  "patchwork"      
))

--- 
