# DSE4101 Capstone Project in Data Science and Economics I  
## Anxiety in Academia: Causal Insights on Mental Health Disparities in Higher Education  

---

### 📌 Table of Contents  
- [Project Overview](#-project-overview)  
- [Repository Structure](#-repository-structure)  
- [Installation & Setup](#%EF%B8%8F-installation--setup)  
- [Usage](#-usage)  
- [Outputs](#-outputs)  
- [References](#-references)  

---

### 🎯 Project Overview  
``` This study examines the impact of postgraduate degree attainment on student mental health to answer a critical question: How does this educational milestone affect psychological well-being, and who are most vulnerable? The primary methodological approach involves implementing Causal Forest to estimate Conditional Average Treatment Effect (CATE) and extract key features driving treatment heterogeneity. Additionally, Double Machine Learning (DML), Frisch-Waugh-Lovell (FWL) Partialling Out, and X-Learner will serve as supplementary methods to validate results and evaluate the robustness of the study. By analysing the differences in subgroup responses to academic pressures, this study seeks to contribute to the growing field of mental health research in higher education and provide actionable insights for university policymakers. 
---

### 📂 Repository Structure  
```plaintext
.
├── /data/                   # Raw dataset
├── data_preprocessing_and_exploration.R  # EDA + cleaning (run first)
├── combined.R               # Master script (all models + evaluation)
├── /models/                 # Individual model implementations
│   ├── causalforest.R       # Causal forest (grf)
│   ├── fwl_partiallingout.R # Double/debiased ML
│   └── xlearner.R           # X-learner meta-algorithm
└── subgroup_analysis.R      # CATE subgroup investigation
