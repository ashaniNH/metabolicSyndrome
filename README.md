# metabolic-syndrome-risk-prediction
# 🩺 Metabolic Syndrome Risk Analysis and Prediction

## 📌 Project Overview
This project analyzes and predicts **Metabolic Syndrome**, a medical condition that significantly increases the risk of heart disease, stroke, and type 2 diabetes.  

Using a Kaggle dataset containing **2,402 individuals**, the study combines demographic information and clinical biomarkers to develop machine learning models capable of identifying individuals at risk.

The goal is to build an accurate and interpretable predictive system that supports **early risk screening and data-driven healthcare decision-making**.

---

## 📊 Dataset Description

- Source: Kaggle Dataset
- Observations: 2,402 individuals
- Problem Type: Binary Classification
- Target Variable: `MetabolicSyndrome` (Yes / No)

### Features Included

#### Demographic Variables
- Age
- Sex
- Race
- Marital Status
- Income Level

#### Clinical & Laboratory Measures
- Waist Circumference
- Body Mass Index (BMI)
- Albuminuria
- Urinary Albumin-to-Creatinine Ratio
- Uric Acid
- Blood Glucose
- HDL Cholesterol
- Triglycerides

---

## 🔎 Project Workflow

### 1️⃣ Data Preprocessing
- Data type conversion
- Missing value handling and imputation
- Quantile transformation for skewed clinical variables
- Stratified train–test split
- Outlier detection and analysis

---

### 2️⃣ Exploratory Data Analysis (EDA)
- Distribution of metabolic syndrome prevalence
- Associations with:
  - Albuminuria
  - Age groups
  - Core metabolic biomarkers
- Correlation heatmap analysis
- FAMD (Factor Analysis for Mixed Data)
  - Dimension reduction
  - Factor loading interpretation

---

### 3️⃣ Machine Learning Models
The following classification models were trained and compared:

- Logistic Regression
- Random Forest
- Support Vector Machine (SVM)
- XGBoost

---

## 📈 Model Evaluation

Evaluation metrics:
- Accuracy
- Sensitivity (Recall)
- F1-score

| Model | Training Performance | Test Performance | Observation |
|------|---------------------|-----------------|-------------|
| Random Forest | Very High | Lower | Overfitting |
| XGBoost | Very High | Lower | Overfitting |
| SVM | High | Moderate | Slight overfitting |
| Logistic Regression | **≈ 0.85 Accuracy** | **≈ 0.85 Accuracy** | Best generalization |

---

## ⭐ Key Findings
- Tree-based and boosting models achieved strong training accuracy but showed reduced generalization.
- Logistic Regression demonstrated stable performance across training and testing datasets.
- Clinical indicators such as glucose, triglycerides, waist circumference, and BMI strongly influence metabolic syndrome risk.
- Interpretable models can be highly effective for real-world healthcare prediction tasks.

---

## 🛠️ Technologies Used
- R
- tidyverse
- caret
- FactoMineR (FAMD)
- randomForest
- xgboost
- e1071 (SVM)
- ggplot2

---

## 🚀 Results & Impact
The study shows that metabolic syndrome risk can be effectively predicted using routine clinical and demographic data. The resulting model provides a practical foundation for **early screening systems and clinical decision support tools**.
