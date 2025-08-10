# 🔋 Energy Consumption Analysis – Predictive Modeling & Seasonal Trends  

## 📌 Project Overview  
This project analyzes **building energy consumption patterns** using statistical modeling, correlation studies, and seasonal trend analysis.  
The goals are to:  
- Predict monthly energy usage from building characteristics.  
- Assess the impact of seasonal variations (summer vs. winter) on energy demand.  
- Identify key factors influencing consumption.  
- Forecast future energy usage patterns.  

---

## 📂 Repository Contents  
- **`Capstone_11_Module 6_Discussion_Final Project.pdf`**  
  Final report containing:
  - Data cleaning & preprocessing steps  
  - Predictive modeling using GLM + Lasso  
  - Seasonal impact analysis with Kruskal-Wallis test  
  - Correlation studies between occupancy and usage  
  - Time series forecasting (ETS)  
  - Visualizations and insights  

- **`Energy_Consumption_Analysis.twbx`**  
  Tableau workbook with interactive dashboards visualizing:
  - Monthly energy consumption patterns  
  - Seasonal variations in usage  
  - Key predictors from the statistical models  
  - Forecast trends for upcoming months  

---

## 📊 Key Insights  
1. **Prediction Accuracy** – GLM with Lasso achieved R² = 0.9991 and RMSE = 19,623.  
2. **Top Predictors** – Total annual KWH, peak month usage (January & June), and occupancy percentage.  
3. **Seasonal Impact** – Summer shows highest variability due to cooling demand; winter usage driven by heating.  
4. **Occupancy Correlation** – Weak positive correlation (0.22) between occupancy and energy usage.  
5. **Forecast Trends** – ETS model predicts stable summer usage and higher winter uncertainty.  

---

## 🛠 Methodology  
- **Data Cleaning** – Removed irrelevant features, handled outliers, converted categorical data.  
- **Predictive Modeling** – GLM with L1 regularization (Lasso) for feature selection.  
- **Statistical Testing** – Spearman correlation & Kruskal-Wallis seasonal comparisons.  
- **Forecasting** – ETS model for time series predictions.  

---

## 📦 Technologies Used  
- **Language:** R  
- **Libraries:** `tidyverse`, `dplyr`, `ggplot2`, `glmnet`, `forecast`  
- **Visualization:** Tableau, ggplot2, plotly  

---

## 📜 License  
This project is for academic and research purposes only.  
