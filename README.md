## 🏠 What Moves the U.S. Housing Market?

## Project Overview

This project analyzes **what drives short-run fluctuations in U.S. new single-family home sales** using monthly macroeconomic data from **1991–2024**.
The focus is on **cyclical movements**, not long-term trends, combining **econometric modeling** with **time-series analysis**.

---

## Short Description

Using detrended macroeconomic variables and **OLS + ARIMAX models**, this project identifies how interest rates, mortgage costs, labor markets, housing supply, and consumer sentiment influence housing sales over time.

---

## Technology / Tools Used

* **R**
* Libraries: `tidyverse`, `ggplot2`, `readxl`, `lubridate`, `psych`, `corrplot`
* Econometric Methods:
  * OLS Regression
  * Time-Series Detrending
  * ARIMAX Modeling
  * Residual Diagnostics (ACF, PACF, Ljung–Box)

---

## Key Features

* Cleaned and merged **multiple U.S. macroeconomic datasets**
* Exploratory Data Analysis with visualizations
* Linear detrending to avoid spurious correlations
* Comparison of **static vs dynamic models**
* Robust time-series diagnostics
* Interpretation focused on **real economic intuition**

---

## Project Process

1. **Data Collection & Cleaning**
   Combined housing, interest rate, labor market, price, and sentiment data.
2. **Exploratory Data Analysis**
   Summary statistics, trends, correlations, and scatterplots.
3. **Detrending**
   Removed long-run trends to isolate short-run deviations.
4. **Modeling**
   * Baseline detrended OLS
   * Extended OLS (COVID dummy, interactions)
   * Dynamic ARIMAX models
5. **Model Selection & Validation**
   Used AIC, residual diagnostics, and Ljung–Box tests.
6. **Economic Interpretation**
   Translated statistical results into market insights.

---

## What I Learned

* How to handle **time-series data properly** in applied econometrics
* Why detrending is critical in macroeconomic analysis
* Limitations of static OLS for dynamic systems
* Practical use of **ARIMAX models** for economic forecasting
* Interpreting results beyond coefficients and p-values

---

## Overall Growth

This project strengthened my ability to:

* Think like a **quantitative analyst**, not just a coder
* Connect **economic theory with statistical evidence**
* Build models that are both **technically sound and interpretable**
* Communicate complex results clearly

---

## How This Project Can Be Improved

* Use **regional or state-level data** instead of national aggregates
* Apply **non-linear detrending or filtering methods**
* Add **forecasting and out-of-sample validation**
* Explore **causal identification techniques** (IVs, natural experiments)

---

## Running This Project

### 1. Clone the Repository

* Clone the repository to your local machine

### 2. Open the Project

* Open the project folder in **RStudio or VS Code**
* Ensure required R packages are installed

### 3. Run the Main Script

* Execute the Project main script `.R` file from top to bottom
* All figures, diagnostics, and results will be generated automatically

---


