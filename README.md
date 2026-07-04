# NHANES Sleep & Physical Activity - Survey-Weighted Epidemiological Analysis
**NHANES 2017–2018 | R | Survey-Weighted Logistic Regression**


## Research Question

Does physical activity predict healthy sleep duration (7-9.5 hours) in U.S. adults, and how do demographic and clinical cofactors modify this relationship?


## Overview

This project examines associations between five domains of physical activity, sedentary behavior, and sleep duration using NHANES 2017–2018 data. All analyses use proper complex survey design methods to generate nationally representative estimates for the U.S. adult population.

The pipeline pulls raw data directly from the CDC using the `nhanesA` package — no manual downloads, no hardcoded local paths, fully reproducible from scratch.


## Key Findings

- Physical activity (across 5 MET-based domains) was **not significantly associated** with healthy sleep duration
- Sedentary time (PAD680) showed marginal significance (p = 0.04, OR = 1.004)
- **Race/ethnicity, gender, and education** emerged as stronger predictors than activity levels
- 152 clinically undiagnosed diabetic participants identified by cross-referencing HbA1c lab values (≥6.5%) against physician diagnosis — included in the final analytic variable
- Findings suggest sleep health is shaped more by **social determinants** than lifestyle behaviors alone

## Data Source

| Module | NHANES File | Variables |
|--------|-------------|-----------|
| Physical Activity | PAQ_J | PAQ605-PAD680 (5 domains, MET conversion) |
| Sleep | SLQ_J | SLD012, SLD013, SLQ030-SLQ120 |
| Body Measures | BMX_J | BMXBMI |
| Blood Pressure | BPX_J | BPXSY1–BPXDI4 (averaged) |
| Smoking | SMQ_J | SMQ020, SMQ040 |
| Demographics | DEMO_J | RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, INDFMPIR, WTMEC2YR, SDMVPSU, SDMVSTRA |
| Diabetes | GHB_J + DIQ_J | LBXGH (HbA1c) + DIQ010 (physician diagnosis) |

**Final analytic dataset:** 9,254 participants

## Pipeline

CDC (nhanesA) → Module Cleaning (01–03) → Merge (04) → Analysis (05)



## Methods

**Outcome:** Binary sleep duration - healthy (7-9.5 hours = 1) vs unhealthy (< 7 or > 9.5 hours = 0)

**Predictors:** Five MET-based physical activity domains + sedentary time (PAD680)

**Covariates:** Age, sex, race/ethnicity, BMI, education, income, smoking, diabetes

**Survey Design:** Complex survey design with NHANES sampling weights (WTMEC2YR), PSU (SDMVPSU), and strata (SDMVSTRA) using the `survey` package

**Models:**
- Unadjusted - physical activity only (svyglm)
- Unadjusted - sedentary time only (svyglm)
- Combined - PA + sedentary (svyglm)
- Fully adjusted - PA + sedentary + all covariates (svyglm)

**Statistical Tests:** Survey-weighted t-test (svyttest) for continuous variables, survey-weighted chi-square (svychisq) for categorical variables

###How to Reproduce


```r
# Install required packages
install.packages(c("nhanesA", "dplyr", "survey", "gtsummary",
                   "ggplot2", "corrplot", "DiagrammeR", "glue"))

# Run scripts in order
source("R_Scripts/HBNS_01_physical_activity.R")
source("R_Scripts/HBNS_02_sleep.R")
source("R_Scripts/HBNS_03_bmi_bp_smoking_diabetes_demographics.R")
source("R_Scripts/HBNS_04_merge.R")
source("R_Scripts/HBNS_05_analysis.R")
```

No local data files needed - all data pulled directly from CDC at runtime.

## Tools & Packages

| Category | Tools |
|----------|-------|
| Data Access | nhanesA |
| Data Wrangling | dplyr, tidyr |
| Survey Analysis | survey, svyglm, svyttest, svychisq |
| Tables | gtsummary, tbl_svysummary |
| Visualization | ggplot2, corrplot |
| Flow Diagram | DiagrammeR, glue |


## Author

**Nousheen Jahan Shaik**
M.S. Bioinformatics & Data Science - University of Delaware
[LinkedIn](https://linkedin.com/in/nousheenjahan)

NHANES-Sleep-PhysicalActivity-Analysis
Analysis of NHANES 2017-2018 physical activity data, focusing on moderate vs vigorous activity patterns and data preprocessing in R


