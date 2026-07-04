# NHANES Sleep & Physical Activity - Survey-Weighted Epidemiological Analysis
**NHANES 2017–2018 | R | Survey-Weighted Logistic Regression**

## Research Question
Does physical activity predict healthy sleep duration (7-9.5 hours) in U.S. adults, and how do demographic and clinical cofactors modify this relationship?

## Overview
This project examines associations between five domains of physical activity, sedentary behavior, and sleep duration using NHANES 2017–2018 data. All analyses use proper complex survey design methods to generate nationally representative estimates for the U.S. adult population.

The pipeline pulls raw data directly from the CDC using the nhanesA package — no manual downloads, no hardcoded local paths, fully reproducible from scratch.

## Key Findings
- Physical activity (across 5 MET-based domains) was not significantly associated with healthy sleep duration
- Sedentary time (PAD680) showed marginal significance (p = 0.04, OR = 1.004)
- Race/ethnicity, gender, and education emerged as stronger predictors than activity levels
- 152 clinically undiagnosed diabetic participants identified by cross-referencing HbA1c lab values against physician diagnosis
- Findings suggest sleep health is shaped more by social determinants than lifestyle behaviors alone
