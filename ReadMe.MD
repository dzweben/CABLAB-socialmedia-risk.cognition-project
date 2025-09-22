# Cognitive Mechanisms & Social Media Dimensions — README

## Overview
This repository contains R Markdown code and analysis pipelines for two related studies examining how **social media/smartphone use (SSM)** relates to **risk-taking behavior** and its **cognitive underpinnings**.

- **Study 1 (`DEFT_coding.rmd`)**  
  - Undergraduate sample (Temple University).  
  - Focus: Do **impulsivity, reward sensitivity, and exploration** explain the SSM–risk relationship?  
  - Examines **social vs. parasocial platform use**.  

- **Study 2 (`lite2-deft_coding.rmd`)**  
  - Larger developmental sample (adolescents + young adults).  
  - Replicates Study 1 analyses with broader age coverage.  
  - Includes newer platforms (e.g., TikTok).  
  - Uses REDCap-based data pulls.  

---

## What the Code Does

### Common Preprocessing
- Import raw data (`.csv` from REDCap or `.xlsx` from Excel).  
- Clean variables (string trimming, recodes for time-use categories).  
- Standardize (z-scores) cognitive, risk, and engagement measures.  
- Remove outliers (> ±3 SD).  
- Build composite scores:  
  - `mtes_zscore` (Mobile Technology Engagement Scale)  
  - `social_score` and `parasocial_score`  
  - `total_risk` (average of positive/negative risk + YRBS)  

---

### Analyses in `DEFT_coding.rmd` (Study 1)
1. **Correlations**  
   - MTES, impulsivity, sensation seeking (SS), exploration, risk measures.  
   - Full correlation matrices with r/p formatting.  

2. **Hierarchical Regressions**  
   - Predict `parasocial_score` and `social_score` from:  
     1. Gender + impulsivity  
     2. + reward sensitivity  
     3. + exploration  
     4. + PNRT/BRP (positive/negative risk)  
   - Incremental R² change tests.  

3. **Mediation Analyses**  
   - Test whether **Risk Variety** mediates:  
     - Reward sensitivity → MTES  
     - Exploration → MTES  
     - Impulsivity → MTES  

4. **Shapley Value Regressions**  
   - Partition R² across predictors (impulsivity, SS, exploration, risk).  
   - Output: % contribution tables.  

5. **Platform Contrasts (Steiger’s tests)**  
   - Compare whether predictors are more strongly correlated with **social vs parasocial platforms**.  

6. **Overall Correlations & Double-Check Mediation**  
   - Full numeric correlations for all continuous variables.  
   - Bootstrapped indirect effect calculation as a secondary check.  

---

### Analyses in `lite2-deft_coding.rmd` (Study 2)
1. **Cohort Splits**  
   - Teen sample (`PID < 3000`)  
   - Adult sample (`PID > 3000`)  

2. **Composite & Variety Risk Scores**  
   - Generate composite indices of risk-taking.  
   - Compute **Risk Variety** scores indexing engagement across multiple categories.  

3. **Correlations**  
   - MTES, cognitive constructs, `total_risk`.  
   - Separate matrices for teens and adults.  

4. **Reliability Analyses**  
   - Compute Cronbach’s alpha for each cognitive and risk-taking scale.  
   - Verify internal consistency before proceeding to composites.  

5. **Mediation Analyses**  
   - Teen and adult models separately.  
   - Test indirect effects of Zuckerman (SS) and Barretts (Impulsivity) via **total_risk → MTES**.  

6. **Hierarchical Regressions**  
   - Teen and adult regressions predicting MTES from:  
     1. Gender + impulsivity  
     2. + sensation seeking  
     3. + exploration  
     4. + YRBS + PNRT subscales (positive/negative risk).  
   - Incremental R² and standardized betas reported.  

7. **Steiger’s Tests**  
   - Compare correlations between predictors and **social vs parasocial engagement**.  
   - Run separately for teen and adult cohorts.  
   


8. **Shapley Value Regressions**  
   - Partition variance in MTES for each cohort.  
   - Predictors: `total_risk`, sensation seeking, impulsivity, sex.  


---

## Requirements
- R (≥4.0)  
- Packages: `dplyr`, `tidyr`, `stringr`, `readxl`, `psych`, `Hmisc`, `cocor`, `mediation`, `ShapleyValue`, `lm.beta`, `kableExtra`  

Install with:
```r
install.packages(c("dplyr","tidyr","stringr","readxl","psych","Hmisc","cocor","mediation","lm.beta","kableExtra"))
remotes::install_github("nliulab/ShapleyValue")
