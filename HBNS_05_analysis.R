# ── Analysis Script ───────────────────────────────────────────────────────────
# NHANES 2017-2018 | HBNS Project
# Goal: Descriptive analysis of sleep, physical activity, and covariates

library(readr)
library(dplyr)
library(gtsummary)
library(labelled)
library(corrplot)
library(broom.helpers)
library(parameters)
library(survey)
library(DiagrammeR)
library(ggplot2)
library(gridExtra)
# ── STEP 1: Load data and setup ───────────────────────────────────────────────
project <- read_csv("/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/HBNS_Final_Dataset.csv",
                    show_col_types = FALSE)

# initial look
str(project)
names(project)
dim(project)
nrow(project)
ncol(project)
sapply(project, class)
summary(project)
colSums(is.na(project))

# standardize diabetes variable
project$DM_diag <- ifelse(is.na(project$diabetes_final), NA,
                          ifelse(project$diabetes_final == 1, 1, 0))

# check distribution
table(project$DM_diag, useNA = "ifany")

# survey design
nhanes_design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  data = project,
  nest = TRUE
)

# quick check
head(project[, c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "DM_diag")])

# ── STEP 2: Descriptive Statistics ───────────────────────────────────────────
# continuous columns
continuous_cols <- names(project)[sapply(project, is.numeric)]
continuous_cols

# sleep outcome distribution
table(project$SLD012)
prop.table(table(project$SLD012))

# MET variable summaries
met_vars <- c("MET_Vigorous_work", "MET_Moderate_work", "MET_Walk_bike",
              "MET_Vigorous_recreation", "MET_Moderate_recreation")

for (var in met_vars) {
  cat("\n--------------------------------------\n")
  cat("Descriptive stats for:", var, "\n")
  print(summary(project[[var]]))
}

# correlation heatmap for MET variables only
met_cor <- cor(project[, met_vars], use = "pairwise.complete.obs")
corrplot(met_cor, method = "color",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.9, tl.col = "black",
         title = "Correlation Heatmap: Physical Activity (MET Variables)",
         mar = c(0,0,2,0))

# sedentary behavior summary
summary(project$PAD680)

# ── STEP 3: Physical Activity + Sedentary Correlation ────────────────────────
# adding PAD680 (sedentary time) to the correlation matrix
corr_vars <- c(met_vars, "PAD680")
cor_matrix <- cor(project[, corr_vars], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.9, tl.col = "black",
         title = "Correlation Heatmap: Physical Activity and Sedentary Time",
         mar = c(0,0,2,0))

# ── STEP 4: Missingness and Complete Cases ────────────────────────────────────
colSums(is.na(project))
colSums(is.na(project)) / nrow(project)

# complete cases for main model variables
model_vars <- c("SLD012", met_vars, "PAD680", "RIDAGEYR", "RIAGENDR",
                "BMXBMI", "RIDRETH3", "SMQ020")
complete_cases <- complete.cases(project[, model_vars])
sum(complete_cases)
mean(complete_cases)

# ── STEP 5: Continuous and Categorical Covariate Summaries ───────────────────
continuous_cov <- c("RIDAGEYR", "BMXBMI", "INDFMPIR", "SBP_mean", "DBP_mean")
for (var in continuous_cov) {
  cat("\n--------------------------------------\n")
  cat("Descriptive stats for:", var, "\n")
  print(summary(project[[var]]))
}

categorical_cov <- c("RIAGENDR", "RIDRETH3", "DMDEDUC2", "SMQ020")
for (var in categorical_cov) {
  cat("\n--------------------------------------\n")
  cat("Frequency table for:", var, "\n")
  print(table(project[[var]], useNA = "ifany"))
  print(prop.table(table(project[[var]], useNA = "ifany")))
}

# ── STEP 6: Diabetes Distribution Check ──────────────────────────────────────
# DM_diag already standardized in step 1 - just verifying distribution here
table(project$DM_diag, useNA = "ifany")


# ── STEP 7: Create derived variables for analysis ─────────────────────────────
# these are not repeating module cleaning - these are analysis-specific variables

# sleep status binary - healthy vs unhealthy
project <- project %>%
  filter(!is.na(SLD012)) %>%
  mutate(SleepStatus = ifelse(SLD012 == 1, 1, 0))

# BP category - add labels to numeric codes
project$BP_category <- factor(project$BP_category,
                              levels = c(0, 1, 2, 3),
                              labels = c("Hypotension", "Normal", "Elevated", "High"))

# education - handle NAs (asked only to adults 18+, some not asked)
project$Edu_clean <- ifelse(is.na(project$DMDEDUC2), "Not_Asked", project$DMDEDUC2)

# income - group poverty income ratio into 3 categories
project$IncomeGroup <- cut(project$INDFMPIR,
                           breaks = c(-Inf, 1.3, 3.0, Inf),
                           labels = c("Low_Income", "Middle_Income", "High_Income"))
project <- project %>%
  mutate(
    Race_collapsed = case_when(
      RIDRETH3 == "Non-Hispanic White" ~ "Non-Hispanic White",
      RIDRETH3 == "Non-Hispanic Black" ~ "Non-Hispanic Black",
      RIDRETH3 == "Non-Hispanic Asian" ~ "Non-Hispanic Asian",
      RIDRETH3 %in% c("Mexican American",
                      "Other Hispanic",
                      "Other Race - Including Multi-Racial") ~ "Other/Hispanic",
      TRUE ~ NA_character_
    ),
    Race_collapsed = factor(Race_collapsed,
                            levels = c("Non-Hispanic White",
                                       "Non-Hispanic Black",
                                       "Non-Hispanic Asian",
                                       "Other/Hispanic"))
  )
# quick check
# quick check
table(project$SleepStatus, useNA = "ifany")
table(project$BP_category, useNA = "ifany")
table(project$Edu_clean, useNA = "ifany")
table(project$IncomeGroup, useNA = "ifany")
table(project$Race_collapsed, useNA = "ifany")  # add this

# ── STEP 8: Rebuild survey design after filtering ─────────────────────────────
# rebuilding after filter(!is.na(SLD012)) in step 7
nhanes_design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  data = project,
  nest = TRUE
)

# ── STEP 9: Survey Weighted Table 1 ──────────────────────────────────────────
table1_weighted <- tbl_svysummary(
  nhanes_design,
  by = SleepStatus,
  include = c(
    RIDAGEYR, BMXBMI, PAD680,
    MET_Vigorous_work, MET_Moderate_work, MET_Walk_bike,
    MET_Vigorous_recreation, MET_Moderate_recreation,
    RIAGENDR, RIDRETH3, Edu_clean, IncomeGroup,
    SMQ020, DM_diag
  ),
  statistic = list(
    all_continuous()  ~ "{mean} ± {sd}",
    all_categorical() ~ "{p}%"
  ),
  missing = "ifany"
) %>%
  add_n() %>%
  add_p() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1. Weighted Characteristics by Sleep Status**")

table1_weighted

# ── STEP 10: Survey Weighted Statistical Tests ────────────────────────────────
# continuous variables - survey weighted t-test
continuous_vars <- c("RIDAGEYR", "BMXBMI", "PAD680",
                     "MET_Vigorous_work", "MET_Moderate_work",
                     "MET_Walk_bike", "MET_Vigorous_recreation",
                     "MET_Moderate_recreation")

for (var in continuous_vars) {
  cat("\n--------------------------------------\n")
  cat("Survey weighted t-test for:", var, "\n")
  formula <- as.formula(paste(var, "~ SleepStatus"))
  result <- svyttest(formula, nhanes_design)
  print(result)
}

# categorical variables - survey weighted chi-square
categorical_vars <- c("RIAGENDR", "RIDRETH3", "Edu_clean",
                      "IncomeGroup", "SMQ020", "DM_diag")

for (var in categorical_vars) {
  cat("\n--------------------------------------\n")
  cat("Survey weighted chi-square for:", var, "\n")
  formula <- as.formula(paste("~ SleepStatus +", var))
  tbl <- svytable(formula, nhanes_design)
  result <- svychisq(formula, nhanes_design)
  print(result)
}

# ── STEP 11: Weighted Distribution Plots ──────────────────────────────────────
library(ggplot2)

cb_palette <- c(
  "#1f77b4", # blue
  "#ff7f0e", # orange
  "#2ca02c", # green
  "#d62728", # red
  "#9467bd", # purple
  "#8c564b"  # brown
)

# 1. weighted age distribution
ggplot(project, aes(x = RIDAGEYR)) +
  geom_histogram(aes(weight = WTMEC2YR), bins = 30,
                 fill = cb_palette[1], color = "white") +
  labs(title = "Weighted Age Distribution",
       x = "Age (years)", y = "Weighted Count") +
  theme_minimal()

# 2. weighted BMI distribution
ggplot(project, aes(x = BMXBMI)) +
  geom_histogram(aes(weight = WTMEC2YR), bins = 30,
                 fill = cb_palette[2], color = "white") +
  labs(title = "Weighted BMI Distribution",
       x = "BMI", y = "Weighted Count") +
  theme_minimal()

# 3. weighted race/ethnicity by sleep status
race_weighted <- svytable(~RIDRETH3 + SleepStatus, nhanes_design)
race_df <- as.data.frame(prop.table(race_weighted, margin = 2))

ggplot(race_df, aes(x = factor(SleepStatus), y = Freq, fill = RIDRETH3)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cb_palette) +
  labs(title = "Weighted Race/Ethnicity by Sleep Status",
       x = "Sleep Status", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# 4. weighted gender by sleep status
gender_weighted <- svytable(~RIAGENDR + SleepStatus, nhanes_design)
gender_df <- as.data.frame(prop.table(gender_weighted, margin = 2))

ggplot(gender_df, aes(x = factor(SleepStatus), y = Freq, fill = RIAGENDR)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cb_palette[1:2]) +
  labs(title = "Weighted Gender by Sleep Status",
       x = "Sleep Status", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# 5. weighted BMI boxplot by sleep status
ggplot(project, aes(x = factor(SleepStatus), y = BMXBMI,
                    fill = factor(SleepStatus))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = cb_palette[1:2]) +
  labs(title = "Weighted BMI by Sleep Status",
       x = "Sleep Status", y = "BMI") +
  theme_minimal()

# 6. weighted sleep status distribution
sleep_weighted <- svytable(~SleepStatus, nhanes_design)
sleep_df <- as.data.frame(prop.table(sleep_weighted))

ggplot(sleep_df, aes(x = factor(SleepStatus), y = Freq,
                     fill = factor(SleepStatus))) +
  geom_col() +
  scale_fill_manual(values = cb_palette[1:2]) +
  labs(title = "Weighted Distribution of Sleep Status",
       x = "Sleep Status (0 = Unhealthy, 1 = Healthy)",
       y = "Weighted Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# ── STEP 12: Survey Weighted Correlation Matrices by Sleep Status ─────────────
vars <- c("MET_Vigorous_work", "MET_Moderate_work", "MET_Walk_bike",
          "MET_Vigorous_recreation", "MET_Moderate_recreation",
          "PAD680", "BMXBMI", "RIDAGEYR")

sleep_levels <- unique(project$SleepStatus)

for (sleep in sleep_levels) {
  design_sub <- subset(nhanes_design, SleepStatus == sleep)

  weighted_cov <- svyvar(as.formula(paste("~", paste(vars, collapse = "+"))),
                         design_sub, na.rm = TRUE)

  cov_matrix <- as.matrix(weighted_cov)
  sd_vec <- sqrt(diag(cov_matrix))
  cor_matrix <- cov_matrix / (sd_vec %*% t(sd_vec))

  corrplot(cor_matrix, method = "color",
           col = colorRampPalette(c("blue", "white", "red"))(200),
           addCoef.col = "black", number.cex = 0.7,
           tl.cex = 0.9, tl.col = "black",
           title = paste0("Survey-Weighted Correlation (SleepStatus = ", sleep, ")"),
           mar = c(0,0,2,0))
}

# ── STEP 13: STROBE Diagram (unweighted - just counting participants) ─────────
n_total <- nrow(project)

step1 <- project %>% filter(!is.na(SLD012))
n_excl_sleep <- n_total - nrow(step1)

step2 <- step1 %>% filter(complete.cases(across(all_of(met_vars))))
n_excl_pa <- nrow(step1) - nrow(step2)

step3 <- step2 %>% filter(!is.na(PAD680))
n_excl_sed <- nrow(step2) - nrow(step3)

covariates <- c("RIDAGEYR", "RIAGENDR", "BMXBMI", "RIDRETH3", "SMQ020")
step4 <- step3 %>% filter(complete.cases(across(all_of(covariates))))
n_excl_cov <- nrow(step3) - nrow(step4)

cat("STROBE Flow Diagram\n")
cat("Total participants:                     ", n_total, "\n")
cat("Remaining after sleep filter:           ", nrow(step1),
    " (Excluded: ", n_excl_sleep, ")\n")
cat("After PA filter:                        ", nrow(step2),
    " (Excluded: ", n_excl_pa, ")\n")
cat("After Sedentary filter:                 ", nrow(step3),
    " (Excluded: ", n_excl_sed, ")\n")
cat("After Covariates filter:                ", nrow(step4),
    " (Excluded: ", n_excl_cov, ")\n")

# dynamic STROBE diagram
n_step1 <- nrow(step1)
n_step2 <- nrow(step2)
n_step3 <- nrow(step3)
n_step4 <- nrow(step4)

grViz(glue("
digraph STROBE {{
  node [shape=box, style=filled, color=lightblue, fontname=Arial]

  Total [label='Total participants\\n{n_total}']
  Sleep [label='Remaining after sleep data\\n{n_step1}']
  PA    [label='After PA filter\\n{n_step2}']
  Sed   [label='After Sedentary filter\\n{n_step3}']
  Cov   [label='After Covariates filter\\n{n_step4}']

  ExclSleep [label='Excluded: {n_excl_sleep}', shape=note, color=lightpink]
  ExclPA    [label='Excluded: {n_excl_pa}',    shape=note, color=lightpink]
  ExclSed   [label='Excluded: {n_excl_sed}',   shape=note, color=lightpink]
  ExclCov   [label='Excluded: {n_excl_cov}',   shape=note, color=lightpink]

  Total -> Sleep
  Sleep -> PA
  PA    -> Sed
  Sed   -> Cov

  Sleep -> ExclSleep [style=dashed]
  PA    -> ExclPA    [style=dashed]
  Sed   -> ExclSed   [style=dashed]
  Cov   -> ExclCov   [style=dashed]
}}
"))

# ── STEP 14: Survey Weighted Regression (svyglm) ──────────────────────────────
# unadjusted - PA only
model_phy <- svyglm(SleepStatus ~ MET_Vigorous_work + MET_Moderate_work +
                      MET_Walk_bike + MET_Vigorous_recreation +
                      MET_Moderate_recreation,
                    design = nhanes_design, family = binomial)
summary(model_phy)

# unadjusted - sedentary only
model_sed <- svyglm(SleepStatus ~ PAD680,
                    design = nhanes_design, family = binomial)
summary(model_sed)

# unadjusted - PA + sedentary combined
model_combined <- svyglm(SleepStatus ~ MET_Vigorous_work + MET_Moderate_work +
                           MET_Walk_bike + MET_Vigorous_recreation +
                           MET_Moderate_recreation + PAD680,
                         design = nhanes_design, family = binomial)
summary(model_combined)

# fully adjusted
# fully adjusted - using Race_collapsed to avoid separation issue with Other Hispanic
model_adjusted <- svyglm(SleepStatus ~ MET_Vigorous_work + MET_Moderate_work +
                           MET_Walk_bike + MET_Vigorous_recreation +
                           MET_Moderate_recreation + PAD680 +
                           RIDAGEYR + RIAGENDR + BMXBMI + Race_collapsed + SMQ020,
                         design = nhanes_design, family = binomial)
summary(model_adjusted)
exp(coef(model_adjusted))
# ── STEP 15: Weighted Table 2 - Regression Results ────────────────────────────
library(broom)

# extract coefficients and CIs directly from the model
coefs <- coef(model_adjusted)
se <- sqrt(diag(vcov(model_adjusted)))

table2 <- data.frame(
  term    = names(coefs),
  OR      = round(exp(coefs), 4),
  CI_low  = round(exp(coefs - 1.96 * se), 4),
  CI_high = round(exp(coefs + 1.96 * se), 4),
  p_value = round(2 * pnorm(abs(coefs / se), lower.tail = FALSE), 4)
)

print(table2)

# save table2 as pdf


# convert table2 to grob and save
pdf("/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/table2_regression.pdf",
    width = 12, height = 8)

grid.table(table2)

dev.off()
