# ── Body Measures Module ──────────────────────────────────────────────────────
# NHANES 2017-2018 | Module: BMX_J
# Goal: Extract BMI only

library(nhanesA)
library(dplyr)

# ── STEP 1: Pull raw data from CDC ───────────────────────────────────────────
bmx <- nhanes("BMX_J")

# initial look
dim(bmx)
names(bmx)

# ── STEP 2: Keep only what we need ───────────────────────────────────────────
# from body measures only BMI is relevant for this project
bmi <- bmx %>%
  select(SEQN, BMXBMI)

# ── STEP 3: Quick check ───────────────────────────────────────────────────────
summary(bmi$BMXBMI)
colSums(is.na(bmi))

# ── STEP 4: Save ─────────────────────────────────────────────────────────────
write.csv(bmi,
          "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/bmi_clean.csv",
          row.names = FALSE)


#----------------------------X--------------------------------------------------
#----------------------------X--------------------------------------------------

# ── Blood Pressure Module ─────────────────────────────────────────────────────
# NHANES 2017-2018 | Module: BPX_J
# Goal: Average multiple BP readings, categorize, and recode for analysis

# ── STEP 1: Pull raw data from CDC ───────────────────────────────────────────
bp <- nhanes("BPX_J")

# initial look
dim(bp)
names(bp)

# ── STEP 2: Select only the BP readings we need ───────────────────────────────
# 4 systolic and 4 diastolic readings per participant
bp_selected <- bp %>%
  select(
    SEQN,
    BPXSY1, BPXDI1,
    BPXSY2, BPXDI2,
    BPXSY3, BPXDI3,
    BPXSY4, BPXDI4
  )

head(bp_selected)
colSums(is.na(bp_selected))

# ── STEP 3: Average the 4 readings ───────────────────────────────────────────
# using na.rm = TRUE so partial readings are still used
bp_avg <- bp_selected %>%
  mutate(
    SBP_mean = rowMeans(select(., BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE),
    DBP_mean = rowMeans(select(., BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE)
  )

# quick check
bp_avg %>%
  select(SEQN, SBP_mean, DBP_mean) %>%
  slice(1:10)

# ── STEP 4: Categorize BP ────────────────────────────────────────────────────
# hypotension checked first (most restrictive, clinically important to flag)
# then normal, elevated, high
bp_final <- bp_avg %>%
  mutate(
    BP_category = case_when(
      SBP_mean < 90  | DBP_mean < 60                        ~ "Hypotension",
      SBP_mean < 120 & DBP_mean < 80                        ~ "Normal",
      SBP_mean >= 120 & SBP_mean < 130 & DBP_mean < 80      ~ "Elevated",
      SBP_mean >= 130 | DBP_mean >= 80                      ~ "High",
      TRUE                                                   ~ NA_character_
    )
  )

# check distribution
bp_final %>% count(BP_category)

# ── STEP 5: Keep only what we need ───────────────────────────────────────────
bp_final_clean <- bp_final %>%
  select(SEQN, SBP_mean, DBP_mean, BP_category)

head(bp_final_clean)

# ── STEP 6: Recode BP category to numeric for modelling ──────────────────────
# Hypotension = 0, Normal = 1, Elevated = 2, High = 3
bp_final_clean <- bp_final_clean %>%
  mutate(
    BP_category = case_when(
      BP_category == "Hypotension" ~ 0L,
      BP_category == "Normal"      ~ 1L,
      BP_category == "Elevated"    ~ 2L,
      BP_category == "High"        ~ 3L,
      TRUE                         ~ NA_integer_
    )
  )

# final check
bp_final_clean %>% count(BP_category)
colSums(is.na(bp_final_clean))
glimpse(bp_final_clean)

# ── STEP 7: Save ─────────────────────────────────────────────────────────────
write.csv(bp_final_clean,
          "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/bp_clean.csv",
          row.names = FALSE)


# ── Smoking Module ────────────────────────────────────────────────────────────
# NHANES 2017-2018 | Module: SMQ_J
# Goal: Extract smoking status variables

# ── STEP 1: Pull raw data from CDC ───────────────────────────────────────────
smoking <- nhanes("SMQ_J")

# initial look
dim(smoking)
names(smoking)

# ── STEP 2: Keep only what we need ───────────────────────────────────────────
# SMQ020 - smoked at least 100 cigarettes in life (ever smoker)
# SMQ040 - do you now smoke cigarettes (current smoker)
smoking_clean <- smoking %>%
  select(SEQN, SMQ020, SMQ040)

# quick check
head(smoking_clean)
smoking_clean %>% count(SMQ020)
smoking_clean %>% count(SMQ040)
colSums(is.na(smoking_clean))

# ── STEP 3: Save ─────────────────────────────────────────────────────────────
write.csv(smoking_clean,
          "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/smoking_clean.csv",
          row.names = FALSE)


# ── Diabetes Module ───────────────────────────────────────────────────────────
# NHANES 2017-2018 | Modules: GHB_J (lab A1c) + DIQ_J (questionnaire)
# Goal: Create a robust diabetes variable combining lab and self-report data
# This was the most complex module - cross verifying lab values with doctor diagnosis
# ── STEP 1: Pull A1c from lab ─────────────────────────────────────────────────
# using lab data as the primary source - more objective than self report
a1c <- nhanes("GHB_J")

head(a1c)
dim(a1c)
summary(a1c$LBXGH)

# keep only what we need
a1c_clean <- a1c %>%
  select(SEQN, LBXGH)  # LBXGH = HbA1c value

# ── STEP 2: Pull diabetes questionnaire ──────────────────────────────────────
# DIQ010 = doctor told you that you have diabetes (yes/no/borderline)
diq <- nhanes("DIQ_J")

diq_clean <- diq %>%
  select(SEQN, DIQ010)

head(diq_clean)
diq_clean %>% count(DIQ010)

# ── STEP 3: Merge lab and questionnaire by SEQN ───────────────────────────────
# inner join - only keep participants present in both
merged_dm <- a1c_clean %>%
  inner_join(diq_clean, by = "SEQN")

head(merged_dm)
dim(merged_dm)

# ── STEP 4: Cross verification - A1c >= 6.5 group ────────────────────────────
# clinical cutoff for diabetes diagnosis is HbA1c >= 6.5%
# checking how lab values align with doctor diagnosis

# total with A1c >= 6.5
count_lab_diabetes <- merged_dm %>%
  filter(LBXGH >= 6.5) %>%
  summarise(n = n())
count_lab_diabetes
# finding: 751 participants with A1c >= 6.5

# group 1: A1c >= 6.5 AND doctor said yes (lab confirmed + diagnosed)
group1_diagnosed <- merged_dm %>%
  filter(LBXGH >= 6.5, DIQ010 == "Yes") %>%
  select(SEQN, LBXGH, DIQ010)
nrow(group1_diagnosed)
# finding: 559 participants

# group 2: A1c >= 6.5 BUT doctor said no (undiagnosed diabetics)
group2_undiagnosed <- merged_dm %>%
  filter(LBXGH >= 6.5, DIQ010 == "No") %>%
  select(SEQN, LBXGH, DIQ010)
nrow(group2_undiagnosed)
# finding: 152 participants - these are undiagnosed diabetics

# the remaining 40: A1c >= 6.5 but neither yes nor no
# finding: borderline classification even though A1c meets diabetic threshold
missing_40 <- merged_dm %>%
  filter(LBXGH >= 6.5, !(DIQ010 %in% c("Yes", "No"))) %>%
  select(SEQN, LBXGH, DIQ010)
nrow(missing_40)
print(as.data.frame(missing_40))
# total: 559 + 152 + 40 = 751 - numbers check out

# ── STEP 5: Cross verification - normal A1c group ────────────────────────────
# checking the other direction - people with normal A1c but doctor said yes
lab_normal <- merged_dm %>%
  filter(LBXGH < 6.5) %>%
  select(SEQN, LBXGH, DIQ010)

table(lab_normal$DIQ010, useNA = "ifany")
# finding: 4908 correctly classified as non-diabetic
# 254 reported doctor diagnosis despite normal A1c - likely treated/controlled cases
# 128 reported borderline despite normal A1c
# decision: doctor diagnosed cases (DIQ010 == Yes) included even with normal A1c
# because treatment can lower A1c below 6.5 in diagnosed diabetics

# ── STEP 6: Create final diabetes variable ────────────────────────────────────
# logic:
# A1c >= 6.5 → 1 (lab confirmed diabetic)
# A1c < 6.5 but doctor said Yes → 1 (doctor diagnosed, likely treated)
# everything else (No, Borderline, Don't know, NA) → 0

merged_dm <- merged_dm %>%
  mutate(
    diabetes_final = case_when(
      LBXGH >= 6.5                        ~ 1,  # lab confirmed
      LBXGH < 6.5 & DIQ010 == "Yes"      ~ 1,  # doctor diagnosed
      TRUE                                ~ 0   # all others
    )
  )

# check distribution
merged_dm %>% count(diabetes_final)

# ── STEP 7: Save outputs ──────────────────────────────────────────────────────
# file 1: SEQN + diabetes_final only (for merging)
dm_final <- merged_dm %>%
  select(SEQN, diabetes_final)

write.csv(dm_final,
          "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/diabetes_final.csv",
          row.names = FALSE)

# file 2: full info for reference and verification
dm_full <- merged_dm %>%
  select(SEQN, LBXGH, DIQ010, diabetes_final)

write.csv(dm_full,
          "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/diabetes_full_info.csv",
          row.names = FALSE)

# final check
glimpse(dm_final)
colSums(is.na(dm_final))

# ── Demographics Module ───────────────────────────────────────────────────────
# NHANES 2017-2018 | Module: DEMO_J
# Goal: Extract key demographic variables

library(nhanesA)
library(dplyr)

# ── STEP 1: Pull raw data from CDC ───────────────────────────────────────────
demo <- nhanes("DEMO_J")
# initial look
dim(demo)
names(demo)

# ── STEP 2: Keep only what we need ───────────────────────────────────────────
# RIDAGEYR - Age
# RIAGENDR - Sex
# RIDRETH3 - Race/Ethnicity
# DMDEDUC2 - Education
# INDFMPIR - Poverty Income Ratio

demo_clean <- demo %>%
  select(
    SEQN,
    RIDAGEYR, RIAGENDR, RIDRETH3,
    DMDEDUC2, INDFMPIR,
    WTMEC2YR, SDMVPSU, SDMVSTRA  # survey weights - live in DEMO_J
  )


# quick check
head(demo_clean)
demo_clean %>% count(RIAGENDR)
demo_clean %>% count(RIDRETH3)
demo_clean %>% count(DMDEDUC2)
summary(demo_clean$RIDAGEYR)
summary(demo_clean$INDFMPIR)
colSums(is.na(demo_clean))

# ── STEP 3: Save ─────────────────────────────────────────────────────────────
write.csv(demo_clean,
          "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/demographics_clean.csv",
          row.names = FALSE)


