# ── Final Merging Script ──────────────────────────────────────────────────────
# NHANES 2017-2018
# Goal: Merge all cleaned modules into one final analytic dataset using SEQN

library(dplyr)
library(nhanesA)

# ── STEP 1: Load all cleaned datasets ────────────────────────────────────────
# note: if you are running this as a standalone script, load the saved CSVs
# if you are running this right after all module scripts, objects are already in environment

phy_clean    <- read.csv("/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/physical_activity_clean.csv")
sleep_final  <- read.csv("/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/sleep_clean.csv")
bmi          <- read.csv("/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/bmi_clean.csv")
bp_final_clean <- read.csv("/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/bp_clean.csv")
smoking_clean  <- read.csv("/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/smoking_clean.csv")
demo_clean   <- read.csv("/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/demographics_clean.csv")
dm_final     <- read.csv("/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/diabetes_final.csv")

# ── STEP 2: Merge all modules by SEQN ────────────────────────────────────────
# survey weights already included in demo_clean - no separate step needed

final_dataset <- phy_clean %>%
  left_join(sleep_final,    by = "SEQN") %>%
  left_join(bmi,            by = "SEQN") %>%
  left_join(bp_final_clean, by = "SEQN") %>%
  left_join(smoking_clean,  by = "SEQN") %>%
  left_join(demo_clean,     by = "SEQN") %>%
  left_join(dm_final,       by = "SEQN")

# ── STEP 3: Quick checks ──────────────────────────────────────────────────────
dim(final_dataset)
names(final_dataset)
head(final_dataset)
colSums(is.na(final_dataset))

# ── STEP 4: Reorder columns ───────────────────────────────────────────────────
# keeping yes/no flag next to its corresponding MET value
final_dataset <- final_dataset %>%
  select(
    SEQN,
    SLD012, SLD013,
    PAQ605, MET_Vigorous_work,
    PAQ620, MET_Moderate_work,
    PAQ635, MET_Walk_bike,
    PAQ650, MET_Vigorous_recreation,
    PAQ665, MET_Moderate_recreation,
    PAD680,
    BMXBMI,
    SMQ020, SMQ040,
    diabetes_final,
    SBP_mean, DBP_mean, BP_category,
    RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, INDFMPIR,
    WTMEC2YR, SDMVPSU, SDMVSTRA
  )

# ── STEP 5: Save final dataset ────────────────────────────────────────────────
write.csv(final_dataset,
          "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/HBNS_Final_Dataset.csv",
         row.names = FALSE)
