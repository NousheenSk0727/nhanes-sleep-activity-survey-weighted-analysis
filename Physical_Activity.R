# ── Physical Activity Module ──────────────────────────────────────────────────
# NHANES 2017-2018 | Module: PAQ_J
# Goal: Understand physical activity patterns and convert to MET-min/week
install.packages("nhanesA")
library(nhanesA)
library(dplyr)
library(tidyr)

# ── STEP 1: Pull raw data from CDC ───────────────────────────────────────────
phy <- nhanes("PAQ_J")

# initial look at the data
str(phy)
names(phy)
dim(phy)

# ── STEP 2: Start simple - just the 5 yes/no activity variables ──────────────
# these are the entry point variables telling us WHETHER someone does each activity
# PAQ605 - Vigorous work activity
# PAQ620 - Moderate work activity
# PAQ635 - Walk or bicycle
# PAQ650 - Vigorous recreational activities
# PAQ665 - Moderate recreational activities

vars <- c("PAQ605", "PAQ620", "PAQ635", "PAQ650", "PAQ665")

# what values actually exist in these columns?
# not assuming yes/no only - checking the raw codes first
raw_code_counts <- lapply(vars, function(v) {
  phy %>%
    count(!!rlang::sym(v), name = "n") %>%
    rename(code = !!rlang::sym(v)) %>%
    mutate(variable = v)
}) %>%
  bind_rows() %>%
  select(variable, code, n) %>%
  arrange(variable, code)

raw_code_counts
# finding: not just yes/no - there are "Don't know" responses too
# but only in vigorous work (PAQ605) and moderate work (PAQ620)

# ── STEP 3: Investigate the "Don't know" cases ───────────────────────────────
# who are these people and what does the rest of their data look like?

# all pa variables to give full picture
pa_vars_all <- c(
  "PAQ605", "PAQ610", "PAD615",
  "PAQ620", "PAQ625", "PAD630",
  "PAQ635", "PAQ640", "PAD645",
  "PAQ650", "PAQ655", "PAD660",
  "PAQ665", "PAQ670", "PAD675",
  "PAD680"
)

# keep only variables that exist in the pulled data
pa_vars_exist <- intersect(pa_vars_all, names(phy))
key_vars_exist <- intersect(vars, names(phy))

# filter to don't know rows and see full pa profile
dk_table <- phy %>%
  filter(if_any(all_of(key_vars_exist), ~ . == "Don't know")) %>%
  select(SEQN, all_of(pa_vars_exist)) %>%
  arrange(SEQN)

dk_table
nrow(dk_table)
# decision: treat "Don't know" as NA - we cannot assume yes or no

# ── STEP 4: Dive deeper into vigorous work specifically ──────────────────────
# understanding what happens AFTER someone says yes
# does everyone who said yes have days/week AND minutes/day filled in?

vig_yes <- phy %>%
  filter(PAQ605 == "Yes") %>%
  select(SEQN, PAQ605, PAQ610, PAD615) %>%
  arrange(desc(PAQ610))

head(vig_yes, 10)

# edge case check: said yes, has days/week, but minutes/day is missing
vig_missing_min <- phy %>%
  mutate(
    PAQ610 = suppressWarnings(as.numeric(PAQ610)),
    PAD615 = suppressWarnings(as.numeric(PAD615))
  ) %>%
  filter(
    PAQ605 == "Yes",
    !is.na(PAQ610),
    is.na(PAD615)
  ) %>%
  select(SEQN, PAQ605, PAQ610, PAD615)

vig_missing_min
nrow(vig_missing_min)
# finding: 8 people said yes, reported days/week, but minutes/day is missing
# decision: these become NA - cant compute minutes/week without both values

# ── STEP 5: Now apply same logic across all 5 activities ─────────────────────
# minutes/week = days/week * minutes/day
# rules:
# yes + both values present = days * minutes
# no = NA
# yes but missing days or minutes = NA
# don't know = NA

num <- function(x) suppressWarnings(as.numeric(x))

phy <- phy %>%
  mutate(
    Vigorous_work_min_week = ifelse(
      PAQ605 == "Yes" & !is.na(num(PAQ610)) & !is.na(num(PAD615)),
      num(PAQ610) * num(PAD615), NA_real_),

    Moderate_work_min_week = ifelse(
      PAQ620 == "Yes" & !is.na(num(PAQ625)) & !is.na(num(PAD630)),
      num(PAQ625) * num(PAD630), NA_real_),

    Walk_bike_min_week = ifelse(
      PAQ635 == "Yes" & !is.na(num(PAQ640)) & !is.na(num(PAD645)),
      num(PAQ640) * num(PAD645), NA_real_),

    Vigorous_recreation_min_week = ifelse(
      PAQ650 == "Yes" & !is.na(num(PAQ655)) & !is.na(num(PAD660)),
      num(PAQ655) * num(PAD660), NA_real_),

    Moderate_recreation_min_week = ifelse(
      PAQ665 == "Yes" & !is.na(num(PAQ670)) & !is.na(num(PAD675)),
      num(PAQ670) * num(PAD675), NA_real_)
  )

# quick check on distributions
summary(phy$Vigorous_work_min_week)
summary(phy$Moderate_work_min_week)
summary(phy$Walk_bike_min_week)
summary(phy$Vigorous_recreation_min_week)
summary(phy$Moderate_recreation_min_week)

# ── STEP 6: Convert minutes/week to MET-min/week ─────────────────────────────
# MET values based on standard compendium:
# vigorous activity = 8 METs
# moderate activity and walking = 4 METs

phy <- phy %>%
  mutate(
    MET_Vigorous_work       = Vigorous_work_min_week * 8,
    MET_Moderate_work       = Moderate_work_min_week * 4,
    MET_Walk_bike           = Walk_bike_min_week * 4,
    MET_Vigorous_recreation = Vigorous_recreation_min_week * 8,
    MET_Moderate_recreation = Moderate_recreation_min_week * 4,

    Total_MET = rowSums(
      cbind(MET_Vigorous_work, MET_Moderate_work,
            MET_Walk_bike, MET_Vigorous_recreation,
            MET_Moderate_recreation),
      na.rm = TRUE)
  )

# ── STEP 7: Recode yes/no to 1/0 for modelling ───────────────────────────────
# yes = 1, no = 0, don't know = NA

phy <- phy %>%
  mutate(across(c(PAQ605, PAQ620, PAQ635, PAQ650, PAQ665),
                ~ case_when(. == "Yes" ~ 1,
                            . == "No"  ~ 0,
                            TRUE       ~ NA_real_)))

# ── STEP 8: Final clean dataset ───────────────────────────────────────────────
# keep only what we need for merging later

phy_clean <- phy %>%
  select(SEQN,
         PAQ605, PAQ620, PAQ635, PAQ650, PAQ665,
         MET_Vigorous_work, MET_Moderate_work,
         MET_Walk_bike, MET_Vigorous_recreation,
         MET_Moderate_recreation,
         Total_MET,
         PAD680)  # sedentary time - keeping for analysis

# final check
glimpse(phy_clean)
colSums(is.na(phy_clean))
View(phy_clean)

write.csv(phy_clean, "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/physical_activity_clean.csv", row.names = FALSE)
