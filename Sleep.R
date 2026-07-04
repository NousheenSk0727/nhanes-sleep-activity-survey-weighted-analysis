# ── Sleep Module ──────────────────────────────────────────────────────────────
# NHANES 2017-2018 | Module: SLQ_J
# Goal: Understand sleep patterns and recode sleep duration into healthy/unhealthy

library(nhanesA)
library(dplyr)
library(readr)

# STEP 1: Pull raw data from CDC
sleep <- nhanes("SLQ_J")

# initial look at the data
dim(sleep)
names(sleep)
head(sleep, 10)
summary(sleep)

# ── STEP 2: Investigate missing values ───────────────────────────────────────
# how many missing per column?
colSums(is.na(sleep))

# who has missing values and what does their full profile look like?
missing_rows <- sleep %>%
  filter(if_any(everything(), is.na)) %>%
  select(SEQN, everything())

missing_rows
nrow(missing_rows)
# finding: 37 missing in SLQ300, few more in others
# saving for reference
write.csv(missing_rows,
          "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/sleep_missing_rows.csv",
          row.names = FALSE)

# ── STEP 3: Recode improper values to NA ─────────────────────────────────────
# "Don't know" in time variables → NA
# special numeric codes 777, 999 in duration variables → NA

sleep_clean <- sleep %>%
  mutate(
    across(
      c(SLQ300, SLQ310, SLQ320, SLQ330),
      ~ na_if(.x, "Don't know")
    ),
    across(
      c(SLD012, SLD013),
      ~ replace(.x, .x %in% c(777, 999), NA_real_)
    )
  )

# quick check - did the recoding work?
colSums(is.na(sleep_clean))

# ── STEP 4: Drop time variables, keep what we need ───────────────────────────
# removing SLQ300, SLQ310, SLQ320, SLQ330
# keeping SLD012, SLD013, SLQ030, SLQ040, SLQ050, SLQ120

sleep_clean <- sleep_clean %>%
  select(-SLQ300, -SLQ310, -SLQ320, -SLQ330)

# sanity check
names(sleep_clean)

# ── STEP 5: Explore remaining variables ──────────────────────────────────────
# understanding what values exist in SLQ030, SLQ040, SLQ050, SLQ120

sleep_clean %>% summarise(
  SLQ030_n = n_distinct(SLQ030, na.rm = TRUE),
  SLQ040_n = n_distinct(SLQ040, na.rm = TRUE),
  SLQ050_n = n_distinct(SLQ050, na.rm = TRUE),
  SLQ120_n = n_distinct(SLQ120, na.rm = TRUE)
)

sleep_clean %>% count(SLQ030)
sleep_clean %>% count(SLQ040)
sleep_clean %>% count(SLQ050)
sleep_clean %>% count(SLQ120)

# ── STEP 6: Recode sleep duration into binary ─────────────────────────────────
# healthy sleep = 7 to 9.5 hours → 1
# unhealthy sleep = < 7 or > 9.5 hours → 0
# NA stays NA

sleep_clean <- sleep_clean %>%
  mutate(
    SLD012 = case_when(
      !is.na(SLD012) & SLD012 < 7                      ~ 0L,
      !is.na(SLD012) & SLD012 > 9.5                    ~ 0L,
      !is.na(SLD012) & SLD012 >= 7 & SLD012 <= 9.5     ~ 1L,
      TRUE                                              ~ NA_integer_
    ),
    SLD013 = case_when(
      !is.na(SLD013) & SLD013 < 7                      ~ 0L,
      !is.na(SLD013) & SLD013 > 9.5                    ~ 0L,
      !is.na(SLD013) & SLD013 >= 7 & SLD013 <= 9.5     ~ 1L,
      TRUE                                              ~ NA_integer_
    )
  )

# check distribution of recoded variable
sleep_clean %>% count(SLD012)
sleep_clean %>% count(SLD013)

# ── STEP 7: Final clean dataset ───────────────────────────────────────────────
# keep only SEQN, weekday and weekend sleep duration

sleep_final <- sleep_clean %>%
  select(SEQN, SLD012, SLD013)

# final check
glimpse(sleep_final)
colSums(is.na(sleep_final))

# save
write.csv(sleep_final,
          "/Users/nousheenjahanshaik/Documents/HBNS.1/DATASET/sleep_clean.csv",
          row.names = FALSE)
