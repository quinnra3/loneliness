############################
## Prelim Analyses - Coding
############################

## Prelim Analysis Plan:
## 1. LOAD LIBRARIES AND IMPORT RAW DATA
## 2. APPLY INCLUSION/EXCLUSION CRITERIA
## 3. EXPLORATORY ANALYSES - AGE
## 4. CLEANING/RECODING
## 5. LONELINESS SCORING
## 6. DESCRIPTIVES (TABLE 1S)
## 7. MODELS (TABLE 2S)


# -------------------------------------
# 1. LOAD LIBRARIES AND IMPORT RAW DATA
# -------------------------------------

# load libraries
library(tidyverse)

# find directory location
getwd()

# load dataset (raw, NOT labels version)
raw_LSU = read.csv(file = "./data/LonelinessAndSubstan-ALLDATA_DATA_2026-01-07_1250.csv")

# check
skimr::skim(raw_LSU)
names(raw_LSU)
raw_LSU
view(raw_LSU) # <- use to view file from RStudio


# ---------------------------------------------
# 2. APPLY INCLUSION/EXCLUSION CRITERIA
# ---------------------------------------------

analytic_df <- raw_LSU |> 
  filter(
    !(record_id %in% c(
      105, 287, 289, 
      296, 383, 393, 
      126, 160, 361)))

# Record IDs 105, 287, 289, 296, 383, 393 -> incomplete lonely scores -> excluded
# Record IDs 126, 160, 361                -> age > 75                 -> excluded

# check
view(analytic_df)  
nrow(raw_LSU) - nrow(analytic_df)


# -----------------------------
# 3. EXPLORATORY ANALYSES - AGE
# -----------------------------

# Age ('patient_age')
analytic_df |> 
  summarise(
    age_mean = mean(patient_age),
    age_sd = sd(patient_age))
    
ggplot(data = raw_LSU, aes(x = patient_age))+
geom_histogram()

skimr::skim(raw_LSU, patient_age)

# Age by decade
age_df <- analytic_df |> 
  mutate(
    age_decade = cut(
      patient_age,
      breaks = c(18, 30, 40, 50, Inf),
      labels = c("18-29", "30-39", "40-49", "50+"),
      right = FALSE),
    age_decade = factor(
      age_decade,
      levels = c(
        "18-29",
        "30-39",
        "40-49",
        "50+")))

# check
age_df |> 
  count(age_decade, patient_age) |> 
  arrange(patient_age)

age_filter_df <- age_df |> 
  filter(patient_age < 18 | patient_age > 75)
view(age_filter_df)

  
# Age by category
age_df <- age_df |> 
  mutate(
    age_cat = case_when(
      patient_age >= 18 & patient_age <= 34 ~ "Young Adults (18-34)",
      patient_age >= 35 & patient_age <= 49 ~ "Middle Aged Adults (35-49)",
      patient_age >= 50 ~ "Older Adults (50+)"),
    age_cat = factor(
      age_cat,
      levels = c(
        "Young Adults (18-34)",
        "Middle Aged Adults (35-49)",
        "Older Adults (50+)")))

# check
age_df |> 
  count(age_cat, patient_age) |> 
  arrange(patient_age)

# Age continuous Histogram
ggplot(age_df, aes(x = patient_age)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2, fill = "steelblue", color = "white")+
  geom_density(alpha = .2, fill = "navy") +
  labs(title = "Distribution of Participant Age", x = "Participant Age", y = "Count")

# Age by categories (18-35, 35-50, 50+)
ggplot(age_df, aes(x = age_cat)) + 
  geom_bar(fill = "grey36") +
  labs(title = "Participants Grouped by age_cat")

# Age by decade
ggplot(age_df, aes(x = age_decade)) + 
  geom_bar(fill = "maroon") +
  labs(title = "Participants Grouped by Decade")

# Continuous Age Summary
age_df |> 
  summarise(
    mean_age = mean(patient_age),
    sd_age = sd(patient_age),
    median_age = median(patient_age),
    IQR_age = IQR(patient_age),
    min_age = min(patient_age),
    max_age = max(patient_age))


# --------------------
# 4. CLEANING/RECODING
# --------------------

# view var names in 'age_df'
var_names = names(age_df)
view(var_names)
var_names = data.frame(variable = var_names)

# new data frame with selected variables
clean_df <- age_df |> 
  select(
    record_id,
    patient_age,
    age_cat,
    age_decade,
    patient_gender,
    patient_ethnicity,
    patient_race,
    patient_race_2,
    gad_score,
    phq_score,
    in_tune,
    companion,
    turn_to,
    alone,
    group,
    outgoing,
    common,
    close,
    interest_ideas,
    close_people,
    left_out,
    relationship,
    knows_you,
    isolated,
    companionship,
    understand,
    shy,
    around_you,
    talk_to,
    turn,
    tobacco,
    males,
    females,
    drugs,
    meds,
    cigarette_1,
    alcohol_1,
    alcohol_2,
    marijuana_1,
    cocaine_1,
    heroin_1,
    opioid_1,
    med_anxiety_1,
    adhd_1,
    illegal_drug_1)

# check
view(clean_df)
clean_df |> count(patient_gender, sort = TRUE)
str(clean_df$patient_gender)


# recode dichot variables from 1(yes) 2(no) -> 1(yes) 0(no)
recode_df = clean_df |> 
  mutate(
    gender = case_when(
      patient_gender == 1 ~ 0, # female '0' reference
      patient_gender == 2 ~ 1, # male   '1'
      TRUE                ~ NA), 
    cigarette = case_when(
      cigarette_1 == 1 ~ 1,
      cigarette_1 == 2 ~ 0,
      TRUE             ~ NA),
    alcohol = case_when(
      alcohol_1 == 1 ~ 1,
      alcohol_1 == 2 ~ 0,
      TRUE           ~ NA),
    alcohol2 = case_when(
      alcohol_2 == 1 ~ 1,
      alcohol_2 == 2 ~ 0,
      TRUE           ~ NA),
    marijuana = case_when(
      marijuana_1 == 1 ~ 1,
      marijuana_1 == 2 ~ 0,
      TRUE             ~ NA),
    cocaine = case_when(
      cocaine_1 == 1 ~ 1,
      cocaine_1 == 2 ~ 0,
      TRUE           ~ NA),
    heroin = case_when(
      heroin_1 == 1 ~ 1,
      heroin_1 == 2 ~ 0,
      TRUE          ~ NA),
    opioid = case_when(
      opioid_1 == 1 ~ 1,
      opioid_1 == 2 ~ 0,
      TRUE          ~ NA),
    med_anxiety = case_when(
      med_anxiety_1 == 1 ~ 1,
      med_anxiety_1 == 2 ~ 0,
      TRUE          ~ NA),
    adhd = case_when(
      adhd_1 == 1 ~ 1,
      adhd_1 == 2 ~ 0,
      TRUE        ~ NA),
    illegal_drug = case_when(
      illegal_drug_1 == 1 ~ 1,
      illegal_drug_1 == 2 ~ 0,
      TRUE        ~ NA))

# check
view(recode_df)
write_csv(recode_df, "recode_df_1.7.26.csv")
table(clean_df$cigarette_1, recode_df$cigarette, useNA = "ifany")
table(clean_df$patient_gender, recode_df$gender, useNA = "ifany")

recode_df |>
  summarise(
    gender_min = min(gender, na.rm = TRUE),
    gender_max = max(gender, na.rm = TRUE),

    cigarette_min = min(cigarette, na.rm = TRUE),
    cigarette_max = max(cigarette, na.rm = TRUE),
    
    alcohol_min = min(alcohol, na.rm = TRUE),
    alcohol_max = max(alcohol, na.rm = TRUE),
    
    alcohol2_min = min(alcohol2, na.rm = TRUE),
    alcohol2_max = max(alcohol2, na.rm = TRUE),
    
    marijuana_min = min(marijuana, na.rm = TRUE),
    marijuana_max = max(marijuana, na.rm = TRUE),
    
    cocaine_min = min(cocaine, na.rm = TRUE),
    cocaine_max = max(cocaine, na.rm = TRUE),
    
    heroin_min = min(heroin, na.rm = TRUE),
    heroin_max = max(heroin, na.rm = TRUE),
    
    opioid_min = min(opioid, na.rm = TRUE),
    opioid_max = max(opioid, na.rm = TRUE),
    
    med_anxiety_min = min(med_anxiety, na.rm = TRUE),
    med_anxiety_max = max(med_anxiety, na.rm = TRUE),
    
    adhd_min = min(adhd, na.rm = TRUE),
    adhd_max = max(adhd, na.rm = TRUE),
    
    illegal_drug_min = min(illegal_drug, na.rm = TRUE),
    illegal_drug_max = max(illegal_drug, na.rm = TRUE))


# ---------------------
# 5. LONELINESS SCORING
# ---------------------

# lonely score questions:

lonely_items <- c("in_tune","companion","turn_to","alone","group",
                  "outgoing","common","close","interest_ideas","close_people",
                  "left_out","relationship","knows_you","isolated","companionship",
                  "understand","shy","around_you","talk_to","turn")


# ---------------------------------------------
# 5a. IMPUTE MISSING QUESTIONS FOR 2 RECORD IDs
# ---------------------------------------------
record_ids <- c(98, 421)

recode_df <- recode_df |> 
  rowwise() |> 
  mutate(
    companionship = if_else(
      record_id == 98,
      round(mean(c(
        in_tune, companion, turn_to, alone, group,
             outgoing, common, close, interest_ideas, close_people,
             left_out, relationship, knows_you, isolated, understand,
             shy, around_you, talk_to, turn
        ), na.rm = TRUE)),
      companionship),
    
    left_out = if_else(
      record_id == 421,
      round(mean(c(
        in_tune, companion, turn_to, alone, group,
               outgoing, common, close, interest_ideas, close_people,
               relationship, knows_you, isolated, companionship, understand,
               shy, around_you, talk_to, turn
        ), na.rm = TRUE)),
      left_out)
    ) |> 
  ungroup()

# check
recode_df |>
  filter(record_id %in% c(98, 421)) |>
  select(record_id, companionship, left_out)

# ---------------------------------------------
# 5b. REVERSE CODING:
#     'in_tune', 'group', 'common','outgoing',
#     'close_people', 'companionship', 'understand',
#     'talk_to', 'turn'
# ---------------------------------------------

# METHOD 1 - preference
score_df <- recode_df |> 
  mutate(
    in_tune_2 = 5 - in_tune,
    group_2 = 5 - group,
    common_2 = 5 - common,
    outgoing_2 = 5 - outgoing,
    close_people_2 = 5 - close_people,
    companionship_2 = 5 - companionship,
    understand_2 = 5 - understand,
    talk_to_2 = 5 - talk_to,
    turn_2 = 5 - turn)

# check
names(score_df)
view(score_df)
view(score_df |>
       summarise(
         in_tune_2_min = min(in_tune_2, na.rm = TRUE),
         in_tune_2_max = max(in_tune_2, na.rm = TRUE),
         
         group_2_min = min(group_2, na.rm = TRUE),
         group_2_max = max(group_2, na.rm = TRUE),
         
         common_2_min = min(common_2, na.rm = TRUE),
         common_2_max = max(common_2, na.rm = TRUE),
         
         outgoing_2_min = min(outgoing_2, na.rm = TRUE),
         outgoing_2_max = max(outgoing_2, na.rm = TRUE),
         
         close_people_2_min = min(close_people_2, na.rm = TRUE),
         close_people_2_max = max(close_people_2, na.rm = TRUE)))


# ---------------------------------
# 5c. CALCULATE TOTAL LONELY SCORES
# ---------------------------------

score_df <- score_df|>
  mutate(
    lonely_total = (in_tune_2 + companion + turn_to + alone + group_2 + common_2 + 
      close + interest_ideas + outgoing_2 + close_people_2 + 
      left_out + relationship + knows_you + isolated + companionship_2 + 
      understand_2 + shy + around_you + talk_to_2 + turn_2))

# check
view(score_df)
view(score_df |>
       summarise(
         in_tune_2_min = min(in_tune_2, na.rm = TRUE),
         in_tune_2_max = max(in_tune_2, na.rm = TRUE),
         
         group_2_min = min(group_2, na.rm = TRUE),
         group_2_max = max(group_2, na.rm = TRUE),
         
         common_2_min = min(common_2, na.rm = TRUE),
         common_2_max = max(common_2, na.rm = TRUE),
         
         outgoing_2_min = min(outgoing_2, na.rm = TRUE),
         outgoing_2_max = max(outgoing_2, na.rm = TRUE),
         
         close_people_2_min = min(close_people_2, na.rm = TRUE),
         close_people_2_max = max(close_people_2, na.rm = TRUE),
         
         lonely_min = min(lonely_total, na.rm = TRUE),
         lonely_max = max(lonely_total, na.rm = TRUE)))

# loneliness score range: 20-74

# -----------------
# 5d. DICHOT LONELY
# -----------------

cutoff <- 43

prev_tbl <- score_df |>
  summarise(
    N = n(),  # 298
    lonely = sum(lonely_total >= cutoff, na.rm = TRUE),
    prevalence = lonely / N)

prev_tbl

# check
nrow(score_df) # N = 298
sum(is.na(score_df$lonely_total)) # NAs total = 8

mean_tbl <- score_df |> 
  summarise(
    N = n(), # 298
    mean_total = mean(lonely_total, na.rm = TRUE),
    sd_total = sd(lonely_total, na.rm = TRUE)) |> 
  mutate(
    lonely_total = sprintf("%0.2f (%0.2f)", mean_total, sd_total),
    select(N, lonely_total))


# lonely dichot variable creation
score_df <- score_df |>
  mutate(
    loneliness_dichot = case_when(
      UCLA_total >= cutoff ~ 1,
      UCLA_total <  cutoff ~ 0,
      TRUE ~ NA_real_))



# --------------------------
# 6. DESCRIPTIVES (TABLE 1S)
# --------------------------



# --------------------
# 7. MODELS (TABLE 2S)
# --------------------


















