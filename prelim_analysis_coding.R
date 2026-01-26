############################
## Prelim Analyses - Coding
## 12.6.25 - 1.22.26
## Quinn Anderson
## RC, CUIMC ED
############################

## Prelim Analysis Plan:
## 1. LOAD LIBRARIES AND IMPORT RAW DATA
## 2. APPLY INCLUSION/EXCLUSION CRITERIA
## 3. EXPLORATORY ANALYSES
## 4. CLEANING/RECODING
## 5. LONELINESS SCORING
## 6. DESCRIPTIVES (TABLE 1S)
## 7. MODELS - 3m OUTCOMES (TABLE 2)
## 8. MODELS - 12M OUTCOMES (TABLE 2)

# -------------------------------------
# 1. LOAD LIBRARIES AND IMPORT RAW DATA
# -------------------------------------

# load libraries
library(tidyverse)
library(broom)

# find directory location
getwd()

# load dataset (raw, NOT labels version)
raw_data = read.csv(file = "./data/LonelinessAndSubstan-ALLDATA_DATA_2026-01-07_1250.csv")

# check
skimr::skim(raw_data)
names(raw_data)
raw_data
view(raw_data)

# ---------------------------------------------
# 2. APPLY INCLUSION/EXCLUSION CRITERIA
# ---------------------------------------------

clean_df <- raw_data |> 
  filter(
    !(record_id %in% c(
      105, 287, 289, 
      296, 383, 393, 
      126, 160, 361)))

# Record IDs 105, 287, 289, 296, 383, 393 -> incomplete lonely scores -> excluded
# Record IDs 126, 160, 361                -> age > 75                 -> excluded

# check
view(clean_df)  
nrow(raw_data) - nrow(clean_df) # should be 9

# -----------------------------
# 3. EXPLORATORY ANALYSES - AGE
# -----------------------------

# Age ('patient_age')
clean_df |> 
  summarise(
    age_mean = mean(patient_age),
    age_sd = sd(patient_age))
    
ggplot(data = raw_data, aes(x = patient_age))+
geom_histogram()

skimr::skim(raw_data, patient_age)

# Age by decade
clean_df <- clean_df |> 
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
clean_df |> 
  count(age_decade, patient_age) |> 
  arrange(patient_age)

clean_df_check <- clean_df |> 
  filter(patient_age < 18 | patient_age > 75)
view(clean_df_check) # should be 0/none

# Age by category
clean_df <- clean_df |> 
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
clean_df |> 
  count(age_cat, patient_age) |> 
  arrange(patient_age)

table(clean_df$age_cat, useNA = "ifany")

# Age continuous Histogram
ggplot(clean_df, aes(x = patient_age)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2, fill = "steelblue", color = "white")+
  geom_density(alpha = .2, fill = "navy") +
  labs(title = "Distribution of Participant Age", x = "Participant Age", y = "Count")

# Age by categories (18-35, 35-50, 50+)
ggplot(clean_df, aes(x = age_cat)) + 
  geom_bar(fill = "grey36") +
  labs(title = "Participants Grouped by age_cat")

# Age by decade
ggplot(clean_df, aes(x = age_decade)) + 
  geom_bar(fill = "maroon") +
  labs(title = "Participants Grouped by Decade")

# Continuous Age 'patient_age' Summary
clean_df |> 
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

# view var names in 'clean_df'
var_names = names(clean_df)
view(var_names)
var_names = data.frame(variable = var_names)

# new data frame with selected variables
clean_df <- clean_df |> 
  select(
    record_id,
    patient_age,
    age_cat,
    age_decade,
    patient_gender,
    patient_ethnicity,
    patient_race,
    patient_race_2,
    nervous_2,
    worrying_2,
    worry_2,
    relax_2,
    restless_2,
    annoyed_2,
    afraid_2,
    gad_score,
    interest,
    down,
    sleep,
    relax,
    appetite,
    bad,
    concentrate,
    fidget,
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
clean_df <- clean_df |> 
  mutate(
    gender = case_when(
      patient_gender == 1 ~ 0, # female '0' ref
      patient_gender == 2 ~ 1, # male   '1'
      TRUE                ~ NA), 
    cigarette = case_when(
      cigarette_1 == 1 ~ 1,    # yes '1'
      cigarette_1 == 2 ~ 0,    # no  '0' ref
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
view(clean_df)
# write_csv(clean_df, "clean_df_1.7.26.csv")
table(clean_df$cigarette_1, clean_df$cigarette, useNA = "ifany")
table(clean_df$patient_gender, clean_df$gender, useNA = "ifany")

clean_df |>
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

# -------------------------
# 4b. PHQ/GAD SCORE DICHOTS
# -------------------------

clean_df <- clean_df |> 
  rowwise() |> 
  mutate(
    gad_score = if_else(
      record_id %in% c(61, 275),
      sum(c(nervous_2, worrying_2, worry_2, relax_2, restless_2, annoyed_2, afraid_2), na.rm = FALSE),
      gad_score),
    
    phq_score = if_else(
      record_id %in% c(61, 275),
      sum(c(interest, down, sleep, relax, appetite, bad, concentrate, fidget), na.rm = FALSE),
      phq_score)) |> 
  ungroup()

clean_df <- clean_df |> 
  mutate(
    gad_dichot = case_when(
      gad_score >= 10 ~ 1,
      gad_score < 10 ~ 0,
      TRUE ~ NA_real_),
    
    phq_dichot = case_when(
      phq_score >= 10 ~ 1,
      phq_score < 10 ~ 0,
      TRUE ~ NA_real_))

view(clean_df)
table(clean_df$phq_score, clean_df$phq_dichot, useNA = "ifany")
table(clean_df$gad_score, clean_df$gad_dichot, useNA = "ifany")

# ---------------------
# 4c. NEW RACE VARIABLE
# ---------------------

clean_df <- clean_df |> 
  mutate(
    new_race = case_when(
      patient_ethnicity == 1 ~ 1, # Hispanic/Latino (any race)
      patient_ethnicity == 2 & patient_race == 1 ~ 2, # White, Non-Hispanic
      patient_ethnicity == 2 & patient_race == 2 ~ 3, # Black/African American, Non-Hispanic
      patient_ethnicity == 2 & patient_race %in% c(3,4) ~ 4, # Asian or AI/AN
      TRUE ~ NA_integer_),
    new_race_f = factor(
      new_race,
      levels = c(1,2,3,4),
      labels = c(
        "Hispanic/Latino",
        "White, Non-Hispanic",
        "Black/African American, Non-Hispanic",
        "Asian or American Indian/Alaskan Native")))
  
# check
table(clean_df$new_race, useNA = "ifany")
table(clean_df$new_race_f, useNA = "ifany")

## 13 NAs - discuss with Zach
## discussed on 1/15/26 - leave as is. that's to be expected for now

# ------------------------------
# 4d. NEW ILLICIT DRUGS VARIABLE
# ------------------------------

clean_df <- clean_df |> 
  mutate(
    illicit = as.integer(
      rowSums(across(c(
        cocaine, heroin, opioid, 
        med_anxiety, adhd, illegal_drug))) > 0))

# check
table(clean_df$illicit) # should be 34

# -----------------------
# 4e. NAs for 'alcohol_2'
# -----------------------

clean_df <- clean_df |> 
  mutate(alcohol2 = replace_na(alcohol2, 0))

# check
view(clean_df)

# ---------------------------------
# 4f. 12-MONTH SUBSTANCE USE DICHOT
# ---------------------------------

str(clean_df$tobacco)
unique(clean_df$tobacco)
table(clean_df$tobacco, useNA = "ifany")
table(clean_df$meds, useNA = "ifany")

clean_df <- clean_df |> 
  mutate(
    tobacco_dichot = case_when(tobacco %in% 1:4 ~ 1,
                               tobacco == 5     ~ 0,
                               TRUE             ~ NA_real_),
    
    males_dichot = case_when(males %in% 1:4 ~ 1,
                             males == 5     ~ 0,
                             TRUE           ~ NA_real_),
    
    females_dichot = case_when(females %in% 1:4 ~ 1,
                               females == 5     ~ 0,
                               TRUE             ~ NA_real_),
    
    drugs_dichot = case_when(drugs %in% 1:4 ~ 1,
                             drugs == 5     ~ 0,
                             TRUE           ~ NA_real_),
    
    meds_dichot = case_when(meds %in% 1:4 ~ 1,
                            meds == 5     ~ 0,
                            TRUE          ~ NA_real_))

# -------------------------------
# 4g. 12-MONTH BINGE ALC VARIABLE
# -------------------------------

clean_df <- clean_df |> 
  mutate(
    binge_12mo_raw = coalesce(males, females), 
    
    binge_12mo = case_when(
      binge_12mo_raw %in% 1:4 ~ 1,
      binge_12mo_raw == 5     ~ 0,
      TRUE                    ~ NA_real_))

# check all 12 month outcomes
table(clean_df$tobacco, clean_df$tobacco_dichot, useNA = "ifany")
table(clean_df$drugs, clean_df$drugs_dichot, useNA = "ifany")
table(clean_df$meds, clean_df$meds_dichot, useNA = "ifany") # NA meds is ok, that happens...
table(clean_df$males, clean_df$males_dichot, useNA = "ifany")
table(clean_df$females, clean_df$females_dichot, useNA = "ifany")
table(clean_df$binge_12mo_raw, clean_df$binge_12mo, useNA = "ifany")

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

clean_df <- clean_df |> 
  rowwise() |> 
  mutate(
    companionship = if_else(
      record_id == 98,
      round(mean(c(
        in_tune, companion, turn_to, alone, group,
             outgoing, common, close, interest_ideas, close_people,
             left_out, relationship, knows_you, isolated, understand,
             shy, around_you, talk_to, turn), na.rm = TRUE)),
      companionship),
    
    left_out = if_else(
      record_id == 421,
      round(mean(c(
        in_tune, companion, turn_to, alone, group,
               outgoing, common, close, interest_ideas, close_people,
               relationship, knows_you, isolated, companionship, understand,
               shy, around_you, talk_to, turn), na.rm = TRUE)),
      left_out)) |> 
  ungroup()

# check
clean_df |>
  filter(record_id %in% c(98, 421)) |>
  select(record_id, companionship, left_out)

# --------------------------------------------------
# 5b. REVERSE CODING:
#     'in_tune', 'group', 'common','outgoing',
#     'close_people', 'companionship', 'understand',
#     'talk_to', 'turn'
# --------------------------------------------------

# METHOD 1 - preference
score_df <- clean_df |> 
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

# -----------------------------------------------------------------------------
# 5c. CALCULATE TOTAL LONELY SCORES (continuous lonely variable, 'lonely_cont')
# -----------------------------------------------------------------------------

score_df <- score_df|>
  mutate(
    lonely_cont = (in_tune_2 + companion + turn_to + alone + group_2 + common_2 + 
      close + interest_ideas + outgoing_2 + close_people_2 + 
      left_out + relationship + knows_you + isolated + companionship_2 + 
      understand_2 + shy + around_you + talk_to_2 + turn_2))

# check
view(score_df |>
       summarise(
         lonely_min = min(lonely_cont, na.rm = TRUE),
         lonely_max = max(lonely_cont, na.rm = TRUE))) 

# loneliness score range: 20-74

# lonely_cont summary
score_df |> 
  summarise(
    mean_lonely = mean(lonely_cont),
    sd_lonely = sd(lonely_cont),
    median_lonely = median(lonely_cont),
    IQR_lonely = IQR(lonely_cont),
    min_lonely = min(lonely_cont),
    max_lonely = max(lonely_cont))

# -----------------
# 5d. DICHOT LONELY
# -----------------

cutoff <- 43

score_df <- score_df |>
  mutate(
    lonely_dichot = case_when(
      lonely_cont >= 43 ~ 1,
      lonely_cont <  43 ~ 0,
      TRUE ~ NA_real_),
    
    lonely_dichot = factor(
      lonely_dichot,
      levels = c(0, 1),
      labels = c("None/Mild", "Mod/High")))


# check
class(score_df$lonely_dichot)
table(score_df$lonely_cont, score_df$lonely_dichot, useNA = "ifany")

# -----------------------------
# 5e. EXPLORATION LONELY SCORES
# -----------------------------

# Loneliness Prevalence for entire sample
prev_tbl <- score_df |>
  summarise(
    N = n(),  # n=296
    lonely = sum(lonely_cont >= 43, na.rm = TRUE),
    prevalence = lonely / N)

prev_tbl

# check
nrow(score_df) # n = 296
sum(is.na(score_df$lonely_cont)) # should be 0

# Loneliness Average for entire sample
mean_tbl <- score_df |> 
  summarise(
    N = n(), # n=296
    mean_total = mean(lonely_cont, na.rm = TRUE),
    sd_total = sd(lonely_cont, na.rm = TRUE)) |> 
  mutate(
    lonely_cont = sprintf("%0.2f (%0.2f)", mean_total, sd_total))

view(mean_tbl)

# ==============================
# 6a. DESCRIPTIVES
# ==============================

# exposure distributions
table(score_df$lonely_cont, useNA = "ifany")
prop.table(table(score_df$lonely_cont))

## outcome distributions
# 12-month
lapply(
  clean_df[c("tobacco_dich", "males_dich", "females_dich", "drugs_dich", "meds_dich")],
  table,
  useNA = "ifany")


# unadjusted cross-tabs


# ==========================================================
# 6b. TABLE 1 - Sample characteristics of ED patients (N=X) 

# 6b1. TABLE 1 - dichotomous loneliness
# ==========================================================

# totals
n_total <- nrow(score_df)
n_none_mild <- sum(score_df$lonely_dichot == "None/Mild", na.rm = TRUE)
n_mod_high  <- sum(score_df$lonely_dichot == "Mod/High", na.rm = TRUE)

## Sex 'gender'
# total
total_gender <- table(score_df$gender)
prop.table(total_gender)*100

# stratified
tab_gender <- table(score_df$gender, score_df$lonely_dichot)
prop.table(tab_gender, margin = 2)*100

# chi-square
chisq.test(tab_gender)$p.value # OR
chisq.test(table(score_df$gender, score_df$lonely_dichot))


## Age 'age_cat'
# total
total_age <- table(score_df$age_cat)
prop.table(total_age)*100

# stratified
tab_age   <- table(score_df$age_cat, score_df$lonely_dichot)
prop.table(tab_age, margin = 2)*100

# chi-square
chisq.test(table(score_df$age_cat, score_df$lonely_dichot))
 

## Race/Ethnicity 'new_race'
# total
total_race <- table(score_df$new_race)
prop.table(total_race)*100

# stratified
tab_race <- table(score_df$new_race, score_df$lonely_dichot)
prop.table(tab_race, margin = 2)*100

# chi-sq
chisq.test(table(score_df$age_cat, score_df$lonely_dichot))


## Anxiety 'gad_dichot'
# total
total_anxiety <- table(score_df$gad_dichot)
prop.table(total_anxiety)*100

# stratified
tab_anxiety <- table(score_df$gad_dichot, score_df$lonely_dichot)
prop.table(tab_anxiety, margin = 2)*100

# chi-sq
chisq.test(table(score_df$gad_dichot, score_df$lonely_dichot))


## Depression 'phq_dichot'
# total
total_dep <- table(score_df$phq_dichot)
prop.table(total_dep)*100

# stratified
tab_dep <- table(score_df$phq_dichot, score_df$lonely_dichot)
prop.table(tab_dep, margin = 2)*100

# chi-sq
chisq.test(table(score_df$phq_dichot, score_df$lonely_dichot))


## Cigarette Use 'cigarette'
# total
total_cig <- table(score_df$cigarette)
prop.table(total_cig)*100

# stratified
tab_cig <- table(score_df$cigarette, score_df$lonely_dichot)
prop.table(tab_cig, margin = 2)*100

# chi-sq
chisq.test(table(score_df$cigarette, score_df$lonely_dichot))


## Binge Drink 'alcohol2'
# total
total_alc <- table(score_df$alcohol2)
prop.table(total_alc)*100

# stratified
tab_alc <- table(score_df$alcohol2, score_df$lonely_dichot)
prop.table(tab_alc, margin = 2)*100

# chi-sq
chisq.test(table(score_df$cigarette, score_df$lonely_dichot))


## Cannabis 'marijuana'
# total
total_cannabis <- table(score_df$marijuana)
prop.table(total_cannabis)*100

# stratified
tab_can <- table(score_df$marijuana, score_df$lonely_dichot)
prop.table(tab_can, margin = 2)*100

# chi-sq
chisq.test(table(score_df$marijuana, score_df$lonely_dichot))


## Illicit 'illicit'
# total
total_illicit <- table(score_df$illicit)
total_illicit
prop.table(total_illicit)*100

# stratified
tab_illicit <- table(score_df$illicit, score_df$lonely_dichot)
tab_illicit
prop.table(tab_illicit, margin = 2)*100

# chi-sq
chisq.test(table(score_df$illicit, score_df$lonely_dichot))


# --------------------------------------
# 6b2. TABLE 1 - continuous loneliness
# --------------------------------------

# totals
n_total <- nrow(score_df)
n_none_mild <- sum(score_df$lonely_dichot == "None/Mild", na.rm = TRUE)
n_mod_high  <- sum(score_df$lonely_dichot == "Mod/High", na.rm = TRUE)

## Sex 'gender'
# total
total_gender <- table(score_df$gender)
prop.table(total_gender)*100

# stratified
tab_gender <- table(score_df$gender, score_df$lonely_dichot)
prop.table(tab_gender, margin = 2)*100




## Age 'age_cat'
# total
total_age <- table(score_df$age_cat)
prop.table(total_age)*100

# stratified
tab_age   <- table(score_df$age_cat, score_df$lonely_dichot)
prop.table(tab_age, margin = 2)*100




## Race/Ethnicity 'new_race'
# total
total_race <- table(score_df$new_race)
prop.table(total_race)*100

# stratified
tab_race <- table(score_df$new_race, score_df$lonely_dichot)
prop.table(tab_race, margin = 2)*100




## Anxiety 'gad_dichot'
# total
total_anxiety <- table(score_df$gad_dichot)
prop.table(total_anxiety)*100

# stratified
tab_anxiety <- table(score_df$gad_dichot, score_df$lonely_dichot)
prop.table(tab_anxiety, margin = 2)*100




## Depression 'phq_dichot'
# total
total_dep <- table(score_df$phq_dichot)
prop.table(total_dep)*100

# stratified
tab_dep <- table(score_df$phq_dichot, score_df$lonely_dichot)
prop.table(tab_dep, margin = 2)*100



## Cigarette Use 'cigarette'
# total
total_cig <- table(score_df$cigarette)
prop.table(total_cig)*100

# stratified
tab_cig <- table(score_df$cigarette, score_df$lonely_dichot)
prop.table(tab_cig, margin = 2)*100




## Binge Drink 'alcohol2'
# total
total_alc <- table(score_df$alcohol2)
prop.table(total_alc)*100

# stratified
tab_alc <- table(score_df$alcohol2, score_df$lonely_dichot)
prop.table(tab_alc, margin = 2)*100




## Cannabis 'marijuana'
# total
total_cannabis <- table(score_df$marijuana)
prop.table(total_cannabis)*100

# stratified
tab_can <- table(score_df$marijuana, score_df$lonely_dichot)
prop.table(tab_can, margin = 2)*100



## Illicit 'illicit'
# total
total_illicit <- table(score_df$illicit)
total_illicit
prop.table(total_illicit)*100

# stratified
tab_illicit <- table(score_df$illicit, score_df$lonely_dichot)
tab_illicit
prop.table(tab_illicit, margin = 2)*100









# ================================================
# 7. LOGISTIC REGRESSION MODELS — 3-MONTH OUTCOMES
# ================================================

# Outcomes:
#   a) Binge alcohol use    'alcohol2'
#   b) Cannabis use         'marijuana'
#   c) Illicit drug use     'illicit'
#   d) Tobacco use          'cigarette'
#
# For each outcome:
#   - Loneliness modeled as dichotomous and continuous
#   - Models estimated hierarchically:
#       b0 = crude (unadjusted)
#       b1 = + demographics
#       b2 = + anxiety and depression
#       b3 = + tobacco use (except when cigarette is the outcome)

# OR (95% CI) function:
or_ci <- function(model) {
  out <- exp(cbind(
    OR = coef(model),
    confint(model)
  ))
  return(out)
}

# ------------------------------------------------
# 7a. Binge Alcohol Use - 3mo - Loneliness (dich)
# ------------------------------------------------

# b0: crude
m_alc_3m_dich_b0 <- glm(alcohol2 ~ lonely_dichot, data = score_df, family = binomial)
summary(m_alc_3m_dich_b0)
or_ci(m_alc_3m_dich_b0)

broom::tidy(
  m_alc_3m_dich_b0,
  exponentiate = TRUE,
  conf.int = TRUE) |> 
  filter(grepl("^lonely_dichot", term)) |> 
  mutate(
    OR = round(estimate, 2),
    CI = paste0("(", round(conf.low, 2), ",", round(conf.high, 2), ")"),
    p = signif(p.value, 3)) |> 
  select(OR, CI, p)
  
# b1: 
m_alc_3m_dich_b1 <- glm(alcohol2 ~ lonely_dichot + age_cat + gender + new_race,
                        data = score_df,
                        family = binomial)
summary(m_alc_3m_dich_b1)
or_ci(m_alc_3m_dich_b1)

# b2: 
m_alc_3m_dich_b2 <- glm(alcohol2 ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot,
                        data = score_df,
                        family = binomial)
summary(m_alc_3m_dich_b2)
or_ci(m_alc_3m_dich_b2)

# b3: 
m_alc_3m_dich_b3 <- glm(alcohol2 ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot + cigarette,
                        data = score_df,
                        family = binomial)
summary(m_alc_3m_dich_b3)
or_ci(m_alc_3m_dich_b3)

results_alc_3m_dich <- bind_rows(
  b0 = tidy(m_alc_3m_dich_b0, exponentiate = TRUE, conf.int = TRUE),
  b1 = tidy(m_alc_3m_dich_b1, exponentiate = TRUE, conf.int = TRUE),
  b2 = tidy(m_alc_3m_dich_b2, exponentiate = TRUE, conf.int = TRUE),
  b3 = tidy(m_alc_3m_dich_b3, exponentiate = TRUE, conf.int = TRUE),
  .id = "model") |>
  filter(grepl("^lonely_dichot", term))
view(results_alc_3m_dich)

# ------------------------------------------------
# 7a. Binge Alcohol Use — 3mo — Loneliness (cont)
# ------------------------------------------------

# b0: crude
m_alc_3m_cont_b0 <- glm(alcohol2 ~ lonely_cont, data = score_df, family = binomial)
summary(m_alc_3m_cont_b0)
or_ci(m_alc_3m_cont_b0)

# b1:
m_alc_3m_cont_b1 <- glm(alcohol2 ~ lonely_cont + age_cat + gender + new_race, 
                        data = score_df, family = binomial)
summary(m_alc_3m_cont_b1)
or_ci(m_alc_3m_cont_b1)

# b2:
m_alc_3m_cont_b2 <- glm(alcohol2 ~ lonely_cont + age_cat + gender + new_race + phq_dichot + gad_dichot, 
                        data = score_df, family = binomial)
summary(m_alc_3m_cont_b2)
or_ci(m_alc_3m_cont_b2)

# b3:
m_alc_3m_cont_b3 <- glm(alcohol2 ~ lonely_cont + age_cat + gender + new_race + phq_dichot + gad_dichot + cigarette, 
                        data = score_df, family = binomial)
summary(m_alc_3m_cont_b3)
or_ci(m_alc_3m_cont_b3)


# --------------------------------------------
# 7b. Cannabis Use — 3mo — Loneliness (dichot)
# --------------------------------------------

# b0: crude
m_can_3m_dich_b0 <- glm(marijuana ~ lonely_dichot, data = score_df, family = binomial)
summary(m_can_3m_dich_b0)
or_ci(m_can_3m_dich_b0)

# b1:
m_can_3m_dich_b1 <- glm(marijuana ~ lonely_dichot + age_cat + gender + new_race,
                        data = score_df,
                        family = binomial)
summary(m_can_3m_dich_b1)
or_ci(m_can_3m_dich_b1)

# b2:
m_can_3m_dich_b2 <- glm(marijuana ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot,
                        data = score_df,
                        family = binomial)
summary(m_can_3m_dich_b2)
or_ci(m_can_3m_dich_b2)

# b3:
m_can_3m_dich_b3 <- glm(marijuana ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot + cigarette,
                        data = score_df,
                        family = binomial)
summary(m_can_3m_dich_b3)
or_ci(m_can_3m_dich_b3)

# ------------------------------------------
# 7b. Cannabis Use — 3mo — Loneliness (cont)
# ------------------------------------------

# b0: crude
m_can_3m_cont_b0 <- glm(marijuana ~ lonely_cont, data = score_df, family = binomial)
summary(m_can_3m_cont_b0)
or_ci(m_can_3m_cont_b0)

# b1:
m_can_3m_cont_b1 <- glm(marijuana ~ lonely_cont + age_cat + gender + new_race,
                        data = score_df, family = binomial)
summary(m_can_3m_cont_b1)
or_ci(m_can_3m_cont_b1)

# b2:
m_can_3m_cont_b2 <- glm(marijuana ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot,
                        data = score_df, family = binomial)
summary(m_can_3m_cont_b2)
or_ci(m_can_3m_cont_b2)

# b3:
m_can_3m_cont_b3 <- glm(marijuana ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot + cigarette,
                        data = score_df, family = binomial)
summary(m_can_3m_cont_b3)
or_ci(m_can_3m_cont_b3)

# ------------------------------------------------
# 7c. Illicit Drug Use — 3mo — Loneliness (dichot)
# ------------------------------------------------

# b0: crude
m_il_3m_dich_b0 <- glm(illicit ~ lonely_dichot, data = score_df, family = binomial)
summary(m_il_3m_dich_b0)
or_ci(m_il_3m_dich_b0)

# b1:
m_il_3m_dich_b1 <- glm(illicit ~ lonely_dichot + age_cat + gender + new_race,
                       data = score_df, family = binomial)
summary(m_il_3m_dich_b1)
or_ci(m_il_3m_dich_b1)

# b2:
m_il_3m_dich_b2 <- glm(illicit ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot,
                       data = score_df, family = binomial)
summary(m_il_3m_dich_b2)
or_ci(m_il_3m_dich_b2)

# b3:
m_il_3m_dich_b3 <- glm(illicit ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot + cigarette,
                       data = score_df, family = binomial)
summary(m_il_3m_dich_b3)
or_ci(m_il_3m_dich_b3)

# ------------------------------------------------
# 7c. Illicit Drug Use — 3mo — Loneliness (cont)
# ------------------------------------------------

# b0: crude
m_il_3m_cont_b0 <- glm(illicit ~ lonely_cont, data = score_df, family = binomial)
summary(m_il_3m_cont_b0)
or_ci(m_il_3m_cont_b0)

# b1:
m_il_3m_cont_b1 <- glm(illicit ~ lonely_cont + age_cat + gender + new_race, data = score_df, family = binomial)
summary(m_il_3m_cont_b1)
or_ci(m_il_3m_cont_b1)

# b2:
m_il_3m_cont_b2 <- glm(illicit ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot,
                       data = score_df, family = binomial)
summary(m_il_3m_cont_b2)
or_ci(m_il_3m_cont_b2)

# b3:
m_il_3m_cont_b3 <- glm(illicit ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot + cigarette,
                       data = score_df, family = binomial)
summary(m_il_3m_cont_b3)
or_ci(m_il_3m_cont_b3)

# ------------------------------------------------
# 7d. Tobacco Use — 3mo — Loneliness (dichot)
# ------------------------------------------------

# b0: crude
m_tob_3m_dich_b0 <- glm(cigarette ~ lonely_dichot, data = score_df, family = binomial)
summary(m_tob_3m_dich_b0)
or_ci(m_tob_3m_dich_b0)

# b1:
m_tob_3m_dich_b1 <- glm(cigarette ~ lonely_dichot + age_cat + gender + new_race,
                        data = score_df, family = binomial)
summary(m_tob_3m_dich_b1)
or_ci(m_tob_3m_dich_b1)

# b2:
m_tob_3m_dich_b2 <- glm(cigarette ~ lonely_dichot + age_cat + gender + new_race + gad_score + phq_dichot,
                        data = score_df, family = binomial)
summary(m_tob_3m_dich_b2)
or_ci(m_tob_3m_dich_b2)

# b3 omitted for cigarette outcome (not applicable)

# ------------------------------------------------
# 7d. Tobacco Use — 3mo — Loneliness (dichot)
# ------------------------------------------------

# b0: crude
m_tob_3m_cont_b0 <- glm(cigarette ~ lonely_cont, data = score_df, family = binomial)
summary(m_tob_3m_cont_b0)
or_ci(m_tob_3m_cont_b0)

# b1
m_tob_3m_cont_b1 <- glm(cigarette ~ lonely_cont + age_cat + gender + new_race, data = score_df, family = binomial)
summary(m_tob_3m_cont_b1)
or_ci(m_tob_3m_cont_b1)

# b2
m_tob_3m_cont_b2 <- glm(cigarette ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot,
                        data = score_df, family = binomial)
summary(m_tob_3m_cont_b2)
or_ci(m_tob_3m_cont_b2)

# b3 N/A

# =================================================
# 8. LOGISTIC REGRESSION MODELS — 12-MONTH OUTCOMES
# =================================================

# Outcomes:
#   a) Binge alcohol use     'binge_12mo'
#   b) Illicit drug use      'drugs_dichot'
#   c) Prescription med use  'meds_dichot'
#   d) Tobacco use           'tobacco_dichot'
#
# For each outcome:
#   - Loneliness modeled as dichotomous and continuous
#   - Models estimated hierarchically:
#       b0 = crude (unadjusted)
#       b1 = + demographics
#       b2 = + anxiety and depression
#       b3 = + tobacco use (except when cigarette is the outcome)

# ------------------------------------------------
# 8a. Binge Alcohol Use - 12mo - Loneliness (dich)
# ------------------------------------------------

# b0: crude
m_alc_12m_dich_b0 <- glm(binge_12mo ~ lonely_dichot, data = score_df, family = binomial)
summary(m_alc_12m_dich_b0)
or_ci(m_alc_12m_dich_b0)

# b1:
m_alc_12m_dich_b1 <- glm(binge_12mo ~ lonely_dichot + age_cat + gender + new_race,
                         data = score_df, family = binomial)
summary(m_alc_12m_dich_b1)
or_ci(m_alc_12m_dich_b1)

# b2:
m_alc_12m_dich_b2 <- glm(binge_12mo ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot,
                         data = score_df, family = binomial)
summary(m_alc_12m_dich_b2)
or_ci(m_alc_12m_dich_b2)

# b3:
m_alc_12m_dich_b3 <- glm(binge_12mo ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot + tobacco_dichot,
                         data = score_df, family = binomial)
summary(m_alc_12m_dich_b3)
or_ci(m_alc_12m_dich_b3)

# ------------------------------------------------
# 8a. Binge Alcohol Use — 12mo — Loneliness (cont)
# ------------------------------------------------

# b0: crude
m_alc_12mo_cont_b0 <- glm(binge_12mo ~ lonely_cont, data = score_df, family = binomial)
summary(m_alc_12mo_cont_b0)
or_ci(m_alc_12mo_cont_b0)

# b1:
m_alc_12mo_cont_b1 <- glm(binge_12mo ~ lonely_cont + age_cat + gender + new_race, data = score_df, family = binomial)
summary(m_alc_12mo_cont_b1)
or_ci(m_alc_12mo_cont_b1)

# b2:
m_alc_12mo_cont_b2 <- glm(binge_12mo ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot,
                          data = score_df, family = binomial)
summary(m_alc_12mo_cont_b2)
or_ci(m_alc_12mo_cont_b2)

# b3:
m_alc_12mo_cont_b3 <- glm(binge_12mo ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot + tobacco_dichot,
                          data = score_df,family = binomial)
summary(m_alc_12mo_cont_b3)
or_ci(m_alc_12mo_cont_b3)

# ------------------------------------------------
# 8b. Illicit Drug Use - 12mo - Loneliness (dich)
# ------------------------------------------------

# b0: crude
m_il_12m_dich_b0 <- glm(drugs_dichot ~ lonely_dichot, data = score_df, family = binomial)
summary(m_il_12m_dich_b0)
or_ci(m_il_12m_dich_b0)

# b1:
m_il_12m_dich_b1 <- glm(drugs_dichot ~ lonely_dichot + age_cat + gender + new_race,
                        data = score_df, family = binomial)
summary(m_il_12m_dich_b1)
or_ci(m_il_12m_dich_b1)

# b2:
m_il_12m_dich_b2 <- glm(drugs_dichot ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot,
                        data = score_df, family = binomial)
summary(m_il_12m_dich_b2)
or_ci(m_il_12m_dich_b2)

# b3:
m_il_12m_dich_b3 <- glm(drugs_dichot ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot + tobacco_dichot,
                        data = score_df, family = binomial)
summary(m_il_12m_dich_b3)
or_ci(m_il_12m_dich_b3)

# ------------------------------------------------
# 8b. Illicit Drug Use — 12mo — Loneliness (cont)
# ------------------------------------------------

# b0: crude
m_il_12mo_cont_b0 <- glm(drugs_dichot ~ lonely_cont, data = score_df, family = binomial)
summary(m_il_12mo_cont_b0)
or_ci(m_il_12mo_cont_b0)

# b1:
m_il_12mo_cont_b1 <- glm(drugs_dichot ~ lonely_cont + age_cat + gender + new_race, data = score_df, family = binomial)
summary(m_il_12mo_cont_b1)
or_ci(m_il_12mo_cont_b1)

# b2:
m_il_12mo_cont_b2 <- glm(drugs_dichot ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot,
                         data = score_df, family = binomial)
summary(m_il_12mo_cont_b2)
or_ci(m_il_12mo_cont_b2)

# b3:
m_il_12mo_cont_b3 <- glm(drugs_dichot ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot + tobacco_dichot,
                         data = score_df, family = binomial)
summary(m_il_12mo_cont_b3)
or_ci(m_il_12mo_cont_b3)

# ----------------------------------------------------
# 8c. Prescription Meds Use - 12mo - Loneliness (dich)
# ----------------------------------------------------

# b0: crude
m_med_12m_dich_b0 <- glm(meds_dichot ~ lonely_dichot, data = score_df, family = binomial)
summary(m_med_12m_dich_b0)
or_ci(m_med_12m_dich_b0)

# b1:
m_med_12m_dich_b1 <- glm(meds_dichot ~ lonely_dichot + age_cat + gender + new_race,
                         data = score_df, family = binomial)
summary(m_med_12m_dich_b1)
or_ci(m_med_12m_dich_b1)

# b2:
m_med_12m_dich_b2 <- glm(meds_dichot ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot,
                         data = score_df, family = binomial)
summary(m_med_12m_dich_b2)
or_ci(m_med_12m_dich_b2)

# b3:
m_med_12m_dich_b3 <- glm(meds_dichot ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot + tobacco_dichot,
                        data = score_df, family = binomial)
summary(m_med_12m_dich_b3)
or_ci(m_med_12m_dich_b3)

# ----------------------------------------------------
# 8c. Prescription Meds Use - 12mo - Loneliness (cont)
# ----------------------------------------------------

# b0: crude
m_med_12mo_cont_b0 <- glm(drugs_dichot ~ lonely_cont, data = score_df, family = binomial)
summary(m_med_12mo_cont_b0)
or_ci(m_med_12mo_cont_b0)

# b1:
m_med_12mo_cont_b1 <- glm(drugs_dichot ~ lonely_cont + age_cat + gender + new_race,data = score_df, family = binomial)
summary(m_med_12mo_cont_b1)
or_ci(m_med_12mo_cont_b1)

# b2:
m_med_12mo_cont_b2 <- glm(drugs_dichot ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot,
                          data = score_df, family = binomial)
summary(m_med_12mo_cont_b2)
or_ci(m_med_12mo_cont_b2)

# b3:
m_med_12mo_cont_b3 <- glm(drugs_dichot ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot + tobacco_dichot,
                          data = score_df, family = binomial)
summary(m_med_12mo_cont_b3)
or_ci(m_med_12mo_cont_b3)


# --------------------------------------------
# 8d. Tobacco Use - 12mo - Loneliness (dich)
# --------------------------------------------

# b0: crude
m_tob_12m_dich_b0 <- glm(tobacco_dichot ~ lonely_dichot, data = score_df, family = binomial)
summary(m_tob_12m_dich_b0)
or_ci(m_tob_12m_dich_b0)

# b1:
m_tob_12m_dich_b1 <- glm(tobacco_dichot ~ lonely_dichot + age_cat + gender + new_race,
                         data = score_df, family = binomial)
summary(m_tob_12m_dich_b1)
or_ci(m_tob_12m_dich_b1)

# b2:
m_tob_12m_dich_b2 <- glm(tobacco_dichot ~ lonely_dichot + age_cat + gender + new_race + gad_dichot + phq_dichot,
                         data = score_df, family = binomial)
summary(m_tob_12m_dich_b2)
or_ci(m_tob_12m_dich_b2)

# b3: N/A

# --------------------------------------------
# 8d. Tobacco Use - 12mo - Loneliness (cont)
# --------------------------------------------

# b0: crude
m_tob_12mo_cont_b0 <- glm(tobacco_dichot ~ lonely_cont, data = score_df, family = binomial)
summary(m_tob_12mo_cont_b0)
or_ci(m_tob_12mo_cont_b0)

# b1:
m_tob_12mo_cont_b1 <- glm(tobacco_dichot ~ lonely_cont + age_cat + gender + new_race,
                          data = score_df, family = binomial)
summary(m_tob_12mo_cont_b1)
or_ci(m_tob_12mo_cont_b1)

# b2:
m_tob_12mo_cont_b2 <- glm(tobacco_dichot ~ lonely_cont + age_cat + gender + new_race + gad_dichot + phq_dichot,
                          data = score_df, family = binomial)
summary(m_tob_12mo_cont_b2)
or_ci(m_tob_12mo_cont_b2)

# b3: N/A

# =================================================
# 9. STRATIFIED ANALYSIS BY ANXIETY AND DEPRESSION
# =================================================

#------------------------------
# Stratify by:
#     if either 'gad_dichot' = 1 OR 'phq_dichot' = 1 (moderate/severe anxiety OR moderate/severe depression symptoms),
#     then 'mh_strata' = 1. 
#
#     else, if 'gad_dichot' = 0 AND 'phq_dichot' = 0 (mild/none anxiety AND mild/none depression symptoms),
#     then 'mh_strata' = 1.
#------------------------------

mh_df <- score_df |> 
  mutate(
    mh_strata = case_when(
      gad_dichot == 1 | phq_dichot == 1 ~ 1,
      gad_dichot == 0 & phq_dichot == 0 ~ 0,
      TRUE ~ NA_real_),
    
    mh_strata = factor(
      mh_strata,
      levels = c(0,1),
      labels = c("None/Mild MH symptoms", "Moderate/Severe MH symptoms")
      )
    )

# check
table(mh_df$mh_strata, useNA = "ifany")
prop.table(table(mh_df$mh_strata))

## results:
## mh_strata = 1 (mod/severe) N=103
## mh_strata = 0 (mild/none)  N=193

# descriptives

tbl_mh_demo <- mh_df |> 
  filter(!is.na(mh_strata)) |> 
  tbl_summary(
    by = mh_strata,
    include = c(age_cat, gender, new_race),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"),
    missing = "no")





