############################
## Prelim Analyses - Coding
## 
############################

## Prelim Analysis Plan:
## 1. LOAD
## 2. CLEAN
## 3. RECODE
## 4. CALCULATE
## 5. ANALYZE

## 1. LOAD LIBRARIES; UPLOAD DATA

# load libraries
library(tidyverse)

# find directory location
getwd()

# load dataset (raw, NOT labels version)
raw_LSU = read.csv(file = "./data/LonelinessAndSubstan-ALLDATA_DATA_2025-12-16_1345.csv")


# check that everything worked
skimr::skim(raw_LSU)
names(raw_LSU)
raw_LSU
view(raw_LSU) # <- use to view file from RStudio


## 2. EXPLORATORY ANALYSIS

# Age ('patient_age')
raw_LSU |> 
  summarise(
    age_mean = mean(patient_age),
    age_sd = sd(patient_age))
    
ggplot(data = raw_LSU, aes(x = patient_age))+
geom_histogram()

skimr::skim(raw_LSU, patient_age)

# Age by decade
age_df <- raw_LSU |> 
  mutate(
    age_decade = cut(
      patient_age,
      breaks = c(18, 29, 39, 49, Inf),
      labels = c("18-29", "30-39", "40-49", "50+"),
      right = FALSE),
    age_decade = factor(
      age_decade,
      levels = c(
        "18-29",
        "30-39",
        "40-49",
        "50+")))

# Age by category
age_df <- age_df |> 
  mutate(
    age_cat = case_when(
      patient_age >= 18 & patient_age <= 34 ~ "Young Adults (18-35)",
      patient_age >= 35 & patient_age <= 49 ~ "Middle Aged Adults (35-50)",
      patient_age >= 50 ~ "Older Adults (50+)"),
    age_cat = factor(
      age_cat,
      levels = c(
        "Young Adults (18-35)",
        "Middle Aged Adults (35-50)",
        "Older Adults (50+)")))

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



## 2. CLEAN DATA FOR PRELIM ANALYSIS

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
    
view(clean_df)


# recode dichot variables from 1(yes) 2(no) -> 1(yes) 0(no)
clean_df = clean_df |> 
  mutate(
    gender = case_when(
      patient_gender == 1 ~ 1,
      patient_gender == 2 ~ 0,
      TRUE                ~ NA), # <- are all these necessary?
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

# quick check it worked
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

table(clean_df$cigarette_1, clean_df$cigarette, useNA = "ifany")


# LONELINESS SCALE SCORING

# reverse coding for 'in_tune', 'group', 'common','outgoing',
#                    'close_people', 'companionship', 'understand', 'talk_to', 'turn'
# scoring total loneliness score

# first try
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
    turn_2 = 5 - turn,
    
    UCLA_total = 
      in_tune_2 + companion + turn_to + alone + group_2 + common_2 + 
      close + interest_ideas + outgoing_2 + close_people_2 + 
      left_out + relationship + knows_you + isolated + companionship_2 + 
      understand_2 + shy + around_you + talk_to_2 + turn_2)

# quick check!!!!!



# OR ALTERNATIVELY:
score_df_2 <- clean_df |> 
  mutate(
    in_tune_2 = case_when(
      in_tune == 1 ~ 4,
      in_tune == 2 ~ 3,
      in_tune == 3 ~ 2,
      in_tune == 4 ~ 1),
    group_2 = case_when(
      group == 1 ~ 4,
      group == 2 ~ 3,
      group == 3 ~ 2,
      group == 4 ~ 1),
    common_2 = case_when(
      common == 1 ~ 4,
      common == 2 ~ 3,
      common == 3 ~ 2,
      common == 4 ~ 1),
    outgoing_2 = case_when(
      outgoing == 1 ~ 4,
      outgoing == 2 ~ 3,
      outgoing == 3 ~ 2,
      outgoing == 4 ~ 1),
    close_people_2 = case_when(
      close_people == 1 ~ 4,
      close_people == 2 ~ 3,
      close_people == 3 ~ 2,
      close_people == 4 ~ 1),
    companionship_2 = case_when(
      companionship == 1 ~ 4,
      companionship == 2 ~ 3,
      companionship == 3 ~ 2,
      companionship == 4 ~ 1),
    understand_2 = case_when(
      understand == 1 ~ 4,
      understand == 2 ~ 3,
      understand == 3 ~ 2,
      understand == 4 ~ 1),
    talk_to_2 = case_when(
      talk_to == 1 ~ 4,
      talk_to == 2 ~ 3,
      talk_to == 3 ~ 2,
      talk_to == 4 ~ 1),
    turn_2 = case_when(
      turn == 1 ~ 4,
      turn == 2 ~ 3,
      turn == 3 ~ 2,
      turn == 4 ~ 1),
    
    UCLA_total = 
      in_tune_2 + companion + turn_to + alone + group_2 + common_2 + 
      close + interest_ideas + outgoing_2 + close_people_2 + 
      left_out + relationship + knows_you + isolated + companionship_2 + 
      understand_2 + shy + around_you + talk_to_2 + turn_2)

# quick check!!!!





## 3. Table 1 - Overall prevalence and prevalence by loneliness



## 4. Table 2 - Logistic Regression
















