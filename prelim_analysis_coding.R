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

# Age by category
age_df <- age_df |> 
  mutate(
    age_cat = case_when(
      patient_age >= 18 & patient_age <= 35 ~ "Young Adults (18-35)",
      patient_age >= 35 & patient_age <= 50 ~ "Middle Aged Adults (35-50)",
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

# view var names
var_names = names(raw_LSU)
view(var_names)
class(var_names)
var_names = data.frame(variable = var_names)
#write_csv(var_names, "var_names_table.csv")

# select needed variables
loneliness_df = raw_LSU |> 
  select(
    record_id,
    patient_age,
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
    
    
    
    
view(loneliness_df)



# recode dichot variables from 1(yes) 2(no) -> 1(yes) 0(no)
loneliness_df = loneliness_df |> 
  mutate(
    gender = case_when(
      patient_gender == 1 ~ 1,
      patient_gender == 2 ~ 0,
      TRUE                ~ NA),
    alcohol = case_when(
      alcohol_1 == 1 ~ 1,
      alcohol_1 == 2 ~ 0,
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
      TRUE          ~ NA)
    )
  

## 3. Table 1 - Overall prevalence and prevalence by loneliness


## 4. Table 2 - Logistic Regression
















