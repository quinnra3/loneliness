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
raw_LSU = read.csv(file = "./data/LonelinessAndSubstan-ALLDATA_DATA_2025-12-03_1400.csv")

# check that everything worked
skimr::skim(raw_LSU)
names(raw_LSU)
raw_LSU
view(raw_LSU) # <- use to view file from RStudio


## 2. CLEAN DATA

# view var names
var_names = names(raw_LSU)
view(var_names)
class(var_names)
var_names = data.frame(variable = var_names)

write_csv(var_names, "var_names_table.csv")

# select needed variables
loneliness_df = raw_LSU |> 
  select(
    record_id,
    patient_age,
    patient_gender,
    patient_ethnicity,
    patient_race,
    patient_race_2,
    males,
    females,
    alcohol_1,
    alcohol_2,
    alcohol_3,
    alcohol_4,
    marijuana_1,
    marijuana_2,
    marijuana_3,
    drugs,
    cocaine_1,
    cocaine_2,
    cocaine_3,
    heroin_1,
    heroin_2,
    heroin_3,
    in_tune,
    companion,
    turn_to,
    alone,
    group,
    common,
    close,
    interest_ideas,
  # outgoing, <-- this is in UCLA scale on REDCap, data dictionary, not in downloaded CSVs?
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
    gad_score,
    phq_score)

view(loneliness_df)

# first, make age_cat
loneliness_df = loneliness_df |> 
  mutate(
    age_cat = case_when(
      patient_age >= 18 & patient_age < 35 ~ "18-34",
      patient_age >= 35 & patient_age < 50 ~ "35-49",
      patient_age >= 50 ~ "50+"))

loneliness_df |> 
  count(age_cat)

loneliness_df |> 
  mutate(
    age_group_check = cut(patient_age, breaks = c(17, 34.999, 49.999, Inf))) |> 
  filter(!is.na(patient_age)) |> 
  count(age_group_check, age_cat)

# recode dichot variables from 1(yes) 2(no) -> 1(yes) 0(no)
loneliness_df = loneliness_df |> 
  mutate(
    gender_dichot = case_when(
      patient_gender == 1 ~ 1,
      patient_gender == 2 ~ 0,
      TRUE                ~ NA),
    alcohol_dichot = case_when(
      alcohol_1 == 1 ~ 1,
      alcohol_1 == 2 ~ 0,
      alcohol_2 == 1 ~ 1,
      alcohol_2 == 2 ~ 0,
      alcohol_3 == 1 ~ 1,
      alcohol_3 == 2 ~ 0,
      alcohol_4 == 1 ~ 1,
      alcohol_4 == 2 ~ 0,
      TRUE           ~ NA),
    marijuana_dichot = case_when(
      marijuana_1 == 1 ~ 1,
      marijuana_1 == 2 ~ 0,
      marijuana_2 == 1 ~ 1,
      marijuana_2 == 2 ~ 0,
      marijuana_3 == 1 ~ 1,
      marijuana_3 == 2 ~ 0,
      TRUE             ~ NA),
    cocaine_dichot = case_when(
      cocaine_1 == 1 ~ 1,
      cocaine_1 == 2 ~ 0,
      cocaine_2 == 1 ~ 1,
      cocaine_2 == 2 ~ 0,
      cocaine_3 == 1 ~ 1,
      cocaine_3 == 2 ~ 0,
      TRUE           ~ NA),
    heroin_dichot = case_when(
      heroin_1 == 1 ~ 1,
      heroin_1 == 2 ~ 0,
      heroin_1 == 1 ~ 1,
      heroin_1 == 2 ~ 0,
      heroin_1 == 1 ~ 1,
      heroin_1 == 2 ~ 0,
      TRUE          ~ NA)
    )
  


## 3. 














