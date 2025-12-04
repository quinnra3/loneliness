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
# view(raw_LSU) <- use to view file from RStudio


## 2. CLEAN DATA

# view var names
var_names = names(raw_LSU)
view(var_names)

# select needed variables
prelim_LSU = raw_LSU |> 
  select(
    
  )



## 3. 