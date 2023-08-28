# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 16 September, 2021 by Trevor Incerti

# This code cleans the merged voter file

# NOTE: VOTER FILE NOT INCLUDED IN DATA FOLDER AS PROPRIETARY INFORMATION.
# THIS SCRIPT THEREFORE CONTAINS REPLICATION CODE ONLY!

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES AND FUNCTIONS ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(janitor)
library(gender)
library(eeptools)
library(lubridate)
library(pryr)

# Functions
source("code/0. functions.R")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DATA IMPORT AND SEED ----
# ______________________________________________________________________________

# Set seed
set.seed(999) 

# Load data frame
load("data/cleaned/vf_all_renters.Rdata")
vf_clean <- vf_all_renters

# Remove original dataframe to save memory
rm(vf_all_renters) 

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# REDUCE DATASET TO MERGES WITH POSTERIOR PROBABILITY OF 99% OR GREATER ----
# ______________________________________________________________________________

vf_clean <- vf_clean %>%
  mutate(likely_renter = ifelse(posterior < 0.999 | is.na(likely_renter), 0, 1))

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# REMOVE UNNEEDED VARIABLES TO SAVE MEMORY ----
# ______________________________________________________________________________

# Remove unneeded variables
vf_clean <- vf_clean %>%
  select(
    -state, # All records are in California
    -id, # This is a parcel ID for city planning records
    -unit_type1, # All merged records are apartment buildings
    -address, # From parcel records and incorrect where not a match 
    -house_fraction, # Level of precision not needed
    -posterior, -gamma.1, -gamma.2, -gamma.3, -gamma.4 # Fastlink variables
  )

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CLEAN COVARIATES ----
# ______________________________________________________________________________

#### Convert date of birth to age and add years registered ####
vf_clean <- vf_clean %>%
  mutate(
    birth_date = if_else(birth_date == "00/00/0000", "01/01/1000", birth_date),
    birth_date = mdy(birth_date),
    age = age(birth_date, "2021-09-01"),
    age = if_else(age > 500, -99, age),
    reg_date = if_else(reg_date == "00/00/0000", "01/01/1000", reg_date),
    reg_date = mdy(reg_date),
    years_registered = age(reg_date, "2021-09-01"),
    years_registered = if_else(years_registered > 500, -99, years_registered)
    )

#### Estimate gender for individfuals with missing gender data ####
gender2 <- 
  gender(
    vf_clean$name_first,
    year = c(1919, 2002),
    method = "ssa",
    countries = "United States"
  ) %>%
  distinct() %>%
  select(name, gender) %>%
  rename(name_first = name, gender2 = gender) %>%
  mutate(gender2 = if_else(gender2 == "male", "M", "F"))

vf_clean <- left_join(vf_clean, gender2) %>%
  mutate(gender = coalesce(gender, gender2)) %>%
  select(-gender2) %>%
  mutate(gender = if_else(gender == "M", 0, 1))

#### Additional pre-treatment covariates ####
vf_clean <- vf_clean %>%
  mutate(
    # Partisanship
    dem = ifelse(party == "DEM", 1, 0),
    rep = ifelse(party == "REP", 1, 0),
    npp = ifelse(party == "NPP", 1, 0),
    party_reduced = case_when(
      party == "DEM" ~ "Democrat",
      party == "REP" ~ "Republican",
      party == "NPP" ~ "Independent",
      TRUE ~ "Other"),
    # Unit height
    lowrise = ifelse(unit_type2 == "4 Stories or Less", 1, 0),
    mid_highrise = ifelse(unit_type2 == "5 Stories or More", 1, 0),
    # Language
    english = ifelse(language == "ENG", 1, 0),
    # Birth place
    ca_native = ifelse(birth_place == "CA", 1, 0),
    # Vote history
    vote_2020_general = ifelse(vh_2020_general == "N" | is.na(vh_2020_general),
                               0, 1),
    vote_2016_general = ifelse(vh_2016_general == "N" | is.na(vh_2016_general),
                               0, 1),
    vote_2017_municipal = ifelse(vh_2017_municipal == "N" | is.na(vh_2017_municipal),
                             0, 1)
  ) %>%
  select(-unit_type2)

#### Add random ID number to dataframe ####
vf_clean <- vf_clean %>% mutate(random_id = sample(nrow(vf_clean)))

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CLEAN CONTACT INFORMATION VARIABLES ----
# ______________________________________________________________________________

#### Email ####
vf_clean <- vf_clean %>%
  mutate(
    email = str_replace_all(email,"[^[:graph:]]", ""),
    email = str_trim(tolower(email)),
  )

#### Phone numbers: change invalid phone numbers to -99 ####
vf_clean <- vf_clean %>%
  mutate(
    phone_1 = if_else(substr(phone_1, 1, 1) == "0", "-99", phone_1), # Cannot start with 0
    phone_1 = if_else(str_length(phone_1) != 10, "-99", phone_1) # Must be ten digits in length
  )

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# SAVE CLEANED VOTER FILE DATAFRAME ----
# ______________________________________________________________________________

# Save cleaned data
save(vf_clean, file = "data/cleaned/vf_clean.Rdata")
