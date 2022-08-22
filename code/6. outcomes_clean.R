# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 11 October, 2021 by Trevor Incerti

# This prepares the outcome data for analysis

# NOTE: MATCHING TREATMENT STATUS WITH OUTCOMES IS CONDUCTED ON INDIVIDUAL NAMES
# AND EMAIL ADDRESSES. THIS SCRIPT THEREFORE CONTAINS REPLICATION CODE ONLY!

# AN ANONYMIZED VERSION WITH NO PERSONALLY IDENTIFYING INFORMATION CAN BE 
# FOUND IN vf_clean.Rdata (I.E. RANDOM IDS, COVARIATE VALUES,
# AND RENTER IDENTIFICATION ONLY)

# AN ANONYMIZED VERSION OF THE RANDOM ASSIGMENT CAN BE FOUND IN 
# random_assignment.Rdata

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(optmatch)

# Functions
source("code/0. functions.R")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# IMPORT, CLEAN, AND MERGE EMAIL OUTCOMES ----
# ______________________________________________________________________________

#### Import files ####
load("data/assignments/random_assignment.Rdata")

# Filter to treated cities only
treated_cities <- c("SANTA MONICA", "BEVERLY HILLS", "WHITTIER", 
                    "RANCHO PALOS VERDES", "MANHATTAN BEACH",
                    "NORWALK", "SIERRA MADRE", "CULVER CITY")
email_treat <- full_ra %>% filter(city %in% treated_cities)

# Import email outcomes
email_out <- read_dir(path = "data/outcomes/emails/", 
                      extension = "csv", delim = ";", filename = T)

##### Clean email outcome data ####
# Treatment list
email_treat <- email_treat %>% mutate(email = tolower(email))

# Remove Culver City opens after meeting date
# Outcomes
email_out <- email_out %>%
  rename(email = Email_ID) %>%
  mutate(
    opened = ifelse(Open_Count == "0", 0, 1),
    Clicked_Links_Count = str_trim(as.character(Clicked_Links_Count)),
    clicked = ifelse(Clicked_Links_Count == "0", 0, 1),
    unsub = ifelse(is.na(Unsubscribe_Date), 0, 1)) %>%
  select(email, opened, clicked, unsub, filename)

#### Merge respondent and outcome data #### 
email <- left_join(email_treat, email_out) %>%
  relocate(name_first, name_last, .before = treatment) %>%
  group_by(city) %>% 
  fill(filename, .direction = "down") %>%
  ungroup()

rm(email_out, email_treat)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ADD COVARIATES ----
# ______________________________________________________________________________

#### Import covariates ####
load("data/cleaned/vf_clean.Rdata")

vf_clean <- vf_clean %>%
  filter(likely_renter == 1) %>%
  select(
    name_first, name_last, email,
    gender, english, age, yearbuilt, units, lowrise, mid_highrise, 
    dem, rep, npp, vote_2020_general, vote_2017_municipal, vote_2016_general
    )

#### Merge outcome data with covariate data ###
email <- left_join(email, vf_clean, by = c("email", "name_first", "name_last"))
email <- email %>% distinct(email, name_first, name_last, .keep_all = T)

#### Mean inpute missing covariates ####
# Check percentage of missing covariates: less than 10% missing
email %>%
  select(gender:vote_2016_general) %>%
  summarise_all(funs(sum(is.na(.))/nrow(email)*100))

# Mean inpute missing covariates
email <- email %>% 
  mutate(
    across(gender:vote_2016_general, as.numeric),
    across(gender:vote_2016_general, ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  )

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ADD INDICATORS FOR NA EMAIL OUTCOMES ----
# ______________________________________________________________________________

#### Change failed email sends outcomes from from NA to 0 ####
email <- email %>% 
  mutate(
    opened = if_else(is.na(opened), 0, opened),
    clicked = if_else(is.na(clicked), 0, clicked),
    unsub = if_else(is.na(unsub), 0, unsub),
    ) %>%
  arrange(city, treatment, random_id)

#### Save email outcome data ####
write_csv(email, path = "data/outcomes/email_outcomes.csv")

#### Save comment outcome files for manual additions
export <- email %>% 
  select(random_id, email, name_first, name_last, city, treatment) %>%
  mutate(spoken_comment = 0, written_comment = 0, 
         custom_comment = 0, pro_comment = 0, anti_comment = 0)

for (i in unique(export$city)) {
  commenters <- export %>% filter(city == i)
  write.csv(x = commenters, file = paste0("data/outcomes/commenters/blank/", i,".csv"))
}

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# MERGE WITH MANUALLY ADDED COMMENT OUTCOME DATA ----
# ______________________________________________________________________________

# Import comment outcomes
commenters <- read_dir(path = "data/outcomes/commenters/", 
                       extension = "csv", delim = ",", filename = T)

# Merge with email outcome data
commenters <- commenters %>% 
  select(random_id, spoken_comment:anti_comment) %>%
  mutate(random_id = as.numeric(random_id))

email <- left_join(email, commenters, by = "random_id")

# Clean comment outcome data
comments <- email %>% 
  mutate(comment = ifelse(spoken_comment == 1 | written_comment == 1, 1, 0))

#### Save outcome data ####
write_csv(comments, path = "data/outcomes/comments.csv")

