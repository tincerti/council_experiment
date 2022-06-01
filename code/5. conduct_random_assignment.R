# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 7 October, 2021 by Trevor Incerti

# This code conducts the random assignment procedure

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES AND IMPORT ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(DeclareDesign)

# Set seed
set.seed(999) 

# Functions
source("code/0. functions.R")

# Import clened voter file data
load("data/cleaned/vf_clean.Rdata")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FILTER TO LIKELY RENTERS WITH EMAIL ONLY ----
# ______________________________________________________________________________

vf_clean <- vf_clean %>% 
  filter(likely_renter == 1 & !is.na(email)) %>%
  mutate(email = tolower(email))

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FINAL DATA PROCESSESSING BEFORE RANDOM ASSIGNMENT ----
# ______________________________________________________________________________

#### Remove pilot individuals from sample #### 
# Import pilot treated email addresses
pilot_treat <- read_dir(path = "data/email_lists/pilot", 
                        extension = "csv", delim = ",", filename = T)

pilot_treat <- pilot_treat %>% mutate(email = tolower(email))

vf_clean <- vf_clean[!(vf_clean$email %in% pilot_treat$email),]

#### Remove duplicate email addresses and create cluster variable
vf_clean <- vf_clean %>%
  distinct(email, .keep_all = TRUE) %>%
  unite(address, 
        house_number, pre_dir, street, post_dir, building_number, 
        apartment_number, city, zip,
        sep = " ",
        remove = FALSE, na.rm = TRUE)

# Confirm no duplicate email addresses
vf_clean %>% group_by(email) %>% filter(n()>1) # Confirmed

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ASSIGN INDIVIDUALS TO TREATMENT: FULL EMAIL SAMPLE ----
# ______________________________________________________________________________

# Block random assignment by city
# 10% probability of assignment to placebo
# 30% probability of assignment to each treatment group
vf_clean$treatment <- 
  block_and_cluster_ra(
    blocks = vf_clean$city,
    clusters = vf_clean$address,
    conditions = c("Placebo", "Treatment 1", "Treatment 2", "Treatment 3"),
    prob_each = c(0.1, 0.3, 0.3, 0.3)
  )

vf_clean <- vf_clean %>%
  mutate(
    treated = case_when(
      str_detect(as.character(treatment), "Treatment") ~ "Treatment",
      TRUE ~ "Placebo")
  )

# Look at randomization
ra <- vf_clean %>% group_by(city, treatment) %>% summarise(n = n())
city_n <- vf_clean %>% group_by(city) %>% summarise(n = n())

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# SAVE RANDOM ASSIGNMENTS AS INDIVIDUAL CSVs ----
# ______________________________________________________________________________

# Save full treatment assignment
full_ra <- vf_clean %>% 
  select(random_id, email, name_first, name_last, treatment, city, address, 
         treated)

save(full_ra, file = "data/assignments/random_assignment.Rdata")

# Save by city and treatment status for export to email software
email_export <- vf_clean %>% 
  select(email, name_first, name_last, city, treatment) %>%
  unite(city_treatment, c("city", "treatment"), remove = FALSE)

for (i in unique(email_export$city_treatment)) {
  email_list <- email_export %>%
    filter(city_treatment == i) %>%
    select(email, name_first, name_last) %>%
    rename(firstname = name_first, lastname = name_last)
  
  write.csv(x = email_list, file = paste0("data/email_lists/", i,".csv"))
}

