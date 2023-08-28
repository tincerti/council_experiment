# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 15 September, 2021 by Trevor Incerti

# This code imports the LA County voter file and merges with LA County
# parcel data records of mult-iunit apartment buildings

# NOTE: VOTER FILE NOT INCLUDED IN DATA FOLDER AS PROPRIETARY INFORMATION.
# THIS SCRIPT THEREFORE CONTAINS REPLICATION CODE ONLY!
# AN ANONYMIZED VERSION WITH NO PERSONALLY IDENTIFYING INFORMATION CAN BE 
# FOUND IN vf_clean.Rdata (I.E. RANDOM IDS, COVARIATE VALUES,
# AND RENTER IDENTIFICATION ONLY)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(janitor)
library(fastLink)
library(ids)
library(gender)
library(eeptools)
library(lubridate)

# Set seed
set.seed(999) 

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# IMPORT VOTER FILE DATA ----
# ______________________________________________________________________________

vf <- read_delim("data/Countywide_EID4193 3577 3496_Ct.txt",
                 "\t", escape_double = FALSE, trim_ws = TRUE)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# REDUCE DATA PRIOR TO MERGE ----
# ______________________________________________________________________________

vf_reduced <- vf %>%
  rename(
    vh_2020_general = `20 11/03/2020 general election 4193`,
    vh_2016_general = `159 11/08/2016 general election 3496`,
    vh_2017_municipal = `123 03/07/2017 consolidated municipal 3577`
  ) %>%
  select(
    name_last, name_first, house_number:precinct, precinct_name, 
    party, phone_1, phone_2, email, gender, birth_date, birth_place, mail_state, language,
    reg_date, vh_2020_general, vh_2016_general, vh_2017_municipal
  ) %>%
  mutate(
    street = str_c(street, type, sep = " ")
  ) %>%
  select(-type)

# Remove full dataframe from memory
rm(vf)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# MERGE WITH LA COUNTY PARCEL DATA USING FASTLINK: EMAIL ----
# Takes ~10 hours on 2017 Macbook Pro 16GB RAM 2.5 GHz Dual-Core Intel Core i7
# ______________________________________________________________________________

# Import apartment data and Keep needed variables only
apartments <- 
  read_csv("data/cleaned/apartments.csv") %>%
  rename(zip = zip5) %>%
  select(-zip9)

# Parallel package broken without following in R 4.0.
# Parallel needed for fastlink
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

# Perform merge using fastlink
matches.out <- fastLink(
  dfA = vf_reduced, dfB = apartments, 
  varnames = c("street", "city", "zip", "house_number"),
  stringdist.match = c("street", "city", "zip", "house_number"),
  partial.match = c("street")
)

save(matches.out, file = "data/fastlink_match_all.Rdata")

# Create dataset from fastlink matches
vf_renters <- getMatches(
  dfA = vf_reduced, dfB = apartments, 
  fl.out = matches.out, threshold.match = 0.85
)

save(vf_renters, file = "data/cleaned/vf_renters.Rdata")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# Merge back with original voter file and add likely renter indicator ----
# ______________________________________________________________________________

# Deduplicate voter file
vf_reduced_merge <- vf_reduced %>% distinct()

# Add renter indicator variable and deduplicate renter data
vf_renters <- vf_renters %>% 
  mutate(likely_renter = 1) %>% # Add indicator
  distinct() # Deduplicate

# Merge with full voter file to add renter indicator variable
vf_reduced_merge <- left_join(vf_reduced_merge, vf_renters)
vf_all_renters <- vf_reduced_merge %>%
  distinct(across(name_last:vh_2017_municipal), .keep_all = TRUE)

save(vf_all_renters, file = "data/vf_all_renters.Rdata")