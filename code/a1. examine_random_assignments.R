# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 6 February, 2021 by Trevor Incerti

# This file examines the random assignments and provides summary statistics

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES AND IMPORT ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(DeclareDesign)
library(modelsummary)
library(kableExtra)

# Functions
source("code/0. functions.R")

# Import data
load("data/random_assignment.Rdata")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# SUMMARY STATISTICS ----
# ______________________________________________________________________________

# By city
cities <- ra_anon %>%
  group_by(city) %>%
  summarise(N = n())

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# MERGE RANDOM ASSIGNMENT WITH COVARIATE DATA ----
# ______________________________________________________________________________

#### Import covariates ####
load("data/vf_clean.Rdata")

covs <- vf_anon %>%
  filter(likely_renter == 1) %>%
  select(
    random_id, gender, english, age, yearbuilt, units, dem, rep, npp,
    vote_2020_general, vote_2017_municipal, vote_2016_general
  )

#### Merge outcome data with covariate data ###
ra_covs <- left_join(ra_anon, covs, by = c("random_id"))

# Filter to treated cities
treated_cities <- c("SANTA MONICA", "BEVERLY HILLS", "WHITTIER",
                    "RANCHO PALOS VERDES", "MANHATTAN BEACH",
                    "NORWALK", "SIERRA MADRE", "CULVER CITY")

# Create dataframe of treatment vs placebo with block ids
ra_covs_tp <- ra_covs %>% 
  filter(city %in% treated_cities) %>%
  select(-random_id, -treatment) %>%
  rename(blocks = city)

# Create dataframe of all treatments and placebo with block ids
ra_covs_all <- ra_covs %>% 
  filter(city %in% treated_cities) %>%
  select(-random_id, -treated) %>%
  rename(blocks = city)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# TABLES ----
# ______________________________________________________________________________

# Create balance table: treatment vs. placebo
datasummary_balance(~treated,
                    data = ra_covs_tp,
                    fmt = 2, 
                    dinm_statistic = "p.value",
                    output = "kableExtra") %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                font_size = 10) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  row_spec(c(1,3,5,7,9,11), background = '#D3D3D3') %>%
  save_kable("tables/ra_balance.png", zoom = 5)

# Create balance table: all treatments vs. placebo
datasummary_balance(~treatment,
                    data = ra_covs_all,
                    fmt = 2, 
                    dinm_statistic = "p.value",
                    output = "kableExtra") %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                font_size = 10) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  row_spec(c(1,3,5,7,9,11), background = '#D3D3D3') %>%
  save_kable("tables/ra_balance_all.png", zoom = 5)
