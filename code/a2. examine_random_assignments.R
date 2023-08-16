# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 16 August, 2023 by Trevor Incerti

# This file examines the random assignments and provides summary statistics
# Creates Tables A3 and A4

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
vf_clean <- readRDS("data/vf_clean.rds")

covs <- vf_clean %>%
  filter(likely_renter == 1) %>%
  select(
    random_id, 
    Female = gender, 
    `Speak English` = english, 
    Age = age, 
    `Year building constructed` = yearbuilt, 
    `Units in building` = units, 
    `Democrat` = dem, 
    `Republican` = rep, 
    `Independent` = npp,
    `Voted in 2020 general election` = vote_2020_general, 
    `Voted in 2017 municipal election` = vote_2017_municipal, 
    `Voted in 2016 general election` = vote_2016_general
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

# Create Table A3: balance table treatment vs. placebo
datasummary_balance(~treated,
                    data = ra_covs_tp,
                    fmt = 2, 
                    dinm_statistic = "p.value",
                    output = "latex") %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                font_size = 10) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  row_spec(c(1,3,5,7,9,11), background = '#D3D3D3') %>%
  save_kable("tables/tblA3.tex")

# Create Table A4: balance table all treatments vs. placebo
datasummary_balance(~treatment,
                    data = ra_covs_all,
                    fmt = 2, 
                    dinm_statistic = "p.value",
                    output = "latex") %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                font_size = 10) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  row_spec(c(1,3,5,7,9,11), background = '#D3D3D3') %>%
  save_kable("tables/tblA4.tex")
