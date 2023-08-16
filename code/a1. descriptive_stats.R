# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 16 August, 2023 by Trevor Incerti

# This code examines descriptive statistics from the voter file
# Creates Appendix Figures A1 and A2

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES AND IMPORT ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(DeclareDesign)
library(modelsummary)
library(kableExtra)
library(visdat)
library(ggridges)
library(viridis)
library(gridExtra)

# Options
options(scipen=999)

# Functions
source("code/0. functions.R")

# Import clened voter file data
vf_clean <- readRDS("data/vf_clean.rds")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# PREPARE DATA ----
# ______________________________________________________________________________

# Take random sample of data
vf_sample <- vf_clean %>% sample_n(10000) 

# Balance table
balance <- vf_clean %>%
  mutate(likely_renter = if_else(likely_renter == 1, 
                                 "Confirmed renter", "Not confirmed renter")) %>%
  select(
    likely_renter,
    Email = email,
    Phone = phone,
    Age = age,
    `Years registered` = years_registered,
    Female = gender,
    `Speak English` = english,
    `CA native` = ca_native, 
    `Year building constructed` = yearbuilt,
    `Units in building` = units,
    Democrat = dem,
    Republican = rep,
    Independent = npp,
    `Voted in 2020 general election` = vote_2020_general,
    `Voted in 2017 municipal election` = vote_2017_municipal,
    `Voted in 2016 general election` = vote_2016_general
    )
  
# Likely renters only
lr <- balance %>% filter(likely_renter == "Confirmed renter") %>%
  select(-likely_renter)

# Renter balance variables
renter_balance <- balance %>%
  select(-`Units in building`, -`Year building constructed`)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# COMPARE RENTERS AND NON-RENTERS IN VOTER FILE ----
# ______________________________________________________________________________

#### Create Table A1: Balance table renters vs non-renters ####
datasummary_balance(
  ~likely_renter,
  data = renter_balance,
  fmt = 2,
  title = "Balance table: confirmed renters vs. non-confirmed renters",
  dinm_statistic = "p.value",
  output = "latex") %>% 
  row_spec(c(1,3,5,7,9,11, 13), background = '#D3D3D3') %>%
  kable_styling(latex_options = c("scale_down")) %>%
  save_kable("tables/tblA1.tex")

#### Create Table A2: Balance table email in voter file ####
lr <- lr %>%
  mutate(Email = if_else(Email == 1, "Email listed", "Email not listed")) 

datasummary_balance(
  ~Email,
  data = lr,
  fmt = 2,
  title = "Balance table: renters with emails listed in voter file vs. those without",
  dinm_statistic = "p.value",
  output = "latex") %>% 
  row_spec(c(1,3,5,7,9,11,13), background = '#D3D3D3') %>%
  kable_styling(latex_options = c("scale_down")) %>%
  save_kable("tables/tblA2.tex")

