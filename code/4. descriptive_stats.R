# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 21 August, 2021 by Trevor Incerti

# This code examines descriptive statistics from the voter file

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
load("data/cleaned/vf_clean.Rdata")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# PREPARE DATA ----
# ______________________________________________________________________________
  
# Add indicators for phone and email addresses
vf_clean <- vf_clean %>%
  mutate(
    Email = if_else(is.na(email), 0, 1),
    Phone = if_else(is.na(phone_1), 0, 1)
         )

# Take random sample of data
vf_sample <- vf_clean %>% sample_n(10000) 

# Balance table
balance <- vf_clean %>%
  mutate(likely_renter = if_else(likely_renter == 1, 
                                 "Confirmed renter", "Not confirmed renter")) %>%
  select(
    likely_renter,
    Email,
    Phone,
    Age = age,
    `Years registered` = years_registered,
    Female = gender,
    English = english,
    `CA native` = ca_native, 
    Democrat = dem,
    Republican = rep,
    Independent = npp,
    `Voted 2016 general` = vote_2016_general,
    `Voted 2020 general` = vote_2020_general,
    `Voted 2017 municipal` = vote_2017_municipal
    )
  
# Likely renters only
lr <- vf_clean %>% filter(likely_renter == 1)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# COMPARE RENTERS AND NON-RENTERS IN VOTER FILE ----
# ______________________________________________________________________________

#### Balance table ####
datasummary_balance(
  ~likely_renter,
  data = balance,
  fmt = 2,
  title = "Balance table: confirmed renters vs. non-confirmed renters",
  dinm_statistic = "p.value") %>% 
  #row_spec(c(1,3,4,7,8,9,11,12,13), background = 'lightblue') %>%
  row_spec(c(1,3,5,7,9,11,13), background = 'lightcyan') %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  kable_styling(font_size = 10) %>%
  save_kable("tables/balance.png", zoom = 5)
  
# Density plot: age
plot_age <- vf_sample %>% 
  filter(age != "-99") %>%
  mutate(
    likely_renter = if_else(likely_renter == 1, 
                            "Confirmed renter", "Not confirmed renter"),
    likely_renter = as.factor(likely_renter)) %>%
  ggplot(aes(x = age, y = likely_renter, fill = ..x..)) +
  geom_density_ridges_gradient() +
  scale_fill_viridis(alpha = 0.8) +
  scale_x_continuous(limits = c(10, 110), breaks=seq(0, 110, 10)) +
  ggtitle("Age") +
  gglayers

# Histogram: length of time registered
plot_years_registered <- vf_sample %>% 
  filter(years_registered < 10) %>%
  mutate(
    likely_renter = as.factor(likely_renter),
    likely_renter = if_else(likely_renter == 1, 
                            "Confirmed renter", "Not confirmed renter")) %>%
  ggplot(aes(x = years_registered, y = likely_renter, fill = ..x..)) +
  geom_density_ridges_gradient(stat = "binline", bins = 10) +
  scale_fill_viridis(alpha = 0.8) +
  scale_x_continuous(limits = c(0, 10), breaks=seq(0, 10, 2)) +
  ggtitle("Years registered to vote at current address") +
  gglayers

# Save figures
plots <- grid.arrange(plot_age, plot_years_registered, nrow = 2)
ggsave(file="figs/age_reg_density.pdf", plots, height = 5, width = 7)





















# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# EXAMINE RENTER VOTER FILE DESCRIPTIVE STATISTICS ----
# ______________________________________________________________________________

# Numerical variables
vf_clean %>%
  select(-value, -lat, -long, -house_number, -bedrooms) %>%
  rename(
    `Units in building` = units, 
    `Year built` = yearbuilt,
    `Age` = age, 
    `Gender` = gender,
    `English` = english
  ) %>%
  select(Age, `Year built`, `Units in building`, Gender, English) %>%
  datasummary_skim(histogram = TRUE, fmt = "%.2f") %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                font_size = 10) %>%
  save_kable("tables/numeric_covariates_summary.png", zoom = 4)

# Categorical variable
vf_email_clean %>%
  mutate(
    language = case_when(
      language == "ENG" ~ "English",
      language == "SPA" ~ "Spanish",
      language == "KOR" ~ "Korean",
      language == "CHN" ~ "Chinese",
      TRUE ~ "Other"),
    language = factor(language, c("English", "Spanish", "Korean", "Chinese")),
    party_reduced = factor(party_reduced, c("Democrat", "Independent", "Republican", "Other")),
    Gender = if_else(gender == 1, "Female", "Male")
    ) %>%
  rename(
    Party = party_reduced, 
    `Registered language` = language, 
    `Unit type` = unit_type2
    ) %>%
  select(Party, `Registered language`, `Unit type`) %>%
  datasummary_skim(histogram = TRUE, type = "categorical", fmt = "%.2f") %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                font_size = 10) %>%
  save_kable("tables/categorical_covariates_summary.png", zoom = 4)


# Missingness
vf_email_clean %>% 
  select(gender, english, age, yearbuilt, units, party, birth_place, 
         language, mail_state, bedrooms, unit_type2) %>%
  vis_miss(warn_large_data = FALSE) +
  theme(plot.margin = unit(c(1,2,1,1),"cm")) %>%
ggsave(file="figs/covariate_missingness.png", height = 5, width = 7)

vf_email_renters %>% 
  select(gender, yearbuilt, units, party) %>%
  vis_miss(warn_large_data = FALSE)

