# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 16 August, 2023 by Trevor Incerti

# This file analyzes the results from email experiments

# NOTE: NAMES AND AND EMAIL ADDRESSES HAVE BEEN REMOVED FROM THE DATA
# HOME ADDRESSES HAVE BEEN CONVERTED TO UNIQUE RANDOM IDS

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(DeclareDesign)
library(modelsummary)
library(kableExtra)
library(car)
library(logistf)

# Options
options(scipen=999)

# Functions
source("code/0. functions.R")

# Import data
comments <- read_csv("data/comments.csv")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FINAL DATA PREP ----
# ______________________________________________________________________________

##### Create list of covariates for covariate adjustment in estimation ####
comments <- comments %>% 
  mutate(
    date = case_when(
      str_detect(filename, "10.12") ~ "10.12"
    ))

# Add treated indicator
comments <- comments %>% 
  mutate(received_treatment = ifelse(opened == 1 & treated == "Treatment", 1, 0))

# List of covariates
covs = ~ gender + english + age + 
  yearbuilt + units + dem + rep + npp +
  vote_2020_general + vote_2017_municipal + vote_2016_general + city

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ANALYSIS: AS ONE EXPERIMENT, ALL TREATMENT VERSUS PLACEBO ----
# ______________________________________________________________________________

# Estimate ITT and CACE: Treatment vs. placebo (with covariates)
itt_cace <- list(
  "ITT" = lm_lin(comment ~ treated, covs, 
                 data = comments, clusters = address),
  "CACE" = lm_lin(comment ~ treated, covs, 
                  data = comments, subset = opened == 1, clusters = address)
)

# Estimate ITT and CACE: Treatment vs. placebo (without covariates)
itt_cace_nocovs <- list(
  "ITT" = lm_robust(comment ~ treated + city, 
                    data = comments, clusters = address),
  "CACE" = lm_robust(comment ~ treated + city, 
                     data = comments, subset = opened == 1, clusters = address)
)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ANALYSIS: AS ONE EXPERIMENT, EACH TREATMENT VERSUS PLACEBO ----
# ______________________________________________________________________________

# Estimate CACE: All treatments vs. placebo (with covariates)
itt_cace_all <- list(
  "ITT" = lm_lin(comment ~ treatment, covs, 
                 data = comments, clusters = address),
  "CACE" = lm_lin(comment ~ treatment, covs, 
                  data = comments, subset = opened == 1, clusters = address)
)

# Estimate ITT and CACE: All treatments vs. placebo (without covariates)
itt_cace_all_nocovs <- list(
  "ITT" = lm_robust(comment ~ treatment + city,
                    data = comments, clusters = address),
  "CACE" = lm_robust(comment ~ treatment + city,
                     data = comments, subset = opened == 1, clusters = address)
)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ANALYSIS: TREATMENT VERSUS TREATMENT ----
# ______________________________________________________________________________

cace = lm_robust(comment ~ treatment, fixed_effects = ~ city,
                 data = comments, subset = opened == 1, clusters = address)

linearHypothesis(cace, "treatmentTreatment 3 - treatmentTreatment 2 = 0")
linearHypothesis(cace, "treatmentTreatment 2 - treatmentTreatment 1 = 0")
linearHypothesis(cace, "treatmentTreatment 3 - treatmentTreatment 1 = 0")

# Cost treatments compared to information treatment
comments <- comments %>%
  mutate(
    cost_treatment = case_when(
      treatment == "Placebo" ~ "Placebo",
      treatment == "Treatment 1" ~ "Information",
      TRUE ~ "Cost"),
    cost_treatment = factor(cost_treatment, c("Placebo", "Information", "Cost"))
  )

cost_any = lm_robust(comment ~ cost_treatment, fixed_effects = ~ city,
                     data = comments, subset = opened == 1, clusters = address)

linearHypothesis(cost_any, "cost_treatmentCost - cost_treatmentInformation = 0")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CREATE FIGURES ----
# ______________________________________________________________________________

#### Coefficient plots ####
# Create Figure 1: All treatment vs. placebo
modelplot(itt_cace, coef_map = treatments, 
          coef_omit = "Constant", draw = F) %>%
  filter(term != "Constant") %>%
  mutate(term = model) %>%
  mutate(model = fct_reorder(model, c("IIT", "CACE"))) %>%
  mutate(across(estimate:conf.high, ~ . * 100)) %>%
  ggplot(aes(x = estimate, y = model, group = model)) +
  gglayers +
  scale_x_continuous(limits = c(-0.5, 2.5), breaks = seq(-0.5, 2.5, by = 0.5))

#ggsave(file="figs/fg1.pdf", height = 1.5, width = 7)

# Create Figure A6: All treatments vs. placebo without adjustment
modelplot(itt_cace_nocovs, coef_map = treatments, 
          coef_omit = "Constant", draw = F) %>%
  filter(term != "Constant") %>%
  mutate(term = model) %>%
  mutate(model = fct_reorder(model, c("IIT", "CACE"))) %>%
  mutate(across(estimate:conf.high, ~ . * 100)) %>%
  ggplot(aes(x = estimate, y = model, group = model)) +
  gglayers +
  scale_x_continuous(limits = c(-0.5, 2.5), breaks = seq(-0.5, 2.5, by = 0.5))

#ggsave(file="figs/fgA6.pdf", height = 1.5, width = 7)

# Create Figure 3: Each treatment vs. placebo
modelplot(itt_cace_all, coef_map = treatments, 
          coef_omit = "Constant", draw = F) %>%
  filter(term != "Constant") %>%
  mutate(term = fct_reorder(term, -estimate)) %>%
  mutate(across(estimate:conf.high, ~ . * 100)) %>%
  ggplot(aes(x = estimate, y = term, group = term)) +
  facet_wrap(~ model, nrow = 2) +
  gglayers +
  scale_x_continuous(limits = c(-0.5, 2.5), breaks = seq(-0.5, 2.5, by = 0.5))

#ggsave(file="figs/fg3.pdf", height = 3, width = 7)

# Create Figure A7: Each treatment vs. placebo without adjustment
modelplot(itt_cace_all_nocovs, coef_map = treatments, 
          coef_omit = "Constant", draw = F) %>%
  filter(term != "Constant") %>%
  mutate(term = fct_reorder(term, -estimate)) %>%
  mutate(across(estimate:conf.high, ~ . * 100)) %>%
  ggplot(aes(x = estimate, y = term, group = term)) +
  facet_wrap(~ model, nrow = 2) +
  gglayers +
  scale_x_continuous(limits = c(-0.5, 2.5), breaks = seq(-0.5, 2.5, by = 0.5))

#ggsave(file="figs/fgA7.pdf", height = 3.5, width = 7)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CREATE TABLES ----
# ______________________________________________________________________________

# Combine lists into one table
table <- c(itt_cace, itt_cace_nocovs, itt_cace_all, itt_cace_all_nocovs)

names(table)[1] <- "ITT cov"
names(table)[2] <- "CACE cov"
names(table)[3] <- "ITT nocov"
names(table)[4] <- "CACE nocov"
names(table)[5] <- "ITT all cov"
names(table)[6] <- "CACE all cov"
names(table)[7] <- "ITT all nocov"
names(table)[8] <- "CACE all nocov"

itt_table <- c(table[1], table[3], table[5], table[7])
cace_table <- c(table[2], table[4], table[6], table[8])

# Add covariate adjustment indicator
rows <- tribble(~term, ~treat_cov,  ~treat_nocov, ~all_cov, ~all_nocov,
                'Covariate adjustment:', 'Yes', 'No', 'Yes', 'No')
attr(rows, 'position') <- 16

# Create Table A6: Intent-to-treat effects
modelsummary(itt_table, 
             coef_map = treatments,
             statistic = c("({std.error})",
                           "conf.int"), 
             stars = TRUE, gof_omit = omit, add_rows = rows, fmt = 4,
             notes = c("Notes: Standard errors clustered at the address level in parentheses. 95 percent confidence intervals in brackets."),
             output = "latex") %>%
  kable_styling(latex_options = c("scale_down")) %>%
  row_spec(c(1,4,7,10,13), background = '#D3D3D3') %>%
  add_header_above(c(" " = 1, "All treatment groups vs. placebo" = 2, 
                     "Individual treatments vs. placebo" = 2)) #%>%
  #save_kable("tables/tblA6.tex")

# Create Table A7: Complier average causal effects
modelsummary(cace_table, 
             coef_map = treatments,
             statistic = c("({std.error})",
                           "conf.int"), 
             stars = TRUE, gof_omit = omit, add_rows = rows, fmt = 4,
             notes = c("Notes: Standard errors clustered at the address level in parentheses. 95 percent confidence intervals in brackets."), 
             output = "latex") %>%
  kable_styling(latex_options = c("scale_down")) %>%
  row_spec(c(1,4,7,10,13), background = '#D3D3D3') %>%
  add_header_above(c(" " = 1, "All treatment groups vs. placebo" = 2, 
                     "Individual treatments vs. placebo" = 2)) #%>%
  #save_kable("tables/tblA7.tex")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ROBUSTNESS: PENALIZED LIKELIHOOD ----
# ______________________________________________________________________________

# Run penalized maximum likelihood models
pl <- list(
  "ITT" = 
    logistf(comment ~ treated, data = comments),
  "CACE" = 
    logistf(comment ~ treated, data = comments[which(comments$opened==1),]),
  "ITT " = 
    logistf(comment ~ treatment, data = comments),
  "CACE " = 
    logistf(comment ~ treatment, data = comments[which(comments$opened==1),])
)

# Export to table
modelsummary(pl, 
             coef_map = treatments,
             statistic = c("({std.error})", "conf.int"), 
             stars = TRUE, gof_omit = omit, fmt = 4,
             notes = c("Notes: Standard errors clustered at the address level in parentheses. 95 percent confidence intervals in brackets."),
             output = "latex") %>%
    row_spec(c(1,4,7,10,13), background = '#D3D3D3') %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_header_above(c(" " = 1, "All treatment groups vs. placebo" = 2, 
                     "Individual treatments vs. placebo" = 2)) #%>%
  #save_kable("tables/tblA14.tex")
