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

# Set seed for randomization inference
set.seed(999)

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
# ROBUSTNESS: RANDOMIZATION INFERENCE ----
# ______________________________________________________________________________

#### Histogram of distriubution of outcomes ####
# Create Figure A9
comments %>%
  filter(opened == 1) %>%
  mutate(comment = as.character(comment)) %>%
  ggplot(aes(x = comment)) +
  geom_histogram(stat = "count", width = 0.25, fill = "steelblue2", color = "steelblue2") +
  scale_x_discrete("", labels = c("0" = "Did not comment", "1" = "Commented")) +
  facet_wrap(~ treatment, nrow = 3) +
  ylab("Number of observations") +
  theme_classic() 

#ggsave(file="figs/fgA9.pdf", height = 4.25, width = 7)

#### CACE ####
# Track time taken to run randomization inference:
# 3.4 hours with 10,000 sims, 21.8 minutes with 1000 sims
start_time <- Sys.time() 
set.seed(999)
# Set the number of simulated random assignments
sims <- 10000

# Create an empty vector to store our estimates
ri_ests_all <- rep(NA, sims)
t1_ri_ests <- rep(NA, sims)
t2_ri_ests <- rep(NA, sims)
t3_ri_ests <- rep(NA, sims)
t2_t1_ri_ests <- rep(NA, sims)
t3_t1_ri_ests <- rep(NA, sims)
t3_t2_ri_ests <- rep(NA, sims)
ri_cost_info_ests <- rep(NA, sims)

for (i in 1:sims) {
  # Conduct new random assignment according to same procedure as original
  comments$ri_assignment <-
    block_and_cluster_ra(
      blocks = comments$city,
      clusters = comments$address,
      conditions = c("Placebo", "Treatment 1", "Treatment 2", "Treatment 3"),
      prob_each = c(0.1, 0.3, 0.3, 0.3)
    )
  
  # Create hypothetical "treated" group and cost group
  comments <- comments %>% 
    mutate(
      ri_treated = ifelse(ri_assignment != "Placebo", 1, 0),
      ri_cost = case_when(
        ri_assignment == "Placebo" ~ "Placebo",
        ri_assignment == "Treatment 1" ~ "Information",
        TRUE ~ "Cost"),
      ri_cost = factor(ri_cost, c("Placebo", "Information", "Cost")))
  
  # Calculate effect sizes under simulated random assignment: all treatments
  ri_est_all <- lm_robust(comment ~ ri_treated, data = comments,
                      clusters = address, fixed_effects = ~ city,
                      subset = opened == 1)
  
  # Calculate effect sizes under simulated random assignment: each treatment
  ri_est_each <- lm_robust(comment ~ ri_assignment, data = comments,
                      clusters = address, fixed_effects = ~ city,
                      subset = opened == 1)
  
  # Calculate effect sizes under simulated random assignment: cost treatments
  ri_est_cost <- lm_robust(comment ~ ri_cost, data = comments,
                           clusters = address, fixed_effects = ~ city,
                           subset = opened == 1)
  
  # Store estimates of treatment effect from simulated random assignment
  ri_ests_all[i] <- summary(ri_est_all)$coefficients[1]
  t1_ri_ests[i] <- summary(ri_est_each)$coefficients[1]
  t2_ri_ests[i] <- summary(ri_est_each)$coefficients[2]
  t3_ri_ests[i] <- summary(ri_est_each)$coefficients[3]
  t2_t1_ri_ests[i] <- summary(ri_est_each)$coefficients[2] - summary(ri_est_each)$coefficients[1]
  t3_t1_ri_ests[i] <- summary(ri_est_each)$coefficients[3] - summary(ri_est_each)$coefficients[1]
  t3_t2_ri_ests[i] <- summary(ri_est_each)$coefficients[3] - summary(ri_est_each)$coefficients[2]
  ri_cost_info_ests[i] <- summary(ri_est_cost)$coefficients[2] - summary(ri_est_cost)$coefficients[1]
}

# Calculate true estimates from actual random assignment
true_est_all <- lm_robust(comment ~ treated, data = comments,
                      clusters = address, fixed_effects = ~ city,
                      subset = opened == 1)

true_est_each <- lm_robust(comment ~ treatment, data = comments,
                      clusters = address, fixed_effects = ~ city,
                      subset = opened == 1)

true_est_cost <- lm_robust(comment ~ cost_treatment, data = comments, 
                           clusters = address, fixed_effects = ~ city,
                           subset = opened == 1)

true_est_all <- true_est_all$coefficients[1]
t1_true_est <- true_est_each$coefficients[1]
t2_true_est <- true_est_each$coefficients[2]
t3_true_est <- true_est_each$coefficients[3]
t2_t1_true_est <- summary(true_est_each)$coefficients[2] - summary(true_est_each)$coefficients[1]
t3_t1_true_est <- summary(true_est_each)$coefficients[3] - summary(true_est_each)$coefficients[1]
t3_t2_true_est <- summary(true_est_each)$coefficients[3] - summary(true_est_each)$coefficients[2]
cost_info_true_est <- summary(true_est_cost)$coefficients[2] - summary(true_est_cost)$coefficients[1]

# Calculate proportion of "fake" random assignments with larger effects
p_value_all <- mean(abs(ri_ests_all) > true_est_all) # 0.0436
t1_p_value <- mean(abs(t1_ri_ests) > t1_true_est) # 0.3861
t2_p_value <- mean(abs(t2_ri_ests) > t2_true_est) # 0.0711
t3_p_value <- mean(abs(t3_ri_ests) > t3_true_est) # 0.0114
t2_t1_p_value <- mean(abs(t2_t1_ri_ests) > t2_t1_true_est) # 0.1978
t3_t1_p_value <- mean(abs(t3_t1_ri_ests) > t3_t1_true_est) # 0.0206
t3_t2_p_value <- mean(abs(t3_t2_ri_ests) > t3_t2_true_est) # 0.3259
cost_info_p_value <- mean(abs(ri_cost_info_ests) > cost_info_true_est) # 0.0342

end_time <- Sys.time()
end_time - start_time

#### ITT ####
# Track time taken to run randomization inference
start_time <- Sys.time() # Time process: 10.5 hours with 1000 sims
set.seed(999)
# Set the number of simulated random assignments
sims <- 1000

# Create an empty vector to store our estimates
ri_ests_all <- rep(NA, sims)
t1_ri_ests <- rep(NA, sims)
t2_ri_ests <- rep(NA, sims)
t3_ri_ests <- rep(NA, sims)
t2_t1_ri_ests <- rep(NA, sims)
t3_t1_ri_ests <- rep(NA, sims)
t3_t2_ri_ests <- rep(NA, sims)
ri_cost_info_ests <- rep(NA, sims)

for (i in 1:sims) {
  # Conduct new random assignment accord to same procedure as original
  comments$ri_assignment <-
    block_and_cluster_ra(
      blocks = comments$city,
      clusters = comments$address,
      conditions = c("Placebo", "Treatment 1", "Treatment 2", "Treatment 3"),
      prob_each = c(0.1, 0.3, 0.3, 0.3)
    )
  
  # Create hypothetical "treated" group and cost group
  comments <- comments %>% 
    mutate(
      ri_treated = ifelse(ri_assignment != "Placebo", 1, 0),
      ri_cost = case_when(
        ri_assignment == "Placebo" ~ "Placebo",
        ri_assignment == "Treatment 1" ~ "Information",
        TRUE ~ "Cost"),
      ri_cost = factor(ri_cost, c("Placebo", "Information", "Cost")))
  
  # Calculate effect sizes under simulated random assignment: all treatments
  ri_est_all <- lm_robust(comment ~ ri_treated, data = comments,
                          clusters = address, fixed_effects = ~ city)
  
  # Calculate effect sizes under simulated random assignment: all treatments
  ri_est_each <- lm_robust(comment ~ ri_assignment, data = comments,
                           clusters = address, fixed_effects = ~ city)
  
  # Calculate effect sizes under simulated random assignment: cost treatments
  ri_est_cost <- lm_robust(comment ~ ri_cost, data = comments,
                           clusters = address, fixed_effects = ~ city)
  
  # Store estimates of treatment effect from simulated random assignment
  ri_ests_all[i] <- summary(ri_est_all)$coefficients[1]
  t1_ri_ests[i] <- summary(ri_est_each)$coefficients[1]
  t2_ri_ests[i] <- summary(ri_est_each)$coefficients[2]
  t3_ri_ests[i] <- summary(ri_est_each)$coefficients[3]
  t2_t1_ri_ests[i] <- summary(ri_est_each)$coefficients[2] - summary(ri_est_each)$coefficients[1]
  t3_t1_ri_ests[i] <- summary(ri_est_each)$coefficients[3] - summary(ri_est_each)$coefficients[1]
  t3_t2_ri_ests[i] <- summary(ri_est_each)$coefficients[3] - summary(ri_est_each)$coefficients[2]
  ri_cost_info_ests[i] <- summary(ri_est_cost)$coefficients[2] - summary(ri_est_cost)$coefficients[1]
}

# Calculate true estimates from actual random assignment
true_est_all <- lm_robust(comment ~ treated, data = comments,
                         clusters = address, fixed_effects = ~ city)

true_est_each <- lm_robust(comment ~ treatment, data = comments,
                           clusters = address, fixed_effects = ~ city)

true_est_cost <- lm_robust(comment ~ cost_treatment, data = comments, 
                           clusters = address, fixed_effects = ~ city)

#true_est_all <- true_est_all$coefficients[1]
t1_true_est <- true_est_each$coefficients[1]
t2_true_est <- true_est_each$coefficients[2]
t3_true_est <- true_est_each$coefficients[3]
t2_t1_true_est <- summary(true_est_each)$coefficients[2] - summary(true_est_each)$coefficients[1]
t3_t1_true_est <- summary(true_est_each)$coefficients[3] - summary(true_est_each)$coefficients[1]
t3_t2_true_est <- summary(true_est_each)$coefficients[3] - summary(true_est_each)$coefficients[2]
cost_info_true_est <- summary(true_est_cost)$coefficients[2] - summary(true_est_cost)$coefficients[1]

# Calculate proportion of "fake" random assignments with larger effects
p_value_all_itt <- mean(abs(ri_ests_all) > true_est_all) # 0.074
t1_p_value_itt <- mean(abs(t1_ri_ests) > t1_true_est) # 0.38
t2_p_value_itt <- mean(abs(t2_ri_ests) > t2_true_est) # 0.089
t3_p_value_itt <- mean(abs(t3_ri_ests) > t3_true_est) # 0.039
t2_t1_p_value_itt <- mean(abs(t2_t1_ri_ests) > t2_t1_true_est) # 0.266
t3_t1_p_value_itt <- mean(abs(t3_t1_ri_ests) > t3_t1_true_est) # 0.082
t3_t2_p_value_itt <- mean(abs(t3_t2_ri_ests) > t3_t2_true_est) # 0.565
cost_info_p_value_itt <- mean(abs(ri_cost_info_ests) > cost_info_true_est) # 0.086

end_time <- Sys.time()
end_time - start_time

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
