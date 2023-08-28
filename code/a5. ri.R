# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 24 August, 2023 by Trevor Incerti

# This file conducts the randomization inference robustness checks

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
set.seed(999)

# Functions
source("code/0. functions.R")

# Import data
#email <- read_csv("data/outcomes/sm_outcomes.csv")
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

# Cost treatments compared to information treatment
comments <- comments %>%
  mutate(
    cost_treatment = case_when(
      treatment == "Placebo" ~ "Placebo",
      treatment == "Treatment 1" ~ "Information",
      TRUE ~ "Cost"),
    cost_treatment = factor(cost_treatment, c("Placebo", "Information", "Cost"))
  )

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ROBUSTNESS: RANDOMIZATION INFERENCE (VS PLACEBO) ----
# ______________________________________________________________________________

#### CACE ####
# Track time taken to run randomization inference:
# 1.5 hours with 10,000 sims
start_time <- Sys.time() 

# Set the number of simulated random assignments
sims <- 10000

# Create an empty vector to store our estimates
ri_ests_all <- rep(NA, sims)
t1_ri_ests <- rep(NA, sims)
t2_ri_ests <- rep(NA, sims)
t3_ri_ests <- rep(NA, sims)

for (i in 1:sims) {
  # Conduct new random assignment accord to same procedure as original
  comments$ri_assignment <-
    block_and_cluster_ra(
      blocks = comments$city,
      clusters = comments$address,
      conditions = c("Placebo", "Treatment 1", "Treatment 2", "Treatment 3"),
      prob_each = c(0.1, 0.3, 0.3, 0.3)
    )
  
  # Create hypothetical "treated" group
  comments <- comments %>% 
    mutate(ri_treated = ifelse(ri_assignment != "Placebo", 1, 0))
  
  # Calculate effect sizes under simulated random assignment: all treatments
  ri_est_all <- lm_robust(comment ~ ri_treated, data = comments,
                          clusters = address, fixed_effects = ~ city,
                          subset = opened == 1)
  
  # Calculate effect sizes under simulated random assignment: each treatment
  ri_est_each <- lm_robust(comment ~ ri_assignment, data = comments,
                           clusters = address, fixed_effects = ~ city,
                           subset = opened == 1)
  
  # Store estimates of treatment effect from simulated random assignment
  ri_ests_all[i] <- summary(ri_est_all)$coefficients[1]
  t1_ri_ests[i] <- summary(ri_est_each)$coefficients[1]
  t2_ri_ests[i] <- summary(ri_est_each)$coefficients[2]
  t3_ri_ests[i] <- summary(ri_est_each)$coefficients[3]
  
  # Print number of simulations completed
  message('Processing image ', i, ' of ', sims)
}

# Calculate true estimates from actual random assignment
true_est_all <- lm_robust(comment ~ treated, data = comments,
                          clusters = address, fixed_effects = ~ city,
                          subset = opened == 1)

true_est_each <- lm_robust(comment ~ treatment, data = comments,
                           clusters = address, fixed_effects = ~ city,
                           subset = opened == 1)

true_est_all <- true_est_all$coefficients[1]
t1_true_est <- true_est_each$coefficients[1]
t2_true_est <- true_est_each$coefficients[2]
t3_true_est <- true_est_each$coefficients[3]

# Calculate proportion of "fake" random assignments with larger effects
p_value_all <- mean(abs(ri_ests_all) > true_est_all) # 0.0436
p_value_all
t1_p_value <- mean(abs(t1_ri_ests) > t1_true_est) # 0.3861
t1_p_value
t2_p_value <- mean(abs(t2_ri_ests) > t2_true_est) # 0.0711
t2_p_value
t3_p_value <- mean(abs(t3_ri_ests) > t3_true_est) # 0.0114
t3_p_value

end_time <- Sys.time()
end_time - start_time

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ROBUSTNESS: RANDOMIZATION INFERENCE (OTHER COMPARISONS) ----
# ______________________________________________________________________________

#### CACE ####
# Track time taken to run randomization inference:
# 1.5 hours with 10,000 sims
start_time <- Sys.time() 
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
  
  # Calculate effect sizes under simulated random assignment: each treatment
  ri_est_each <- lm_robust(comment ~ ri_assignment, data = comments,
                           clusters = address, fixed_effects = ~ city,
                           subset = opened == 1)
  
  # Calculate effect sizes under simulated random assignment: cost treatments
  ri_est_cost <- lm_robust(comment ~ ri_cost, data = comments,
                           clusters = address, fixed_effects = ~ city,
                           subset = opened == 1)
  
  # Store estimates of treatment effect from simulated random assignment
  t1_ri_ests[i] <- summary(ri_est_each)$coefficients[1]
  t2_ri_ests[i] <- summary(ri_est_each)$coefficients[2]
  t3_ri_ests[i] <- summary(ri_est_each)$coefficients[3]
  t2_t1_ri_ests[i] <- summary(ri_est_each)$coefficients[2] - summary(ri_est_each)$coefficients[1]
  t3_t1_ri_ests[i] <- summary(ri_est_each)$coefficients[3] - summary(ri_est_each)$coefficients[1]
  t3_t2_ri_ests[i] <- summary(ri_est_each)$coefficients[3] - summary(ri_est_each)$coefficients[2]
  ri_cost_info_ests[i] <- summary(ri_est_cost)$coefficients[2] - summary(ri_est_cost)$coefficients[1]
  
  # Print number of simulations completed
  message('Processing image ', i, ' of ', sims)
}

# Calculate true estimates from actual random assignment
true_est_each <- lm_robust(comment ~ treatment, data = comments,
                           clusters = address, fixed_effects = ~ city,
                           subset = opened == 1)

true_est_cost <- lm_robust(comment ~ cost_treatment, data = comments, 
                           clusters = address, fixed_effects = ~ city,
                           subset = opened == 1)

t2_t1_true_est <- summary(true_est_each)$coefficients[2] - summary(true_est_each)$coefficients[1]
t3_t1_true_est <- summary(true_est_each)$coefficients[3] - summary(true_est_each)$coefficients[1]
t3_t2_true_est <- summary(true_est_each)$coefficients[3] - summary(true_est_each)$coefficients[2]
cost_info_true_est <- summary(true_est_cost)$coefficients[2] - summary(true_est_cost)$coefficients[1]

# Calculate proportion of "fake" random assignments with larger effects
t2_t1_p_value <- mean(abs(t2_t1_ri_ests) > t2_t1_true_est) # 0.199
t2_t1_p_value
t3_t1_p_value <- mean(abs(t3_t1_ri_ests) > t3_t1_true_est) # 0.024
t3_t1_p_value
t3_t2_p_value <- mean(abs(t3_t2_ri_ests) > t3_t2_true_est) # 0.323
t3_t2_p_value
cost_info_p_value <- mean(abs(ri_cost_info_ests) > cost_info_true_est) # 0.034
cost_info_p_value

end_time <- Sys.time()
end_time - start_time

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ROBUSTNESS: RANDOMIZATION INFERENCE, ITT (VS PLACEBO) ----
# ______________________________________________________________________________

# Track time taken to run randomization inference
start_time <- Sys.time() # Time process: 3 hours with 1000 sims

# Set the number of simulated random assignments
sims <- 1000

# Create an empty vector to store our estimates
ri_ests_all <- rep(NA, sims)
t1_ri_ests <- rep(NA, sims)
t2_ri_ests <- rep(NA, sims)
t3_ri_ests <- rep(NA, sims)

for (i in 1:sims) {
  # Conduct new random assignment accord to same procedure as original
  comments$ri_assignment <-
    block_and_cluster_ra(
      blocks = comments$city,
      clusters = comments$address,
      conditions = c("Placebo", "Treatment 1", "Treatment 2", "Treatment 3"),
      prob_each = c(0.1, 0.3, 0.3, 0.3)
    )
  
  # Create hypothetical "treated" group
  comments <- comments %>% 
    mutate(ri_treated = ifelse(ri_assignment != "Placebo", 1, 0))
  
  # Calculate effect sizes under simulated random assignment: all treatments
  ri_est_all <- lm_robust(comment ~ ri_treated, data = comments,
                          clusters = address, fixed_effects = ~ city)
  
  # Calculate effect sizes under simulated random assignment: all treatments
  ri_est_each <- lm_robust(comment ~ ri_assignment, data = comments,
                           clusters = address, fixed_effects = ~ city)
  
  # Store estimates of treatment effect from simulated random assignment
  ri_ests_all[i] <- summary(ri_est_all)$coefficients[1]
  t1_ri_ests[i] <- summary(ri_est_each)$coefficients[1]
  t2_ri_ests[i] <- summary(ri_est_each)$coefficients[2]
  t3_ri_ests[i] <- summary(ri_est_each)$coefficients[3]
  
  # Print number of simulations completed
  message('Processing image ', i, ' of ', length(sims))
}

# Calculate true estimates from actual random assignment
true_est_all <- lm_robust(comment ~ treated, data = comments,
                          clusters = address, fixed_effects = ~ city)

true_est_each <- lm_robust(comment ~ treatment, data = comments,
                           clusters = address, fixed_effects = ~ city)

true_est_all <- true_est_all$coefficients[1]
t1_true_est <- true_est_each$coefficients[1]
t2_true_est <- true_est_each$coefficients[2]
t3_true_est <- true_est_each$coefficients[3]

# Calculate proportion of "fake" random assignments with larger effects
p_value_all_itt <- mean(abs(ri_ests_all) > true_est_all) # 0.074
p_value_all_itt
t1_p_value_itt <- mean(abs(t1_ri_ests) > t1_true_est) # 0.38
t1_p_value_itt
t2_p_value_itt <- mean(abs(t2_ri_ests) > t2_true_est) # 0.089
t2_p_value_itt
t3_p_value_itt <- mean(abs(t3_ri_ests) > t3_true_est) # 0.039
t3_p_value_itt

end_time <- Sys.time()
end_time - start_time

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ROBUSTNESS: RANDOMIZATION INFERENCE, ITT (ALL OTHER COMPARISONS) ----
# ______________________________________________________________________________

# Track time taken to run randomization inference
start_time <- Sys.time() # Time process: 3 hours with 1000 sims

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
  
  # Calculate effect sizes under simulated random assignment: each treatment
  ri_est_each <- lm_robust(comment ~ ri_assignment, data = comments,
                           clusters = address, fixed_effects = ~ city)
  
  # Calculate effect sizes under simulated random assignment: cost treatments
  ri_est_cost <- lm_robust(comment ~ ri_cost, data = comments,
                           clusters = address, fixed_effects = ~ city)
  
  # Store estimates of treatment effect from simulated random assignment
  t1_ri_ests[i] <- summary(ri_est_each)$coefficients[1]
  t2_ri_ests[i] <- summary(ri_est_each)$coefficients[2]
  t3_ri_ests[i] <- summary(ri_est_each)$coefficients[3]
  t2_t1_ri_ests[i] <- summary(ri_est_each)$coefficients[2] - summary(ri_est_each)$coefficients[1]
  t3_t1_ri_ests[i] <- summary(ri_est_each)$coefficients[3] - summary(ri_est_each)$coefficients[1]
  t3_t2_ri_ests[i] <- summary(ri_est_each)$coefficients[3] - summary(ri_est_each)$coefficients[2]
  ri_cost_info_ests[i] <- summary(ri_est_cost)$coefficients[2] - summary(ri_est_cost)$coefficients[1]
  
  # Print number of simulations completed
  message('Processing image ', i, ' of ', length(sims))
}

# Calculate true estimates from actual random assignment
true_est_each <- lm_robust(comment ~ treatment, data = comments,
                           clusters = address, fixed_effects = ~ city)

true_est_cost <- lm_robust(comment ~ cost_treatment, data = comments, 
                           clusters = address, fixed_effects = ~ city)

t2_t1_true_est <- summary(true_est_each)$coefficients[2] - summary(true_est_each)$coefficients[1]
t3_t1_true_est <- summary(true_est_each)$coefficients[3] - summary(true_est_each)$coefficients[1]
t3_t2_true_est <- summary(true_est_each)$coefficients[3] - summary(true_est_each)$coefficients[2]
cost_info_true_est <- summary(true_est_cost)$coefficients[2] - summary(true_est_cost)$coefficients[1]

# Calculate proportion of "fake" random assignments with larger effects
t2_t1_p_value_itt <- mean(abs(t2_t1_ri_ests) > t2_t1_true_est) # 0.266
t2_t1_p_value_itt
t3_t1_p_value_itt <- mean(abs(t3_t1_ri_ests) > t3_t1_true_est) # 0.082
t3_t1_p_value_itt
t3_t2_p_value_itt <- mean(abs(t3_t2_ri_ests) > t3_t2_true_est) # 0.565
t3_t2_p_value_itt
cost_info_p_value_itt <- mean(abs(ri_cost_info_ests) > cost_info_true_est) # 0.086
cost_info_p_value_itt

end_time <- Sys.time()
end_time - start_time