# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 22 August, 2023 by Trevor Incerti

# This file implements the Bayesian analysis referenced in the main text.
# Creates Figures A10 and A11. 

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(bayestestR)
library(car)
library(brms)
library(gridExtra)

# Functions
source("code/0. functions.R")

# Import data
comments <- read_csv("data/comments.csv")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FINAL DATA PREP ----
# ______________________________________________________________________________

# Add complier indicator
compliers <- comments %>% filter(opened == 1)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# PRIOR DISTRIBUTION FROM PRE-REGISTERED POWER ANALYSIS ----
# ______________________________________________________________________________

#### Define priors using distribution from pre-registration power analysis ####
priors <- 
  c(prior(normal(0.004, 0.1), class = b, coef = treatmentTreatment1),
    prior(normal(0.0077, 0.1), class = b, coef = treatmentTreatment2),
    prior(normal(0.01, 0.1), class = b, coef = treatmentTreatment3),
    prior(normal(0.001, 0.01), class = Intercept))

# Check priors made it into Stan code
make_stancode(comment ~ treatment, data = compliers, family = gaussian(),
              prior = priors)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# RUN BAYESIAN MIXED MODEL AND CALCULATE EVIDENCE RATIOS ----
# ______________________________________________________________________________

# Run model: all treatments
bayes_ols_fx = brm(
  comment ~ treatment + city,
  data  = compliers,
  prior = priors,
  sample_prior = "yes",
  family = gaussian,
  cores = 4,
  seed = 444
)

#### Calculate posterior probability (evidence ratio) under the hypothesis against alternative ####
hypothesis(bayes_ols_fx, "treatmentTreatment 3 - treatmentTreatment 1 > 0")
hypothesis(bayes_ols_fx, "treatmentTreatment 3 - treatmentTreatment 2 > 0")

#### Create Figure A10: Plot coefficients and posterior distributions ####
mcmc_plot(bayes_ols_fx, 
          type = "areas",
          prob = 0.95,
          variable = c("b_Intercept", "b_treatmentTreatment1", 
                       "b_treatmentTreatment2", "b_treatmentTreatment3")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(labels = c("Intercept", "Instructions-only",
                             "Economic cost", "Costly abstension"))

#ggsave(file="figs/fgA10.pdf", height = 3, width = 7)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DIFFERENCES IN COEFFICIENTS PLOTS ----
# ______________________________________________________________________________

#### Plot: Costly abstention vs. Instructions only ####
bfit_post <- posterior_samples(bayes_ols_fx)
bfit_diff <- bfit_post$b_treatmentTreatment3 - bfit_post$b_treatmentTreatment1

mp <- as.data.frame(posterior_samples(bayes_ols_fx))
var1_post <- dplyr::select(mp, b_treatmentTreatment3)
var2_post <- dplyr::select(mp, b_treatmentTreatment1)
diff <- as.data.frame(var1_post - var2_post)

mpost_var1 <- data.frame(post = var1_post, Coefficient = "Costly abstension")
colnames(mpost_var1) <- c("post", "Coefficient")
mpost_var2 <- data.frame(post = var2_post, Coefficient = "Instructions only")
colnames(mpost_var2) <- c("post", "Coefficient")
mpost_diff <- data.frame(post = diff, Coefficient = "Costly abstension - Instructions only")
colnames(mpost_diff) <- c("post", "Coefficient")

mpost_long_df <- rbind(mpost_var1, mpost_var2, mpost_diff)
mpost_long_df <- mpost_long_df %>% group_by(Coefficient) %>% 
  mutate(post_mean = mean(post), 
         post_lwr = quantile(post, probs = .025),
         post_upr = quantile(post, probs = .975),
         post_lwr90 = quantile(post, probs = .05),
         post_upr90 = quantile(post, probs = .95)) %>% ungroup()

# Create figure A11 (left panel)
bayes_t3_t1 <- ggplot(mpost_long_df) +
  geom_density(aes(x = post, fill = Coefficient), color = "white", alpha = .75) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "\nCoefficient", y = "Density\n") +
  scale_fill_manual(values = c("steelblue2", "seagreen3", "steelblue3")) +
  scale_color_manual(values = c("steelblue2", "seagreen3", "steelblue3")) +
  geom_segment(aes(x=post_lwr, xend=post_upr, y=.05, yend=.05), color = "grey25", size = .2) +
  geom_point(aes(x = post_mean, y = 0.05), color = "grey25", size = 1) +
  theme_classic() +
  theme(text=element_text(size=12)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~factor(Coefficient, 
                     levels=c('Costly abstension','Instructions only',
                              'Costly abstension - Instructions only')), 
             ncol = 1, strip.position="top")

#### Plot: Costly abstention vs. economic cost ####
bfit_post <- posterior_samples(bayes_ols_fx)
bfit_diff <- bfit_post$b_treatmentTreatment3 - bfit_post$b_treatmentTreatment2

mp <- as.data.frame(posterior_samples(bayes_ols_fx))
var1_post <- dplyr::select(mp, b_treatmentTreatment3)
var2_post <- dplyr::select(mp, b_treatmentTreatment2)
diff <- as.data.frame(var1_post - var2_post)

mpost_var1 <- data.frame(post = var1_post, Coefficient = "Costly abstension")
colnames(mpost_var1) <- c("post", "Coefficient")
mpost_var2 <- data.frame(post = var2_post, Coefficient = "Economic cost")
colnames(mpost_var2) <- c("post", "Coefficient")
mpost_diff <- data.frame(post = diff, Coefficient = "Costly abstension - Economic cost")
colnames(mpost_diff) <- c("post", "Coefficient")

mpost_long_df <- rbind(mpost_var1, mpost_var2, mpost_diff)
mpost_long_df <- mpost_long_df %>% group_by(Coefficient) %>% 
  mutate(post_mean = mean(post), 
         post_lwr = quantile(post, probs = .025),
         post_upr = quantile(post, probs = .975),
         post_lwr90 = quantile(post, probs = .05),
         post_upr90 = quantile(post, probs = .95)) %>% ungroup()

# Create Figure A11 (right panel)
bayes_t3_t2 <- ggplot(mpost_long_df) +
  geom_density(aes(x = post, fill = Coefficient), color = "white", alpha = .75) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "\nCoefficient", y = "Density\n") +
  scale_fill_manual(values = c("steelblue2", "seagreen3", "steelblue3")) +
  scale_color_manual(values = c("steelblue2", "seagreen3", "steelblue3")) +
  geom_segment(aes(x=post_lwr, xend=post_upr, y=.05, yend=.05), color = "grey25", size = .2) +
  geom_point(aes(x = post_mean, y = 0.05), color = "grey25", size = 1) +
  theme_classic() +
  theme(text=element_text(size=12)) +
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~factor(Coefficient, 
                     levels=c('Costly abstension','Economic cost',
                              'Costly abstension - Economic cost')), 
             ncol = 1, strip.position="top")

# Create Figure A11: Combine above into two figures and save
bayes_hypotheses <- grid.arrange(bayes_t3_t1, bayes_t3_t2, nrow = 1)
#ggsave(bayes_hypotheses, file="figs/fgA11.pdf", height = 5, width = 7)