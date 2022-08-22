# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 2 February, 2022 by Trevor Incerti

# This file analyzes the results from email experiments

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(DeclareDesign)
library(modelsummary)
library(kableExtra)
library(formattable)
library(gridExtra)

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

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DIFFERENTIAL COMPLIANCE: BY TREATMENT ----
# ______________________________________________________________________________

##### Opened a message: differential compliance check ##### 
# Create table of compliance rates
compliance_rates <- comments %>%
  group_by(treatment, opened) %>%
  summarize(n = n()) %>%
  mutate(open_rate = round(n / sum(n), 3)) %>%
  filter(opened == 1)

# Differential compliance
opened <- lm_robust(opened ~ treatment + city, data = comments)

# Coefficient plot
modelplot(opened, coef_map = treatments, coef_omit = "Constant", draw = F) %>%
  ggplot(aes(x = estimate, y = term, group = term)) +
  gglayers +
  xlab("Change in opening rate (reference group = placebo)") +
  scale_x_continuous(limits = c(-0.05, 0.05), breaks = seq(-0.05, 0.05, by = 0.02),
                     labels = scales::percent_format(accuracy = 1))

ggsave(file="figs/differential_compliance.pdf", height = 2.5, width = 7)

# Are covariates similarly predictive of compliance in placebo and treatment?
opened_covs <- list(
  "Placebo" = lm_robust(opened ~ 
                          gender + english + age + yearbuilt + units + 
                          dem + rep + npp + vote_2020_general + 
                          vote_2017_municipal + vote_2016_general, 
                        data = comments, subset = treatment == "Placebo"),
  "Treatment 1" = lm_robust(opened ~ 
                            gender + english + age + yearbuilt + units + 
                            dem + rep + npp + vote_2020_general + 
                            vote_2017_municipal + vote_2016_general, 
                          data = comments, subset = treatment == "Treatment 1"),
  "Treatment 2" = lm_robust(opened ~ 
                              gender + english + age + yearbuilt + units + 
                              dem + rep + npp + vote_2020_general + 
                              vote_2017_municipal + vote_2016_general, 
                            data = comments, subset = treatment == "Treatment 2"),
  "Treatment 3" = lm_robust(opened ~ 
                              gender + english + age + yearbuilt + units + 
                              dem + rep + npp + vote_2020_general + 
                              vote_2017_municipal + vote_2016_general, 
                            data = comments, subset = treatment == "Treatment 3")
)

# Table 
modelsummary(opened_covs, stars = T, gof_omit = omit, output = 'latex') %>%
  kable_styling(latex_options = c("striped"), stripe_color = "gray!20") %>%
  save_kable("tables/covs_compliance.tex")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DIFFERENTIAL COMPLIANCE: BY COUNCIL MEETING ----
# ______________________________________________________________________________

# Estimate compliance by city 
compliance_city <- list(
  "Beverly Hills 10/12" = lm_robust(opened ~ treatment, 
                                    data = comments, subset = city == "BEVERLY HILLS"),
  "Santa Monica 10/12" = lm_robust(opened ~ treatment, 
                                   data = comments, subset = city == "SANTA MONICA"),
  "Whittier 10/12" = lm_robust(opened ~ treatment, 
                               data = comments, subset = city == "WHITTIER"),
  "Rancho Palos Verdes 10/19" = lm_robust(opened ~ treatment, 
                               data = comments, subset = city == "RANCHO PALOS VERDES"),
  "Manhattan Beach 11/02" = lm_robust(opened ~ treatment, 
                                          data = comments, subset = city == "MANHATTAN BEACH"),
  "Norwalk 11/02" = lm_robust(opened ~ treatment, 
                                          data = comments, subset = city == "NORWALK"),
  "Sierra Madre 11/09" = lm_robust(opened ~ treatment, 
                              data = comments, subset = city == "SIERRA MADRE"),
  "Culver City 12/10" = lm_robust(opened ~ treatment, 
                                   data = comments, subset = city == "CULVER CITY")
)

# Plot
modelplot(compliance_city, coef_map = treatments, 
          coef_omit = "Constant", draw = F) %>%
  filter(term != "Constant") %>%
  extract(model, into = c('city', 'date'), remove = F, 
          regex = '(.*)\\s+([^ ]+)$') %>%
  arrange(desc(date), city, term) %>%
  mutate(model = factor(model, levels=unique(model))) %>%
  
  ggplot(aes(x = estimate, y = model, group = term, color = term)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("seagreen3", "steelblue2", "firebrick")) +
  geom_errorbarh(aes(y = model, group = term, 
                     xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5),
                 color="grey30", size=0.5, alpha = 0.25, height = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.1),
                     labels = scales::percent_format(accuracy = 1)) +
  xlab("Change in opening rate (reference group = placebo)") +
  ylab("") +
  theme_classic() +
  theme(
    legend.position="bottom",
    panel.spacing = unit(0.1, "lines"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size=12),
    legend.title=element_blank()
  )

ggsave(file="figs/differential_compliance_city.pdf", height = 6, width = 8)

