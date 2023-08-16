# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 16 August, 2023 by Trevor Incerti

# This file combines the results of individual council meetings 
# using meta-analysis

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(DeclareDesign)
library(modelsummary)
library(kableExtra)
library(metafor)
library(gridExtra)
library(stargazer)

# Options
options(scipen=999)

# Functions
source("code/0. functions.R")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# IMPORT AND MERGE DATA ----
# ______________________________________________________________________________

# Import data
pilot <- read_csv("data/pilot_outcomes.csv")
comments <- read_csv("data/comments.csv")

# Create indicator for pilot studies and merge 
pilot <- pilot %>% 
  mutate(
    pilot = 1,
    city = if_else(str_detect(filename, "longbeach"), 
                   "LONG BEACH", "SANTA MONICA"),
    comment = ifelse(spoken_comment == 1 | written_comment == 1, 1, 0),
    treated = if_else(treatment == "placebo", "Placebo", "Treatment")
  )

all <- bind_rows(pilot, comments)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ANALYSES ----
# ______________________________________________________________________________

##### Create list of covariates for covariate adjustment in estimation ####
covs = ~ gender + english + age + 
  yearbuilt + units + dem + rep + npp +
  vote_2020_general + vote_2017_municipal + vote_2016_general

# Add treated indicator
all <- all %>% 
  mutate(received_treatment = ifelse(opened == 1 & treated == "Treatment", 1, 0))

#### Complier Average Causal Effect / Local Average Treatment Effect ####
# Santa Monica 8.26
n <- 91
p <- (1/(n+2))
se = sqrt(p*(1-p)/n)
santamonica_08.26_cace <- 
  tibble(term = "Treatment", estimate = 0, 
         std.error = se, conf.low = 0-(1.96*se), conf.high = 0+(1.96*se),
         model = "santamonica_08.26_cace")

# Long beach 9/07
longbeach_9.07_cace <- 
  lm_lin(comment ~ treated, covs, data = all, 
         subset = opened == 1 & str_detect(filename, "9.07")) %>%
  tidy %>% mutate(model = "longbeach_9.07_cace")

# Long Beach 9/14
longbeach_9.14_cace <- 
  lm_lin(comment ~ treated, covs, data = all, 
         subset = opened == 1 & str_detect(filename, "9.14")) %>% 
  tidy %>% mutate(model = "longbeach_9.14_cace")

# Beverly Hills 10/12 (design matrix rank deficient for lm_lin)
beverlyhills_10.12_cace <- 
  lm_robust(comment ~ treated, data = all, 
         subset = opened == 1 & str_detect(filename, "beverlyhills_10.12")) %>% 
  tidy %>% mutate(model = "beverlyhills_10.12_cace")

# Santa Monica 10/12
santamonica_10.12_cace <- 
  lm_lin(comment ~ treated, 
         ~ gender + age + 
           yearbuilt + units + dem + rep + npp +
           vote_2020_general + vote_2017_municipal + vote_2016_general
         , data = all, 
         subset = opened == 1 & str_detect(filename, "santamonica_10.12")) %>% 
  tidy %>% mutate(model = "santamonica_10.12_cace")

# Whittier 10/12 (design matrix rank deficient for lm_lin)
whittier_10.12_cace <- 
  lm_robust(comment ~ treated, data = all, 
         subset = opened == 1 & str_detect(filename, "whittier_10.12")) %>% 
  tidy %>% mutate(model = "whittier_10.12_cace")

# Rancho Palos Verdes 10/19 (design matrix rank deficient for lm_lin)
ranchopalosverdes_10.19_cace <- 
  lm_robust(comment ~ treated, data = all, 
            subset = opened == 1 & str_detect(filename, "ranchopalosverdes_10.19")) %>% 
  tidy %>% mutate(model = "ranchopalosverdes_10.19_cace")

# Manhattan Beach 11/02
comments %>% group_by(city, opened) %>% summarize(n = n())
n <- 70
p <- (1/(n+2))
se = sqrt(p*(1-p)/n)
manhattanbeach_11.02_cace <- 
  tibble(term = "treatedTreatment", estimate = 0, 
         std.error = se, conf.low = 0-(1.96*se), conf.high = 0+(1.96*se),
         model = "manhattanbeach_11.02_cace")

# Norwalk 11/02 (design matrix rank deficient for lm_lin)
norwalk_11.02_cace <- 
  lm_robust(comment ~ treated, data = all, 
            subset = opened == 1 & str_detect(filename, "norwalk_11.02")) %>% 
  tidy %>% mutate(model = "norwalk_11.02_cace")

# Sierra Madre 11/09
comments %>% group_by(city, opened) %>% summarize(n = n())
n <- 31
p <- (1/(n+2))
se = sqrt(p*(1-p)/n)
sierramadre_11.09_cace <- 
  tibble(term = "treatedTreatment", estimate = 0, 
         std.error = se, conf.low = 0-(1.96*se), conf.high = 0+(1.96*se),
         model = "sierramadre_11.09_cace")

# Culver city 12/10
culvercity_12.10_cace <- 
  lm_robust(comment ~ treated, data = all, 
            subset = opened == 1 & str_detect(filename, "culvercity_12.10")) %>% 
  tidy %>% mutate(model = "culvercity_12.10_cace")


# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ADD INDIVIDUALS STUDIES TO SINGLE DATAFRAME
# ______________________________________________________________________________

# Create dataframe
meta_cace <- 
  bind_rows(longbeach_9.07_cace, longbeach_9.14_cace,
            beverlyhills_10.12_cace, santamonica_10.12_cace, 
            whittier_10.12_cace, ranchopalosverdes_10.19_cace,
            manhattanbeach_11.02_cace, norwalk_11.02_cace,
            sierramadre_11.09_cace, culvercity_12.10_cace) %>%
  filter(term == "treatedTreatment") %>% 
  mutate(term = "Treatment", estimand = "CACE") %>%
  bind_rows(santamonica_08.26_cace)

# Clean meeting names
meta_cace <- meta_cace %>% 
  mutate(meeting = case_when(
  str_detect(model, "longbeach_9.07") ~ "Long Beach 9/7",
  str_detect(model, "longbeach_9.14") ~ "Long Beach 9/14",
  str_detect(model, "santamonica_08.26") ~ "Santa Monica 8/26",
  str_detect(model, "beverlyhills_10.12") ~ "Beverly Hills 10/12",
  str_detect(model, "santamonica_10.12") ~ "Santa Monica 10/12",
  str_detect(model, "whittier_10.12") ~ "Whittier 10/12",
  str_detect(model, "ranchopalosverdes_10.19") ~ "Rancho Palos Verdes 10/19",
  str_detect(model, "manhattanbeach_11.02_cace") ~ "Manhattan Beach 11/02",
  str_detect(model, "norwalk_11.02_cace") ~ "Norwalk 11/02",
  str_detect(model, "sierramadre_11.09_cace") ~ "Sierra Madre 11/09",
  str_detect(model, "culvercity_12.10_cace") ~ "Culver City 12/10"
  )
  )

# Remove pilot studies for robustness analysis
meta_cace_nopilot <- meta_cace %>%
  filter(!meeting %in% 
           c("Long Beach 9/14", "Long Beach 9/7", "Santa Monica 8/26"))
  

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# PERFORM META-ANALYSIS
# ______________________________________________________________________________

# Random effects model
re_cace = rma.uni(yi = estimate, sei = std.error, data = meta_cace)

# Fixed effects model
fe_cace = rma.uni(yi = estimate, sei = std.error, weighted = TRUE,
                    method = "FE", data = meta_cace)

# Random effects model - no pilot
re_cace_nopilot = rma.uni(yi = estimate, sei = std.error, 
                          data = meta_cace_nopilot)

# Fixed effects model - no pilot
fe_cace_nopilot = rma.uni(yi = estimate, sei = std.error, weighted = TRUE,
                  method = "FE", data = meta_cace_nopilot)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# PREPARE FOR PLOTTING
# ______________________________________________________________________________

#### Create plot object: CACE #### 
meta_fe_cace = tibble(
  estimate = fe_cace$beta, conf.high = fe_cace$ci.ub, conf.low = fe_cace$ci.lb,
  meeting = "Fixed-effects")

meta_re_cace = tibble(
  estimate = re_cace$beta, conf.high = re_cace$ci.ub, conf.low = re_cace$ci.lb,
  meeting = "Random-effects")

meta_cace <- meta_cace %>% 
  select(estimate, conf.low, conf.high, meeting)
meta_cace <- rbind(meta_cace, meta_fe_cace, meta_re_cace)

# Mutliply effect size by 100
meta_cace <- meta_cace %>% mutate(across(estimate:conf.high, ~ . * 100))

# Re-order factor levels
meta_cace <- meta_cace %>% 
  mutate(
    meeting = 
      fct_relevel(meeting, 
                  "Random-effects", "Fixed-effects",
                  "Culver City 12/10", "Sierra Madre 11/09", 
                  "Norwalk 11/02", "Manhattan Beach 11/02", 
                  "Rancho Palos Verdes 10/19",
                  "Whittier 10/12", "Santa Monica 10/12", "Beverly Hills 10/12",
                  "Long Beach 9/14", "Long Beach 9/7", "Santa Monica 8/26" # Pilots
                  ))

#### Create plot object: CACE without pilot studies #### 
meta_fe_cace_nopilot = tibble(
  estimate = fe_cace_nopilot$beta, 
  conf.high = fe_cace_nopilot$ci.ub, conf.low = fe_cace_nopilot$ci.lb,
  meeting = "Fixed-effects")

meta_re_cace_nopilot = tibble(
  estimate = re_cace_nopilot$beta, 
  conf.high = re_cace_nopilot$ci.ub, conf.low = re_cace_nopilot$ci.lb,
  meeting = "Random-effects")

meta_cace_nopilot <- meta_cace_nopilot %>% 
  select(estimate, conf.low, conf.high, meeting)
meta_cace_nopilot <- rbind(meta_cace_nopilot, 
                           meta_fe_cace_nopilot, meta_re_cace_nopilot)

# Mutliply effect size by 100
meta_cace_nopilot <- meta_cace_nopilot %>% 
  mutate(across(estimate:conf.high, ~ . * 100))

# Re-order factor levels
meta_cace_nopilot <- meta_cace_nopilot %>% 
  mutate(
    meeting = 
      fct_relevel(meeting, 
                  "Random-effects", "Fixed-effects",
                  "Culver City 12/10", "Sierra Madre 11/09", 
                  "Norwalk 11/02", "Manhattan Beach 11/02", 
                  "Rancho Palos Verdes 10/19",
                  "Whittier 10/12", "Santa Monica 10/12", "Beverly Hills 10/12"
                  ))

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# PLOT
# ______________________________________________________________________________

#### Create Figure 2: CACE by city and meta-analyiss #### 
ggplot(meta_cace, aes(estimate, meeting)) +
  geom_point(color = "steelblue2", size = 1.5) + 
  geom_point(data = subset(meta_cace, 
                           meeting == "Long Beach 9/7" | meeting  == "Long Beach 9/14" |
                           meeting == "Santa Monica 8/26"), 
             size = 1.5, color = "seagreen3") +
  geom_point(data = subset(meta_cace, 
                           meeting == "Fixed-effects" | meeting  == "Random-effects"), 
             size = 1.5, color = "black", fill = "black") +
  geom_errorbarh(aes(y = meeting, xmin = conf.low, xmax = conf.high),
                 color="grey30", size=0.5, alpha = 0.5, height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 2.5, linetype = "solid") +
  xlab("Change in comments submitted (percentage points)") + 
  scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, by = 1)) +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

# Save figure
ggsave(file="figs/fg2.pdf", height = 4, width = 7)

#### Create Figure A8: Meta-analysis and CACE without pilot studies #### 
meta_cace_nopilot %>%
  ggplot(aes(estimate, meeting)) +
  geom_point(color = "steelblue2", size = 1.5) + 
  geom_point(data = subset(meta_cace_nopilot, 
                           meeting == "Fixed-effects" | meeting  == "Random-effects"), 
             size = 1.5, color = "black", fill = "black") +
  geom_errorbarh(aes(y = meeting, xmin = conf.low, xmax = conf.high),
                 color="grey30", size=0.5, alpha = 0.5, height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 2.5, linetype = "solid") +
  xlab("Change in comments submitted (percentage points)") + 
  scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, by = 1)) +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

# Save figures
ggsave(file="figs/fgA8.pdf", height = 4, width = 7)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CREATE TABLES
# ______________________________________________________________________________

# Individual city CACE estimates -----------------------------------------------
# Pull out number of observations in each city
unique(all$filename)
city_n <- all %>% 
  mutate(meeting = case_when(
    str_detect(filename, "longbeach_9.07") ~ "Long Beach 9/7",
    str_detect(filename, "longbeach_9.14") ~ "Long Beach 9/14",
    str_detect(filename, "santamonica_8.26") ~ "Santa Monica 8/26",
    str_detect(filename, "beverlyhills_10.12") ~ "Beverly Hills 10/12",
    str_detect(filename, "santamonica_10.12") ~ "Santa Monica 10/12",
    str_detect(filename, "whittier_10.12") ~ "Whittier 10/12",
    str_detect(filename, "ranchopalosverdes_10.19") ~ "Rancho Palos Verdes 10/19",
    str_detect(filename, "manhattanbeach_11.02") ~ "Manhattan Beach 11/02",
    str_detect(filename, "norwalk_11.02") ~ "Norwalk 11/02",
    str_detect(filename, "sierramadre_11.09") ~ "Sierra Madre 11/09",
    str_detect(filename, "culvercity_12.10") ~ "Culver City 12/10"
  )) %>%
  group_by(meeting, opened) %>% 
  summarize(n = n()) %>%
  filter(opened == 1) %>%
  select(meeting, n)

# Merge number of observations with estimates for each city
city_cace <- left_join(meta_cace, city_n)

# Format data for table export
city_cace <- city_cace %>%
  filter(meeting != "Fixed-effects", meeting != "Random-effects") %>%
  mutate(`95% CI` = 
           paste0("[", round(conf.low, 3), " , ", round(conf.high, 3), "]")
  ) %>%
  select(Meeting = meeting, CACE = estimate, `95% CI`, N = n)

# Create Table A9: Export using stargazer
stargazer::stargazer(city_cace,
                     out = "tables/tblA9.tex",
                     title= "CACEs for each city council meeting",
                     label = "city_cace",
                     digits = 3,
                     column.sep.width = "30pt",
                     rownames = FALSE, 
                     summary = FALSE,
                     notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place. N is equal to the number of compliers in each city.}"
)

# Meta analysis estimates ------------------------------------------------------
# Create row labels
Value = 
  c("Weighted fixed effects, w/ pilot studies", 
    "" ,
    "Random effects, w/ pilot studies", 
    "" ,
    "Weighted fixed effects, w/o pilot studies", 
    "" ,
    "Random effects, w/o pilot studies", 
    "" )

# Populate rows with point estimates and standard errors
Estimate = 
  c(round(fe_cace$beta[1], 3), 
    paste0("(", format(unlist(round(fe_cace$se, 3))),")"), 
    round(re_cace$beta[1], 3), 
    paste0("(", format(unlist(round(re_cace$se, 3))),")"),
    round(fe_cace_nopilot$beta[1], 3), 
    paste0("(", format(unlist(round(fe_cace_nopilot$se, 3))),")"), 
    round(re_cace_nopilot$beta[1], 3), 
    paste0("(", format(unlist(round(re_cace_nopilot$se, 3))),")"))

# Create confidence intervals
`95% CI` = 
  c(paste0("[", round(fe_cace$ci.lb, 3), " , ", round(fe_cace$ci.ub, 3), "]"),
    "", 
    paste0("[", round(re_cace$ci.lb, 3), " , ", round(re_cace$ci.ub, 3), "]"),
    "",
    paste0("[", round(fe_cace_nopilot$ci.lb, 3), " , ", round(fe_cace_nopilot$ci.ub, 3), "]"),
    "", 
    paste0("[", round(re_cace_nopilot$ci.lb, 3), " , ", round(re_cace_nopilot$ci.ub, 3), "]"),
    "")

# Number of observations
N =   c("4545", "" , "4545", "" , "3381", "" , "3381", "" )

# Combine into dataframe
meta = data.frame(Value, Estimate, `95% CI`, N, check.names = FALSE)

# Create Table A10: Export using stargazer
stargazer::stargazer(meta,
                     out = "tables/tblA10.tex",
                     title= "Meta-analysis estimates",
                     label = "meta",
                     digits = 3,
                     column.sep.width = "30pt",
                     rownames = FALSE, 
                     summary = FALSE,
                     notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. N is equal to the number of compliers.}"
)
