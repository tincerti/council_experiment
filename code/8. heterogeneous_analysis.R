# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 15 October, 2021 by Trevor Incerti

# This file analyzes the results from email experiments

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES ----
# ______________________________________________________________________________

# Install any required packages
list.of.packages <- c("tidyverse", "DeclareDesign", "modelsummary", 
                      "kableExtra", "formattable", "gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries
library(tidyverse)
library(DeclareDesign)
library(modelsummary)
library(kableExtra)
library(formattable)
library(gridExtra)

# Options
options(scipen=999)
set.seed(999)

# Functions
source("code/0. functions.R")

# Import data
comments <- read_csv("data/outcomes/comments.csv")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FINAL DATA PREP ----
# ______________________________________________________________________________

comments <- comments %>% mutate(treated = ifelse(treated == "Placebo", 0, 1))

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# HETEROGENOUS TREATMENT EFFECTS: CONDITIONAL AVERAGE TREATMENT EFFECTS ----
# ______________________________________________________________________________

# Vote history in local elections
vh_cate <- lm_robust(comment ~ treated + vote_2017_municipal + 
                       treated:vote_2017_municipal + city,
                     data = comments, subset = opened == 1, clusters = address)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# HETEROGENOUS TREATMENT EFFECTS: RANDOMIZATION INFERENCE  ----
# ______________________________________________________________________________

# Calculate difference in CATEs by vote history
voted = lm_robust(comment ~ treated, fixed_effects = ~city, 
                            data = comments, clusters = address, 
                            subset = opened == 1 & vote_2017_municipal == 1)

abstained = lm_robust(comment ~ treated, fixed_effects = ~city, 
                      data = comments, clusters = address, 
                      subset = opened == 1 & vote_2017_municipal == 0)

dic <- abs(voted$coefficients[[1]] - abstained$coefficients[[1]])

# Use randomization inference to test null that CATEs in both groups equal ATE
sims = 10000
null <- comments
dic_null <- vector(mode = "integer", length = sims)

# 1. # Form full schedule of potential outcomes assuming constant effects
null$ate = lm_robust(comment ~ treated, fixed_effects = ~city, 
                     data = comments, clusters = address, 
                     subset = opened == 1)$coefficients[1]
null$comment_null = with(null, ifelse(treated == 1, comment + ate, comment))

for (i in seq_along(dic_null)) { 
  
  # 2. Assign subjects randomly to treatment or control
  null$z_null <-
    block_and_cluster_ra(
      blocks = comments$city,
      clusters = comments$address,
      conditions = c("Placebo", "Treatment 1", "Treatment 2", "Treatment 3"),
      prob_each = c(0.1, 0.3, 0.3, 0.3)
    )
  
  # 3. Calculate difference between estimated CATEs
  vote_null = lm(comment_null ~ z_null, data = null, subset = vote_2017_municipal == 1)
  abstain_null = lm(comment_null ~ z_null, data = null, subset = vote_2017_municipal == 0)
  dic_null[[i]] <- abs(vote_null$coefficients[[2]] - abstain_null$coefficients[[2]])
}

# 4. Calculate two sided p-value (0.062)
p_vote <- sum(abs(dic_null) >= dic)/length(dic_null)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# VISUALIZATIONS AND TABULATIONS ----
# ______________________________________________________________________________

#### Coefficient plot ####
# Perform CATE analysis using subgroup method rather than interactions
vote_cate <- list(
  "Voted in 2017 municipal election" = 
    lm_robust(comment ~ treated, fixed_effects = ~city, 
              data = comments, clusters = address, 
              subset = opened == 1 & vote_2017_municipal == 1),
  
  "Did not vote in 2017 municipal election" = 
    lm_robust(comment ~ treated, fixed_effects = ~city, 
              data = comments, clusters = address,
              subset = opened == 1 & vote_2017_municipal == 0)
)

# Create coefficient plot
modelplot(vote_cate, coef_map = c('treated' = 'Treated'), 
          coef_omit = "Constant", draw = F) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = model) %>%
  mutate(across(estimate:conf.high, ~ . * 100)) %>%
  ggplot(aes(x = estimate, y = model, group = model)) +
  gglayers +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1))

ggsave(file="figs/voting_cates.pdf", height = 1.5, width = 7)

#### Table ####
vh <- list("CATE" = vh_cate)

rows <- tribble(~term, ~CATE,
                'City fixed effects:', 'Yes')
attr(rows, 'position') <- 9

modelsummary(vh, stars = TRUE,
             coef_map = c(
               '(Intercept)' = 'Constant',
               'treated' = 'Treated',
               'vote_2017_municipal' = 'Voted in 2017 municipal election',
               'treated:vote_2017_municipal' = 'Treated x Voted'
             ),
             notes = c("Notes: CATE standard errors clustered at the address level."),
             gof_omit = omit,
             add_rows = rows,
             output = "latex") %>%
  #kable_styling(latex_options = c("scale_down")) %>%
  row_spec(c(1,3,5,7), background = '#D3D3D3') %>%
  save_kable("tables/cate_table.tex")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# HETEROGENOUS TREATMENT EFFECTS: EXPLORATORY (NOT PRE-REGISTERED)  ----
# ______________________________________________________________________________

# Partisanship: CATEs
lm_robust(comment ~ treated + dem +  treated:dem, fixed_effects = ~city,
          data = comments, subset = opened == 1, clusters = address)

lm_robust(comment ~ treated + rep + treated:rep, fixed_effects = ~city,
          data = comments, subset = opened == 1, clusters = address)

# Partisanship: assumptions
lm_robust(comment ~ dem, data = comments, 
          subset = opened == 1 & treated == 1, clusters = address)

lm_robust(comment ~ rep, data = comments, 
          subset = opened == 1 & treated == 1, clusters = address)


