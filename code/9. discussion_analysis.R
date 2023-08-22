# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 16 August, 2023 by Trevor Incerti

# This file combines conducts all analyses included in the discussion section
# Creates Figure 5 and Table 1

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES ----
# ______________________________________________________________________________

# Load libraries
library(tidyverse)
library(DeclareDesign)
library(modelsummary)
library(kableExtra)
library(formattable)

# Options
options(scipen=999)

# Functions
source("code/0. functions.R")

# Import comment data
comments <- read_csv("data/comments.csv")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# SPOKEN VS WRITTEN, PRO VS. ANTI HOUSING, AND PRE-WRITTEN VS CUSTOM ----
# ______________________________________________________________________________

# Create pre-written comment variable
comments <- comments %>%
  mutate(prewritten_comment = if_else(custom_comment == 0 & written_comment == 1, 1, 0))

#### Percentages: spoken vs written, pre-written vs. custom ####
percent_written = sum(comments$written_comment)/sum(comments$comment)
percent_custom = sum(comments$custom_comment)/sum(comments$comment)
percent_custom_written <- comments %>% filter(spoken_comment != 1)
percent_custom_written = sum(percent_custom_written$custom_comment)/sum(percent_custom_written$comment)
percent_anti = sum(comments$anti_comment)/sum(comments$comment)

#### Run analyses ####
pro_anti_custom_cace <- list(
  "Spoken comment" = lm_robust(spoken_comment ~ treated, 
                            data = comments, subset = opened == 1, clusters = address),
  "Written comment" = lm_robust(written_comment ~ treated, 
                            data = comments, subset = opened == 1, clusters = address),
  "Pro-housing" = lm_robust(pro_comment ~ treated, 
                         data = comments, subset = opened == 1, clusters = address),
  "Anti-housing" = lm_robust(anti_comment ~ treated, 
                          data = comments, subset = opened == 1, clusters = address),
  "Custom" = lm_robust(custom_comment ~ treated, 
                    data = comments, subset = opened == 1, clusters = address),
  "Pre-written" = lm_robust(prewritten_comment ~ treated, 
                       data = comments, subset = opened == 1, clusters = address)
)

##### Create Figure 5: CACE by type of comment ####
modelplot(pro_anti_custom_cace, coef_map = treatments, 
          coef_omit = "Constant", draw = F) %>%
  filter(term != "Constant") %>%
  mutate(
    type = case_when(
      model == "Spoken comment" | model == "Written comment" ~ "Spoken vs written comments",
      model == "Anti-housing" | model == "Pro-housing" ~ "Pro vs anti housing comments",
      model == "Custom" | model == "Pre-written" ~ "Pre-written vs custom comments")) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = model) %>%
  mutate(across(estimate:conf.high, ~ . * 100)) %>%
  ggplot(aes(x = estimate, y = model, group = model)) +
  facet_wrap(~factor(type, levels=c("Spoken vs written comments",
                                    "Pre-written vs custom comments",
                                    "Pro vs anti housing comments"
                                    )), 
             nrow = 3, scales = "free") +
  gglayers +
  scale_x_continuous(limits = c(-0.5, 1.5), breaks = seq(-0.5, 1.5, by = 0.5))

ggsave(file="figs/fg5.pdf", height = 3, width = 7)

##### Create Table A12: Complier average causal effects by outcome ####
modelsummary(pro_anti_custom_cace, stars = TRUE,
             coef_map = c(
               '(Intercept)' = 'Constant',
               'treatedTreatment' = 'Treated'
             ),
             notes = c("Notes: CATE standard errors clustered at the address level."),
             gof_omit = omit,
             output = "latex") %>%
  kable_styling(latex_options = c("scale_down")) %>%
  row_spec(c(1,3,5,7), background = '#D3D3D3') %>%
  save_kable("tables/tblA12.tex")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# SUBSTANTIVE CHANGE ----
# ______________________________________________________________________________
tally <- readxl::read_excel("data/comments_tally.xlsx") %>%
  select(-City, -Date, -`Treatment pro-housing comments`,
         -`Comment increase (%)`, -`Pro-housing comment increase (%)`)

color_bar <- function (color = "#49CA69", fun = "proportion", ...)
{
  fun <- match.fun(fun)
  formatter("span", style = function(x) style(display = "inline-block",
                                              `border-radius` = "0px", `padding-right` = "0px",
                                              `background-color` = csscolor(color),
                                              width = percent(fun(as.numeric(gsub(",", "", x)), ...))))
}

perc_scale = function(x) (x/1)
unit_scale = function(x) (x - min(x)) / (100 - min(x))

#### Create Table 1: Examination of public comments in treated council meetings
readxl::read_excel("data//comments_tally.xlsx") %>%
  select(-City, -Date, -`Treatment anti-housing comments`,
         -`Treatment pro-housing comments`,
         -`Comment increase (%)`, -`Pro-housing comment increase (%)`) %>%
  mutate(
    # `Total comments (incl. treatment induced)` = 
    #   color_bar("deepskyblue")(`Total comments (incl. treatment induced)`),
    `Pro-housing comments (not incl. treatment induced)` = 
      color_bar("springgreen", fun = unit_scale)(`Pro-housing comments (not incl. treatment induced)`),
    `Pro-housing comments (incl. treatment-induced)` = 
      color_bar("limegreen", fun = unit_scale)(`Pro-housing comments (incl. treatment-induced)`),
    `Anti-housing comments (incl. treatment-induced)` = 
      color_bar("orange", fun = unit_scale)(`Anti-housing comments (incl. treatment-induced)`)
    ) %>%
  kable("html", escape = F, align = c("l", "l", "l", "l", "l", "l", "l")) %>%
  kable_styling(
    "condensed", full_width = F, html_font = "Cambria", font_size = 18
  ) %>%
  column_spec(1, width = "11em") %>%
  column_spec(2, width = "4em") %>%
  column_spec(3:5, width = "6em") %>%
  row_spec(0, color = "black", bold = T) %>%
  row_spec(1:2, color = "Steel Gray", bold = F) %>%
  row_spec(9, color = "black", bold = T)

