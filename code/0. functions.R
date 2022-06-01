# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 22 September, 2021 by Trevor Incerti

# This file contains author-written functions used throughout the analysis

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FUNCTIONS ----
# ______________________________________________________________________________

##### Age calculation function ##### 
age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

##### Import and append all files from directory ##### 
read_plus <- function(flnm, delim) {
  read_delim(flnm, delim = delim, col_types = cols(.default = "c")) %>% 
    mutate(filename = tools::file_path_sans_ext(fs::path_file(flnm)))
}

read_dir = function(path, extension, delim, filename) {
  
  if (filename == TRUE) {
  list.files(path = path,
             pattern = paste0("*.", extension),
             full.names = T) %>%
    map_df(~read_plus(., delim = delim))
    
  } else {
    
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      map_df(~read_delim(., delim = delim))
  }
}

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ANALYSIS HELPERS ----
# ______________________________________________________________________________

# List of covariates in analyses
covs = ~ gender + english + age + 
  yearbuilt + units + lowrise + mid_highrise + dem + rep + npp +
  vote_2020_general + vote_2017_municipal + vote_2016_general

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# GRAPHIC ELEMENTS ----
# ______________________________________________________________________________

#####  ggplot layers ##### 
gglayers = list(
    geom_point(size = 1.5, position = position_dodge(width = 0.5), color = "steelblue"),
    geom_errorbarh(aes(y = term, xmin = conf.low, xmax = conf.high),
                   position = position_dodge(width = 0.5),
                   color="grey30", size=0.5, alpha = 0.25, height = 0.1),
    geom_vline(xintercept = 0, linetype = "dashed"),
  theme_classic() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size=12)
    ),
  xlab("Change in comments submitted (percentage points)"), 
  ylab("")
)

#### Table meta-parameters ####
treatments_pilot <- c(
  '(Intercept)' = 'Placebo mean',
  'treatment3treatment' = 'Treated',
  'treatment2t1' = 'Instructions-only treatment',
  'treatment2t2' = 'Message treatment',
  'treatmentt1' = 'Instructions-only treatment',
  'treatmentt2a' = 'Economist message',
  'treatmentt2b' = 'NGO message',
  'treatmentt2c' = 'Community leader message'
)

treatments <- c(
  '(Intercept)' = 'Constant',
  'treatedTreatment' = 'Treated',
  'treatmentTreatment 1' = 'Instructions-only treatment',
  'treatmentTreatment 2' = 'Economic cost treatment',
  'treatmentTreatment 3' = 'Costly abstention treatment'

)

omit <- 'RMSE|estimate|Std.Errors|DF|R2|AIC|BIC|se_type|p.value.endogeneity|p.value.weakinst|statistic.endogeneity|statistic.endogeneity|statistic.weakinst|p.value.overid|statistic.overid'
