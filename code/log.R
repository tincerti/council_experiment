#### Generate log files ####

# 6. heterogeneous_analysis.R
sink("./log6.txt")
source("code/6. primary_analysis.R",echo=T, max.deparse.length=1e3)
sink()

# 7. heterogeneous_analysis.R
sink("./log7.txt")
source("code/7. heterogeneous_analysis.R",echo=T, max.deparse.length=1e3)
sink()

# Files 8-A3
sink("./log.txt")
source("code/8. meta_analysis.R",echo=T, max.deparse.length=1e3)
rm(list = ls())
source("code/9. discussion_analysis.R",echo=T, max.deparse.length=1e3)
rm(list = ls())
source("code/a1. descriptive_stats.R",echo=T, max.deparse.length=1e3)
rm(list = ls())
source("code/a2. examine_random_assignments.R",echo=T, max.deparse.length=1e3)
rm(list = ls())
source("code/a3. differential_compliance.R",echo=T, max.deparse.length=1e3)
sink()

# Script A4 (for some reason crashes when run in single log file)
sink("./logA4.txt")
source("code/a4. bayes.R",echo=T, max.deparse.length=1e3)
sink()