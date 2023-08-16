# council_experiment

Replication code and data for "Countering capture in local politics: Evidence from eight field experiments."

Note that while replication code is available for the creation of the identified renter sample (i.e., merging the voter file with Los Angeles Department of City Planning records of multi- unit housing developments), the full voter file cannot be provided for both legal and ethical reasons. However, all data used in the analyses are available in anonymized form.

### Replication Code (relative path "~/code")

0. Functions.R
- Note: This file does not need to be run as it is loaded by other .R files. 
- This code creates user-written functions that are used throughout the other R scripts. 

1. identify_renters.R
- Imports parcel shapefiles from LA County Socrata API, identifies multi-unit apartment buildings, and cleans address information. 

2. merge_voter_file_renters.R
- Imports the LA County voter file and merges it with LA County parcel data records of multi-unit apartment buildings to identify likely renters in the voter file. 

3. clean_merged_voter_file.R
- Cleans the merged voter file to prepare for random assignment and analysis.

4. conduct_random_assignment.R
- Conducts the random assignment to treatment conditions. Note that random assignment is conducted for all LA County cities regardless of whether or not a city received treatment as random assignment was conducted prior to announcements of council meeting agendas. 

5. outcomes_clean.R 
- Prepares the outcome data for analysis. Note that outcome data was collected via manual matching with meeting minutes and videos, so full replicability is not possible here. Meeting minutes and videos are available upon request.

6. primary_analysis.R
- Calculates the intent-to-treat and complier average causal effects.
- Creates Figure 1: Intent-to-treat effect and complier average causal effect, all cities
- Creates Figure 3: Effect by treatment group, all cities
- Creates Figure A6: Intent-to-treat effect and complier average causal effect, all cities (without covariate adjustment)
- Creates Figure A7: Effect by treatment group, all cities (without covariate adjustment)
- Creates Figure A9: Distribution of outcomes by treatment group (compliers only)
- Creates Table A6: Intent-to-treat effects
- Creates Table A7: Complier average causal effects
- Calculates results described in Table A8: linear hypothesis tests
- Calculates results described in the main text on pp. 9-11. 
- Creates Table A14: ITT and CACE estimates from penalized maximum likelihood 

7. heterogeneous_analysis.R
- Calculates heterogeneous treatment effects by vote history. 
- Creates Figure 4: Complier average causal effects by turnout
- Calculates results described in the main text on p. 12. 

8. meta_analysis.R
- Conducts meta-analysis across each individual city. 
- Creates Figure 2: Meta-analysis of complier average causal effects, by council meeting
- Creates Figure A8: Meta-analysis of complier average causal effects by city, excluding pilot studies
- Creates Table A9: CACEs for each city council meeting
- Creates Table A10: Tabular meta analysis estimates 

9. discussion_analysis.R
- Conducts all analyses in the discussion section of the paper. I.e., estimates for spoken vs. written comments, pro vs. anti housing comments, and custom vs. pre-written comments, as well as Table 1. 

%%%%% Results in the Online Appendix only start here %%%%%

a1. descriptive_stats.R
- Calculates descriptive statistics included in the appendix. 
- Creates Table A1: balance table confirmed renters vs. non-confirmed renters
- Creates Table A2: Balance tables renters with emails vs. those without
- Creates Figure A1: Change in housing net worth by age and income percentile

a2. examine_random_assignment.R
- Examines the random assignments, provides summary statistics, and creates balance tables. 
- Creates Table A3: Covariate balance and difference in means test: treatment vs. placebo
- Creates Table A4: Covariate balance across all treatment groups

a3. differential_compliance.R
- Conducts checks for differential compliance by treatment group. 
- Creates Figure A4: Average treatment effect on email opening, all cities
- Creates Figure A5: Average treatment effect on email opening, by city
- Creates Table A5: Covariate predictiveness of compliance by treatment group 

### Data Files (relative path "~/data")

Replication data:

- vf_clean.R (Anonymized voter file containing only randomly ordered IDs and covariates). 
- random_assignment.R (Anonymized random assignment data containing only random IDs, city, and treatment assigment). 
- pilot_outcomes.csv (Outcomes from pilot studies displayed in meta-analysis). 
- comments.csv (Main outcome data file including treatment group and comment status).
- comments_tally.csv (Data used to create Table 1). 
