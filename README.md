# council_experiment

Replication code and data for "Countering capture in local politics: Evidence from eight field experiments."

Note that while replication code is available for the creation of the identified renter sample (i.e., merging the voter file with Los Angeles Department of City Planning records of multi- unit housing developments), the full voter file cannot be provided for both legal and ethical reasons. However, the data output from scripts 1-5 are available in anonymized form and can be used to replicate all analyses by running scripts 6-9 and a1-a4.

Code run using R version 4.3.0 (2023-04-21) on macOS Ventura 13.4. 

### Replication Code (relative path "~/code")

0. Functions.R
- Creates user-written functions that are used throughout the other R scripts. 
- NOTE: This file does not need to be run as it is loaded by the other .R files. 

1. identify_renters.R
- Imports parcel shapefiles from LA County Socrata API, identifies multi-unit apartment buildings, and cleans address information. 
- Note that running this file requires an LA County Socrata username and password. 

2. merge_voter_file_renters.R
- Imports the LA County voter file and merges it with LA County parcel data records of multi-unit apartment buildings to identify likely renters in the voter file. 
- NOTE: This script cannot be run without the original voter file as it requires unique name and address information. 

3. clean_merged_voter_file.R
- Cleans the merged voter file to prepare for random assignment and analysis (e.g., renames and recodes variables for ease of interpretation).
- NOTE: This script cannot be run without the original voter file as it requires unique name, address, and email address information. 

4. conduct_random_assignment.R
- Conducts the random assignment to treatment conditions. 
- Note that random assignment is conducted for all LA County cities regardless of whether or not a city received treatment as random assignment was conducted prior to announcements of council meeting agendas. 
- NOTE: This script cannot be run without the original voter file as it requires unique name, address, and email address information. However, an anonymized version of the random assignment output is available as "random_assignment.Rdata".

5. outcomes_clean.R 
- Prepares the outcome data for analysis. Note that outcome data was collected via manual matching with meeting minutes and videos. Meeting minutes and videos are available upon request.
- NOTE: This script cannot be run without the original voter file as it requires unique name, address, and email address information. However, an anonymized version of the outcome of the script is available as "vf_clean.RDS".

6. primary_analysis.R
- NOTE: ALL ANALYSES CAN BE REPLICATED FROM THIS POINT USING ANONYMIZED DATA.
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

a4. bayes. R
- Implements the Bayesian analysis referenced in the main text
- Calculates evidence ratios referenced on p. 11. 
- Creates Figure A10: Bayesian multilevel model: coefficient estimates and posterior distributions 
- Creates Figure A11: Posterior distributions of costly abstention treatment, instructions only treatment, and difference. 

### Data Files (relative path "~/data")

Replication data:

- vf_clean.RDS (Anonymized voter file containing only randomly ordered IDs and covariates). 
- random_assignment.Rdata (Anonymized random assignment data containing only random IDs, city, and treatment assignment). 
- pilot_outcomes.csv (Outcomes from pilot studies displayed in meta-analysis). 
- comments.csv (Main outcome data file including treatment group and comment status).
- comments_tally.csv (Data used to create Table 1). 
