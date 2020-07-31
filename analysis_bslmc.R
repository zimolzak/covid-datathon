library(dplyr)

PATH = "/Users/ajz/Desktop/covid_datathon_git/"
FILE = "COVID_1_SLH.tab"

setwd(PATH)
stluke_lab = read.csv(paste(PATH, FILE,   sep=''), sep="\t", stringsAsFactors = FALSE)

## NOTED_DATE is first date a CC was noted by the provider
## NOTED DATE is specific to Problem List!!
## covid_1_slh is somewhat joined up.
## about 50 pts per TAB file

stluke_lab %>%
filter(PAT_CLASS == "Outpatient") ->
out_only
# What is "outpatient":
# still is really outpatient, outpatient test xray us etc. monitoring etc.
# endoscopy, outpat cath, outpat ed.

table(stluke_lab$ADT_PAT_CLASS_C)
table(stluke_lab$PAT_CLASS)
#   101    102    103    104    106    129 
#422644  12248  54964  23553   5910   5436 
# Very good that Rory mapped adt_pat_class_c to pat_class and also kept both.


######## NOTES

# never been done - TODO -->
# 	6.	inpatient: testing late in admission: rate of this, rate of positives.
# cool to have ER data.

# My process:
# (1) Understand Rory's process; Plus Explore the .tab files & understand.
# (2) Analyses according to thoughts.pdf. Do what's feasible.

## Data Pull Process is as follows:
## took lab for covid
## find all pts who had those lab
## randomize
## took first 150 pat ids
## ran those pat ids over whole hosp record

# Next pull:
# hosp admission disch dx
# pulse ox
# maaaaybe the order date vs result date
# totally okay to NOT join admissions with the problem list.

# Where will datathon people get their data dictionaries? (Gloria has it: need just a BCM ECA to log in/)
# Are people going to want to download and do locally: deident???
