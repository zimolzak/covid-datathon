library(dplyr)
library(ggplot2)

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

# Emergency 
# Hospital Outpatient Surgery
# Inpatient             Inpatient Rehab                 Observation                  Outpatient 


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

stluke_lab %>%
select(PAT_ID, ORDERING_DATE, PROC_NAME, NAME, ORD_VALUE, ORD_NUM_VALUE, PAT_CLASS) %>%
filter(ORD_VALUE != "NULL" | ORD_NUM_VALUE != "NULL") %>%
distinct() %>%
arrange(PAT_ID, ORDERING_DATE) ->
lab_no_join

# people who touch chart
# specimen collect TIME not just date
# mortality !! pat_enc_hosp, also "care name"
# emp id versus "ser" which is about role / job description




#### 2020-08-07 ####

stluke_lab %>%
select(PAT_ID, DX_NAME, CURRENT_ICD10_LIST) %>%
distinct() %>%
arrange(PAT_ID, CURRENT_ICD10_LIST) ->
dx

# NOTED_DATE is sometimes == "NULL"
# but nonetheless, later we should exclude ones that are to recent
# ^^^^^ TODO

dx %>% count(PAT_ID) %>% arrange(desc(n)) # output - who's "sickest"

dx %>%
group_by(PAT_ID) %>%
summarise(dm = sum(grepl("diab", DX_NAME, ignore.case = TRUE))) ->
has_dm

#table(cov_lab$ORD_VALUE)
#    Detected     Negative Not Detected     Positive
#           6           27           24            3

lab_no_join %>%
filter(grepl("cov", NAME, ignore.case = TRUE) & ! grepl("performing", NAME, ignore.case = TRUE)) %>%
select(-ORD_NUM_VALUE, -PROC_NAME) %>%
mutate(cov_result = case_when(
	ORD_VALUE == "Negative" | ORD_VALUE == "Not Detected" ~ 0,
	ORD_VALUE == "Positive" | ORD_VALUE == "Detected" ~ 1
)) ->
cov_lab_tall
# 60 rows for about 50 pts, if we use "tall"

# Inspect these because we're about to lose them in a summarise()
table(cov_lab_tall$NAME) # yes all 60 are the same
table(cov_lab_tall$PAT_CLASS) # ER 9, inp 33, obs 3, outp 15

cov_lab_tall %>%
group_by(PAT_ID) %>%
summarise(n_pos = sum(cov_result), n_done = n()) %>%
mutate (proportion_pos = n_pos / n_done) %>%
arrange(desc(proportion_pos)) ->
cov_lab
# only one has P = 0.5

#### finally join diagnoses with covid results!

#> dim(cov_lab)
#[1] 45  4
#> dim(has_dm)
#[1] 49  2
# TODO why is this

has_dm %>%
full_join(cov_lab) %>%
mutate(covid_boolean = (proportion_pos > 0),
	dm_boolean = (dm>0))->
dm_by_covid

dm_cov_table = with(dm_by_covid, table(covid_boolean, dm_boolean))
dm_cov_table
# 4:5 ratio diabetics had + covid
# 4:32 ratio non-dm had + covid

# trivial stats for the sake of stats

F = fisher.test(dm_cov_table)
F
# p-value = 0.03886
# OR =   6.040232 (0.8439568 45.8211413)
c(F$conf.int[1], F$estimate, F$conf.int[2])

df = data.frame(est = F$estimate,
	lower = F$conf.int[1],
	upper = F$conf.int[2],
	test_num = factor(c(1))
)

p = ggplot(df, aes(test_num, est))
p + geom_pointrange(aes(ymin = lower, ymax = upper)) + scale_y_log10()
