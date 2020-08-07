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
# Inpatient
# Inpatient Rehab
# Observation
# Outpatient

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
#### Analyze problem list diagnoses ####

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
summarise(dm     = sum(grepl("diab", DX_NAME, ignore.case = TRUE)),
	      asthma = sum(grepl("asth", DX_NAME, ignore.case = TRUE)),
	      copd   = sum(grepl("copd", DX_NAME, ignore.case = TRUE)),) ->
comorb_count

# TODO - hypertension


#### filter & standardize covid results

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
#> dim(comorb_count)
#[1] 49  2
# TODO why is this: some pts w/o covid test??

comorb_count %>%
full_join(cov_lab) %>%
mutate(covid_boolean = (proportion_pos > 0),
       dm_boolean = (dm>0),
       asthma_boolean = (asthma>0),
       copd_boolean = (copd>0)
)->
comorb_covid



#### explore asthma copd htn
# the code below looks nicer than table()
dx %>%
select(DX_NAME) %>%
group_by(DX_NAME) %>%
summarise(n=n()) %>%
arrange(desc(n)) ->
counts_dx

cat('explore:')
counts_dx %>% filter(grepl("copd", DX_NAME, ignore.case = TRUE))
counts_dx %>% filter(grepl("obstr", DX_NAME, ignore.case = TRUE)) # not needed
counts_dx %>% filter(grepl("asth", DX_NAME, ignore.case = TRUE))
counts_dx %>% filter(grepl("hypert", DX_NAME, ignore.case = TRUE)) # filter out pulm htn
counts_dx %>% filter(grepl("htn", DX_NAME, ignore.case = TRUE)) # not needed

# 2 x 2 tables

dm_cov_table = with(comorb_covid, table(covid_boolean, dm_boolean))
# 4:5 ratio diabetics had + covid
# 4:32 ratio non-dm had + covid

Fd = fisher.test(dm_cov_table)
# p-value = 0.03886
# OR =   6.040232 (0.8439568 45.8211413)
asth_cov_table = with(comorb_covid, table(covid_boolean, asthma_boolean))
Fa = fisher.test(asth_cov_table)
# -ast= 36:7, +ast= 1:1
copd_cov_table = with(comorb_covid, table(covid_boolean, copd_boolean))
Fc = fisher.test(copd_cov_table)
# -copd= 34:8, +copd= 3:0

####print stuff
cat('outputs:')
dm_cov_table
Fd
asth_cov_table
Fa
copd_cov_table
Fc



#### plots
# fishers = rbind(Fd, Fa, Fc) # matrix
fishers = list(Fd, Fa, Fc) # for some reason, need [[1]] ??

df = data.frame()

for (i in seq(3)){
	temp = data.frame(est = fishers[i][[1]]$estimate,
		lower = fishers[i][[1]]$conf.int[1],
		upper = fishers[i][[1]]$conf.int[2],
		comorb_name = fishers[i][[1]]$data.name
	)
	df = rbind(df, temp)
}




odds_plot = ggplot(fishers, aes(data.name, estimate)) +
geom_pointrange(aes(ymin = conf.int[1], ymax = conf.int[2])) +
scale_y_log10() +
ylab('Odds ratio') + xlab('') + ggtitle('Odds of COVID +:- in diabetes vs. not')

ggsave("Rplots_bslmc.pdf", odds_plot)


