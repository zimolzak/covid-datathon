library(dplyr)
library(ggplot2)
#library(tidyr)

PATH = "/Users/ajz/Desktop/covid_datathon_git/"
FILE = "COVID_1_SLH.tab"
# onedrive --> files / covid-19 / covid_datathon / data / slh_data

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
arrange(PAT_ID, ORDERING_DATE) -> # TODO - maybe should get rid of this arrange(). Maybe select ORDER_PROC_ID too.
lab_no_join

# Thoughts about future stuff to pull:
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
comorb_covid ## A nice and neat table for later

#### explore asthma copd htn
# the code below looks nicer than table()
dx %>%
select(DX_NAME) %>%
group_by(DX_NAME) %>%
summarise(n=n()) %>%
arrange(desc(n)) ->
counts_dx

cat('\n\nexplore----\n')
counts_dx %>% filter(grepl("copd", DX_NAME, ignore.case = TRUE))
counts_dx %>% filter(grepl("obstr", DX_NAME, ignore.case = TRUE)) # not needed
counts_dx %>% filter(grepl("asth", DX_NAME, ignore.case = TRUE))
counts_dx %>% filter(grepl("hypert", DX_NAME, ignore.case = TRUE)) # filter out pulm htn
counts_dx %>% filter(grepl("htn", DX_NAME, ignore.case = TRUE)) # not needed




#### Analyses
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
cat('\n\noutputs----\n')
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

odds_plot = ggplot(df, aes(comorb_name, est)) +
geom_pointrange(aes(ymin = lower, ymax = upper)) +
scale_y_log10() +
ylab('Odds ratio') + ggtitle('Odds of COVID +:- in disease vs. not') +
geom_hline(yintercept = 1)




######## Oxygenation, univariate/descriptive ########

# still TODO -->
# 	* inpatient: testing late in admission: rate of this, rate of positives.
# 	* rate of admissions/ER , by risk factors
#   couple ?s for Rory

#   Highest priorities for future data pulls
# in hospital mortality
# pulse ox <-- Would be good, but wait til further notice. (NB we have CPO order status)
# in absence of SpO2: get ABG & VBG.
#  For future ref: FIO2 needed too for proper interp.
# hosp admission disch dx
# specimen collect time
# EMP_ID and/or SER


# proc_name = "abg" or "blood gas" or...... "venous blood gas" or.......
# NAME all of : {pH, pCO2, pO2, SaO2, FIO2 !!!!!!, L/min !!!!!, HCO3_art, CO2, base_excess}
#               ^^^^ ^^^^  !!!!


# proc_name = blood gas, arterial

cat('\n\nWhat are ABG components----\n')
lab_no_join %>% filter(PROC_NAME == "BLOOD GAS, ARTERIAL") %>% select(NAME) %>% group_by(NAME) %>% summarise(n())

lab_no_join %>%
filter(PROC_NAME == "BLOOD GAS, ARTERIAL", NAME == "FIO2 (BEAKER)") %>%
mutate(FIO2 = as.numeric(ORD_NUM_VALUE)) %>%
select(PAT_ID, ORDERING_DATE, FIO2) ->
fio2s

cat('\n\nSome pts have mult ABGs per date----\n')
fio2s %>%
group_by(PAT_ID, ORDERING_DATE) %>%
summarise(n=n()) %>%
arrange(desc(n))

######## Oxygenation, both pO2 and FIO2 ########
## TODO - this looks like a job for... tidyr! But need to upgrade R.

lab_no_join %>%
filter(PROC_NAME == "BLOOD GAS, ARTERIAL") %>%
head()

# found out order_proc_id is the magic key that you want to group them
stluke_lab %>% filter(PROC_NAME == "BLOOD GAS, ARTERIAL" & (NAME == "FIO2 (BEAKER)" | NAME =='PO2 ARTERIAL (BEAKER)')) %>% select(ORDER_PROC_ID, PAT_ID, ORDERING_DATE, PROC_ID, NAME) %>% unique() %>% head(20)

stluke_lab %>%
filter(NAME == "FIO2 (BEAKER)" | NAME =='PO2 ARTERIAL (BEAKER)') %>%
select(ORDER_PROC_ID, PAT_ID, ORDERING_DATE, NAME, ORD_NUM_VALUE) %>%
distinct() ->
oxygenation

# here is where I usually would do tidyr

oxygenation %>%
filter(NAME == "FIO2 (BEAKER)" ) %>%
mutate(fio2 = as.numeric(ORD_NUM_VALUE)) %>%
select(-ORD_NUM_VALUE, -NAME) ->
fio2_tbj

oxygenation %>%
filter(NAME =='PO2 ARTERIAL (BEAKER)') %>%
mutate(po2 = as.numeric(ORD_NUM_VALUE)) %>%
select(ORDER_PROC_ID, po2) %>%
full_join(fio2_tbj, by = "ORDER_PROC_ID") %>%
mutate(pfr = 100 * po2 / fio2,
	category = case_when(
		pfr < 200 ~ 'ARDS',
		(pfr < 300) & (pfr >= 200) ~ 'ALI',
		pfr >= 300 ~ 'Normal'
	)
) ->
oxy_joined

# todo exclude old abgs. i got 3 from 2014. rest = 2020.
# TODO - How many of the patients in the dataset *lack* ABGs?
# i.e. Get idea of how bad confound by test indication can be.



######## Oxygenation vs COVID test ########

comorb_covid %>%
select(PAT_ID, covid_boolean, dm_boolean, asthma_boolean, copd_boolean) %>%
full_join(oxy_joined) %>%
filter(!is.na(covid_boolean)) -> # todo count how many na
covid_comorb_oxy

ggplot(covid_comorb_oxy, aes(fio2, po2, color=category, shape=covid_boolean)) +
geom_point() ->
ards_scatter_shape

# po2

ggplot(covid_comorb_oxy, aes(covid_boolean, po2)) +
geom_boxplot(outlier.shape = NA) +
geom_jitter(width=0.2) ->
po2_covid_box

wilcox.test(covid_comorb_oxy[covid_comorb_oxy$covid_boolean == FALSE, ]$po2,
			covid_comorb_oxy[covid_comorb_oxy$covid_boolean == TRUE,  ]$po2)
# p-value = 1.187e-05 , no surprise

# pfr

ggplot(covid_comorb_oxy, aes(covid_boolean, pfr)) +
geom_boxplot(outlier.shape = NA) +
geom_jitter(width=0.2) ->
pfr_covid_box

wilcox.test(covid_comorb_oxy[covid_comorb_oxy$covid_boolean == FALSE, ]$pfr,
			covid_comorb_oxy[covid_comorb_oxy$covid_boolean == TRUE,  ]$pfr)
# p-value = 3.161e-05 , also no surprise




pdf("Rplots_inpat.pdf")
odds_plot
ards_scatter_shape
po2_covid_box
pfr_covid_box
dev.off()
