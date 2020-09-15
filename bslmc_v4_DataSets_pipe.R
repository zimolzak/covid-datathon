library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

PATH = "/Users/ajz/Desktop/aa-git/covid_datathon/DataSets/" # end with slash
SETWDPATH = "/Users/ajz/Desktop/aa-git/covid_datathon/"

setwd(SETWDPATH)

prl = c('PROBLEM/PROBLEM_GRP_3 20200814 1855.csv',
    'PROBLEM/PROBLEM_GRP_2 20200814 1854.csv',
    'PROBLEM/PROBLEM_GRP_1 20200814 1853.csv')
ptl = c('PAT_ID_2/PAT_ID_GRP_2 20200814 1407.csv',
    'PAT_ID_2/PAT_ID_GRP_1 20200814 1359.csv',
    'PAT_ID_2/PAT_ID_GRP_3 20200814 1506.csv')
orl = c('ORDER_RESULTS/ORDER_RESULTS_GRP_3 20200814 1840.csv',
    'ORDER_RESULTS/ORD_RSLTS_GRP_1 20200814 1805.csv',
    'ORDER_RESULTS/ORDER_RESULTS_GRP_2.csv')
hsl = c('HSP/HSP_GRP_2 20200814 2207.csv',
    'HSP/HSP_GRP_1 20200814 2205.csv',
    'HSP/HSP_GRP_3 20200814 2208.csv')
enl = c('ENC_DX/ENC_DX_GRP_2 20200814 1710.csv',
    'ENC_DX/ENC_DX_GRP_1 20200814 1604.csv',
    'ENC_DX/ENC_DX_GRP_3 20200814 1713.csv')

# onedrive --> files / covid-19 / covid_datathon / data / DataSets / DATA_HSP_LAB_PROC

list2df <- function(L) {
    # todo - check about length & class of L
    d = read.csv(paste(PATH, L[1], sep=''), sep="|",
        stringsAsFactors = FALSE, na.strings="null")
    for (i in seq(2,length(L))) {
        di = read.csv(paste(PATH, L[i], sep=''), sep="|",
	        stringsAsFactors = FALSE, na.strings="null")
        d %>% bind_rows(di) -> d
    }
    return(d)
}

chdate = function(x) {
    return(as.Date(x, '%m/%d/%Y' ))
}

chtime = function(x){
    return(parse_date_time(x, orders='mdYIMSp'))
}

prob = list2df(prl) %>%
    select(-PROBLEM_LIST_ID, -DX_ID, -CHRONIC_YN) %>%
    mutate_at(vars(NOTED_DATE), ~ chdate(.))
pat = list2df(ptl) %>%
    select(-IDENTITY_ID) %>%
    mutate_at(vars(BIRTH_DATE, DEATH_DATE), ~ chdate(.)) %>%
    mutate(age = as.numeric(as.Date('2020-08-30', '%Y-%m-%d') - BIRTH_DATE) / 365)
ord = list2df(orl) %>%
    select(-PROC_ID, -LAB_STATUS_C, -COMPONENT_ID) %>%
    mutate_at(vars(ORDERING_DATE, RESULT_DATE), ~ chdate(.)) %>%
    mutate(latency = RESULT_DATE - ORDERING_DATE)
hosp = list2df(hsl) %>%
    select(-IDENTITY_ID, -IDENTITY_TYPE_ID, -ID_TYPE_NAME,
        -HSP_ACCOUNT_ID, -BIRTH_DATE, -ETHNIC_GROUP_C, -DEATH_DATE,
        -SEX_C, -ADT_PAT_CLASS_C, -LEVEL_OF_CARE_C, -ADMIT_SOURCE_C,
        -PATIENT_RACE_C) %>%
    mutate_at(vars(HOSP_ADMSN_TIME, HOSP_DISCH_TIME), ~  chtime(.)) %>%
    mutate(los = difftime(HOSP_DISCH_TIME, HOSP_ADMSN_TIME, units="days"))
enc = list2df(enl) %>%
    select(-ENC_TYPE_C, -enc_dx_id)

cat('\nDims of prob, pat, ord, hosp, enc----\n')
dim(prob)
dim(pat)
dim(ord)  # 181907     13
dim(hosp) # 2226 x 33
dim(enc)  # 2068 x 9

#### Hosp basic tables

cat('\nHospitalization fields----\n')
table(hosp$ETHNIC_GROUP)
cat('\nrace----\n')
table(hosp$PAT_RACE)
cat('\nclass----\n')
table(hosp$PAT_CLASS)
cat('\ndisp----\n')
table(hosp$DISCHARGE_DISP)
cat('\nlvl----\n')
table(hosp$LVL_OF_CARE)
#          (null)    Critical Care Medical/Surgical Progressive Care        Telemetry 
#            2063               19               91                6               47 
table(hosp$ID_TYPE_NAME)
table(hosp$NAME)
cat('\nCrosstab Pt Class vs. Disposition----\n')
table(hosp$PAT_CLASS, hosp$DISCHARGE_DISP)

hosp %>%
select(PAT_ID, ZIP, ETHNIC_GROUP, NAME, PAT_RACE) %>%
distinct() ->
pt_data_fr_hosp

hosp %>%
select(-ZIP, -ETHNIC_GROUP, -NAME, -PAT_RACE) ->
hosp # destructive. Don't re-run interactive.

cat('\nInspect these rows. We are about to discard some.----\n')
pt_data_fr_hosp %>%
count(PAT_ID) %>%
filter(n>1) %>%
left_join(pt_data_fr_hosp)

pt_data_fr_hosp %>%
group_by(PAT_ID) %>%
summarise_all(list(first)) ->
pt_data_fr_hosp_uniq

cat("dims of unjoined pt data----\n")
dim(pt_data_fr_hosp)
# [1] 148   5
dim(pt_data_fr_hosp_uniq)
# 146 x 5

pat %>%
full_join(pt_data_fr_hosp_uniq) %>%
arrange(PAT_ID) ->
pat # destructive. Don't re-run interactive.




#### Comorbs

prob %>% #copy paste works
group_by(PAT_ID) %>%
summarise(dm_p     = sum(grepl("diab", DX_NAME, ignore.case = TRUE)),
	      asthma_p = sum(grepl("asth", DX_NAME, ignore.case = TRUE)),
	      copd_p   = sum(grepl("copd", DX_NAME, ignore.case = TRUE)),
	      htn_p    = sum(grepl("hypert", DX_NAME, ignore.case = TRUE))) ->
comorb_count_p

# pat has birth date and death date. Death date is a character.
# hosp - no diagnoses?

enc %>%
group_by(PAT_ID) %>% ## REUSE
summarise(dm_e     = sum(grepl("diab", enc_dx_name, ignore.case = TRUE)),
	      asthma_e = sum(grepl("asth", enc_dx_name, ignore.case = TRUE)),
	      copd_e   = sum(grepl("copd", enc_dx_name, ignore.case = TRUE)),
	      htn_e    = sum(grepl("hypert", enc_dx_name, ignore.case = TRUE))) ->
comorb_count_e

comorb_count_p %>%
full_join(comorb_count_e, by = "PAT_ID") ->
nas

nas[is.na(nas)] <- 0

nas %>%
mutate(dm = dm_p+dm_e > 0,
	copd = copd_p+copd_e > 0,
	asthma = asthma_p+asthma_e > 0,
	htn = htn_p+htn_e > 0) %>%
full_join(pat) ->
onept

ggplot(onept, aes(dm, age)) +
    geom_boxplot(outlier.shape = NA, notch = TRUE) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC') ->
    dmage

ggplot(onept, aes(htn, age)) +
    geom_boxplot(outlier.shape = NA, notch = TRUE) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC') ->
    htnage

ggplot(onept, aes(asthma, age)) +
    geom_boxplot(outlier.shape = NA, notch = TRUE) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC') ->
    astage

ggplot(onept, aes(copd, age)) +
    geom_boxplot(outlier.shape = NA, notch = TRUE) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC') ->
    copdage

cat("deceased----\n")
onept %>% filter(DEATH_DATE > 0)




#### COVID tests

ord %>%
filter(grepl("cov", PROC_NAME, ignore.case = TRUE) & LAB_STATUS == 'Final') %>%
select(-ORD_NUM_VALUE) %>%     # drop this field early because it breaks spread()
mutate(COMPONENT = case_when(COMPONENT == "SARS-COV-2 PERFORMING LAB" ~ 'performing_lab',
    COMPONENT == "SARS-COV2/RT-PCR" ~ 'raw_result')) %>%
mutate(ORD_VALUE = case_when(is.na(ORD_VALUE) ~ 'unknown', TRUE ~ ORD_VALUE)) %>%
spread(COMPONENT, ORD_VALUE) %>%
mutate(cov_result = case_when(
	raw_result == "Negative" | raw_result == "Not Detected" ~ 0,
	raw_result == "Positive" | raw_result == "Detected" ~ 1
)) %>%
select(PAT_ID, PAT_ENC_CSN_ID, ORDERING_DATE, RESULT_DATE, performing_lab, cov_result, latency, raw_result) ->
covids

cat('\nCOVID raw results----\n')
table(covids$raw_result)
#     Detected     Negative Not Detected     Positive 
#           14           79           70            8 

cat('\nCOVID performing lab----\n')
table(covids$performing_lab)
#BCM Resp Virus Lab              BSLMC                CPL               SLWH 
#                 2                 74                 66                 21 

#plot covid tests by date
ggplot(covids, aes(x=ORDERING_DATE, color=as.factor(cov_result))) +
    geom_freqpoly(binwidth=7) +
    labs(title="COVID result by date", x='Order date', y='Count', color="Result", subtitle='Inpatient') ->
    posnegdate

qplot(x=ORDERING_DATE, y=latency, color=performing_lab, data=covids) +
    labs(title="Latency of COVID test by date", x='Order date', y='Latency (days)', subtitle='Inpatient') ->
    latency_date

ggplot(data=covids, aes(x=ORDERING_DATE, y=latency)) +
    geom_line() +
    geom_point(shape=3, size=1, alpha = 0.5) +
	facet_grid(rows = vars(performing_lab)) ->
	facetlatency

covids %>%
group_by(PAT_ID) %>%
summarise(positives = sum(cov_result), n = n(), first_test_ordered = min(ORDERING_DATE)) %>%
mutate(proportion_pos = positives / n) %>%
full_join(onept, by='PAT_ID') %>%
mutate(died = case_when(is.na(DEATH_DATE) ~ FALSE, TRUE ~ TRUE)) ->
onept # destructive

cat('\nProportion of COVID tests positive----\n')
table(onept$proportion_pos)

ggplot(onept, aes(x=n)) +
    geom_histogram(binwidth=1) +
    labs(title="Number of COVID tests per patient", x='Tests per patient', y='Count', subtitle='Inpatient') ->
    ntests

ggplot(onept, aes(died, age)) +
    geom_boxplot(outlier.shape = NA, notch = TRUE) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC') ->
    agevsdied

ggplot(onept, aes(NAME, age)) +
    geom_boxplot(outlier.shape = NA, notch = TRUE) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC', x = 'Sex') ->
    agevssex





#### Mortality analysis

with(onept, table(proportion_pos, died))
# Doh, no deaths in the covid group.
# Better pick a different outcome.

onept %>%
rename(sex = NAME) %>% # todo move the rename upstream
filter(proportion_pos > 0) ->
covid_pts

cat('\nCrude tables of categoricals vs. mortality----\n')
with(covid_pts, table(dm, died))
with(covid_pts, table(copd, died))
with(covid_pts, table(asthma, died))
with(covid_pts, table(htn, died))
with(covid_pts, table(sex, died))




#### New outcome, limit to ER ####

hosp %>%
filter(PAT_CLASS == 'Emergency') %>%
select(PAT_ID, SOURCE_OF_ADMISSION, HOSP_ADMSN_TIME, DISCHARGE_DISP, DISCH_DISP_C, los) %>%
mutate(dispo = case_when(DISCH_DISP_C == 1 ~ 'out',
	DISCH_DISP_C == 2 ~ 'in-short',
	DISCH_DISP_C == 4 ~ 'out', # intermediate care facil
	DISCH_DISP_C == 7 ~ 'left',
	DISCH_DISP_C == 9 ~ 'in-admit',
	DISCH_DISP_C == 64 ~ 'out', # nurs facil
	DISCH_DISP_C == 70 ~ 'out', # another hcare inst
	DISCH_DISP_C == 100 ~ 'left', # never arr
	DISCH_DISP_C == 300 ~ 'left',  #left p/ triage
	DISCH_DISP_C == 500 ~ 'left', # triage after test: ??
	DISCH_DISP_C == 600 ~ 'left')) %>%
mutate(admitted = case_when(dispo == 'in-short' ~ TRUE,
    dispo == 'in-admit' ~ TRUE,
    TRUE ~ FALSE))->  # LWBS
er

cat('\nER visits, count by pat id----\n')
table(er$PAT_ID)
cat('\nsource----\n')
table(er$SOURCE_OF_ADMISSION)
cat('\ndisposition----\n')
table(er$DISCHARGE_DISP)
cat('\ndispo codes----\n')
with(er, table(DISCHARGE_DISP, DISCH_DISP_C))
cat('\ngrouped dispo----\n')
with(er, table(dispo))

# TODO - function to replace all this cat('\n----\n') nonsense

qplot(er$los * 24) +
    labs(title='ER duration of stay', x='Duration (hours)', y='Count (ER visits)') ->
    erlos

er %>%
group_by(PAT_ID) %>%
summarise(n_er_visits = n(),
	proportion_admitted = mean(admitted),
	first_er_vis = min(HOSP_ADMSN_TIME),
	max_los = max(los),
	n_admissions = sum(admitted)) %>%
mutate(ever_admitted = (proportion_admitted > 0)) %>%
full_join(onept) ->
onept_join_er

with(onept_join_er, table(ever_admitted))

qplot(onept_join_er$n_er_visits) +
    labs(title='ER utilization per patient', x='Number of ER visits', y = 'Count of patients') ->
    ervisperpt

qplot(onept_join_er$n_admissions) +
    labs(title='ER admisisons per patient', x='Number of admits from ER', y = 'Count of patients') ->
    eradmperpt

qplot(onept_join_er$proportion_admitted) +
    labs(title='Distribution of admisison rate',
    x='Proportion of ER visits w/ admission', y='Count of patients') ->
    admrate

cat('\n-Doh, again covid pos patients have only one outcome (all disch, none admit).---\n')
with(onept_join_er, table(proportion_pos, ever_admitted))

onept_join_er %>%
rename(sex = NAME) %>% # todo move the rename upstream
filter(proportion_pos > 0) ->
covid_pts # clobbers old version

cat('\nCrude tables of categoricals vs. admission----\n')
with(covid_pts, table(dm, ever_admitted))
with(covid_pts, table(copd, ever_admitted))
with(covid_pts, table(asthma, ever_admitted))
with(covid_pts, table(htn, ever_admitted))
with(covid_pts, table(sex, ever_admitted))

cat('\nWhat do the admitted pts look like?----\n')
onept_join_er %>%
filter(ever_admitted) %>%
select(-ends_with('_p'), -ends_with('_e'), -ends_with('_DATE')) %>%
print(width = Inf)

#
#
#
#

cat('\n\n----\n\nEnd of text output. Now plotting.\n\n')
pdf("Rplots_inpat_v4.pdf")
dmage
htnage
astage
copdage
latency_date
facetlatency
posnegdate
ntests
agevsdied
agevssex
erlos
ervisperpt
eradmperpt
admrate
dev.off()
