library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(here)




#### Functions

filename2df <- function(fn) {
    df = read.csv(here("data", "2020-10-19", fn), sep="|",
        stringsAsFactors = FALSE, na.strings="null")
    return(df)
}

df_interactive_mode = function(fn) {
	# crummy function for using R Console
	abs_path = '/Users/ajz/Desktop/aa-git/covid-datathon/data/2020-10-19/'
    df = read.csv(paste(abs_path, fn, sep=''), sep="|",
        stringsAsFactors = FALSE, na.strings="null")
    return(df)
}

chdate = function(x) {
    return(as.Date(x, '%Y-%m-%d' ))
}

chtime = function(x){
    return(parse_date_time(x, orders='ymdHMS')) # lubridate
    # mdYIMSp = mo day yr Hourdecimal min sec am/pm
}

say = function(s) {
	sL = c('\n', s, '----\n')
	cat(paste(sL, collapse=''))
}




#### Loading

INTERACTIVE = FALSE

if(INTERACTIVE) {
	pat = df_interactive_mode('PATIENT_DEMOGRAPHICS.DATA')
	enc = df_interactive_mode('ENCOUNTER.DATA')
	dxs = df_interactive_mode('DX_ENCOUNTER.DATA')
	prb = df_interactive_mode('PROBLEMS_LIST.DATA')
	lab = df_interactive_mode('LAB_RESULTS.DATA') # 1.77 GB, ~55 seconds to load
} else {
	pat = filename2df('PATIENT_DEMOGRAPHICS.DATA')
	enc = filename2df('ENCOUNTER.DATA')
	dxs = filename2df('DX_ENCOUNTER.DATA')
	prb = filename2df('PROBLEMS_LIST.DATA')
	message(paste('Reading labs...', now()))
	lab = filename2df('LAB_RESULTS.DATA')
	message(paste('done.', now()))
}

#prob = list2df(prl) %>%
#    select(-PROBLEM_LIST_ID, -DX_ID, -CHRONIC_YN) %>%
#    mutate_at(vars(NOTED_DATE), ~ chdate(.))

say('Dimensions of pat, enc, dxs, prb, lab')
dim(pat) # 1900 x 14
dim(enc) # 13943    15
dim(dxs) # 44816     9
dim(prb) # 16628     7
dim(lab) # 8490824      17




#### Cleaning (of dates, etc)

dxs %>%
select(PAT_ID, DX_NAME, CURRENT_ICD10_LIST, CONTACT_DATE, PAT_ENC_CSN_ID) %>%
mutate_at(vars(CONTACT_DATE), ~ chdate(.)) ->
dxs_cleaned

enc %>%
select(PAT_ID, PAT_ENC_CSN_ID, CONTACT_DATE, PATIENT_CLASS,
    HOSP_ADMSN_TIME, HOSP_DISCH_TIME, LVL_OF_CARE, DISCHARGE_DISP,
    ACUITY_LEVEL_C, ADMIT_SOURCE_C) %>%
mutate_at(vars(CONTACT_DATE), ~ chdate(.)) %>%
mutate_at(vars(HOSP_ADMSN_TIME, HOSP_DISCH_TIME), ~ chtime(.)) %>%
mutate(los.days = difftime(HOSP_DISCH_TIME, HOSP_ADMSN_TIME, units="days")) ->
enc_cleaned

lab %>% # todo - lab table looks really really weird, may be a problem.
select(PAT_ID, RESULT_TIME, COMPONENT, ORD_VALUE) %>%
distinct() %>%
mutate_at(vars(RESULT_TIME), ~ chtime(.)) ->
lab_distinct

say('Dim of lab_distinct')
dim(lab_distinct)

lab %>%
select(PAT_ID, ORDERING_DATE, PROC_NAME, PROC_CODE) %>%
distinct() %>%
filter(! grepl("LAB", PROC_CODE)) ->
proc_distinct # probably won't use this yet. Need chdate() if we do.

say('Procedure names')
table(proc_distinct$PROC_NAME)

pat %>%
select(PAT_ID, Age, ETHNIC_GROUP, PATIENT_RACE, DEATH_DATE, SEX_C) %>%
mutate_at(vars(DEATH_DATE), ~ chdate(.)) %>%
mutate(sex = case_when(SEX_C == 1 ~ 'F', SEX_C == 2 ~ 'M')) ->
pat_cleaned




#### Constructing new things

dxs_cleaned %>%
group_by(PAT_ID) %>%
summarise(comor.first.vis = min(CONTACT_DATE),
    comor.diab.nvis = sum(grepl("diab", DX_NAME, ignore.case = TRUE)),
    comor.asth.nvis = sum(grepl("asth", DX_NAME, ignore.case = TRUE)),
    comor.copd.nvis = sum(grepl("copd", DX_NAME, ignore.case = TRUE)),
    comor.hypert.nvis = sum(grepl("hypert", DX_NAME, ignore.case = TRUE))
    ) ->
dxs_processed

prb %>%
group_by(PAT_ID) %>%
summarise(comor.diab.probl = sum(grepl("diab", DX_NAME, ignore.case = TRUE)),
    comor.asth.probl = sum(grepl("asth", DX_NAME, ignore.case = TRUE)),
    comor.copd.probl = sum(grepl("copd", DX_NAME, ignore.case = TRUE)),
    comor.hypert.probl = sum(grepl("hypert", DX_NAME, ignore.case = TRUE))
    ) ->
prb_processed

dxs_cleaned %>%
filter(grepl("covid", DX_NAME, ignore.case = TRUE)) %>%
group_by(PAT_ID) %>%
summarise(covid.first.vis = min(CONTACT_DATE)) ->
covid_dx_dates

# todo - this is probably safer than the above:
# filter(grepl("U07.1", CURRENT_ICD10_LIST, ignore.case = TRUE))

enc_cleaned %>%
filter(PATIENT_CLASS == 'Emergency', CONTACT_DATE > chdate('2020-01-01')) ->
er2020

enc_cleaned %>%
mutate(los.days.n = as.numeric(los.days)) %>%
filter(PATIENT_CLASS == 'Inpatient', CONTACT_DATE > chdate('2020-01-01')) ->
inpat2020



# todo - next step - find which Inpatient stays are for COVID.
# join inpat2020 and dxs on PAT_ENC_CSN_ID



#### diagnostics
say('ER discharge disposition')
table(er2020$DISCHARGE_DISP)

table(pat_cleaned$ETHNIC_GROUP)
table(pat_cleaned$PATIENT_RACE)
with(pat_cleaned, table(PATIENT_RACE, ETHNIC_GROUP))

ggplot(pat_cleaned, aes(x = DEATH_DATE)) +
geom_histogram() ->
death_histogram

qplot(inpat2020$los.days.n) +
scale_x_log10() ->
los_histogram



#### plots

say('\n\n----\n\nEnd of text output. Now plotting.')
pdf(here("outputs", "Rplots_v6.pdf"))
death_histogram
los_histogram
dev.off()

# ggsave png here if needed
