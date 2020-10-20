library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(here)

####

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

####

INTERACTIVE = FALSE

if(INTERACTIVE) {
	pat = df_interactive_mode('PATIENT_DEMOGRAPHICS.DATA')
	enc = df_interactive_mode('ENCOUNTER.DATA')
	dxs = df_interactive_mode('DX_ENCOUNTER.DATA')
} else {
	pat = filename2df('PATIENT_DEMOGRAPHICS.DATA')
	enc = filename2df('ENCOUNTER.DATA')
	dxs = filename2df('DX_ENCOUNTER.DATA')
}

#prob = list2df(prl) %>%
#    select(-PROBLEM_LIST_ID, -DX_ID, -CHRONIC_YN) %>%
#    mutate_at(vars(NOTED_DATE), ~ chdate(.))

say('Dimensions of pat, enc, dxs')
dim(pat) # 1900 x 14
dim(enc) # 13943    15
dim(dxs) # 44816     9

dxs %>%
select(PAT_ID, DX_NAME, CURRENT_ICD10_LIST, CONTACT_DATE, PAT_ENC_CSN_ID) %>%
mutate_at(vars(CONTACT_DATE), ~ chdate(.)) ->
dxs_cleaned

enc %>%
select(PAT_ID, PAT_ENC_CSN_ID, CONTACT_DATE, PATIENT_CLASS,
    HOSP_ADMSN_TIME, HOSP_DISCH_TIME, LVL_OF_CARE, DISCHARGE_DISP,
    ACUITY_LEVEL_C, ADMIT_SOURCE_C) %>%
mutate_at(vars(CONTACT_DATE), ~ chdate(.)) %>%
mutate_at(vars(HOSP_ADMSN_TIME, HOSP_DISCH_TIME), ~ chtime(.)) ->
enc_cleaned





dxs_cleaned %>%
group_by(PAT_ID) %>%
mutate() %>%
summarise(comor.first.vis = min(CONTACT_DATE),
    comor.diab.nvis = sum(grepl("diab", DX_NAME, ignore.case = TRUE)),
    comor.asth.nvis = sum(grepl("asth", DX_NAME, ignore.case = TRUE)),
    comor.copd.nvis = sum(grepl("copd", DX_NAME, ignore.case = TRUE)),
    comor.hypert.nvis = sum(grepl("hypert", DX_NAME, ignore.case = TRUE))
    ) ->
dxs_processed

dxs_cleaned %>%
filter(grepl("covid", DX_NAME, ignore.case = TRUE)) %>%
group_by(PAT_ID) %>%
summarise(covid.first.vis = min(CONTACT_DATE)) ->
covid_dx_dates

# enc_cleaned %>% filter(PATIENT_CLASS == 'Emergency')
