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
    return(parse_date_time(x, orders='mdYIMSp'))
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
dim(pat) ## currently messed up, pipe delim has too many rows. Work on others 1st.
dim(enc)
dim(dxs)

dxs %>%
select(PAT_ID, DX_NAME, CURRENT_ICD10_LIST, CONTACT_DATE) %>%
mutate_at(vars(CONTACT_DATE), ~ chdate(.)) %>%
group_by(PAT_ID) %>%
mutate(first_contact = min(CONTACT_DATE)) %>%
summarise(dm_e     = sum(grepl("diab", enc_dx_name, ignore.case = TRUE)),
	      asthma_e = sum(grepl("asth", enc_dx_name, ignore.case = TRUE)),
	      copd_e   = sum(grepl("copd", enc_dx_name, ignore.case = TRUE)),
	      htn_e    = sum(grepl("hypert", enc_dx_name, ignore.case = TRUE)))

->
dxs_cleaned
