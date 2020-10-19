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
    return(as.Date(x, '%m/%d/%Y' ))
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
dim(pat)
dim(enc)
dim(dxs)
