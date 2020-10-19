library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(here)

####

filename2df <- function(fn) {
    df = read.csv(here("data", "2020-10-19", fn), sep=",",
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

pat = filename2df('PATIENT_DEMOGRAPHICS.csv')
enc = filename2df('ENCOUNTER_DATA.csv')
dxs = filename2df('DX_ENCOUNTER.csv')

#prob = list2df(prl) %>%
#    select(-PROBLEM_LIST_ID, -DX_ID, -CHRONIC_YN) %>%
#    mutate_at(vars(NOTED_DATE), ~ chdate(.))

cat('\nDims of pat, enc, dxs----\n')
dim(pat)
dim(enc)
dim(dxs)
