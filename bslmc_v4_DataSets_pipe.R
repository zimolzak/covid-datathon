library(dplyr)
library(ggplot2)

PATH = "/Users/ajz/Desktop/covid_datathon_git/DataSets/" # end with slash

prl = c('PROBLEM/PROBLEM_GRP_3 20200814 1855.csv','PROBLEM/PROBLEM_GRP_2 20200814 1854.csv','PROBLEM/PROBLEM_GRP_1 20200814 1853.csv')
hsl = c('HSP/HSP_GRP_2 20200814 2207.csv','HSP/HSP_GRP_1 20200814 2205.csv','HSP/HSP_GRP_3 20200814 2208.csv')
enl = c('ENC_DX/ENC_DX_GRP_2 20200814 1710.csv','ENC_DX/ENC_DX_GRP_1 20200814 1604.csv','ENC_DX/ENC_DX_GRP_3 20200814 1713.csv')
ptl = c('PAT_ID_2/PAT_ID_GRP_2 20200814 1407.csv','PAT_ID_2/PAT_ID_GRP_1 20200814 1359.csv','PAT_ID_2/PAT_ID_GRP_3 20200814 1506.csv')
orl = c('ORDER_RESULTS/ORDER_RESULTS_GRP_3 20200814 1840.csv','ORDER_RESULTS/ORD_RSLTS_GRP_1 20200814 1805.csv','ORDER_RESULTS/ORDER_RESULTS_GRP_2.csv')

# onedrive --> files / covid-19 / covid_datathon / data / DataSets / DATA_HSP_LAB_PROC

list2df <- function(L) {
	# todo - check about length & class of L
	d = read.csv(paste(PATH, L[1], sep=''), sep="|", stringsAsFactors = FALSE, na.strings="null")
	for (i in seq(2,length(L))) {
		di = read.csv(paste(PATH, L[i], sep=''), sep="|", stringsAsFactors = FALSE, na.strings="null")
		d %>% bind_rows(di) -> d
	}
	return(d)
}

setwd(PATH)

prob = list2df(prl)
pat  = list2df(ptl)
ord  = list2df(orl)
hosp = list2df(hsl)
enc  = list2df(enl)

cat('\nDims of prob, pat, ord, hosp, enc----\n')
dim(prob)
dim(pat)
dim(ord)  # 181907     13
dim(hosp)
dim(enc)

prob %>% #copy paste works
group_by(PAT_ID) %>%
summarise(dm     = sum(grepl("diab", DX_NAME, ignore.case = TRUE)),
	      asthma = sum(grepl("asth", DX_NAME, ignore.case = TRUE)),
	      copd   = sum(grepl("copd", DX_NAME, ignore.case = TRUE)),
	      htn    = sum(grepl("hypert", DX_NAME, ignore.case = TRUE))) ->
comorb_count_p

# pat has birth date and death date. Death date is a character.
# hosp - no diagnoses?

enc %>%
group_by(PAT_ID) %>% ## REUSE
summarise(dm     = sum(grepl("diab", enc_dx_name, ignore.case = TRUE)),
	      asthma = sum(grepl("asth", enc_dx_name, ignore.case = TRUE)),
	      copd   = sum(grepl("copd", enc_dx_name, ignore.case = TRUE)),
	      htn    = sum(grepl("hypert", enc_dx_name, ignore.case = TRUE))) ->
comorb_count_e






comorb_count_p %>%
full_join(comorb_count_e, by = "PAT_ID") %>%
mutate(dm = sum(dm.x,dm.y, na.rm=TRUE),
	copd = sum(copd.x,copd.y, na.rm=TRUE),
	asthma = sum(asthma.x,asthma.y, na.rm=TRUE),
	htn = sum(htn.x,htn.y, na.rm=TRUE)) %>%
select(- contains('.'))
## TODO fixme, numbers not coming out right.

#full_join(pat) %>%
