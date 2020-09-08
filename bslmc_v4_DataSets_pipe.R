library(dplyr)
library(ggplot2)

PATH = "/Users/ajz/Desktop/aa-git/covid_datathon/DataSets/" # end with slash
SETWDPATH = "/Users/ajz/Desktop/aa-git/covid_datathon/" # end with slash

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

setwd(SETWDPATH)

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
full_join(comorb_count_e, by = "PAT_ID") ->
nas

nas[is.na(nas)] <- 0

nas %>%
mutate(dm = dm.x+dm.y > 0,
	copd = copd.x+copd.y > 0,
	asthma = asthma.x+asthma.y > 0,
	htn = htn.x+htn.y > 0) %>%
select(- contains('.')) %>%
full_join(pat) %>%
mutate_at(vars(BIRTH_DATE, DEATH_DATE), ~ as.Date( . , '%m/%d/%Y' )) %>%
mutate(age = as.numeric(as.Date('2020-08-30', '%Y-%m-%d') - BIRTH_DATE) / 365) ->
onept

ggplot(onept, aes(dm, age)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC') ->
    dmage

ggplot(onept, aes(htn, age)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC') ->
    htnage

ggplot(onept, aes(asthma, age)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC') ->
    astage

ggplot(onept, aes(copd, age)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width=0.2) +
    labs(subtitle='Inpatient/BSLMC') ->
    copdage

cat("deceased----\n")
onept %>% filter(DEATH_DATE > 0)



ord %>%
filter(grepl("cov", PROC_NAME, ignore.case = TRUE) & ! grepl("performing", COMPONENT, ignore.case = TRUE) & LAB_STATUS == 'Final') %>%
select(PAT_ID, ORDERING_DATE, RESULT_DATE, ORD_VALUE) %>%
mutate(cov_result = case_when(
	ORD_VALUE == "Negative" | ORD_VALUE == "Not Detected" ~ 0,
	ORD_VALUE == "Positive" | ORD_VALUE == "Detected" ~ 1
)) %>%
mutate(ordering_dt = as.Date(ORDERING_DATE, '%m/%d/%Y')) %>%
mutate(result_dt = as.Date(RESULT_DATE, '%m/%d/%Y')) %>%
mutate(latency = result_dt - ordering_dt) ->
covids

table(covids$ORD_VALUE)
#     Detected     Negative Not Detected     Positive 
#           14           79           70            8 

ord %>%
filter(grepl("cov", PROC_NAME, ignore.case = TRUE) & grepl("performing", COMPONENT, ignore.case = TRUE)  ) ->
performing
table(performing$ORD_VALUE)
#BCM Resp Virus Lab              BSLMC                CPL               SLWH 
#                 2                 74                 66                 21 

#plot covid tests by date
ggplot(covids, aes(x=ordering_dt, color=as.factor(cov_result))) +
    geom_freqpoly(binwidth=7) +
    labs(title="COVID result by date", x='Order date', y='Count', color="Result", subtitle='Inpatient') ->
    posnegdate

pdf("Rplots_inpat_v4.pdf")
dmage
htnage
astage
copdage
posnegdate
dev.off()
