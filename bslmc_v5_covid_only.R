library(dplyr)

# onedrive --> files / covid-19 / covid_datathon / HospitalAdmissionCovid

SETWDPATH = "/Users/ajz/Desktop/aa-git/covid_datathon/"
DATADIR = "HospitalAdmissionCovid/"
DATAPATH = paste(SETWDPATH, DATADIR, sep='')

setwd(SETWDPATH)

dimlf = 'BSLMC_COVID_LABS_Tests.txt'
hospf = 'PHI_DATA_BSLMC Hsp Admission_COVID 01-01-2020 to 06-30-2020.txt'
probf = 'PHI_DATA_BSLMC PROBLEM LIST_DM.COPD.ASTHMA.HTN_COVID 01-01-2020 to 06-30-2020.txt'

str2df <- function(s) {
    d = read.csv(paste(DATAPATH, s, sep=''), sep="\t",
        stringsAsFactors = FALSE, na.strings="NULL")
    return(d)
}
drop_sparse = function(df) {
	ncols = dim(df)[2]
	df %>%
		summarise_all(~ mean(!is.na(.))) ->
		prop_populated
	retain = c()
	for (j in seq(ncols)) {
		if (prop_populated[,j] > 0.5) { # fixme - hard coded cutoff
			retain = c(retain, j)
		}
	}
	return(df[,retain])
}
say = function(s) {
	sL = c('\n', s, '----\n')
	cat(paste(sL, collapse=''))
}

diml = str2df(dimlf)
h_i = str2df(hospf)
p_i = str2df(probf)
say('dims of initial frames')
for (d in list(diml, d_i, h_i, p_i)) {
	print(dim(d))
}

hosp = drop_sparse(h_i)
prob = drop_sparse(p_i)
say('dims of filtered frames')
for (d in list(diag, hosp, prob)) {
	print(dim(d))
}
