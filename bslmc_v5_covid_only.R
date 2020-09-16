# onedrive --> files / covid-19 / covid_datathon / HospitalAdmissionCovid

SETWDPATH = "/Users/ajz/Desktop/aa-git/covid_datathon/"
DATADIR = "HospitalAdmissionCovid/"
DATAPATH = paste(SETWDPATH, DATADIR, sep='')

setwd(SETWDPATH)

dimlf = 'BSLMC_COVID_LABS_Tests.txt'
diagf = 'PHI_DATA_BSLMC Hsp Admission Dx List_COVID 01-01-2020 to 06-30-2020.txt'
hospf = 'PHI_DATA_BSLMC Hsp Admission_COVID 01-01-2020 to 06-30-2020.txt'
probf = 'PHI_DATA_BSLMC PROBLEM LIST_DM.COPD.ASTHMA.HTN_COVID 01-01-2020 to 06-30-2020.txt'

str2df <- function(s) {
    d = read.csv(paste(DATAPATH, s, sep=''), sep="\t",
        stringsAsFactors = FALSE, na.strings="NULL")
    return(d)
}

drop_sparse = function(df) {
	ncols = dim(diag)[2]
	df %>%
		summarise_all(~ mean(!is.na(.))) ->
		prop_populated
	retain = c()
	for (j in seq(ncols)) {
		if (prop_populated[,j] > 0.1) { # fixme - hard coded cutoff
			retain = c(retain, j)
		}
	}
	return(df[,retain])
}

diml = str2df(dimlf)
diag = drop_sparse(str2df(diagf))
hosp = drop_sparse(str2df(hospf))
prob = drop_sparse(str2df(probf))
