library(dplyr)
library(ggplot2)

PATH = "/Users/ajz/Desktop/covid_datathon_git/DataSets/" # end with slash

PRF = "PROBLEM/PROBLEM_GRP_1 20200814 1853.csv"
PTF = "PAT_ID_2/PAT_ID_GRP_1 20200814 1359.csv"
ORF = "ORDER_RESULTS/ORD_RSLTS_GRP_1 20200814 1805.csv"
HSF = "HSP/HSP_GRP_1 20200814 2205.csv"
ENF = "ENC_DX/ENC_DX_GRP_1 20200814 1604.csv"

# onedrive --> files / covid-19 / covid_datathon / data / DataSets / DATA_HSP_LAB_PROC

setwd(PATH)

prob = read.csv(paste(PATH, PRF, sep=''), sep="|", stringsAsFactors = FALSE)
pat  = read.csv(paste(PATH, PTF, sep=''), sep="|", stringsAsFactors = FALSE)
ord  = read.csv(paste(PATH, ORF, sep=''), sep="|", stringsAsFactors = FALSE)
hosp = read.csv(paste(PATH, HSF, sep=''), sep="|", stringsAsFactors = FALSE)
enc  = read.csv(paste(PATH, ENF, sep=''), sep="|", stringsAsFactors = FALSE)

cat('\nDims of prob, pat, ord, hosp, enc----\n')
dim(prob) # 132   7
dim(pat)  # 49  4
dim(ord)  # 44959     13
dim(hosp) # 505   33
dim(enc)  # 222   9

prob %>% #copy paste works
group_by(PAT_ID) %>%
summarise(dm     = sum(grepl("diab", DX_NAME, ignore.case = TRUE)),
	      asthma = sum(grepl("asth", DX_NAME, ignore.case = TRUE)),
	      copd   = sum(grepl("copd", DX_NAME, ignore.case = TRUE)),) ->
comorb_count

# pat has birth date and death date. Death date is a character.
