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

prob = read.csv(paste(PATH, PRF, sep=''), sep="\t", stringsAsFactors = FALSE)
pat  = read.csv(paste(PATH, PTF, sep=''), sep="\t", stringsAsFactors = FALSE)
ord  = read.csv(paste(PATH, ORF, sep=''), sep="\t", stringsAsFactors = FALSE)
hosp = read.csv(paste(PATH, HSF, sep=''), sep="\t", stringsAsFactors = FALSE)
enc  = read.csv(paste(PATH, ENF, sep=''), sep="\t", stringsAsFactors = FALSE)

dim(prob) # 132   1
dim(pat)  # 49  1
dim(ord)  # 44959     1
dim(hosp) # 505   1
dim(enc)  # 222   1
