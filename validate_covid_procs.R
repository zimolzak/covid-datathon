library(dplyr)
path = "/Users/ajz/Desktop/covid_datathon_git/"
setwd(path)
f_procs = "covid_procs_stlukes.csv"
procs = read.csv(paste(path, f_procs, sep=''), stringsAsFactors = FALSE)

dim(procs) # 29 x 148 (!)
