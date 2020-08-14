library(dplyr)
library(ggplot2)
#library(tidyr)

PATH = "/Users/ajz/Desktop/covid_datathon_git/DATA_HSP_LAB_PROC/" # end with slash

HSP_F = "SLH_HSP_0731_1.txt"
LABS_F = "SLH_LABS_0731_1.txt"
PROCS_F = "SLH_PROCS_0731_1.txt"

# onedrive --> files / covid-19 / covid_datathon / data / DATA_HSP_LAB_PROC

setwd(PATH)

hsp = read.csv(paste(PATH, HSP_F,   sep=''), sep="\t", stringsAsFactors = FALSE)
labs = read.csv(paste(PATH, LABS_F,   sep=''), sep="\t", stringsAsFactors = FALSE)
procs = read.csv(paste(PATH, PROCS_F,   sep=''), sep="\t", stringsAsFactors = FALSE)

dim(procs)
# [1] 181889      7
dim(labs)
# [1] 4641   16
dim(hsp)
# [1] 503  21

#### okay forget those here is BSLMC version 3

# whole thing 240 MB
# encounter_dx - all encounters, beginning of time, on these pts
# orders_results - big 50 MB files, times 3.
# pat_id - uh oh the formatting has lots of text art lines and tables.

#### okay forget that. it is not TSV. Await BSLMC version 4.
