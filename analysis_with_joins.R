library(ggplot2)
library(dplyr)
#                                                                         80 #

path = "/Users/ajz/Documents/local-git/covid_datathon/"
setwd(path)

f_patient = "PATIENT.txt"
f_problems = "PAT_PRBL_LIST.txt"
f_visits = "PAT_ENC_DX.txt"
f_flowsheet = "Pat_FlowSheet_PulseOx.txt"
f_tests = "PAT_ORDERS_PROCEDURES.txt"

# PATIENT.txt               # id, mrn, name, dob, age
# PAT_PRBL_LIST.txt         # id, prob number, date, dx name, icd10
# PAT_ENC_DX.txt            # id, date, encType, bp, dx_icd
# Pat_FlowSheet_PulseOx.txt # id, value, time, name (survey question or SpO2)
# PAT_ORDERS_PROCEDURES.txt # id, date, proc name, result date, ORDER_COMPONENT, ORD_VALUE_TEXT, ord val numeric

patient   = read.csv(paste(path, f_patient,   sep=''), sep="\t", stringsAsFactors = FALSE)
problems  = read.csv(paste(path, f_problems,  sep=''), sep="\t", stringsAsFactors = FALSE)
visits    = read.csv(paste(path, f_visits,    sep=''), sep="\t", stringsAsFactors = FALSE)
flowsheet = read.csv(paste(path, f_flowsheet, sep=''), sep="\t", stringsAsFactors = FALSE)
tests     = read.csv(paste(path, f_tests,     sep=''), sep="\t", stringsAsFactors = FALSE)

pulseox =
flowsheet %>%
filter(DISP_NAME == "SpO2") %>%
mutate(value_numeric = as.numeric(MEAS_VALUE))

ggplot(pulseox, aes(value_numeric)) + geom_histogram(binwidth=1) + xlab('Pulse oximetry (%)') + ylab('Count') + scale_x_continuous(breaks = seq(90,100,2))
#ggsave("pulseox_histogram.png")
