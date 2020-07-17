library(ggplot2)
library(dplyr)
#                                                                         80 #

path = "/Users/ajz/Documents/local-git/covid_datathon/"

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

patient = read.csv(paste(path, f_patient, sep=''), sep="\t", stringsAsFactors = FALSE)
