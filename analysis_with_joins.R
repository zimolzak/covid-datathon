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

# %>%  inner_join(table, by = id)
# %>%  inner_join(table)




####### JOIN

covids =
tests %>%
filter(ORDER_COMPONENT == "SARS-COV-2") %>%
select(PAT_ID, PROC_ID, ORDER_RESULT_LINE, RESULT_DATE, ORDER_DATE, ORDER_COMPONENT, ORD_VALUE_TEXT, ORD_VALUE_NUMERIC)

# todo - idea: convert dates from char to date, and plot latency over time

table(covids$ORD_VALUE_NUMERIC) # confirm that numeric is useless
freq = table(tests$ORDER_COMPONENT)
head(sort(freq, decreasing = TRUE), n = 100) # top few order components
# popular: bmp, cmp, cbc, lipid, a1c, tsh, "case report", ua, "diagnosis", "gross", sars-cov-2, ft4, comment, microsc, ...

# CORONAVIRUS 229E # 3
# CORONAVIRUS HKU1 # 3
# CORONAVIRUS NL63 # 3
# CORONAVIRUS OC43
# SARS-COV-2 ANTIBODY,IGG

table(covids$ORD_VALUE_TEXT)

covids_tbj =
covids %>%
select(PAT_ID, ORDER_DATE, ORD_VALUE_TEXT) %>%
mutate(covid_result = case_when(
        ORD_VALUE_TEXT == "NEGATIVE" | ORD_VALUE_TEXT == "Negative" |
                ORD_VALUE_TEXT == "NOT DETECTED" | ORD_VALUE_TEXT == "Not Detected" ~ 0,
        ORD_VALUE_TEXT == "POSITIVE" | ORD_VALUE_TEXT == "Positive" |
                ORD_VALUE_TEXT == "DETECTED" | ORD_VALUE_TEXT == "Detected" ~ 1,
        ORD_VALUE_TEXT == "PRESUMPTIVE POSITIVE" ~ 0.5
        ))

table(covids_tbj$covid_result)

covids_pulseox =
covids_tbj %>%
inner_join(pulseox)
# doh a lot of pulse ox not synchronous

class(covids_pulseox$ENTRY_TIME) # doh, character




ggplot(covids_pulseox, aes(x=value_numeric, fill=as.factor(covid_result))) + geom_histogram(binwidth=1) + xlab('Pulse oximetry (%)') + ylab('Count') + scale_x_continuous(breaks = seq(90,100,2))
