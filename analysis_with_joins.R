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

cat("number of patients----\n")
dim(patient)

freq = table(tests$ORDER_COMPONENT)

cat("top few order components----")
head(sort(freq, decreasing = TRUE), n = 100) # top few order components
# popular: bmp, cmp, cbc, lipid, a1c, tsh, "case report", ua, "diagnosis", "gross", sars-cov-2, ft4, comment, microsc, ...

# other interesting stuff
# CORONAVIRUS 229E # 3
# CORONAVIRUS HKU1 # 3
# CORONAVIRUS NL63 # 3
# CORONAVIRUS OC43
# SARS-COV-2 ANTIBODY,IGG




#### derived datasets pulseox ####

pulseox =
flowsheet %>%
filter(DISP_NAME == "SpO2") %>%
mutate(spo2_value_numeric = as.numeric(MEAS_VALUE)) %>%
rename(spo2_value_text = MEAS_VALUE) %>%
mutate(spo2_date = as.Date(ENTRY_TIME, '%m-%d-%Y')) %>%
rename(spo2_datetime_text = ENTRY_TIME)

ggplot(pulseox, aes(spo2_value_numeric)) + geom_histogram(binwidth=1) + xlab('Pulse oximetry (%)') + ylab('Count') + scale_x_continuous(breaks = seq(90,100,2))

qplot(pulseox$spo2_date)




#### derived datasets from PAT_ORDERS_PROCEDURES ####

covids =
tests %>%
filter(ORDER_COMPONENT == "SARS-COV-2") %>%
select(PAT_ID, PROC_ID, RESULT_DATE, ORDER_DATE, ORDER_COMPONENT, ORD_VALUE_TEXT, ORD_VALUE_NUMERIC) %>%
mutate(covid_res_dt = as.Date(RESULT_DATE, '%m-%d-%Y'), covid_ord_dt = as.Date(ORDER_DATE, '%m-%d-%Y')) %>%
rename(cov_res_dt_txt = RESULT_DATE, cov_ord_dt_txt = ORDER_DATE) %>%
mutate(latency = covid_res_dt - covid_ord_dt) %>%
mutate(covid_result = case_when(
        ORD_VALUE_TEXT == "NEGATIVE" | ORD_VALUE_TEXT == "Negative" |
                ORD_VALUE_TEXT == "NOT DETECTED" | ORD_VALUE_TEXT == "Not Detected" ~ 0,
        ORD_VALUE_TEXT == "POSITIVE" | ORD_VALUE_TEXT == "Positive" |
                ORD_VALUE_TEXT == "DETECTED" | ORD_VALUE_TEXT == "Detected" ~ 1,
        ORD_VALUE_TEXT == "PRESUMPTIVE POSITIVE" ~ 0.5
        )) %>%
rename(cov_result_txt = ORD_VALUE_TEXT)

qplot(covids$latency)
qplot(covids$covid_ord_dt)

cat("confirm that numeric is useless----")
table(covids$ORD_VALUE_NUMERIC) # confirm that numeric is useless

cat("covids ORD_VALUE_TEXT ----")
table(covids$cov_result_txt)

cat("covids covid_result (consolidated) ----")
table(covids$covid_result)




####### JOIN

covids_pulseox =
covids %>%
inner_join(pulseox) %>%
mutate(covid_spo2_diff = covid_ord_dt - spo2_date)

qplot(covids_pulseox$covid_spo2_diff) # not good - very separated
cat("table of time betw pulse ox & covid test----")
table(covids_pulseox$covid_spo2_diff) # not good - very separated
cat("dim pulseox----\n")
dim(pulseox)
cat("dim covids----\n")
dim(covids)
cat("dim covids_pulseox----\n")
dim(covids_pulseox)

cat("table of nearby spo2 and covid----\n")

nearby =
covids_pulseox %>%
filter(-30 < covid_spo2_diff & covid_spo2_diff < 30) %>%
select(covid_ord_dt, covid_result, spo2_value_numeric, spo2_date, covid_spo2_diff)

nearby

cat("dim nearby----\n")
dim(nearby)

#invalid-ish histo
ggplot(covids_pulseox, aes(x=spo2_value_numeric, fill=as.factor(covid_result))) + geom_histogram(binwidth=1) + xlab('Pulse oximetry (%)') + ylab('Count') + scale_x_continuous(breaks = seq(90,100,2))

#unstacked version
ggplot(covids_pulseox, aes(x=spo2_value_numeric, color=as.factor(covid_result))) + geom_freqpoly(binwidth=1) + xlab('Pulse oximetry (%)') + ylab('Count') + scale_x_continuous(breaks = seq(90,100,2))


## valid??
ggplot(nearby, aes(x=spo2_value_numeric, color=as.factor(covid_result))) + geom_freqpoly(binwidth=1) + xlab('Pulse oximetry (%)') + ylab('Count') + scale_x_continuous(breaks = seq(90,100,2))

cat("Wilcoxon Mann Whitney test on rather few samples----")
wilcox.test(nearby[nearby$covid_result == 0, ]$spo2_value_numeric,
			nearby[nearby$covid_result == 1, ]$spo2_value_numeric)
