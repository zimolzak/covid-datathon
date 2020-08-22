library(ggplot2)
library(dplyr)
#                                                                         80 #

PATH = "/Users/ajz/Desktop/covid_datathon_git/"
setwd(PATH)

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

patient   = read.csv(paste(PATH, f_patient,   sep=''), sep="\t", stringsAsFactors = FALSE)
problems  = read.csv(paste(PATH, f_problems,  sep=''), sep="\t", stringsAsFactors = FALSE)
visits    = read.csv(paste(PATH, f_visits,    sep=''), sep="\t", stringsAsFactors = FALSE)
flowsheet = read.csv(paste(PATH, f_flowsheet, sep=''), sep="\t", stringsAsFactors = FALSE)
tests     = read.csv(paste(PATH, f_tests,     sep=''), sep="\t", stringsAsFactors = FALSE)

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
                ORD_VALUE_TEXT == "NOT DETECTED" | ORD_VALUE_TEXT == "Not Detected" ~ 'neg',
        ORD_VALUE_TEXT == "POSITIVE" | ORD_VALUE_TEXT == "Positive" |
                ORD_VALUE_TEXT == "DETECTED" | ORD_VALUE_TEXT == "Detected" ~ 'pos',
        ORD_VALUE_TEXT == "PRESUMPTIVE POSITIVE" ~ 'presump'
        )) %>%
rename(cov_result_txt = ORD_VALUE_TEXT)

ggplot(covids, aes(x=covid_ord_dt, color=as.factor(covid_result))) + geom_freqpoly(binwidth=7) + scale_y_continuous(breaks = seq(0,10,2)) + labs(title="COVID result by date", x='Order date', y='Count', color="Result")

cat("confirm that numeric is useless----")
table(covids$ORD_VALUE_NUMERIC) # confirm that numeric is useless

cat("covids ORD_VALUE_TEXT ----")
table(covids$cov_result_txt)

cat("covids covid_result (consolidated) ----")
table(covids$covid_result)

qplot(data=covids, x=covid_ord_dt, y=latency) + labs(title="COVID test latency over time", x="Order date")




####### JOIN

covids_pulseox =
covids %>%
inner_join(pulseox) %>%
mutate(covid_spo2_diff = covid_ord_dt - spo2_date)

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
select(spo2_date, covid_ord_dt, covid_result, spo2_value_numeric, covid_spo2_diff)

nearby

cat("dim nearby----\n")
dim(nearby)

## valid??
ggplot(nearby, aes(x=spo2_value_numeric, color=as.factor(covid_result))) + geom_freqpoly(binwidth=1) + labs(x='Pulse oximetry (%)', y='Count', title="Outpatient pulse ox distribution by COVID status", color='COVID result') + scale_x_continuous(breaks = seq(90,100,2))

cat("Wilcoxon Mann Whitney test on rather few samples----")
wilcox.test(nearby[nearby$covid_result == 'neg', ]$spo2_value_numeric,
			nearby[nearby$covid_result == 'pos', ]$spo2_value_numeric)



## future directions:
## look for office visit (limit to these only)
## find out why the 160 patients vs 76 tests??
## comorbidities
## basic labs
## test/retest idea

## BSLMC data pull

## limit ORDERS_PROCEDURES and Flowsheet to 2019+
## consider limiting ORDERS_PROCEDURES to certain components?
