library(ggplot2)
library(dplyr)
#                                                                         80 #

pathname = "/Users/ajz/Documents/local-git/covid_datathon/Random_COVID_PAT.tsv"
X = read.csv(pathname, sep="\t", stringsAsFactors = FALSE)

D = 
X %>% 
filter(LAB == "SARS-COV-2") %>%
#  slice(1:200) %>%
select(PAT_ID, PAT_DOB, SEX_C_ID, ORD_VALUE,
		IDENTITY_TYPE_ID, PRBL_ICD10, ENC_ICD10, ENC_TYPE) %>%
mutate(result = case_when(
	ORD_VALUE == "NEGATIVE" | ORD_VALUE == "Negative" | 
		ORD_VALUE == "NOT DETECTED" | ORD_VALUE == "Not Detected" ~ 0,
	ORD_VALUE == "POSITIVE" | ORD_VALUE == "Positive" |
		ORD_VALUE == "DETECTED" | ORD_VALUE == "Detected" ~ 1,
	ORD_VALUE == "PRESUMPTIVE POSITIVE" ~ 0.5
	))

#### outputs

cat("\nCount of COVID test results, by raw result ORD_VALUE\n")
D %>%
  group_by(ORD_VALUE) %>%
  summarise(n=n())

cat("\nCount of COVID test results, by ENC_TYPE\n")
D %>%
  group_by(ENC_TYPE) %>%
  summarise(n=n())

cat("\nCount of COVID test results, by aggregated result\n")
D %>%
  group_by(result) %>%
  summarise(n=n())

cat("\nCount of COVID test results, by patient\n")
D %>%
  group_by(PAT_ID) %>%
  summarise(n=n())

#one_patient = 
#D %>%
#    filter(PAT_ID == "Z1111111") %>% # TODO - fake number
#    filter(IDENTITY_TYPE_ID == 0)




#### Whole new data layout ########

path = "/Users/ajz/Desktop/covid-2/"

## define strings for filenames
## load in txt/TSV files


# PATIENT.txt               # id, mrn, name, dob, age
# PAT_PRBL_LIST.txt         # id, prob number, date, dx name, icd10
# PAT_ENC_DX.txt            # id, date, encType, bp, dx_icd
# Pat_FlowSheet_PulseOx.txt # id, value, time, name (survey question or SpO2)
# PAT_ORDERS_PROCEDURES.txt # id, date, proc name, result date, ORDER_COMPONENT, ORD_VALUE_TEXT, ord val numeric
