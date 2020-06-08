library(ggplot2)
library(dplyr)

#                                                                            #

pathname = "/Users/ajz/Desktop/covid-2/Random_COVID_PAT.tsv"

X = read.csv(pathname, sep="\t", stringsAsFactors = FALSE)
cov_tests_only = X[X$LAB == "SARS-COV-2", ]
table(cov_tests_only$ORD_VALUE)

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

D %>%
  group_by(ENC_TYPE) %>%
  summarise(n=n())

D %>%
  group_by(result) %>%
  summarise(n=n())

D %>%
  group_by(PAT_ID) %>%
  summarise(n=n())

#one_patient = 
#D %>%
#    filter(PAT_ID == "Z1111111") %>% # TODO - fake number
#    filter(IDENTITY_TYPE_ID == 0)

# one_patient
