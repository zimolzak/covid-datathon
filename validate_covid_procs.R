library(dplyr)
path = "/Users/ajz/Desktop/covid_datathon_git/"
setwd(path)
f_procs = "covid_procs_stlukes.csv"
procs = read.csv(paste(path, f_procs, sep=''), stringsAsFactors = FALSE)

# TODO - probably don't need the Perl. Can just tell R what NULL means.
# But it does help me hit spacebar in macOS to skim.

dim(procs) # 29 x 148 (!)

procs %>%
select(PROC_ID, PROC_NAME, PROC_CODE, IS_ACTIVE_YN, PANEL_C, IS_EC_INACTIVE_YN,
	IS_PROC_USED, SHORT_NAME, ORDER_DISPLAY_NAME, TEST_ID, BILL_DESC) ->
selected

	# DEBIT OR CREDIT and TYPE_C have data but seem non useful.
selected %>%
select(PROC_ID, PROC_NAME, SHORT_NAME, ORDER_DISPLAY_NAME, BILL_DESC) ->
names_only
