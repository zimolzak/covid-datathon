library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr) # might not be using
library(here)
library(earth)
library(ROCR)




#### Functions

filename2df <- function(fn) {
    df = read.csv(here("data", "2020-10-19", fn), sep="|",
        stringsAsFactors = FALSE, na.strings="null")
    return(df)
}

df_interactive_mode = function(fn) {
	# crummy function for using R Console
	abs_path = '/Users/ajz/Desktop/aa-git/covid-datathon/data/2020-10-19/'
    df = read.csv(paste(abs_path, fn, sep=''), sep="|",
        stringsAsFactors = FALSE, na.strings="null")
    return(df)
}

chdate = function(x) {
    return(as.Date(x, '%Y-%m-%d' ))
}

chtime = function(x){
    return(parse_date_time(x, orders='ymdHMS')) # lubridate
    # mdYIMSp = mo day yr Hourdecimal min sec am/pm
}

say = function(s) {
	sL = c('\n', s, '----\n')
	cat(paste(sL, collapse=''))
}




#### Loading

INTERACTIVE = FALSE

if(INTERACTIVE) {
	pat = df_interactive_mode('PATIENT_DEMOGRAPHICS.DATA')
	enc = df_interactive_mode('ENCOUNTER.DATA')
	dxs = df_interactive_mode('DX_ENCOUNTER.DATA')
	prb = df_interactive_mode('PROBLEMS_LIST.DATA')
# uncomment later
	# lab = df_interactive_mode('LAB_RESULTS.DATA') # 1.77 GB, ~55 seconds to load
} else {
	pat = filename2df('PATIENT_DEMOGRAPHICS.DATA')
	enc = filename2df('ENCOUNTER.DATA')
	dxs = filename2df('DX_ENCOUNTER.DATA')
	prb = filename2df('PROBLEMS_LIST.DATA')
# uncomment later
	# message(paste('Reading labs...', now()))
	# lab = filename2df('LAB_RESULTS.DATA')
	# message(paste('done.', now()))
}

#prob = list2df(prl) %>%
#    select(-PROBLEM_LIST_ID, -DX_ID, -CHRONIC_YN) %>%
#    mutate_at(vars(NOTED_DATE), ~ chdate(.))

say('Dimensions of pat, enc, dxs, prb, lab')
dim(pat) # 1900 x 14
dim(enc) # 13943    15
dim(dxs) # 44816     9
dim(prb) # 16628     7

# uncomment later
# dim(lab) # 8490824      17





#### Cleaning (of dates, etc)

dxs %>%
select(PAT_ID, DX_NAME, CURRENT_ICD10_LIST, CONTACT_DATE, PAT_ENC_CSN_ID) %>%
mutate_at(vars(CONTACT_DATE), ~ chdate(.)) ->
dxs_cleaned

enc %>%
select(PAT_ID, PAT_ENC_CSN_ID, CONTACT_DATE, PATIENT_CLASS,
    HOSP_ADMSN_TIME, HOSP_DISCH_TIME, LVL_OF_CARE, DISCHARGE_DISP,
    ACUITY_LEVEL_C, ADMIT_SOURCE_C) %>%
mutate_at(vars(CONTACT_DATE), ~ chdate(.)) %>%
mutate_at(vars(HOSP_ADMSN_TIME, HOSP_DISCH_TIME), ~ chtime(.)) %>%
mutate(los.days = difftime(HOSP_DISCH_TIME, HOSP_ADMSN_TIME, units="days")) ->
enc_cleaned

## Todo - important - sometimes HOSP_DISCH_TIME is NA. Why? What to do?

#### lab stuff. Not using it currently. Takes a while.
# uncomment later
# lab %>% # todo - lab table looks really really weird, may be a problem.
# select(PAT_ID, RESULT_TIME, COMPONENT, ORD_VALUE) %>%
# distinct() %>%
# mutate_at(vars(RESULT_TIME), ~ chtime(.)) ->
# lab_distinct

# say('Dim of lab_distinct')
# dim(lab_distinct)

# lab %>%
# select(PAT_ID, ORDERING_DATE, PROC_NAME, PROC_CODE) %>%
# distinct() %>%
# filter(! grepl("LAB", PROC_CODE)) ->
# proc_distinct # probably won't use this yet. Need chdate() if we do.

# say('Procedure names')
# table(proc_distinct$PROC_NAME)

pat %>%
select(PAT_ID, Age, ETHNIC_GROUP, PATIENT_RACE, DEATH_DATE, SEX_C) %>%
mutate_at(vars(DEATH_DATE), ~ chdate(.)) %>%
mutate(sex = case_when(SEX_C == 1 ~ 'F', SEX_C == 2 ~ 'M')) %>%
mutate(race_aggr = case_when(PATIENT_RACE == 'White or Caucasian' ~ 'White or Caucasian',
    PATIENT_RACE == 'Black or African American' ~ 'Black or African American',
    PATIENT_RACE == 'Asian' ~ 'Asian',
    PATIENT_RACE == 'Declined' ~ 'Declined/unknown',
    PATIENT_RACE == 'Unable to Determine' ~ 'Declined/unknown',
    TRUE ~ 'Other'))->
pat_cleaned




#### Constructing new things

dxs_cleaned %>%
group_by(PAT_ID) %>%
summarise(comor.first.vis = min(CONTACT_DATE),
    comor.diab.nvis = sum(grepl("diab", DX_NAME, ignore.case = TRUE)),
    comor.asth.nvis = sum(grepl("asth", DX_NAME, ignore.case = TRUE)),
    comor.copd.nvis = sum(grepl("copd", DX_NAME, ignore.case = TRUE)),
    comor.hypert.nvis = sum(grepl("hypert", DX_NAME, ignore.case = TRUE))
    ) ->
predictors_visit_count

## Todo - note that all these grepls with ignore.case will generate warnings when trying to scan DX_NAME with fancy characters, like if they put an accent on "Guillain-Barre." Consider grepl() within ICD code instead. See also how I generate covid_dx_dates, versus covid_admissions, below.

prb %>%
group_by(PAT_ID) %>%
summarise(comor.diab.probl = sum(grepl("diab", DX_NAME, ignore.case = TRUE)),
    comor.asth.probl = sum(grepl("asth", DX_NAME, ignore.case = TRUE)),
    comor.copd.probl = sum(grepl("copd", DX_NAME, ignore.case = TRUE)),
    comor.hypert.probl = sum(grepl("hypert", DX_NAME, ignore.case = TRUE))
    ) ->
predictors_problem_list

dxs_cleaned %>%
filter(grepl("covid", DX_NAME, ignore.case = TRUE)) %>%
group_by(PAT_ID) %>%
summarise(covid.first.vis = min(CONTACT_DATE)) ->
covid_dx_dates # not used

# todo - the below is probably safer than the above:
# filter(grepl("U07.1", CURRENT_ICD10_LIST, ignore.case = TRUE))

enc_cleaned %>%
filter(PATIENT_CLASS == 'Emergency', CONTACT_DATE > chdate('2020-01-01')) ->
er2020 # barely used

enc_cleaned %>%
mutate(los.days.n = as.numeric(los.days)) %>%
filter(PATIENT_CLASS == 'Inpatient', CONTACT_DATE > chdate('2020-01-01')) ->
inpat2020

# Would need to make analytic dataset from the perspective of ER visit if outcome is admit/not.
# But can make the dataset from the perspective of an inpat visit if outcome is LOS.

dxs_cleaned %>%
select(CURRENT_ICD10_LIST, PAT_ENC_CSN_ID) %>% # Don't need PAT_ID. I verified.
filter(grepl("U07.1", CURRENT_ICD10_LIST, ignore.case = TRUE)) %>%
inner_join(inpat2020, by = "PAT_ENC_CSN_ID") ->
covid_admissions

covid_admissions %>%
left_join(pat_cleaned) %>%
left_join(predictors_visit_count) %>%
left_join(predictors_problem_list) %>%
filter(!is.na(los.days.n)) %>% # TODO - important - about when LOS or discharge is NA
mutate(comor.diab = (comor.diab.nvis + comor.diab.probl > 0),
	comor.asth = (comor.asth.nvis + comor.asth.probl > 0),
	comor.copd = (comor.copd.nvis + comor.copd.probl > 0),
	comor.hypert = (comor.hypert.nvis + comor.hypert.probl > 0),
	died_ever = !is.na(DEATH_DATE)) -> # todo - free parameter
analytic_data




#### Analyses: simple text tables

say('ER discharge disposition')
table(er2020$DISCHARGE_DISP)

say('Tables of ethnicity/race')
table(pat_cleaned$ETHNIC_GROUP)
table(pat_cleaned$PATIENT_RACE)
say('Crosstab of race, ethnicity')
with(pat_cleaned, table(PATIENT_RACE, ETHNIC_GROUP))




#### plots, Bivariate

los_vs_categorical = function(catvar, plottype) {
	result = kruskal.test(analytic_data$los.days.n, analytic_data[,catvar])
	p = result$p.value
	n_categories = dim(table(analytic_data[,catvar]))
	if(n_categories == 2){
		categories = analytic_data %>% select(all_of(catvar)) %>% distinct()
		data_2col = analytic_data %>% select(all_of(catvar), los.days.n)
		logical_vec = (data_2col[,1] == categories[1,])
		los1 = data_2col[logical_vec,2]
		los2 = data_2col[!logical_vec,2]
		median_str = paste('\nMedians:', median(los1, na.rm=TRUE), median(los2, na.rm=TRUE))
	} else {
		median_str = ''
	}
	if(plottype == 'box') {
		g = ggplot(data = NULL, aes(x=analytic_data[,catvar], y=analytic_data$los.days.n)) +
	    geom_boxplot(outlier.shape = NA, notch = TRUE) +
    	geom_jitter(width=0.2, alpha = 0.2) +
    	labs(subtitle=paste('p =', p, median_str), y = 'Length of stay (days)', x = catvar)
    	# todo: put medians on the plot
	} else {
		g = ggplot(data=NULL, aes(x= analytic_data$los.days.n, color= analytic_data[,catvar])) +
		geom_density() +
		scale_x_log10() +
    	labs(subtitle=paste('p =', p, median_str), x = 'Length of stay (days)', color = catvar)
	}
	return(g)
}

qplot(analytic_data$los.days.n) +
scale_x_log10() ->
los_histogram

ggplot(pat_cleaned, aes(x = DEATH_DATE)) +
geom_histogram() ->
death_histogram

ggplot(analytic_data, aes(x=Age, y = los.days.n)) + geom_point(alpha=0.2) -> agepoint
ggplot(analytic_data, aes(x=comor.diab.nvis, y = los.days.n)) + geom_point(alpha=0.1) -> diabpoint

los_vs_categorical('sex', 'box') -> sexbox
los_vs_categorical('ETHNIC_GROUP', 'box') -> ethbox
los_vs_categorical('race_aggr', 'box') -> racebox
los_vs_categorical('comor.diab', 'box') -> diabbox
los_vs_categorical('comor.asth', 'box') -> asthbox
los_vs_categorical('comor.copd', 'box') -> copdbox
los_vs_categorical('comor.hypert', 'box') -> hypertbox

los_vs_categorical('sex', 'dens') -> sexdens
los_vs_categorical('ETHNIC_GROUP', 'dens') -> ethdens
los_vs_categorical('race_aggr', 'dens') -> racedens
los_vs_categorical('comor.diab', 'dens') -> diabdens
los_vs_categorical('comor.asth', 'dens') -> asthdens
los_vs_categorical('comor.copd', 'dens') -> copddens
los_vs_categorical('comor.hypert', 'dens') -> hypertdens

# todo: titles, axes




#### Modeling
# earth, NN, knn, adaboost, rf

analytic_data %>%
select(los.days.n, Age, ETHNIC_GROUP, sex, race_aggr, comor.diab, comor.asth, comor.copd, comor.hypert) %>%
filter(!is.na(los.days.n)) ->
learning_data

say('MARS model, degree 1 (evimp and summary)')
earth.mod = earth(los.days.n ~ ., data = learning_data)
evimp(earth.mod)
summary(earth.mod)
qplot(predict(earth.mod), learning_data$los.days.n)  +
  geom_abline(intercept = 0, slope = 1) +
  labs(title='MARS model, degree 1')-> yyhat1

say('MARS model, degree 2 (evimp and summary)')
earth.mod2 = earth(los.days.n ~ ., data = learning_data, degree=2)
evimp(earth.mod2)
summary(earth.mod2)
qplot(predict(earth.mod2), learning_data$los.days.n)  +
  geom_abline(intercept = 0, slope = 1) +
  labs(title='MARS model, degree 2')-> yyhat2

# todo - cross validation or train/test split?

# logistic

say('Logistic regression of mortality')
logitMod <- glm(died_ever ~ Age + ETHNIC_GROUP + sex + race_aggr + comor.diab + comor.asth + comor.copd + comor.hypert, data=analytic_data, family=binomial(link="logit"))
summary(logitMod)

rocr_pred = prediction(logitMod$fitted.values, logitMod$y)
rocr_perf <- performance(rocr_pred, measure = "tpr", x.measure = "fpr")
say('AUC')
performance(rocr_pred, "auc")

# calibration
# library(rms)
# val.prob(logitMod$fitted.values, logitMod$y)
# val.prob(logitMod$fitted.values, logitMod$y, group=10)
ggplot(data=NULL, aes(x=logitMod$fitted.values, color=analytic_data$died_ever)) +
    geom_freqpoly() + labs(title='Calibration, logistic regression') -> cal1
ggplot(data=NULL, aes(x=logitMod$fitted.values, fill=analytic_data$died_ever)) +
    geom_histogram(binwidth=0.1) + labs(title='Calibration, logistic regression') -> calhist

bind_cols(analytic_data, phat = logitMod$fitted.values) %>%
select(died_ever, phat) %>%
mutate(bin = trunc(phat * 10) / 10) %>%
group_by(bin) %>%
summarise(p_observed = mean(died_ever),
    numerator = sum(died_ever),
    denominator = n()) ->
caldata

say('Logistic model calibration')
caldata

ggplot(caldata, aes(bin, p_observed)) + geom_point(color='red') + geom_abline(intercept=0, slope=1, alpha=0.25) +
xlim(0,1) + ylim(0,1) + geom_line(color='red') + labs(title='Calibration, logistic regression') -> calformal




#### write to plot files

say('\n\n----\n\nEnd of text output. Now plotting.')
pdf(here("outputs", "Rplots_v6.pdf"))
death_histogram
los_histogram
agepoint
diabpoint
sexbox
ethbox
racebox
diabbox
asthbox
copdbox
hypertbox
sexdens
ethdens
racedens
diabdens
asthdens
copddens
hypertdens
plotmo(earth.mod)
plot(earth.mod)
yyhat1
plotmo(earth.mod2)
plot(earth.mod2)
yyhat2
plot(rocr_perf, colorize=TRUE)
cal1
calhist
calformal
dev.off()

# ggsave png here if needed
