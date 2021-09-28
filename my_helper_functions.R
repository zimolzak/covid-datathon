filename2df <- function(fname) {
    df = read.csv(here("data", "survey", fname), sep=",",
        stringsAsFactors = FALSE, na.strings="null")
    return(df)
}

say = function(s) {
	sL = c('\n', s, ' ----\n')
	cat(paste(sL, collapse=''))
}

firstchar2num = function(string) {
	return(
		case_when(
			substr(string, 1, 1) == "1" ~ 1,
			substr(string, 1, 1) == "2" ~ 2,
			substr(string, 1, 1) == "3" ~ 3,
			substr(string, 1, 1) == "4" ~ 4,
			substr(string, 1, 1) == "5" ~ 5
			# fixme - must be better way to do this but do it vectorized
		)
	)
}

truefalse = function(string) {
	return(
		case_when(
			string == "Yes" ~ 1,
			string == "No" ~ 0,
			string == "Checked" ~ 1,
			string == "Unchecked" ~ 0,
			string == "Complete" ~ 1,
			string == "Incomplete" ~ 0,
			string == "Partially" ~ 0.5,
			string == "Planned" ~ 0.5,
			string == "Yes - some of the team members"  ~ 0.5,
			string == "Yes - all of the team member" ~ 1
			# NA if no match
		)
	)
}

fix_teamsize = function(ts){
	return(
		case_when(
			ts == "05-Mar" ~ 4,  # Curse you, MS Excel.
			TRUE ~ as.numeric(ts)
		)
	)
}

gglikert = function(a) {
	 return(ggplot(survey_tidy, a) + geom_bar() + xlim(0.4, 5.6) + ylim(0,15))
}
