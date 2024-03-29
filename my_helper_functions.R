filename2df <- function(fname) {
    df = read.csv(here("data", "survey", fname), sep=",",
        stringsAsFactors = FALSE, na.strings="null")
    return(df)
}

say = function(s) {
	sL = c('\n\n\n\n# ', s, '\n\n')
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

htests_to_subtitle = function(wt, ct, tt){
	decimals = 6
	wp = round(wt$p.value, decimals)
	cp = round(ct$p.value, decimals)
	tp = round(tt$p.value, decimals)
	return(paste('Wilco sgn-rank p =', wp,
		'; Chi trend p =', cp,
		'; Paired T test p =', tp)
	)
}

gathered2chitrend = function(df){
	# expects you to rename "know.use" etc. to "score" which contains the 1-5 scores
	df %>%
	group_by(numeric_prepost, score) %>%
	summarise(count = n()) %>%
	spread(numeric_prepost, count) %>%
	filter(!is.na(score)) %>%
	mutate(total_trials = `0` + `1`) -> table

	return(prop.trend.test(table$`0`, table$total_trials))
}

gfontsize <- function(x) {
	return(
		theme(
		    plot.title = element_text(size = x),
		    axis.title = element_text(size = x),
			axis.text = element_text(size = x)
		)
	)
}

table_pct = function(df_column) {
	c = table(df_column)
	p = round(prop.table(c) * 100, 1)
	Count = addmargins(c)
	Percent = addmargins(p)
	result = t(rbind(Count, Percent))
	return(result)
}
