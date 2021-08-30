library(dplyr)
library(ggplot2)
library(here)




#### Functions

filename2df <- function(fname) {
    df = read.csv(here("data", "survey", fname), sep=",",
        stringsAsFactors = FALSE, na.strings="null")
    return(df)
}

say = function(s) {
	sL = c('\n', s, ' ----\n')
	cat(paste(sL, collapse=''))
}




#### Loading

survey_in = filename2df('BCMDatathonSurvey_DATA_2021-08-10.csv')

say('Dimensions of survey_in')
dim(survey_in)

survey_in




#### Cleaning (of dates, etc)






#### Plot

# qplot(analytic_data$los.days.n) +
# scale_x_log10() ->
# los_histogram




#### write to plot files

# say('\n\n----\n\nEnd of text output. Now plotting.')
# pdf(here("outputs", "Rplots_v6.pdf"))
# death_histogram
# los_histogram
# dev.off()

# ggsave png here if needed
