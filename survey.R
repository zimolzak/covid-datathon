library(dplyr)
library(ggplot2)
library(here)




#### Functions

filename2df <- function(fn) {
    df = read.csv(here("data", "survey", fn), sep=",",
        stringsAsFactors = FALSE, na.strings="null")
    return(df)
}

say = function(s) {
	sL = c('\n', s, '----\n')
	cat(paste(sL, collapse=''))
}




#### Loading

sur = filename2df('BCMDatathonSurvey_DATA_2021-08-10.csv')

say('Dimensions of sur')
dim(sur)




#### Cleaning (of dates, etc)

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
