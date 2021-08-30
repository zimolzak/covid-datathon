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

firstchar2num = function(string) {
	return(as.numeric(substr(string, 1, 1)))
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



#### Loading

survey_in = filename2df('BCMDatathonSurvey_DATA_2021-08-10.csv')

say('Dimensions of survey_in')
dim(survey_in)





#### Cleaning

survey_in %>%
rename(
id = Record.ID,
sid = Survey.Identifier,
time = Survey.Timestamp,
acadRank = Academic.rank,
acadRank.text = Other..specify,
dept.text = Primary.department.section.research.center,
years = Number.of.years.at.BCM,
prior.hack = I.participated.previously.in.a.hackathon.datathon,
prior.emrdata = I.had.experience.using.electronic.medical.record..EMR..data.for.research.or.quality.improvement.prior.to.this.datathon,
teamsize = Number.of.people.on.your.datathon.team,
workedTeam = Have.you.worked.with.the.members.of.your.datathon.team.before,
role.clinical = Your.role.on.your.team...mark.all.that.apply...choice.Content..clinical..expert.,
role.lead = Your.role.on.your.team...mark.all.that.apply...choice.Project.lead.,
role.reviewer = Your.role.on.your.team...mark.all.that.apply...choice.Chart.reviewer.,
role.datasci = Your.role.on.your.team...mark.all.that.apply...choice.Data.scientist.,
role.stats = Your.role.on.your.team...mark.all.that.apply...choice.Statistical.analyst.,
role.datawarehouse = Your.role.on.your.team...mark.all.that.apply...choice.Data.warehouse.expert.,
role.datamgr = Your.role.on.your.team...mark.all.that.apply...choice.Data.manager.,
role.learner = Your.role.on.your.team...mark.all.that.apply...choice.Learner.,
role.other = Your.role.on.your.team...mark.all.that.apply...choice.Other..please.specify..,
role.text = Other..specify.1,
know.avail.pre = How.knowledgeable.were.you.about.data.availability.at.BCM.prior.to.the.datathon,
know.avail.post = How.knowledgeable.were.you.about.data.availability.at.BCM.following.the.datathon,
know.use.pre = How.knowledgeable.were.you.about.how.to.use.the.BCM.data.warehouse.prior.to.the.datathon,
know.use.post = How.knowledgeable.were.you.about.how.to.use.the.BCM.data.warehouse.following.the.datathon,
know.limit.pre = What.was.your.understanding.of.BCM.data.warehouse.limitations.prior.to.the.datathon,
know.limit.post = What.was.your.understanding.of.BCM.data.warehouse.limitations.following.the.datathon,
effortHrs = Estimate.how.many.person.hours.you..personally..spent.on.datathon.work..including.time.spent.with.BCM.information.technology..IT.,
itpercent = What.percentage.of.the.time.spent.on.datathon.work.was.with.BCM.IT,
hard.datapull = How.difficult.was.obtaining.information.from.the.BCM.Data.warehouse,
hard.datawork = How.difficult.was.working.with.BCM.Data.warehouse.files,
completed = I.completed.my.project.by.the.end.of.the.datathon,
answered = I.successfully.answered.my.clinical.question,
collab.outside = I.collaborated.with.individuals.outside.of.my.department,
collab.new = I.collaborated.with.new.individuals.as.part.of.the.datathon,
future.studies = I.plan.to.conduct.future.studies.using.data.from.the.BCM.warehouse,
pub.abstract = Our.team.datathon.project.resulted.in.a.scientific.abstract.for.a.local.or.national.meeting,
pub.paper = Our.team.datathon.project.resulted.in.a.publication,
valuable = Participating.in.the.BCM.datathon.was.a.valuable.experience,
future.datathon = I.would.participate.in.a.future.BCM.datathon,
comment.text = What.worked.well.or.didn.t.work..or.general.comments,
complete = Complete.
) %>%
mutate_at(vars(prior.hack, starts_with("role"), -role.text, completed, answered, collab.outside, collab.new, pub.abstract, pub.paper, complete, workedTeam), ~ truefalse(.)) -> survey_tidy

survey_tidy



#### Plots

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
