#        #         #         #         #         #         #         #        80
library(dplyr)
library(ggplot2)
library(here)
library(tidyr)

source(file=here("my_helper_functions.R"))
survey_in = filename2df('BCMDatathonSurvey_DATA_2021-08-10.csv')




#### Shorten long column names and tidy contents

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
workedTeam = Have.you.worked.with.the.members.of.your.datathon.team.before, # FIXME - camelcase
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
completed = I.completed.my.project.by.the.end.of.the.datathon,  # FIXME var name is too close to another
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
mutate_at(vars(prior.hack, starts_with("role"), -role.text, completed, answered, collab.outside, collab.new, pub.abstract, pub.paper, complete, workedTeam), ~ truefalse(.)) %>%
mutate_at(vars(prior.emrdata, starts_with("know"), starts_with("hard"), starts_with("future"), valuable), ~ firstchar2num(.)) %>%
mutate_at(vars(teamsize), ~ fix_teamsize(.)) %>%
mutate(n_roles = role.clinical + role.lead + role.reviewer + role.datasci +
	role.stats + role.datawarehouse + role.datamgr + role.learner + role.other) %>%
mutate_at(vars(acadRank), ~ case_when(. == "Other (e.g. Staff, Instructor)" ~ "Staff", TRUE ~ .)) -> survey_tidy
# Recoded as "staff" because I inspected all 7 and that's what they are.




#### Calculate new vars about pre/post questions

jitter_absolute = 0.02
jx = jitter_absolute
jy = 5 * jitter_absolute

survey_tidy %>%
gather(`know.use.pre`, `know.use.post`, key="prepost", value="know.use") %>%
mutate(numeric_prepost = case_when(prepost == "know.use.pre" ~ 0, prepost == "know.use.post" ~ 1),
	eps_x = runif(nrow(survey_tidy) * 2, -1 * jx, jx),
	eps_y = runif(nrow(survey_tidy) * 2, -1 * jy, jy)) -> use

survey_tidy %>%
gather(`know.avail.pre`, `know.avail.post`, key="prepost", value="know.avail") %>%
mutate(numeric_prepost = case_when(prepost == "know.avail.pre" ~ 0, prepost == "know.avail.post" ~ 1)) %>%
select(id, numeric_prepost, know.avail) -> available

survey_tidy %>%
gather(`know.limit.pre`, `know.limit.post`, key="prepost", value="know.limit") %>%
mutate(numeric_prepost = case_when(prepost == "know.limit.pre" ~ 0, prepost == "know.limit.post" ~ 1)) %>%
select(id, numeric_prepost, know.limit) -> limit

use %>%
full_join(available, by=c("id", "numeric_prepost")) %>%
full_join(limit, by=c("id", "numeric_prepost")) -> gathered_all




#### Calc vars pertaining to team role

decode_role = data.frame(
	long = c("Clinician", "Lead", "Chart rev.", "Statistics", "Data whse.", "Data mgr.", "Learner", "Data sci.", "Other"),
	abbr = c("clin", "lead", "rev", "stat", "dataware", "datamgr", "learn", "datasci", "other")
)

roles_ordered = decode_role$long[1:7]

survey_tidy %>%
summarise_at(vars(starts_with("role."), -role.text), sum) %>%
rename(clin=role.clinical, lead=role.lead, rev=role.reviewer, stat=role.stats,
	dataware=role.datawarehouse, datamgr=role.datamgr,
	learn=role.learner, , datasci=role.datasci, other=role.other) %>%
gather(`clin`, `lead`, `rev`, `stat`,
	`dataware`,	`datamgr`,
	`learn`, `datasci`, `other`, key="role", value="count") %>%
mutate(role_description = decode_role$long[decode_role$abbr == role]) -> role_count_toplot




#### Calculate fancy combinatorics about those who checked >1 role

# Slice out certain survey Qs & subjects.
# --> 11 people * 7 roles. Looks like:
# 0 0 0 1 0 1 0
# 1 0 1 1 0 0 0
# ...

survey_tidy %>%
filter(n_roles > 1) %>%
select(role.clinical, role.lead, role.reviewer, role.stats, role.datawarehouse,
	role.datamgr, role.learner, n_roles) %>%  # no datasci/other
rename(clin=role.clinical, lead=role.lead, rev=role.reviewer, stat=role.stats,
	dataware=role.datawarehouse, datamgr=role.datamgr,
	learn=role.learner) -> role_matrix

# Encode roles as 1..7
# -->
# 0 0 0 4 0 6 0
# 1 0 3 4 0 0 0
# ...
# Fixme - may be a less fragile way to do the 1..7 coding/decoding

role_matrix %>%
transmute(clin=clin, lead=lead*2, rev=rev*3, stat=stat*4, dataware=dataware*5,
	datamgr=datamgr*6, learn=learn*7) -> role_mat_multiplied

# Build up a data frame of role a:b pairings (edges)
# Each subject has (7 Choose 2 = 7*6/2) = 21 edges. (Complete graph K_7)
# Therefore for 11 Ss, this is 11*21 rows. 231 row * 2 col data.frame like:
# 0 2
# 0 3
# 2 3
# ...one row per possible role pairing in K_7, for each person.
# Zeroes mean they didn't check that role.
# Fixme - could I do this cleaner with a gather() instead?

role_ab_cooccur = data.frame(a=numeric(), b=numeric())  # empty

for (i in 1:nrow(role_mat_multiplied)) {
	myrow = role_mat_multiplied[i,]
	data.frame(t(combn(myrow, 2))) %>%  # ea col comes out as list, not numeric
		transmute(a=as.numeric(X1), b=as.numeric(X2)) -> mycombos
	bind_rows(role_ab_cooccur, mycombos) -> role_ab_cooccur
}

# Filter to <<231 rows * still 2 col

role_ab_cooccur %>%
filter(a > 0) %>%
filter(b > 0) %>%
mutate(
	role_a = factor(roles_ordered[a], levels=roles_ordered),
	role_b = factor(roles_ordered[b], levels=roles_ordered)
) -> role_ab_cooccur

# Build a MAX 21-row table with columns a,b,n (no a,b dups)
# where a in 1..7, b in 1..7
# and where n is the count (value of LU triangle matrix element M_{a,b}, a<b)
# n ALWAYS > 0 (in other words, skip all elements == 0).
# More useful for printing than heatmap plotting.

role_ab_cooccur %>%
group_by(a,  b, role_a, role_b) %>%
summarise(Count = n()) %>%
arrange(desc(Count)) -> unrolled_heatmap

zero_heatmap = data.frame(a=numeric(), b=numeric(),
	role_a = factor(, levels=roles_ordered),
	role_b = factor(, levels=roles_ordered),
	Count = numeric())

for (i in 1:length(roles_ordered)) {
	for (j in 1:length(roles_ordered)) {
		myrow = data.frame(a=i, b=j,
			role_a = factor(roles_ordered[i], levels=roles_ordered),
			role_b = factor(roles_ordered[j], levels=roles_ordered),
			Count = 0
		)
		if (i < j) {  # force triangular matrix
			bind_rows(zero_heatmap, myrow) -> zero_heatmap
		}
	}
}

# build exactly 21-row df with n=whatever but now allowed to be 0.
# Useful for plotting heatmap now.

bind_rows(zero_heatmap, unrolled_heatmap) %>%
group_by(a, b, role_a, role_b) %>%
summarise(Count = sum(Count)) -> zero_filled_heatmap




#### Tables, Univariate

say('Dimensions of survey_in')
dim(survey_in)

# FIXME - put percentages for some categories!!
say("TABLES")
cat("\nacadRank:"); table(survey_tidy$acadRank)  ## FIG ??
cat("\nprior.hack:"); table(survey_tidy$prior.hack)
cat("\nworkedTeam:"); table(survey_tidy$workedTeam)
cat("\ncompleted:"); table(survey_tidy$completed)
cat("\nanswered:"); table(survey_tidy$answered)
cat("\ncollab.outside:"); table(survey_tidy$collab.outside)
cat("\ncollab.new:"); table(survey_tidy$collab.new)
cat("\npub.abstract:"); table(survey_tidy$pub.abstract)
cat("\npub.paper:"); table(survey_tidy$pub.paper)
cat("\ncomplete:"); table(survey_tidy$complete)

say("role_count_toplot")
role_count_toplot

say("Free text ranks (other)")
survey_tidy %>%
filter(acadRank == "Staff") %>%
select(acadRank.text)




#### Tables, role co-occurence

say("Multiple team roles (role_matrix)")
role_matrix

say("unrolled heatmap")
unrolled_heatmap




#### Plots (univar)

qplot(survey_tidy$years, binwidth=2) +
	labs(x="Number of years at BCM") +
	xlim(0,NA) -> uni_yrs
qplot(survey_tidy$teamsize, binwidth=2) +
	labs(x="Number of people on datathon team") +
	xlim(0,NA) -> uni_teamsize
qplot(survey_tidy$effortHrs, binwidth=10) + labs(x="Person-hours worked on datathon") -> uni_effortHrs
qplot(survey_tidy$itpercent, binwidth=10) + labs(x="Percentage of time spent with BCM IT") -> uni_itpercent
# fixme - maybe? Calculate split into IT plus my hours, plot as stacked.
qplot(survey_tidy$n_roles, binwidth=1) + labs(x="Number of roles per participant") -> uni_nroles

gglikert(aes(x = hard.datapull)) + labs(x="How difficult was obtaining data?") -> uni_hard.datapull
gglikert(aes(x = hard.datawork)) + labs(x="How difficult was working with data?") -> uni_hard.datawork
gglikert(aes(x = valuable)) + labs(x="Participating was a valuable experience.") -> uni_valuable  ## FIG 2
gglikert(aes(x = future.datathon)) + labs(x="I would participate in future datathon.") -> uni_future.datathon  ## FIG 3
gglikert(aes(x = prior.emrdata)) + labs(x="I had experience using EMR data.") -> uni_prior.emrdata  ## FIG 1
gglikert(aes(x = future.studies)) + labs(x="I plan to conduct future studies using BCM DW.") -> uni_future.studies  ## FIG 4

qplot(factor(survey_tidy$acadRank,
	levels=c("Student", "Fellow", "Staff", "Assistant", "Associate", "Full"))) +
	labs(title="Distribution of academic rank", x="Academic rank") -> acadRankPlot

ggplot(role_count_toplot, aes(role_description, count)) +
	geom_col() +
	labs(x="Role on team") -> barmaybe

ggplot(zero_filled_heatmap, aes(role_a, role_b, label = Count)) +
	geom_tile(aes(fill = Count)) +
	geom_label() +
	labs(
		title="Co-occurrence of roles, for those with >1 role",
		x="Role A",
		y="Role B"
	) -> multi_role_heatmap




#### Paired pre/post hypoth tests and plots

w_use = wilcox.test(
    survey_tidy$know.use.pre,
    survey_tidy$know.use.post,
    paired = TRUE, conf.int=TRUE, exact=FALSE  # "exact = FALSE" because can't do exact w/ zeroes or ties
)

w_avail = wilcox.test(
	survey_tidy$know.avail.pre,
    survey_tidy$know.avail.post,
    paired = TRUE, conf.int=TRUE, exact=FALSE
)

w_limit = wilcox.test(
	survey_tidy$know.limit.pre,
    survey_tidy$know.limit.post,
    paired = TRUE, conf.int=TRUE, exact=FALSE
)

c_use = gathered2chitrend(use %>% rename(score = know.use))
t_use = t.test(survey_tidy$know.use.pre, survey_tidy$know.use.post, paired = TRUE)

c_avail = gathered2chitrend(available %>% rename(score = know.avail))
t_avail = t.test(survey_tidy$know.avail.pre, survey_tidy$know.avail.post, paired = TRUE)

c_limit = gathered2chitrend(limit %>% rename(score = know.limit))
t_limit = t.test(survey_tidy$know.limit.pre, survey_tidy$know.limit.post, paired = TRUE)

say("Knowl about how to use DW")
w_use
c_use
t_use
say("Knowl about dat avail")
w_avail
c_avail
t_avail
say("Underst of DW limitations")
w_limit
c_limit
t_limit

## FIG 5
ggplot(gathered_all, aes(x = numeric_prepost + eps_x, y = know.use + eps_y, group = id)) +
 geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(0,1), labels = c("Pre", "Post")) +
  labs(title="Knowledge about how to use the data warehouse", y="Likert", x="Time",
      subtitle= htests_to_subtitle(w_use, c_use, t_use)) -> paired1

## FIG 6
ggplot(gathered_all, aes(x = numeric_prepost + eps_x, y = know.avail + eps_y, group = id)) +
 geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(0,1), labels = c("Pre", "Post")) +
  labs(title="Knowledge about data availability", y="Likert", x="Time",
      subtitle= htests_to_subtitle(w_avail, c_avail, t_avail)) -> paired2

## FIG 7
ggplot(gathered_all, aes(x = numeric_prepost + eps_x, y = know.limit + eps_y, group = id)) +
 geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(0,1), labels = c("Pre", "Post")) +
  labs(title="Understanding of data warehouse limitations", y="Likert", x="Time",
      subtitle= htests_to_subtitle(w_limit, c_limit, t_limit)) -> paired3

# fixme - candidate strata: years, effort, prior.emrdata,




#### Write to plot files

say('End of text output. Now plotting.')
pdf(here("outputs", "Rplots_survey.pdf"))
uni_yrs
acadRankPlot
uni_teamsize
barmaybe
uni_nroles
multi_role_heatmap
uni_effortHrs
uni_itpercent
uni_hard.datapull
uni_hard.datawork
uni_valuable
uni_future.datathon
uni_prior.emrdata
uni_future.studies
paired1
paired2
paired3
dev.off()

ggsave(here('pngs-conf', 'amia-fig1-priorexp.png'), uni_prior.emrdata)
ggsave(here('pngs-conf', 'amia-fig2-valuable.png'), uni_valuable)
ggsave(here('pngs-conf', 'amia-fig3-futurethon.png'), uni_future.datathon)
ggsave(here('pngs-conf', 'amia-fig4-futurestud.png'), uni_future.studies)
ggsave(here('pngs-conf', 'amia-fig5-pair1.png'), paired1)
ggsave(here('pngs-conf', 'amia-fig6-pair2.png'), paired2)
ggsave(here('pngs-conf', 'amia-fig7-pair3.png'), paired3)
