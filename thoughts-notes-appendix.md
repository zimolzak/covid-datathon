PowerBI
========

Remember to create *measures.* You need them for Pivot later. Click on "All apps" $\to$ powerBI. "Shared with me." Dot dot dot $\to$ Analyze in Excel. No SQL. Desktop is preferable to browser? Share with me the reports like `BCM COVID-19`.

Two ways to connect to stuff:

1. Get data: More... Database. Oracle. Deprecated: OK. Server?
2. File: Open report.

Trying out
--------

Access works thru browser. Not thru PowerBI Desktop. Downloading a
`.odc` file works. Connects to local Excel appropriately. Sometimes
slow. Note this was Excel connection to "BCM COVID-19 MAP" file. Usual
issues with field names and interpretability. Dictionary in PowerBI
report doesn't have a lot of info. What does it mean when lab is
"CPL"? I *infer* that this is Clinical Pathology Laboratories,
headquarters in Austin, TX.

In Excel sheet, why does field `Calendar.Date` seem to have one row
per day starting on 2017-07-01? Why subset and calculated fields?
Especially within `CA_COVID_19` table. Fields such as: % of Pat Test
Pos 60, CNT LAB STATUS, Total Denominator, Total Positive Results Age,
etc. I guess this is a view or otherwise calculated table of outputs?
Then within `CA_COVID_19` table, under Other Fields, there is a field
for Negative, Positive, ORDER_STATUS, ORDER_STATUS_C, Positive, LINE,
many others.

Walkthrough on May 20
--------

If you do have data, click "get data". dataset shared w/i box. Open it. CA covid data [table] click on "..." $\to$ allows you to "edit query." How to see server: applied steps $\to$ source $\to$ gear. You'll see that there's a SAP HANA database. There's a server but you can't connect to it and pull yet. (close and cancel out). Another thing: get data $\to$ power bi datasets.

How to get access to a shared workspace. your name $\to$ powerBI service. Then left hand side "Workspaces" and "my workspace". Down arrow, create workspace. Files $\to$ Get. Local file. Browse for the `.pbix` file. (Note, this is all in the browser). View Dataset.

Within desktop, can click Publish to a workspace (online).

Fields
--------

appt time, BP, date, ICD 10 list is kind of funny (or not extremely), dx_name, last result "negative", ord_value, provider, mrn, prbl_dx_name (is this like dx_name), prbl icd10, prbl noted date, resulting_agency, zip, `positive`, `negative`. The End.

"last result" is what gets received from lab. Whereas `ORD_Value` means...?

Becomes clear that rows are very duplicated (not really duplicated, but "pre joined.")




APPENDIX: PHONE CALLS AND NOTES
========

May 11
========

Me, chris, Gloria, Vamshi.

Intro/Ideas
--------

COVID outcomes as func of race. Adj for obesity, DM. i2b2 in place?
Feasibility. Epic EHRN. ehrn.org. Structure: bslmc and baylor med. focus on QI and pop health.

**Pop health**: Chris amos. Big dat for biobank? vs delete? Imaging
neck CT or thyroid. **Baylor med**: Dr murphy. htn, mammog rates.
**BSLMC**: Dr Bala & Petersen. GETA vs other anesth for valve replace?
warning score for decompensation.

As baylor gets to LHS vision. Figure out what are problems, barriers,
weaknesses in dataset. Learn what data resides where. There is data
that is produced that we don't really use. BSLMC data last refreshed 6 mo ago.

Timeline
--------

4. april feasibility use case
5. may marketing materials, end of may feasibility testing
6. june do the marketing
7. june july gather proposals
8. july aug review and choose
9. sept prepare the data
10. oct datathon event.

Future
--------

Mapping ready for the event. Self serve if you don't pass selection.
Two to three day October event. i2b2 may be good way to address COVID
+ baylor clin questions. What is the reg environment? We put together
cohort. Maybe I and Chris do IRB? Maybe self serv gets....

To do
--------

- Think about what we need, think about what use cases
- Review website
- Matching? probably look up papers.
- Get proto from chris
- Get Javad's slides from Gloria
- EAM re RedCap

May 13
========

Vamshi: Epic care everywhere says Vamshi. GHH = gtr houston health
connect has an ID, but they charge. Epic has some mapping too. Likes
first two bullet points. Start on Baylor Medicine. Table the matching
for now. But do commit to go back and look at BSLMC.

Chris says antigen testing coming too. Chris likes labs too. Likes
inpatient. (Sicker people, more cost.)

Gloria: ED is outpatient, please note. Where do ED patients go?
Inpatient, obs, home, other facility, ICU, OR. Building "CER rules"
via Epic. Tells you whether they're seen at other hospitals. Seems to
be far along. As long as they're (the other facility) is using Epic.

Rory Laubscher: problem list. Then next to it, list the count of
encounter diagnoses.

Subgroup w/ me, Rory, Gloria

May 21
========

lee leiber, me, ritu patel, jessica davila, erin blair, laura
petersen, vamshi, chris amos, julia aiken, ashok, Laurie ?, Udit Banerjee.

Test cases
--------

1. baylor med, dr murphy. HTN control. Mammography screening rates.
2. BSLMC use cases. Dr Bala & Petersen. TAVR and Warning score. Aanand involved.
3. Pop health: neck CT or thyroid; and COVID.

Timeline
---------

Be prepared to do it virtually. Zoom supports breakout. We have webex and that supports it too.

Marketing
--------

What is the thing that we are going to advertise? Probably advert as
virtual. Target or not? No funding for the patient matching. Workgroup
on the messaging. Faculty senate involvement too.

May 22
========

Take-aways

1. We (COVID) are far along.

2. Not shared mental model. There are projects with very narrow (which
is good). But yet we're recruiting people. And selecting them. Med
student recruitment sounds like a good idea. Slight concern that in
October people will not be equally distributed among the 6 main
projects. Maybe that is OK? If I'm a person with experience applying
random forest to protein mass spec data, and I want to use analytic
skills for clinical project, I might gravitate to HTN, COVID, or
mammography, but not TAVR? I'm used to one flavor of hackathon. New
flavors are OK but this one seems to oscillate between two flavors,
which may not work.

**Rory:** Generally I don't recommend *direct* Clarity access /
Oracle. First, need to know Epic fundamentals class. Need familiarity
with Epic model. Need some institutional knowledge. "Just give me the
data model and we won't need you" is not true, even if you have deep
Oracle experience. Fact is, you may lack *Epic* experience. In other
words, it's not only SQL skills, and it's not only Oracle skills.

Biggie fields in Clarity: `PAT_ID, MRN` (can be weird strings) `PAT_NAME,
STATUS_C_ID, SEX_C_ID, PRBL_BEGIN_DATE, PRBL_DX_NAME`. `STATUS_C` is living
or dead. `BP_S, BP_D, APPT_TIME, APPT_STATUS_C_ID, EPIC_DEPT, PROV_NAME,
PROV_TYPE, STAFF_RESOURCE, ORDER_PROC_ID, ORDERING_DATE, PROC_NAME,
PROC_CODE, COMPONENT_ID, ORD_VALUE, LAB`. `ORD_VALUE` is kind of the money
when it comes to lab results. It's a varchar. There is also `ORD_NUM`
which is legit numeric.

Biggie tables:

    clarity.clarity_edg (diagnoses, INI)
    clarity.clarity_dep
    clarity.order_proc
    clarity.clarity_eap (procedure info)
    clarity.order_results
    clarity.clarity_component (breakdown of a CMP etc)
    clarity.pat_enc
    clarity.problem_list
    clarity.zc_disp_enc_type (name of the encounter like Office Visit)
    clarity.clarity_ser (big one "Doctor information")

*Where does PAT_ID come from?* It's Baylor specific, to start, but I
(andy) am still not sure how the cross-institution (Harris Health
e.g.) matching/linkage is done.

Looks like about 14 `JOIN`s, wow.

Big point of PowerBI is that it **refreshes**. Kind of like access to
Clarity, without *direct* access to Clarity.


Matching
========

Technical term: patient identity management (PIM). Commercial software
options, such as Link Plus and The Link King, which apply
probabilistic algorithms.[^book] Open-source product (Febrl). Gender,
birth date, and ZIP code are an example.[^naess] Febrl = Freely
extensible biomedical record linkage.

[^book]: Gliklich RE, Dreyer NA, Leavy MB, editors. Registries for
Evaluating Patient Outcomes: A User's Guide. 3rd edition. Rockville
(MD): Agency for Healthcare Research and Quality (US); 2014 Apr.
https://www.ncbi.nlm.nih.gov/books/NBK208618/

[^naess]: Naessens JM, Visscher SL, Peterson SM, et al. Incorporating
the Last Four Digits of Social Security Numbers Substantially Improves
Linking Patient Data from De-identified Hospital Claims Databases.
Health Serv Res. 2015;50 Suppl 1(Suppl 1):1339-1350. pmid 26073819.

