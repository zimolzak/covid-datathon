Datathon: Predictors of severe COVID-19 outcomes
========

1. Characterize the BCM experience with COVID, including
hospitalization rate by comorbidity, and ICU utilization.

2. Train a multivariable predictive model for severity of COVID (ICU
admission, incidence of ARDS criteria, length of stay, and in-hospital
mortality) as a function of known and novel factors such as
comorbidity.

3. Study the covariation in severe outcomes with treatment modalities
to evaluate population-level changes.

The file you probably want
--------

`bslmc_v6.R`

Analytic data set sketch
--------

* one row per ED encounter
    * patient ID of course
    * ER visit diagnoses
    * admission diagnoses, if admitted
    * covid test date(s) & results
    * ER visit index date
* outcomes as follows:
    * admitted yes/no
    * ER directly to ICU yes/no
    * length of stay, if admitted (continuous)
    * pao2:fio2 ratio (future summary measure, of oxygenation)
    * mortality yes/no (and date)
    * intubated yes/no (and date)
    * maybe future ICU admit & date if I can manage it
* predictors as follows
    * demographics
        * age
        * sex
        * race
        * ethnicity
        * ZIP
    * comorbidities (two columns for each: N prior visits or rate, and prob list yes/no)
        * diabetes
        * copd
        * asthma
        * hypertension
        * coronary disease
        * cancer
    * number of prior hospital admissions (or rate)
    * number of prior ER visits (or rate)
    * vitals (summary meas if needed)
        * temp
        * pulse
        * respirations
        * BP
        * SpO2
        * height
        * weight
    * labs (summary measure of labs just before/on index date)
        * wbc
        * hgb
        * plt
        * sodium
        * K
        * bicarb
        * creatinine
        * d-dimer
        * CRP
        * LDH
        * BUN
        * HDL
        * direct bilirubin
        * RDW
        * albumin
        * neutrophils
        * lymphocytes
        * ALT
        * P:F ratio can be predictor for more "hard" downstream events

Data pull spec
--------

Include: anyone with *positive* covid test. BSLMC and BCM outpatient.
If more than 10,000 patients, OK to randomly sample. (Seems to be 1100
to 1800). 700 ish at office visit in person.

Tables (inpatient/BSLMC):

- PAT_ID

- ENC_DX including distant past

- PROBLEM including distant past

- ORDER_RESULTS only need recent dates like 2020

- HSP, including all types of visits (ER, inpatient, etc.). OK to
  limit to 2020 only, but not necessary.

- possibly flowsheet: height weight systolic diastolic pulse
  respirations spo2. limit to intubated or other o2 params, and spo2
  po2.

- discharge disposition (looking for discharge disposition)

Tables (outpatient):

- TBD

Where to look in this code
--------

High value areas would be `bslmc_v4_DataSets_pipe.R` and `bslmc_v6.R` for inpatient
data and `analysis_outpat.R` for outpatient. Pay the most attention to
variables & values mentioned within `select()` and `filter()`
statements to get a sense of how the data is structured.

Requirements for current repo
--------

- R, Rscript
- R packages: dplyr, ggplot2, tidyr, lubridate, here, earth, ROCR
- make and usual UNIX-like toolchain (mv, rm, cp)
- pandoc (only for documentation)


Datathon "alpha phase" use case examples
========

|ID| Hard? | Waiting on:   | Description                                                 |
|--|-------|---------------|-------------------------------------------------------------|
|1 | Easy  | **Done**      | Test volume, positive tests, by date. **Foundational.**     |
|2 | Easy  | **Done**      | Count tests, positive tests, by comorbidity (see below).    |
|3 | Easy  | Andy          | Retest volume, likelihood of positivity. By clinic.         |
|4o| Int.  | **Done**      | Pulse oximetry (SpO2) by positive/negative.                 |
|4i| Int.  | **Done**/Rory | " " "                                                       |
|5 | Int.  | Andy          | Basic labs (see below) by positive/negative.                |
|6 | Adva. | Rory          | *I:* People who "touch" the chart (PPE estimate).           |
|7 | Adva. | Andy          | *I:* Rate of testing late in an admission, rate of positive.|
|8 | Adva. | Rory          | *I:* Basic descriptives (LOS, floor/hosp census by date).   |
|9 | Adva. | Andy          | *O:* Rt. of admission/ER (manual rev.?); risk factors.      |
|10| Adva. | Andy          | Anything to do with mortality.                              |

Capture what Baylor Medicine clinic it was sent from. Capture what lab
(vendor) it went to (LabCorp, or whoever), called "test perfomed by."
Most common "lab facility" are CPL and LabCorp. Slicer has the
*confirmed* and the *suspected* registries for COVID.

Define "comorbidity"
--------

1. The "big four": Asthma, COPD, DM, HTN.[^ehrn]

    a. Define first using just the problem list. This is "middle
    school level."

    b. Then define using encounters, "high school:" anyone with 2 or more
    encounters, from the beginning of time, is defined has having the
    diabetes (copd, htn, etc.) phenotype. Also note that we want to
    know the *value* of the counts: really the *distribution* of the
    count over patients. (100 patients have 2 COPD encounters, 10 have
    3 encounters, and 1 patient had 10 encounters, since beginning of
    the database, which means 5 years.) Final note: for big four, this
    depends only on the encounters, not on problem list. (A woman with 3
    COPD office visits in past year should be considered as having
    COPD according to this rule, even if she does not have it in the
    problem list.)

    c. Then meds, "college level." Requires increasingly more work.

    d. Then more fancy, such as moving beyond rules based phenotype
    definitions. Probably don't do this for purposes of this stage in
    the datathon.

2. Also want a broad view of all "medical history" items in problem
list (not just big four). *This is shown on one of Gloria's SlicerDicer
slides as a bar graph.*

Epic has notion of encounter for office, procedure, and rx. Gloria
knows someone for data quality of tobacco use.

Define "labs"
--------

Labs are called "procedure." Labs are of definite interest. Big ones
(values are interesting): cmp, cbc, flu, bmp, crp, d-dimer, ferritin. Within
those lab panels, the values I care most about are: sodium,
creatinine, and white blood cell count, detailed white cell
differential (neutrophils, lymphocytes, monocytes, eosinophils,
basophils).

In broader "non big-one" sense, I *do* want to know *whether* they
have the lab ordered (e.g. A1c, lipid panel: I don't care so much
about the exact value, but I want to know whether or not the patient
had the lab ordered/done). *This is shown on one of Gloria's
SlicerDicer slides.*

[^ehrn]: Epic Health Research Network, https://ehrn.org/
