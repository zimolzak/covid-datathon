Thoughts for COVID Datathon
=======

Andrew Zimolzak, MD, MMSc. August 2020.

Datathon use case examples
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




Things teams may want for October datathon
==========

- raw data
- un-joined?
- data dictionary, but even more thorough.
- What formats provide? Poss Excel and pipe-delim both?

Regarding flowsheet, if BSLMC is anything like outpatient, there is a ton of stuff in there. I would be fine with we select only the lines referring to pulse ox (to begin with). I would guess this yields a much more manageable data set (between 10x and 100x smaller).

People are undoubtedly going to want other vital signs in the future (temp, pulse rate, resp rate, and blood pressure) but I don't strictly need these yet. Other stuff within the flow sheet is "tier 3" at best.

Oh, the CONTACT_DATE field in that screenshot reminds me of another issue: Generally I do not need pulse ox data that is widely separated in time from the COVID result and/or hospital admission. Possible approaches to this issue:

1. Don't limit at all (results in bulkier data sets)

2. Simple limiting by date (no pulse ox before 2020-01-01)

3. Fancy limiting like "grab all pulse ox during admission, plus 7 days before admission, plus 7 days after admission, plus 7 days before COVID test plus 7 days after COVID test"

Option 2 may be the most practical, but I'm open to suggestions.

I can pretty confidently say that I have no interest (for the purposes of this COVID project) in any pulse ox data point before 2020-01-01. Diagnoses are another matter. Concrete example: If the patient had 6 admissions with discharge diagnosis = heart failure in 2018 and 2019, I would want to know about it (eventually—can be in a later data pull). However, it doesn't matter as much that she had 200 pulse ox measurements ranging from 81 to 97 in the years 2018–2019.




Abbreviations
========

PPE, personal protective equipment. LOS, length of stay. COPD, chronic
obstructive pulmonary disease. DM, diabetes mellitus. HTN,
hypertension. cmp, comprehensive metabolic profile. cbc, complete
blood count. bmp, basic metabolic panel. crp, C-reactive protein. Inp,
inpatient. Out, outpatient. Int, intermediate. Adva, advanced.