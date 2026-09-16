# 2. The three ADaM structures

## ADSL — one row per subject
Always required. Holds properties of the subject, not of events: demographics,
planned and actual treatment (TRT01P, TRT01A), treatment start and end
(TRTSDT, TRTEDT), population flags (SAFFL, ITTFL), disposition. Every other
ADaM dataset merges a **subset** of ADSL onto itself, so treatment and
populations are defined once and can never disagree between two tables.
Includes screen failures: ADSL is one row per subject *enrolled*, not per
subject treated. Dropping them breaks disposition tables and the ITT principle.

## BDS — Basic Data Structure
One row per subject × parameter × analysis timepoint. The parameter is a tall
key: PARAMCD (≤ 8 characters) and PARAM (long label). The value is AVAL
(numeric) or AVALC (character). Timing: ADT, ADY, AVISIT, AVISITN. Baseline:
ABLFL flags the baseline row, BASE copies the baseline value onto every row of
that subject/parameter, CHG and PCHG follow. Same idea as a `pivot_longer`
table, with a fixed vocabulary. ADLB, ADVS, ADEG are BDS. So is ADTTE — its
AVAL is a time.

## OCCDS — Occurrence Data Structure
One row per occurrence of an event as recorded. **No AVAL, no PARAMCD.** The
analysis variables are coded terms (AEDECOD, AEBODSYS), dates (ASTDT, AENDT)
and flags (TRTEMFL, occurrence flags). ADAE is the canonical OCCDS dataset;
ADCM and ADMH share the shape. Analysis counts *subjects* with at least one
event, not rows — hence occurrence flags such as AOCCFL.

## A mistake I made at the start
I described ADAE as "a BDS dataset". It is not. BDS summarises a repeatedly
measured quantity; OCCDS counts events. "Change from baseline" is meaningless
for an adverse event. Getting this distinction wrong out loud would end an
interview badly.

## Numbers from this repo
| Dataset | Structure | Rows × cols | Grain |
|---|---|---|---|
| ADSL | subject-level | 306 × 46 | USUBJID |
| ADLB | BDS | 9,079 × 42 | USUBJID, PARAMCD, ADT, LBSEQ (and USUBJID, ASEQ) |
| ADAE | OCCDS | 1,191 × 37 | USUBJID, AESEQ (and USUBJID, ASEQ) |
