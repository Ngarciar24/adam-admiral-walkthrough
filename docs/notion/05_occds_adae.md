# 5. OCCDS — ADAE and treatment-emergent logic

Program: `programs/03_adae.R`. Sources: AE, ADSL. Result: 1,191 × 37.

## Dates — the only imputation this repo actually exercises
AESTDTC/AEENDTC are date-only. 26 AE start dates are partial and are imputed
with `derive_vars_dt(highest_imputation = "M")`; `ASTDTF` records which
component was imputed. Everywhere else in DM/EX/DS the dates are complete, so
the imputation code paths in ADSL never fire.

## TRTEMFL
Treatment-emergent = onset on or after first dose, and in a real study within
a SAP-specified window after last dose. `derive_var_trtemfl()` defaults are
datetime variables (ASTDTM, TRTSDTM); `end_window` defaults to **NULL**, and
setting it without `trt_end_date` errors. Result here: 1,122 of 1,191 events
are treatment-emergent; 217 subjects have at least one.

## The imputation-direction trap
Impute a partial start to the first of the month and it can fall before first
dose, losing a genuinely emergent event. Impute to the last and you can
manufacture one. The direction is an SAP decision and must be visible in the
`--DTF` flag.

## Occurrence flags
Tables count subjects, not events. AOCCFL (first event per subject), AOCCSFL
(first per system organ class), AOCCPFL (first per preferred term) make a
subject-count table a simple filter.

## MedDRA
AEDECOD is the Preferred Term, AEBODSYS the System Organ Class. MedDRA is
licensed and versioned; the version lives in define.xml; an upgrade can rename
or move terms and change counts. Studies fix the version in the SAP.

## Vaccines note
{admiralvaccine} is the pharmaverse extension for vaccine studies
(reactogenicity/e-diary datasets such as ADFACE, immunogenicity ADIS). Not
used here; worth naming for a vaccines role.
