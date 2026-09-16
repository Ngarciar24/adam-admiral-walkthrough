# 4. BDS — ADLB

Program: `programs/02_adlb.R`. Sources: LB, ADSL. Result: 9,079 × 42 across
ALT, AST, BILI, CREAT, HGB.

## PARAMCD is not LBTESTCD
LBTESTCD is SDTM controlled terminology for the test performed; PARAMCD is an
analysis parameter defined by the SAP. They coincide here, which is why the
mapping goes through `metadata/adlb_params.csv` rather than a copy. They
diverge for derived parameters, or when one analyte in two units becomes two
PARAMCDs.

## Carry a subset of ADSL
TRTP/TRTA (BDS analysis treatment variables — distinct from period-level
TRT01P/TRT01A), TRTSDT, TRTEDT, SAFFL, ITTFL, AGEGR1, SEX. Not all 46 columns.

## Timing
`ADT` from LBDTC via `derive_vars_dt()`. `ADY` via `derive_vars_dy(reference_date
= TRTSDT)`: **no day zero** — first-dose day is Day 1, the day before is Day −1.

## The analysis value
`AVAL = LBSTRESN` (standardised), never LBORRES (as reported). `AVALC` matters
on 5 rows only: BILI "<3.42" below the limit of quantification, where AVAL is NA.

## Analysis visits
VISIT passthrough, all unscheduled visits bucketed as AVISIT = "UNSCHEDULED",
AVISITN = 999. Consequence: (USUBJID, PARAMCD, AVISIT) is not unique — 19 keys
cover 42 rows. Real studies use SAP-defined day windows plus DTYPE.

## ABLFL — the baseline flag
Rule adopted: last non-missing value on or before first dose.
```r
restrict_derivation(
  derivation = derive_var_extreme_flag,
  args = params(by_vars = exprs(STUDYID, USUBJID, PARAMCD),
                order = exprs(ADT, LBSEQ), new_var = ABLFL, mode = "last"),
  filter = !is.na(AVAL) & ADT <= TRTSDT
)
```
Ties broken by LBSEQ, explicitly — a baseline that changes when the input is
re-sorted is not reproducible. `restrict_derivation()` leaves other rows'
values untouched but **not their order** (it filters, derives, `bind_rows`),
hence the final `arrange()`.

## BASE, CHG, PCHG
`derive_var_base()` copies the ABLFL row's AVAL onto every row of the
subject/parameter, including pre-baseline rows. `derive_var_chg()` and
`derive_var_pchg()` take only the dataset. CHG is populated on the baseline row
(0) **and on 128 pre-baseline rows**; admiral's own template restricts to
`ADY > 0`. Either is defensible; it must be stated.

## Reference ranges
`ANRLO/ANRHI` from LBSTNRLO/LBSTNRHI. `derive_var_anrind()` → LOW/NORMAL/HIGH
(341/148/8,585; 5 NA). `BNRIND` via `derive_var_base(source_var = ANRIND)`.
`derive_var_shift()` → "NORMAL to HIGH"; its `missing_value` default is the
**string "NULL"**, so 5 rows read "NORMAL to NULL".

## ABLFL versus SDTM's LBBLFL
They disagree on 227 records: 120 of 121 ADaM-only flags are unscheduled
visits SDTM never flags; all 106 SDTM-only flags are SCREENING 1 results
superseded by a later pre-dose value. LBBLFL is what the lab considered
baseline; ABLFL is what this analysis uses. A test asserts the disagreement.
