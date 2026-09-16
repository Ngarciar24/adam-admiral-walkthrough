# 3. ADSL — derivation by derivation

Program: `programs/01_adsl.R`. Sources: DM, EX, DS. Result: 306 × 46.

## Start from DM, drop DOMAIN
DM is already one row per subject. DOMAIN is SDTM housekeeping with no ADaM use.

## Planned versus actual treatment
`TRT01P = ARM`, `TRT01A = ACTARM`. Efficacy is normally analysed as-randomised
(planned), safety as-treated (actual). In this study they differ for exactly 12
subjects (planned High Dose, actual Low Dose). Caveat: ARM is copied
unconditionally, so 52 screen failures carry `TRT01P = "Screen Failure"` — a
non-treatment value in a treatment variable. Sponsors differ on nulling it;
this repo relies on ITTFL/SAFFL to exclude them.

## Dates and imputation — the asymmetry to memorise
`derive_vars_dt()` defaults to `highest_imputation = "n"`: imputes **nothing**.
`derive_vars_dtm()` defaults to `"h"`: imputes **time** by default and writes a
`--TMF` flag. So EX start/end become `TRTSDTM`/`TRTEDTM` with 00:00:00 and
23:59:59 filled in — dates untouched, times invented and flagged. Permitted
`highest_imputation` values differ: dt allows Y/M/D/n; dtm allows Y/M/D/h/m/s/n.
Flag meanings: `--DTF` ∈ {D, M, Y} = highest date component imputed;
`--TMF` ∈ {H, M, S}. `ignore_seconds_flag = TRUE` (default since 1.4.0) **errors**
if the source carries seconds.

## First and last dose without a join
`derive_vars_merged(ex_ext, by_vars, filter_add = <valid dose>, order =
exprs(EXSTDTM, EXSEQ), mode = "first")`. It never changes the row count; a
`left_join` here yields 643 rows from 306. `order` + `mode` is the
deduplication rule, and omitting `order` on duplicated keys is a hard error.
The valid-dose filter is `EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT,
"PLACEBO"))` because placebo is dosed at 0 mg by design.

## Treatment duration
`derive_var_trtdurd()`: `TRTEDT - TRTSDT + 1`. Same-day start/stop is 1 day.

## Disposition
`derive_vars_dt(ds, DSSTDTC)` with no imputation (all 850 DSSTDTC are complete).
`RANDDT` from `DSDECOD == "RANDOMIZED"`. `EOSDT`, `EOSSTT`, `DCSREAS` from
`DSCAT == "DISPOSITION EVENT"` excluding SCREEN FAILURE. **DCSREAS must be null
for completers** — it is the reason for *discontinuation*; my first version
populated it with "COMPLETED" for 110 subjects and a review caught it.

## Population flags
`SAFFL` via `derive_var_merged_exist_flag()`, which is **three-valued**:
`true_value` (condition met ≥ once), `false_value` (in EX, never met),
`missing_value` (absent from EX). Both set to "N" so `filter(SAFFL == "N")`
works; leaving `missing_value` at NA makes 52 rows vanish silently from any
filter. `ITTFL` is hand-written (`ARMCD != "Scrnfail"`): admiral ships no
function because population flags are study-specific. Here SAFFL and ITTFL
are collinear (254/52) — every randomised subject was dosed.

## Age groups from a spec
Cut points read from `metadata/adsl_agegr1.csv` (AGE_LOW/AGE_HIGH), not
literals in `case_when`. Missing AGE yields NA, not the oldest band. Paired
`AGEGR1`/`AGEGR1N` because tables need a non-alphabetical sort.
