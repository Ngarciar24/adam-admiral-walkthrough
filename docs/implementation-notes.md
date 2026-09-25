# Implementation notes

Technical notes and checked facts behind the programs in `programs/`. The
analysis rules themselves are in [sap.md](sap.md); the reviewer-facing summary
is [adrg.md](adrg.md). Counts refer to the CDISCPILOT01 data used here.

## General

- **Sources.**
  - DM, EX, DS, AE, LB and SV come from `{pharmaversesdtm}`.
  - QS is not in that package. It is read from the published pilot SDTM, which
    covers the same study and subjects (all 306 match), using
    `R/pilot_files.R`: the file is pinned to one commit of `phuse-scripts`,
    checked against a SHA-256 and cached in `data/external/`. It is not
    committed, because the source repository states no licence.
- **Blanks to NA.** SAS has no missing value for character variables, so an
  unset value arrives as `""`. `00_setup.R` converts blanks to `NA` for every
  SDTM domain. This is a no-op on `{pharmaversesdtm}` and matters for QS, which
  is read from SAS transport.
- **Global environment.** `run_all.R` sources programs into the global
  environment because admiral's `exprs()` carries no environment. A helper
  called inside it (`format_eosstt()` in `01_adsl.R`) is looked up globally.
- **`restrict_derivation()`** keeps every row but not the row order, so
  programs sort explicitly at the end.
- **`derive_vars_merged()`** cannot change the number of rows. With several
  source records per subject it needs an `order`/`mode` pair and stops on
  duplicates, where a `left_join()` would silently multiply rows.

## ADSL

- **Exposure times.** `EXSTDTC`/`EXENDTC` are dates without time; only the time
  is imputed (00:00:00 start, 23:59:59 end) and flagged in `TRTSTMF`/`TRTETMF`.
- **Last dose.** `TRTEDT` is the end date of the last qualifying EX record. For
  6 subjects that record has no end date and the end-of-study date is used.
  This follows the published pilot definition and gives every dosed subject a
  last-dose date. The earlier rule (last *available* end date) had left 2
  dosed subjects without one and moved 4 adverse events out of the
  treatment-emergent window.
- **Treatment variables.**
  - `TRT01P`/`TRT01A` are missing for the 52 screen failures.
  - `TRT01PN`/`TRT01AN` are the daily dose in mg (`metadata/adsl_trt.csv`).
  - Planned and actual treatment differ for 12 subjects (randomised to high
    dose, received low dose).
- **Populations.**
  - `SAFFL`: at least one qualifying dose (`EXDOSE > 0`, or 0 mg on placebo).
    The three outcomes of `derive_var_merged_exist_flag()` (condition true;
    in EX but never true; absent from EX) are collapsed to Y/N.
  - `ITTFL`: randomised.
  - `EFFFL` (234 subjects): in the safety population with at least one
    post-baseline ADAS-Cog and one post-baseline CIBIC+ assessment.
  - `COMP24FL` (118 subjects): the Week 24 visit took place before the end of
    participation.
  - All four counts equal the published pilot ADSL.
- **Pooled site.** `SITEGR1` pools a site into "900" when any planned arm has
  fewer than 3 ITT subjects there. This reproduces the pilot's pooling exactly
  (7 sites, 31 subjects).
- **Age groups.** Cut points and labels come from `metadata/adsl_agegr1.csv`.

## ADLB (BDS)

- **Parameters.** Five parameters, mapped through `metadata/adlb_params.csv`.
- **AVAL** comes from `LBSTRESN`. `AVALC` adds information on only 5 rows: BILI
  values reported as "<3.42".
- **Visits.** Unscheduled visits share `AVISIT = "UNSCHEDULED"`
  (`AVISITN = 999`) and no windows are applied.
  - (`USUBJID`, `PARAMCD`, `AVISIT`) is therefore not unique.
  - The unique keys are (`USUBJID`, `ASEQ`) and (`USUBJID`, `PARAMCD`, `ADT`,
    `LBSEQ`).
- **Baseline** (`R/derive_ablfl.R`): the last non-missing value on or before
  first dose, with ties broken by `LBSEQ`.
  - The study data contains no ties, so the tie-break is tested on hand-built
    data.
  - EX has no dosing times, so a draw on the day of first dose counts as
    baseline. Only 1 of the 1270 baseline records is dated that day.
- **ABLFL compared with SDTM LBBLFL.** LBBLFL (SCREENING 1) is what the
  published pilot uses. It differs from ABLFL on 227 records.
  - 15 subject-parameters have no LBBLFL at all.
  - 85 would get a different BASE.
- **Post-baseline variables.** `CHG`, `PCHG` and `SHIFT1` are derived only for
  records after first dose (7,676 records), and are missing on the baseline
  record and on the 1,398 records on or before first dose.
  - `SHIFT1` is also missing when either range indicator is missing, rather
    than admiral's default string "NULL".

## ADAE (OCCDS)

- **Source dates.**
  - AESTDTC: 1,165 complete, 15 year-month and 11 year-only.
  - AEENDTC: 718 complete and 473 missing, with no partial dates.
- **ASTDT imputation.**
  - Missing day or month is imputed to the first (`ASTDTF` = D or M).
  - `min_dates = TRTSDT` imputes to first dose when the partial date allows it.
    This changes no record here.
  - The pilot does not impute year-only dates. That accounts for 11 of the
    differences in the pilot comparison.
- **Study days** are relative to `TRTSDT`, not copied from SDTM. SDTM `AESTDY`
  is wrong for one record: 01-716-1063 AESEQ 1 has onset on the day of first
  dose but `AESTDY` = 366.
- **TRTEMFL.** An event is treatment-emergent if its onset is on or after first
  dose and no later than 30 days after last dose. The flag is "Y" or missing.
  - 1,126 events are "Y". All 65 others started before first dose.
  - The 30-day limit excludes nothing in this study: without it the count is
    also 1,126, and with no allowance after last dose it is 1,091.
  - Date variables are passed to `derive_var_trtemfl()` explicitly, because its
    defaults expect datetimes.
- **CQ01NAM** is the pilot's customised query for dermatologic events (493
  events).
- **AOCC01FL** flags each subject's first treatment-emergent dermatologic event
  (152 subjects) and is the event source for ADTTE.
- **Occurrence flags.** `AOCCFL`, `AOCCSFL` and `AOCCPFL` mark the first
  treatment-emergent event per subject, per subject and SOC, and per subject,
  SOC and PT (218 subjects have at least one).
- **ASEQ and AESEQ.** `ASEQ` follows onset order and `AESEQ` collection order.
  They differ on 339 of 1,191 rows.
- **Duration.** `ADURN` is computed also when the start day was imputed. The 4
  such records (subject 01-716-1418) have no duration in the pilot.

## ADTTE (BDS)

- **Definition.** One `TTDE` record per safety subject: time from first dose to
  the first treatment-emergent dermatologic event, or censoring at the end of
  participation (`RFENDT`).
- **Derivation.** Built with `derive_param_tte()`, using an `event_source` on
  ADAE and a `censor_source` on ADSL. Same-day events are ordered by `AESEQ`,
  matching `AOCC01FL`.
- **Result.** 152 events and 102 censored. `ADT`, `AVAL`, `CNSR` and `SRCSEQ`
  equal the published pilot ADTTE for every subject.
- **Treatment.** ADTTE uses actual treatment. The 12 subjects with different
  actual treatment are the only difference from the pilot, which sets actual
  equal to planned.

## ADQSADAS (BDS)

- **Scope.** ADAS-Cog(11) total (`QSTESTCD = "ACTOT"`) for ITT subjects.
- **Windows** (`metadata/adqsadas_windows.csv`) are by study day:
  - Baseline ≤ 1
  - Week 8: 2–84
  - Week 16: 85–140
  - Week 24: ≥ 141
- **Record selection.** Within a window the record closest to the target day is
  analysed (`ANL01FL`); ties go to the later record. 24 observed records are
  not analysed.
- **LOCF.** A subject with a baseline value has one analysed record at each
  post-baseline visit.
  - A visit without one gets the last analysed value (`DTYPE = "LOCF"`, 222
    records).
  - That value is the baseline for the 19 subjects with no post-baseline
    assessment (57 records).
  - Carrying the baseline follows the pilot data.
- **Result.** All 1,016 analysed records equal the pilot ADQSADAS in `AVAL`,
  `BASE`, `CHG`, `ABLFL` and `DTYPE`. Two differences remain, both in LOCF
  bookkeeping, not in values:
  - For 33 LOCF records the pilot records the date of a later, non-analysed
    record.
  - The pilot marks those 19 non-analysed records as LOCF too.

## XPT export

- **Format.** SAS Transport v5, as named in FDA's Study Data Technical
  Conformance Guide.
  - Names ≤ 8 characters, labels ≤ 40, character values ≤ 200 bytes.
  - `ADQSADAS` uses all 8 characters allowed for a name.
- **Specification.** `metadata/adam_spec.csv` holds all 202 variables. It
  drives xportr (type, length, label, order, format) and define.xml (origin,
  source, method, codelist).
  - Lengths of the original variables are the curated observed maxima. Two
    all-missing variables have fixed widths: `RFICDTC` 19, `ACTARMUD` 40.
  - New variables have lengths set in the spec.
- **Dataset labels** are kept in a separate file, `metadata/adam_datasets.csv`.
  Passing the variable spec to `xportr_df_label()` gives a misleading "label
  longer than 40 characters" error.
- **Round trip.** Each file is read back and compared. Character `NA` becomes
  `""`, the expected loss. Declared lengths are read from the NAMESTR header
  records.
- **Checksums.** The header carries a creation timestamp, so files are
  compared by content, not checksum.
- **pandas zeros.** `pandas.read_sas()` decodes an exact zero as about
  5.4e-79. Both Python checks restore exact zeros; without that, a "never 0"
  check would test nothing.

## define.xml

- **Generation.** `94_define.R` writes Define-XML 2.1 with `{xml2}` from
  `metadata/`: datasets, variables, value-level metadata for ADLB `AVAL` by
  `PARAMCD`, 16 codelists, 92 derivation methods, and comments for assigned
  variables.
- **Validation.** `python/validate_define.py` checks the file against the CDISC
  Define-XML 2.1 schema (as shipped with `odmlib`) and resolves every internal
  reference.
  - It checks the elements the specification requires but the schema leaves
    optional: class, location, standard, structure and keys.
  - It compares every variable's name, label, type and length with the XPT
    files.
- **HTML view.** The CDISC stylesheet is not redistributable here, so
  `define.html` is rendered by the same program from the same metadata.

## Tables and figures

- **Display factors** are set in the table programs, not in ADaM. rtables takes
  rows from factor levels, so `RACE = ASIAN` prints as 0: its only 2 subjects
  are screen failures.
- **Table 1** percentages use the non-missing count in the arm as denominator.
- **Table 2** covers post-baseline records only.
  - Its column N is the number of subjects with a post-baseline ALT value (84,
    82, 80), not the record count rtables would print.
  - It uses a custom analysis function, because `tern::summarize_change()`
    recomputes the change and expects baseline to be its own visit.
- **Table 3 and Figure 2** use actual treatment: Kaplan–Meier medians with
  log-log confidence intervals, log-rank tests and Cox hazard ratios against
  placebo.
- **Table 4** is an ANCOVA of the Week 24 change with treatment, pooled site
  and baseline. LS means come from `{emmeans}`, and the dose-response test uses
  dose as a continuous term.
  - The results equal the published pilot table as reproduced by the
    [R Consortium submission pilot](https://rconsortium.github.io/submissions-pilot1/articles/tlf-primary.html):
    N = 79/81/74, differences vs placebo −0.5 (−2.1, 1.1) and −1.0 (−2.7, 0.7),
    p = 0.569 and 0.233, dose-response p = 0.245.
- **Figure 1** draws only visits with at least 10 subjects in every arm, and
  stops if a plotted mean differs from the Table 2 cell.
