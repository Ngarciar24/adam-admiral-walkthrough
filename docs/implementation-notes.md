# Implementation notes

Rationale and checked facts behind the derivations in `programs/`. The programs
cite this file rather than repeating it. Counts refer to the CDISCPILOT01 data in
`{pharmaversesdtm}` as used here. The rules the analysis depends on are also
listed under "Choices made here" in the README.

## General

- **Blanks to NA.** SAS has no missing value for character variables, so an unset
  value arrives as `""`, which `is.na()` does not catch. `00_setup.R` converts
  blanks to `NA` for every SDTM domain. It changes nothing in `{pharmaversesdtm}`
  (already `NA`) but is needed as soon as the input is a SAS transport file.
- **Programs are sourced into the global environment** by `run_all.R`. admiral's
  `exprs()` returns expressions without an environment. A helper function called
  inside them (`format_eosstt()` in `01_adsl.R`) is therefore looked up
  globally, and sourcing into a local environment fails with "could not find
  function".
- **`restrict_derivation()` keeps every row but not the row order**: matching
  rows come back first. Programs sort explicitly at the end.
- **Merges use `derive_vars_merged()`**, which cannot change the number of rows.
  With several source records per subject it needs an `order`/`mode` pair and
  stops on duplicates instead of fanning out rows as a `left_join()` would.

## ADSL

- **Exposure dates.** `EXSTDTC`/`EXENDTC` are complete dates without time.
  `derive_vars_dtm()` imputes only the time: 00:00:00 for the start and 23:59:59
  for the end. `EXSTTMF`/`EXENTMF` flag the imputed times.
- **What counts as a dose:** `EXDOSE > 0`, or `EXDOSE = 0` on placebo (placebo is
  given as 0 mg). In this extract every EX record qualifies, so the placebo
  clause never has to distinguish anything.
- **SAFFL** comes from `derive_var_merged_exist_flag()`. It has three outcomes:
  condition true, subject in EX but the condition never true, and subject absent
  from EX. The last two are both set to `"N"`. All 52 `"N"` values come from
  subjects absent from EX (the screen failures).
- **ITTFL** means "randomised": planned arm not missing and not `Scrnfail`. It is
  keyed on the planned arm because ITT is analysed as randomised. admiral ships
  no ITT function, since population flags are study-specific.
- **TRT01P/TRT01A** are copied from `ARM`/`ACTARM`, so the 52 screen failures
  carry "Screen Failure". Planned and actual treatment differ for 12 subjects
  (planned high dose, actual low dose).
- **End of study.** Not derived for screen failures. `DCSREAS` is missing for
  subjects who completed.
- **Age groups.** Cut points and labels come from `metadata/adsl_agegr1.csv`, so
  changing a band means editing only the CSV. `AGEGR1N` sets the display order.
  AGE is complete (306/306, range 50–89).

## ADLB (BDS)

- **Parameters.** Five parameters, mapped through `metadata/adlb_params.csv`
  rather than assuming `PARAMCD = LBTESTCD`.
- **AVAL** is taken from `LBSTRESN` (the standardised result). `AVALC` adds
  information on only 5 rows: BILI results reported as "<3.42", which have no
  numeric value.
- **Visits.** All unscheduled visits share `AVISIT = "UNSCHEDULED"`
  (`AVISITN = 999`) and no visit windows are applied. As a result
  (`USUBJID`, `PARAMCD`, `AVISIT`) is not unique: 19 keys cover 42 rows. The
  unique keys are (`USUBJID`, `ASEQ`) and (`USUBJID`, `PARAMCD`, `ADT`,
  `LBSEQ`).
- **Baseline** (`R/derive_ablfl.R`): the last non-missing value on or before
  first dose, with ties broken by `LBSEQ`.
  - The study data contains no ties on `ADT`, so the tie-break is tested on
    hand-built data.
  - Only 1 of 1270 baseline records is dated on the day of first dose. EX has
    dates without times, so a same-day draw cannot be placed before or after
    the dose. LB does record times.
- **ABLFL compared with SDTM LBBLFL.** LBBLFL marks SCREENING 1, whereas ABLFL
  follows the rule above.
  - 121 records are flagged only in ADaM, and 106 only in SDTM.
  - 15 subject/parameter groups have no LBBLFL at all.
  - 85 groups would get a different BASE under the SDTM flag.
- **CHG/PCHG** are populated on every row that has both AVAL and BASE, which
  includes 128 rows dated before their own baseline record. admiral's template
  restricts them to post-baseline rows instead.
- **SHIFT1** uses admiral's default `missing_value = "NULL"`, so the 5 BILI rows
  without `ANRIND` read "NORMAL to NULL".
- **SAFFL/ITTFL** are "Y" on every ADLB row, because no screen failure has lab
  data.

## ADAE (OCCDS)

- **Source dates.** AESTDTC has 1165 complete dates, 15 year-month and 11
  year-only. AEENDTC has 718 complete and 473 missing, with no partial dates. No
  value carries a time.
- **ASTDT imputation.**
  - Missing day or month is imputed to the first (15 × `ASTDTF = "D"`,
    11 × `"M"`). A missing year is not imputed.
  - `min_dates = TRTSDT`: if first dose falls within the range a partial date
    allows, the date is imputed to first dose, so a possibly treatment-emergent
    event is not moved before treatment. This changes no record in this data.
  - Imputing to the last day instead of the first would change all 26 dates but
    no `TRTEMFL` value.
- **AENDT** is not imputed. Missing end dates belong to ongoing events.
- **Study days** are relative to `TRTSDT`, not copied from SDTM `AESTDY`/`AEENDY`
  (which are relative to `RFSTDTC`). On complete dates the two agree except for
  one record: 01-716-1063, AESEQ 1. Its onset equals both `RFSTDTC` and
  `TRTSDT`, so the study day is 1, but SDTM `AESTDY` says 366. This is an error
  in the source data. The extreme `ASTDY` of −13469 comes from a year-only
  date ("1977").
- **Duration.** `ADURN` = AENDT − ASTDT + 1 for 718 records (1–444 days). It is
  missing for the 473 ongoing events, so a mean duration is biased short. Four
  records with an imputed start date still get a duration.
- **TRTEMFL.** An event is treatment-emergent if its onset is on or after first
  dose and no later than 30 days after last dose.
  - The flag takes the values "Y" or missing, never "N".
  - 1122 events are "Y". The 69 others split into 65 with onset before first
    dose and 4 with onset more than 30 days after last dose.
  - Other windows give: no upper limit 1126; strictly on treatment 1086.
  - Date variables are passed to `derive_var_trtemfl()` explicitly, because its
    defaults expect datetimes.
- **Occurrence flags** (`AOCCFL`, `AOCCSFL`, `AOCCPFL`) mark the first
  treatment-emergent event per subject, per subject and SOC, and per subject,
  SOC and PT, so incidence tables count flagged rows.
- **ASEQ and AESEQ.** `ASEQ` follows onset order and `AESEQ` follows collection
  order. They differ on 339 of 1191 rows.
- **MedDRA.** `AEDECOD` is the preferred term and `AEBODSYS` the primary system
  organ class. In this data `AETERM` is identical to `AEDECOD` on every row, so
  no coding step is exercised.

## XPT export

- **Why XPT v5.** It is the transport format named in FDA's Study Data Technical
  Conformance Guide. Its fixed-width header fields set the limits: names ≤ 8
  characters, labels ≤ 40, character values ≤ 200 bytes.
- **How close this data comes to the limits.** Longest name 8/8, longest label
  39/40, longest character value 32/200, ADLB dataset label 40/40. No renaming
  was needed.
- **Specification.**
  - `metadata/adam_spec.csv` was first generated from the data, then curated:
    59 labels written, 2 labels corrected (`TRT01P`/`TRT01A`), and 2 lengths
    fixed for all-missing variables (`RFICDTC` 19, `ACTARMUD` 40).
  - Other character lengths are still the observed maxima.
  - Types and formats come from the R classes, so `xportr_type()` confirms that
    the data and spec agree but does not enforce types independently here.
  - Dates are type "numeric" with format `DATE9.`; xportr treats type "date" as
    character.
- **Dataset labels** are in a separate file (`metadata/adam_datasets.csv`).
  Passing the variable-level spec to `xportr_df_label()` produces a misleading
  "label longer than 40 characters" error.
- **Round trip.** Character `NA` is written as blank and read back as `""`, the
  expected loss. Declared lengths are read from the NAMESTR header records,
  because `read_xpt()` does not return them.
- **Checksums.** The file header carries a creation timestamp, so files are
  compared by content, not by checksum.

## Tables and figure

- **Display factors** are set in the table program, not in ADaM. rtables takes
  rows from factor levels, so an empty level still prints. `RACE = ASIAN` shows 0
  because its only 2 subjects are screen failures.
- **Table 1 percentages** use the non-missing count in the arm (the "n" row) as
  denominator, not the column N. The two are equal here because no demographic
  value is missing.
- **Table 2 row blocks.** There is no separate baseline visit: 230 ALT baselines
  are at SCREENING 1 and 24 at unscheduled visits. The 22 SCREENING 1 records
  that are not the baseline have a non-zero CHG, so Mean CHG at SCREENING 1 is
  0.1, not 0.
- **Table 2 column N** is overwritten with the number of subjects; rtables
  counts records by default.
- **Custom analysis function.** Table 2 uses its own function rather than
  `tern::summarize_change()`, which recomputes the change and expects baseline
  to be its own visit.
- **Figure 1** draws only visits with at least 10 subjects in every arm, and
  stops if a plotted mean differs from the Table 2 cell.
