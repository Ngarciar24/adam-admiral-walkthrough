# Analysis Data Reviewer's Guide: CDISCPILOT01

Structure follows the PHUSE ADRG template. This is a portfolio project on
public test data, not a regulatory submission.

## 1. Introduction

### 1.1 Purpose

This guide gives a reviewer the context needed for the ADaM datasets in
`data/adam/`. It covers the sources, the analysis decisions that span datasets,
known differences from the published pilot analysis, and the conformance
checks performed. Variable-level definitions are in
[define.xml](../data/adam/define.xml) (readable view:
[define.html](../data/adam/define.html)).

### 1.2 Study data standards and dictionaries

| Item | Version |
|---|---|
| ADaM Implementation Guide | 1.3 (declared in define.xml) |
| Define-XML | 2.1 |
| Dataset format | SAS Transport v5 |
| Source SDTM | CDISC pilot SDTM as published in `{pharmaversesdtm}` 1.5.0 (DM, EX, DS, AE, LB, SV) and in `phuse-scripts` (QS) |
| MedDRA | As coded in the source SDTM (version not stated in the public data) |

### 1.3 Source data

- All datasets are derived from SDTM; no other data were used.
- QS is read from the published pilot SDTM at a pinned commit, with a
  SHA-256 check (`R/pilot_files.R`, `metadata/pilot_files.csv`).

## 2. Protocol description

- **Design:** randomised, double-blind, placebo-controlled, parallel group, 24
  weeks. Three arms: placebo, xanomeline 54 mg and xanomeline 81 mg.
- **Endpoints:** the primary efficacy endpoint analysed here is the ADAS-Cog(11)
  change from baseline at Week 24. Dermatologic adverse events are of special
  interest.
- **Sample:** 306 subjects screened, 254 randomised.

## 3. Analysis considerations across datasets

- **Core variables:** `STUDYID`, `USUBJID`, treatment, population flags and
  subject characteristics are merged from ADSL. The conformance check GEN-08
  confirms that every copied variable equals its ADSL value.
- **Treatment variables:**
  - efficacy analyses use planned treatment (`TRTP`/`TRTPN`);
  - safety analyses use actual treatment (`TRTA`/`TRTAN`);
  - 12 subjects randomised to 81 mg received 54 mg.
- **Populations:** ITT 254, safety 254, efficacy 234, Week 24 completers 118.
  Screen failures remain in ADSL with all flags "N".
- **Dates and imputation:**
  - partial adverse event start dates are imputed and flagged (`ASTDTF`);
  - end dates are not imputed;
  - last dose uses the end-of-study date when the last dose record has no end
    date (6 subjects).
- **Baseline and change:**
  - laboratory baseline is the last value on or before first dose;
  - ADAS-Cog baseline is the Baseline-window value;
  - change from baseline is derived for post-baseline records only.
- **Rules:** full definitions are in [sap.md](sap.md); technical notes are in
  [implementation-notes.md](implementation-notes.md).

## 4. Analysis data creation and processing

| Order | Program | Output |
|---|---|---|
| 1 | `programs/01_adsl.R` | ADSL |
| 2 | `programs/02_adlb.R` (uses `R/derive_ablfl.R`) | ADLB |
| 3 | `programs/03_adae.R` | ADAE |
| 4 | `programs/04_adtte.R` (from ADSL and ADAE) | ADTTE |
| 5 | `programs/05_adqsadas.R` | ADQSADAS |
| 6 | `programs/90_export_xpt.R` | `.xpt` files |
| 7 | `programs/94_define.R` | define.xml, define.html |

- `run_all.R` runs everything in this order, then the tables and figures, the
  comparison with the pilot and the test suite.
- There are no intermediate datasets; the `.rds` files are the same data as
  the `.xpt` files.

## 5. Analysis dataset descriptions

| Dataset | Class | Structure | Records | Key variables |
|---|---|---|---|---|
| ADSL | Subject level | One record per subject | 306 | STUDYID, USUBJID |
| ADLB | BDS | One record per subject, parameter and source record | 9,079 | USUBJID, PARAMCD, ADT, LBSEQ |
| ADAE | OCCDS | One record per adverse event | 1,191 | USUBJID, AESEQ |
| ADTTE | BDS | One record per subject and parameter | 254 | USUBJID, PARAMCD |
| ADQSADAS | BDS | One record per subject, parameter, analysis visit and date | 1,040 | USUBJID, PARAMCD, AVISITN, ADT, DTYPE |

- **ADLB:** five parameters (ALT, AST, bilirubin, creatinine, haemoglobin).
  Unscheduled visits are grouped, and no analysis windows are applied.
- **ADTTE:** time to first treatment-emergent dermatologic event. Event source
  is `ADAE.AOCC01FL`; censoring is at `RFENDT`. `SRCDOM`, `SRCVAR` and
  `SRCSEQ` trace each record to its source.
- **ADQSADAS:** ADAS-Cog(11) total.
  - Visit windows set `AWLO`, `AWHI`, `AWTARGET` and `AWTDIFF`.
  - `ANL01FL` marks the analysed record.
  - `DTYPE = "LOCF"` marks imputed Week 8/16/24 values (222 records).

## 6. Data conformance summary

| Check | Tool | Result |
|---|---|---|
| Define-XML 2.1 schema, internal references, required elements | `python/validate_define.py` | Pass |
| define.xml against the .xpt files (names, labels, types, lengths) | `python/validate_define.py` | Pass |
| XPT v5 limits and round trip | `programs/90_export_xpt.R` | Pass |
| ADaMIG conformance checks (71 checks) | `python/adam_conformance.py` | Pass, `outputs/qc/adam_conformance.txt` |
| Comparison with the published pilot ADaM (68 items) | `programs/95_compare_pilot.R` | 48 identical, 20 explained, 0 unexplained |
| Independent re-derivation in Python | `python/check_adam.py` | Pass |

- **CDISC CORE.** The open-source CDISC rules engine (CORE) publishes no ADaM
  rules yet; its current rules cover SDTMIG, SENDIG, TIG and USDM. The ADaM
  checks are therefore implemented explicitly in `python/adam_conformance.py`.
  Each check states the ADaMIG requirement it tests.
- **Known issues:**
  - `(USUBJID, PARAMCD, AVISIT)` is not unique in ADLB, because unscheduled
    visits are grouped.
  - MedDRA codes and version are not available in the public data.

### 6.1 Differences from the published pilot analysis

[`outputs/qc/pilot_comparison.txt`](../outputs/qc/pilot_comparison.txt) lists
every compared variable. The explained differences follow from four decisions:

1. **Actual treatment** comes from `DM.ACTARM` (pilot: actual = planned).
   Affects `TRT01A`, `TRTA` and `TRTAN` for 12 subjects.
2. **Year-only onset dates** are imputed and flagged (pilot: left missing).
   Affects 11 adverse events. Duration is kept when only the day was imputed
   (4 events).
3. **Laboratory baseline** is the last value before first dose (pilot: the
   SDTM baseline flag), and change is derived after first dose only. Affects
   `ABLFL`, `BASE`, `CHG` and `BNRIND`. `ANRIND` follows the normal range,
   while the pilot's range indicator codes values outside the range as normal.
4. **LOCF bookkeeping:** identical values, but the pilot gives some LOCF
   records the date of a later, non-analysed record and marks those records as
   LOCF too.

ADTTE matches the pilot for every subject except the 12 with different actual
treatment. Every analysed ADQSADAS value matches, and the primary ANCOVA
(Table 4) reproduces the published pilot result, as reproduced by the
[R Consortium submission pilot](https://rconsortium.github.io/submissions-pilot1/articles/tlf-primary.html).

## 7. Programs

| Program | Purpose |
|---|---|
| `programs/00_setup.R` | Load SDTM and metadata |
| `programs/01_adsl.R` … `05_adqsadas.R` | ADaM datasets |
| `programs/90_export_xpt.R` | Transport files and checks |
| `programs/91_tables.R` | Table 1 (demographics), Table 2 (ALT by visit), traceability trace |
| `programs/92_figures.R` | Figure 1 (ALT change), Figure 2 (Kaplan–Meier) |
| `programs/93_tables_tte_eff.R` | Table 3 (time to dermatologic event), Table 4 (ADAS-Cog ANCOVA) |
| `programs/94_define.R` | define.xml and define.html |
| `programs/95_compare_pilot.R` | Comparison with the published pilot ADaM |
| `tests/testthat/` | Dataset and unit tests (225 expectations) |
| `python/` | Independent checks |
