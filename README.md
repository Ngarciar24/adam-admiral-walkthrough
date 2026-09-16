# Building ADaM datasets with {admiral} — a self-directed learning project

A one-day exercise in which I built ADaM analysis datasets from public CDISC test
data using the [pharmaverse](https://pharmaverse.org/) R toolchain, in order to
learn the CDISC standards hands-on.

**Please read the scope section before anything else.** This is a learning
project on public test data. It is not study work, it is not validated, and it
does not represent submission-ready output.

---

## What this is

| | |
|---|---|
| **Source data** | `{pharmaversesdtm}` 1.5.0 — the public CDISC pilot study `CDISCPILOT01`, shipped inside an R package. No proprietary or patient data of any kind. |
| **Built with** | `{admiral}` 1.5.0, R 4.6.1, dependencies pinned with `{renv}` |
| **Datasets produced** | ADSL (306 × 46), ADLB (9,079 × 42, BDS), ADAE (1,191 × 37, OCCDS) |
| **Tests** | 163 `testthat` expectations, all passing, run by `run_all.R` |
| **Outputs** | Two tables built with `{rtables}`/`{tern}`, an `.xpt` export via `{xportr}`, and a pandas script that re-verifies the export independently |
| **Time spent** | One day |

## How to run it

```r
renv::restore()
source("run_all.R")
```

`run_all.R` rebuilds every dataset from SDTM in dependency order, writes the
tables, and then runs the full test suite with `stop_on_failure = TRUE`.

## Course notes and self-check

`docs/notion/` holds thirteen chapter files written as course notes — the
concepts, the admiral-versus-dplyr differences, the honest caveats, an
interview Q&A and a predict-before-you-look self-check. They are formatted to
be imported into a knowledge base one page per chapter.

## Repository layout

```
programs/00_setup.R          load SDTM, convert blanks to NA, read metadata
programs/01_adsl.R           ADSL   -- subject level
programs/02_adlb.R           ADLB   -- BDS (one row per subject/parameter/timepoint)
programs/03_adae.R           ADAE   -- OCCDS (one row per adverse event)
programs/90_export_xpt.R     SAS Transport v5 export
programs/91_tables.R         demographics + change-from-baseline tables
docs/notion/                 course notes, one chapter per file
python/check_adlb.py         pandas re-check of the .xpt export -- a second language, by a different door
.github/workflows/tests.yml  CI: rebuild + tests on push (not yet run -- first run is on first push)
metadata/                    the miniature dataset specification (CSV)
tests/testthat/              163 expectations across the three datasets
outputs/                     rendered tables and the traceability trace
```

One program per dataset, run in a fixed documented order, is how a real study is
organised — not one monolithic script, and not an R package.

---

## The traceability chain

> **[WRITE THIS SECTION YOURSELF.]** This is the part an interviewer will ask you
> to explain, so it has to be in your own words. The material is below: a real
> chain, printed by `programs/91_tables.R` into
> `outputs/t2_traceability_chain.txt`. Explain in prose what each hop means and
> why the redundant columns are kept.

```
SDTM  LB     USUBJID = 01-701-1028   LBSEQ = 135
             LBTESTCD = ALT   VISIT = WEEK 8   LBDTC = 2013-09-10T09:13
             LBORRES  = 33 U/L   (as the local lab reported it)
             LBSTRESN = 33 U/L   (standardised -- this is what AVAL copies)

ADaM  ADLB   USUBJID = 01-701-1028   ASEQ = 5
             PARAMCD = ALT   AVISIT = WEEK 8   ADT = 2013-09-10
             AVAL = 33   BASE = 26   CHG = 7

             BASE came from this subject's own baseline row:
             ASEQ = 1   ABLFL = Y   AVISIT = SCREENING 1   AVAL = 26

RESULT       one of the 56 values averaged into
             Table 2 / WEEK 8 / Mean CHG / Xanomeline High Dose
```

Origins, as they would appear in define.xml:

| Variable | Origin | Rule |
|---|---|---|
| `AVAL` | Predecessor | `LB.LBSTRESN` |
| `ABLFL` | Derived | last non-missing record with `ADT <= TRTSDT` |
| `BASE` | Derived | `AVAL` where `ABLFL = "Y"`, within subject and parameter |
| `CHG` | Derived | `AVAL - BASE` |

`91_tables.R` performs no join and no window function. It filters and averages
columns that already exist. That is what "analysis-ready" means in the ADaM IG.

---

## Decisions a Statistical Analysis Plan would normally fix

There is no protocol and no SAP for the pilot data, so every choice below was
made by me. In a real study none of these would be a programmer's decision.

| Decision | What I chose | Alternatives |
|---|---|---|
| **Baseline** | Last non-missing value on or before first dose, ties broken by `LBSEQ` | First pre-dose value; mean of screening values; last value strictly before dose |
| **`SAFFL`** | At least one dose, counting 0 mg placebo as a dose | Some studies require a post-dose assessment as well |
| **`ITTFL`** | Randomised, i.e. `ARMCD != "Scrnfail"` | Definitions vary; mITT is common |
| **`TRTEMFL`** | Onset on or after first dose | The post-treatment window is always SAP-defined |
| **`CHG` scope** | Populated on every row, including pre-baseline | admiral's own template restricts it to `ADY > 0` |
| **Analysis visits** | `VISIT` passthrough, unscheduled bucketed | Real studies use SAP-defined day windows plus `DTYPE` |
| **Age groups** | `<65 / 65-80 / >80`, from `metadata/adsl_agegr1.csv` | Entirely study-specific |

## Where this toy data differs from a real study

Worth being explicit about, because the pilot data is unusually clean and hides
several things that dominate real work.

- **No partial dates anywhere in DM, EX or DS.** Every `--DTC` is a complete
  10-character date, so the imputation code in `01_adsl.R` never actually fires.
  Partial dates are routine in real studies and drive `--DTF`/`--TMF` flags. The
  only imputation genuinely exercised in this repo is in ADAE, where 26 adverse
  event start dates are partial.
- **`SAFFL` and `ITTFL` are perfectly collinear** (both 254/52) because every
  randomised subject was dosed. In a real study the difference between them is
  usually the interesting part.
- **No screen-failure subject has a single LB record**, so ADLB never encounters
  a subject with a missing `TRTSDT`. A real ADLB must handle that path, where
  `ADT <= TRTSDT` is `NA` and no baseline can be derived.
- **`ARM` and `ACTARM` differ for only 12 subjects**, and never in a way that
  changes a population flag.
- **One unit per analyte**, which is why `AVALU` looks harmless here. Where the
  same analyte arrives in two units the ADaM answer is two `PARAMCD`s, not one
  `PARAMCD` plus a unit column.
- **No define.xml.** `metadata/adam_spec.csv` is a miniature stand-in covering
  labels, types, lengths and order. Real define.xml also carries codelists,
  computational methods, value-level metadata and documentation links.
- **No double programming and no QC.** In a regulated environment these datasets
  would be independently reproduced by a second programmer and reconciled.

## A detail I did not expect

SDTM carries its own baseline flag, `LBBLFL`. My derived `ABLFL` disagrees with
it on **227 of 9,079 records** — 121 where ADaM flags a baseline and SDTM does
not, 106 the other way. This is not a bug. `LBBLFL` records which result the lab
considered baseline; `ABLFL` records which result *this analysis* uses, per the
SAP. Of the 121 ADaM-only flags, 120 fall on unscheduled visits that SDTM never
flags. The 106 SDTM-only records are all `SCREENING 1` results superseded by a
later pre-dose value. `tests/testthat/test-adlb.R` asserts the disagreement, so
it cannot silently change.

---

## What this demonstrates, and what it does not

**It demonstrates that I can:**

- read the CDISC standards and produce datasets that follow them;
- use `{admiral}` idiomatically — `derive_vars_merged()` rather than `left_join()`,
  `restrict_derivation()` rather than filter-mutate-bind, explicit tie-breaking
  in `derive_var_extreme_flag()`;
- distinguish the three ADaM structures and build one of each;
- keep derivations traceable back to source SDTM records;
- test derivation logic rather than snapshot output — `test-adlb.R` includes unit
  tests of the baseline rule on hand-built data covering ties, missing values and
  subjects with no pre-dose record;
- apply the engineering practice I already use — pinned dependencies, a
  reproducible single-command build, tests that gate the build.

**It does not demonstrate:**

- that I am an experienced statistical programmer. I have never worked on a
  regulated submission;
- any SAS experience. I have never written SAS;
- experience of real clinical trial data, with its partial dates, protocol
  deviations, amendments, multiple periods and reconciliation problems;
- knowledge of a sponsor's internal standards, macro libraries or validation
  procedures, which is most of the actual job;
- production of define.xml, or any Pinnacle 21 conformance run. No validator was
  run against these datasets.

Everything here was built in one day against public test data, by someone who had
not encountered CDISC before starting.

## References

- CDISC ADaM Implementation Guide v1.3
- CDISC SDTM Implementation Guide v3.4
- [admiral documentation](https://pharmaverse.github.io/admiral/)
- [pharmaverse](https://pharmaverse.org/)

## Licence

Code released under the MIT Licence. The source data belongs to
`{pharmaversesdtm}` and is public CDISC test data.
