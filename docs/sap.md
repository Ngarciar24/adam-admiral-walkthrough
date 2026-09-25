# Statistical analysis conventions (SAP extract)

The public pilot data come without their statistical analysis plan. This
document states the rules implemented in `programs/`, in the form a study SAP
would state them. Where the published pilot analysis defines a rule, it is
followed and marked "(pilot)". Where this repository chooses differently, the
difference is stated and explained in the comparison report
(`outputs/qc/pilot_comparison.txt`).

## 1. Study

CDISCPILOT01 is a randomised, double-blind, placebo-controlled, parallel-group
24-week study of xanomeline in mild to moderate Alzheimer's disease. It has
three arms: placebo, xanomeline 54 mg (low dose) and xanomeline 81 mg (high
dose). The primary efficacy endpoint used here is the change from baseline in
ADAS-Cog(11) at Week 24 (pilot).

## 2. Analysis populations

| Population | Flag | Definition | N |
|---|---|---|---|
| Intent-to-treat | `ITTFL` | Randomised (planned arm assigned) | 254 |
| Safety | `SAFFL` | At least one dose of study treatment; placebo counts as a dose | 254 |
| Efficacy | `EFFFL` | Safety population with at least one post-baseline ADAS-Cog and one post-baseline CIBIC+ assessment (pilot) | 234 |
| Week 24 completers | `COMP24FL` | Week 24 visit on or before the end of participation (pilot) | 118 |

Screen failures (52) are included in ADSL with all population flags "N" and
treatment variables missing.

## 3. Treatment

- Efficacy analyses use planned treatment (`TRTP`, as randomised).
- Safety analyses use actual treatment (`TRTA`), from `DM.ACTARM`.
  - 12 subjects randomised to high dose received low dose.
  - The pilot analysis sets actual equal to planned.
- Numeric treatment codes are the daily dose: 0, 54 or 81 mg (pilot).
- First dose (`TRTSDT`) is the start of the first dose record.
- Last dose (`TRTEDT`) is the end of the last dose record. If that record has
  no end date, the end-of-study date is used (pilot).

## 4. Study day and dates

- **Study day.** Day = date − `TRTSDT` + 1 on or after first dose, and
  date − `TRTSDT` before it. There is no day 0.
- **Partial adverse event start dates.** A missing day or month is imputed to
  the first of the period, but never before first dose when the partial date
  allows the first-dose date. A missing year is not imputed.
  - Imputed dates are flagged (`ASTDTF`).
  - The pilot does not impute year-only dates.
- **Adverse event end dates** are not imputed. A missing end date means the
  event was ongoing.

## 5. Baseline

- **Laboratory:** the last non-missing value on or before the date of first
  dose, with ties broken by the higher `LBSEQ`. The pilot uses the SDTM
  baseline flag, which is the SCREENING 1 value.
- **ADAS-Cog:** the analysed record in the Baseline window (study day ≤ 1).
- Change from baseline (`CHG`, `PCHG`) and baseline-to-current shifts
  (`SHIFT1`) are derived for post-baseline records only.

## 6. Analysis visits (ADAS-Cog)

| Visit | Window (study day) | Target |
|---|---|---|
| Baseline | ≤ 1 | 1 |
| Week 8 | 2–84 | 56 |
| Week 16 | 85–140 | 112 |
| Week 24 | ≥ 141 | 168 |

- When several assessments fall in one window, the one closest to the target
  day is analysed (`ANL01FL = "Y"`); ties go to the later assessment.
- A missing Week 8, 16 or 24 value is imputed by last observation carried
  forward from the last analysed value, which may be the baseline value
  (pilot).

Laboratory visits are not windowed: every unscheduled visit is grouped as
"UNSCHEDULED" and excluded from by-visit summaries.

## 7. Adverse events

- **Treatment-emergent:** onset on or after the date of first dose and no
  later than 30 days after the date of last dose. An event with unknown onset
  is emergent unless it ended before first dose.
- **Dermatologic events** (adverse events of special interest, pilot):
  - preferred terms containing APPLICATION, DERMATITIS, ERYTHEMA or BLISTER;
  - or any preferred term in the SOC "Skin and subcutaneous tissue disorders",
    except cold sweat, hyperhidrosis and alopecia.
- Incidence counts subjects, not events. Occurrence flags mark each subject's
  first treatment-emergent event overall, by SOC and by preferred term.

## 8. Analyses

| Output | Population | Method |
|---|---|---|
| Table 1 | ITT | Descriptive statistics by planned treatment; percentages over subjects with non-missing values |
| Table 2, Figure 1 | Safety | ALT observed value and change from baseline by visit, post-baseline records; mean ± SE in the figure |
| Table 3, Figure 2 | Safety | Time to first treatment-emergent dermatologic event from first dose, censored at the end of participation. Kaplan–Meier estimates with log-log 95% CIs, log-rank test and Cox hazard ratio for each dose vs placebo, by actual treatment |
| Table 4 | Efficacy | ANCOVA of the ADAS-Cog(11) change at Week 24 (LOCF): treatment + pooled site + baseline score. LS means, differences vs placebo with 95% CIs, and a dose-response test with dose as a continuous term. No multiplicity adjustment |

**Pooled site** (`SITEGR1`): sites where any planned arm has fewer than 3 ITT
subjects are pooled into one group, "900" (pilot).
