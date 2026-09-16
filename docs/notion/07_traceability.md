# 7. Traceability, concretely

## Definition
Each analysis value can be tied to specific source records and to a stated
derivation rule. In ADaM this is achieved by data, not by documentation alone.

## Three mechanisms
- **Keep the source keys.** ADLB keeps LBSEQ, LBTESTCD, VISIT, LBDTC, LBSTRESN,
  LBBLFL beside PARAMCD, AVISIT, ADT, AVAL. Redundant for analysis, essential
  for review.
- **Origin metadata.** In define.xml each variable is "Predecessor" (copied,
  from a named SDTM variable) or "Derived" (with a rule). AVAL: Predecessor of
  LB.LBSTRESN. CHG: Derived, AVAL − BASE.
- **Rows that do not exist in SDTM** — derived parameters, summary records —
  carry DTYPE and, where possible, SRCDOM/SRCVAR/SRCSEQ pointing at inputs.

## A real chain from this repo
```
SDTM  LB    USUBJID 01-701-1028  LBSEQ 135  LBTESTCD ALT  VISIT WEEK 8
            LBDTC 2013-09-10T09:13  LBORRES 33 U/L  LBSTRESN 33 U/L
ADaM  ADLB  USUBJID 01-701-1028  ASEQ 5  PARAMCD ALT  AVISIT WEEK 8
            ADT 2013-09-10  AVAL 33  BASE 26  CHG 7
            BASE from the same subject's ASEQ 1: ABLFL Y, SCREENING 1, AVAL 26
RESULT      one of 56 values in Table 2 / WEEK 8 / Mean CHG / Xanomeline High Dose
```
`programs/91_tables.R` performs no join and no window function; it filters
and averages columns that already exist. That is the analysis-ready contract.

## What this repo does not have
No define.xml. `metadata/adam_spec.csv` covers labels, types, lengths and order
only — no codelists, computational methods or value-level metadata.
