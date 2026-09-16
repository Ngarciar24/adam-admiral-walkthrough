# 1. Why tabulation data and analysis data are separate layers

## SDTM is what was collected
SDTM (Study Data Tabulation Model) reorganises collected data into standard
domains: DM demographics, EX exposure, LB labs, AE adverse events, DS
disposition. Its rule is fidelity. It represents what was on the CRF or came
from the lab, in standard variables, and it contains **no derived analysis
variables**. A reviewer uses SDTM to answer "what happened to this subject".

## ADaM is what gets analysed
ADaM (Analysis Data Model) is analysis-ready. The defining principle from the
ADaM Implementation Guide: a statistician should produce a result with one
procedure call and no further data manipulation. For "mean change from baseline
in ALT by visit and arm", the ADaM dataset already carries the change column,
the baseline flag, the analysis visit and the arm on every row.

## Three reasons to keep them apart
- **Different consumers.** SDTM serves review and cross-study pooling; ADaM
  serves one study's Statistical Analysis Plan (SAP).
- **Derivations are decisions.** "Baseline is the last non-missing value before
  first dose" is a choice. Keeping it in ADaM keeps SDTM neutral and makes the
  choice reviewable in one place.
- **Traceability.** Because ADaM is derived from SDTM, every ADaM value must be
  explainable back to SDTM records. Two layers give a checkable chain.

## The one-line version for an interview
SDTM answers "what was observed"; ADaM answers "what goes into the table", and
every ADaM value must point back to SDTM.
