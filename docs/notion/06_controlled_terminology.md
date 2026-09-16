# 6. Controlled terminology

## What it is
Many SDTM and ADaM variables may only take values from published code lists
maintained by CDISC and NCI. SEX ∈ {M, F, U, UNDIFFERENTIATED}. LBTESTCD values
such as ALT come from a code list. Flag variables (`--FL`) are Y or null, never
N, with defined exceptions — population flags such as SAFFL/ITTFL are the
common Y/N exception.

## Two vocabularies, one mapping
DS.DSDECOD is CDISC CT ("COMPLETED", "ADVERSE EVENT", …); ADSL.EOSSTT is ADaM
CT ("COMPLETED", "DISCONTINUED"). They are different lists, so an explicit
mapping function is required — never a copy.

## Why interviewers probe it
A dataset can run perfectly and still fail Pinnacle 21 conformance at
submission because a value is off-list. This repo ran no validator, so nothing
here is claimed conformant — only that the variables follow the conventions.

## ADaM-specific rules to remember
PARAMCD ≤ 8 characters and a valid SAS name; PARAMCD ↔ PARAM strictly 1:1
within a dataset; AVISIT is SAP-agreed free text with AVISITN for ordering.
