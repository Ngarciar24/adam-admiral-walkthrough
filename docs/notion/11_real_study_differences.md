# 11. Where a real study differs from the pilot data

The CDISC pilot data is unusually clean. Each item below is something the repo
does **not** exercise, stated so it is never over-claimed.

## Dates
No partial dates in DM, EX or DS — every `--DTC` is a complete 10-character
date. ADSL's imputation branches never fire; `DTHDTF` is NA for all 306 rows
because all 3 death dates are complete. Only ADAE (26 partial starts) exercises
imputation. Real studies: month-only death dates, year-only AE onsets, and the
`--DTF/--TMF` flags carry real information.

## Populations
SAFFL and ITTFL collinear (254/52). EXDOSE is never missing and only 0/54/81,
so the dose filter never meets an NA (which would make the condition NA) or a
zero-dose active record. ARM ≠ ACTARM for 12 subjects and never changes a flag.

## Labs
No screen failure has an LB record, so ADLB never sees a missing TRTSDT — the
path where `ADT <= TRTSDT` is NA and no baseline can be derived. Zero ADT ties
in the pre-dose window, so the LBSEQ tie-break is never actually exercised. One
unit per analyte across all 17 sites. Analysis visit windows not implemented;
DTYPE never needed.

## Disposition
No subject has more than one non-screen-failure disposition event, so the
`order`/`mode` dedup there is purely defensive.

## Process
No define.xml, no Pinnacle 21 run, no double programming, no QC reconciliation,
no validated environment, no sponsor macro library, no MedDRA re-coding, single
treatment period. In a regulated setting each of these is most of the work.
