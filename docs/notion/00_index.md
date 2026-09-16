# ADaM with {admiral} — course notes

Notes from a one-day, self-directed build of ADaM datasets from public CDISC pilot
data. Repository: `adam-admiral-walkthrough`. Each chapter is a separate page;
sections within a chapter are its subchapters.

1. Why tabulation data and analysis data are separate layers
2. The three ADaM structures: ADSL, BDS, OCCDS
3. ADSL — the subject-level dataset, derivation by derivation
4. BDS — ADLB, parameters, baseline, change from baseline
5. OCCDS — ADAE and treatment-emergent logic
6. Controlled terminology
7. Traceability, concretely
8. admiral versus dplyr — where the idiom differs
9. Testing derivations
10. Specification, XPT export and the analysis-ready contract
11. Where a real study differs from the pilot data
12. Interview questions and honest answers
13. Self-check: predict before you look

Facts in these notes were verified by execution against admiral 1.5.0,
pharmaversesdtm 1.5.0 and R 4.6.1 on 2026-09-16. Where a statement is a
convention rather than a rule from the ADaM Implementation Guide, it says so.
