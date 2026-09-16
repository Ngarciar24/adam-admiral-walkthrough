# 12. Interview questions and honest answers

**Why is ADSL one row per subject?** Subject-level facts must be defined once;
every other dataset merges a subset of ADSL so two tables cannot disagree about
a subject's arm or population.

**ABLFL disagrees with SDTM's LBBLFL on 227 records — why?** Different
questions. LBBLFL is what the lab flagged; ABLFL is what this analysis uses.
120 of 121 ADaM-only flags are unscheduled visits SDTM never flags; the 106
SDTM-only are SCREENING 1 results superseded by a later pre-dose value. A test
asserts it.

**What happens with no baseline?** No ABLFL, BASE NA, CHG/PCHG NA — nothing is
guessed. This repo never hits that path (all 1,270 combinations have one).

**SAFFL versus ITTFL?** As-treated versus as-randomised; ITTFL keys off ARMCD,
not ACTARMCD. Collinear here because every randomised subject was dosed.

**PARAMCD versus LBTESTCD?** SDTM test code versus SAP analysis parameter; mapped
via a lookup, not copied, because they diverge for derived parameters or
multi-unit analytes.

**Treatment-emergent with partial dates?** On/after first dose within a SAP
window. Imputation direction can lose or manufacture emergent events; the
`--DTF` flag must show it. `end_window` defaults to NULL and needs
`trt_end_date`.

**Why keep LBSEQ, VISIT, LBSTRESN?** Traceability: USUBJID + LBSEQ reaches the
exact source record; AVAL is visibly a Predecessor of LB.LBSTRESN.

**What is in define.xml that you did not produce?** Nearly all of it —
codelists, methods, value-level metadata, origins, documentation links. My CSV
spec covers labels, types, lengths, order.

**How would you QC this?** Independent double programming from the spec and
reconciliation. My 163 tests are my own assertions about my own code; the
closest to independence is unit-testing the baseline logic on hand-built data
and the mutation testing that showed the tests are not vacuous.

**Where does admiral differ from dplyr?** Merges that cannot change row count;
inconsistent quoting (`exprs()` vs bare conditions); three-valued exist flags;
`restrict_derivation`; `exprs()` carrying no environment.

**You have never written SAS — how would you migrate?** I would be learning to
read SAS while migrating and would say so. I bring the target side: renv,
tests gating merges, code review, containers. Migration is re-deriving intent
from code where the SAP is the true specification, not translation.

**What would break on real data?** Partial dates, missing EXDOSE, visit
windowing, missing TRTSDT in labs, multiple periods, MedDRA versions.

**Why R rather than SAS or Python for this?** SAS is the incumbent but not
where the team is heading and not something I could honestly showcase in a
day; Python has no mature ADaM toolchain equivalent to admiral; R with the
pharmaverse is the open-source standard for exactly this work, and the
resulting `.xpt` files are language-neutral — the repo includes a Python check
that reads them independently.
