# 9. Testing derivations

Suite: 163 expectations in `tests/testthat/`, run by `run_all.R` with
`stop_on_failure = TRUE`.

## Dataset tests versus unit tests
Dataset tests read the built `.rds` and assert invariants (one row per
USUBJID; TRTSDT ≤ TRTEDT; exactly one ABLFL per subject/parameter; CHG = AVAL −
BASE; no ADY of 0). Unit tests run the baseline logic on hand-built tibbles:
two pre-dose records → the later wins; ADT tie → LBSEQ decides; pre-dose NA
AVAL → skipped; no pre-dose record → no ABLFL, BASE stays NA.

## Test by a different door
A test that re-runs the derivation and compares it with itself tests nothing.
The SAFFL test re-derives the flag with plain dplyr from EX. Honest limit: the
dosing condition is copied verbatim, so the test cannot ratify that clause —
only detect the two sides diverging. Ratifying it is what double programming
is for.

## Structural invariants versus characterisation tests
`nrow == 306` and "52 screen failures" describe this extract; they are not
standards requirements. The header of `test-adsl.R` says which assertions are
which. There is no protocol or SAP behind pilot data — do not claim one.

## Mutation testing — proof the tests are not vacuous
Eight deliberate corruptions of a copy of `adsl.rds` (drop a subject, hard-code
ITTFL to N, shift an AGEGR1N, duration on a missing start, …) each produced
1–4 test failures and no R errors. Two early drafts were vacuous and found this
way: a tie-break test with one seeded shuffle, and nothing distinguishing
`ADT <= TRTSDT` from `<`.

## Guards
`all(logical(0))` is TRUE — assert `nrow > 0` before any `all()`. `setdiff` is
not symmetric — check both directions. Set equality beats count equality.

## Known gaps
No independent double programming. `91_tables.R` uses inline `stopifnot()`,
not testthat. Conformance (labels, lengths, codelists) is not tested.
