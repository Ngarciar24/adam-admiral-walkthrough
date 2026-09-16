# 10. Specification, XPT export and the analysis-ready contract

## The spec is data
`metadata/adam_spec.csv`: dataset, variable, label, type, length, order,
format (88 variable rows across ADSL and ADLB). `metadata/adam_datasets.csv`
holds dataset labels. Adding TRTP/TRTA to code without adding them to the spec
**failed the build** — the check that a define-driven workflow gives you.

Honest note: the draft was generated from the built datasets, then curated by
hand (59 labels written in, 2 inherited labels corrected, 2 lengths widened).
Character lengths are otherwise the observed maximum in this extract — a real
spec sets them up front.

## XPT v5
`{xportr}` 0.6.0: `xportr_type → length → label → order → df_label → write`.
Constraints: names ≤ 8 chars, labels ≤ 40, character values ≤ 200, dataset
labels ≤ 40. All ADaM names here already comply. Round trip verified with
`haven::read_xpt()`: row/column counts and labels match. `.xpt` files are
committed; `.rds` are not (reproducible).

## Tables
`{rtables}` 0.6.16 / `{tern}` 0.9.11. `tern::summarize_vars()` no longer
exists — use `analyze_vars()`. `show_colcounts = TRUE` on a BDS table prints
record counts, not subjects; set `col_counts()` to distinct USUBJID per arm.
Table 2 consumes AVAL, BASE, CHG, AVISIT, TRT01P, SAFFL and nothing else.

## A second language, by a different door
`python/check_adlb.py` reads `adlb.xpt` with pandas alone and re-asserts the
BDS invariants (one ABLFL per subject/parameter, CHG = AVAL − BASE, no day
zero) and reproduces one Table 2 cell to the decimal. Nothing from R is
imported; the XPT file is the only shared artefact. That is the point of a
submission-format export: it is language-neutral.
