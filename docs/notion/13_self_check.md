# 13. Self-check — predict before you look

Answer each before opening the answer. These were the questions I was asked
during the build; the answers are what the data showed.

## Q1. DM has 306 subjects, 52 of them screen failures. How many rows in ADSL, and what do TRT01A and SAFFL look like for a screen failure?
<details><summary>Answer</summary>
306. ADSL is one row per enrolled subject. TRT01A is the literal string
"Screen Failure" (ARM is copied, not blank), SAFFL is "N" (via
missing_value, because they are absent from EX), TRTSDT is NA.
</details>

## Q2. A subject has ALT at day −14, day −7 and week 2. How many ADLB rows, which is baseline under "last non-missing on or before first dose", and what is BASE on the week-2 row?
<details><summary>Answer</summary>
Three rows. Baseline is day −7 (last, not first). BASE on every row,
including week 2 and day −14, is the day −7 value.
</details>

## Q3. Two ALT results on the same pre-dose date. What decides ABLFL?
<details><summary>Answer</summary>
The explicit tie-breaker in `order = exprs(ADT, LBSEQ)`. Without it the answer
depends on row order and is not reproducible.
</details>

## Q4. `derive_vars_dt()` and `derive_vars_dtm()` on a date-only string — what do the defaults do?
<details><summary>Answer</summary>
dt: nothing imputed (`"n"`). dtm: time imputed to 00:00:00 (`"h"`) with a
`--TMF` of "H"; the date is never invented.
</details>

## Q5. `left_join(dm, ex)` versus `derive_vars_merged(dm, ex, ...)` — row counts?
<details><summary>Answer</summary>
643 versus 306. admiral cannot change the input's row count and errors on
duplicates unless `order` + `mode` deduplicate.
</details>

## Q6. `derive_var_merged_exist_flag()` with only `false_value = "N"` set — what do the 52 screen failures get?
<details><summary>Answer</summary>
NA, because they are absent from EX (missing_value), and `filter(SAFFL == "N")`
silently returns nothing for them.
</details>

## Q7. Is ADAE a BDS dataset?
<details><summary>Answer</summary>
No — OCCDS. One row per event, no AVAL, no PARAMCD; analysis counts subjects
via occurrence flags.
</details>

## Q8. Where does CHG get populated in this repo, and what does admiral's template do?
<details><summary>Answer</summary>
Every row including baseline (0) and 128 pre-baseline rows; the template
restricts to ADY > 0. Both defensible if stated.
</details>

## Q9. Why did sourcing programs into `new.env()` break `format_eosstt()`?
<details><summary>Answer</summary>
`exprs()` returns bare calls without an environment, so helpers used inside
`new_vars` resolve against the global environment.
</details>

## Q10. What is the smallest honest description of what this repo proves?
<details><summary>Answer</summary>
That I can read the standards and produce datasets that follow them, with
tests and traceability — on clean public data, in one day, with no validator,
no define.xml and no double programming.
</details>
