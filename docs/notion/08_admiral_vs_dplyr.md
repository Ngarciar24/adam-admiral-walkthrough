# 8. admiral versus dplyr — where the idiom differs

## `derive_vars_merged()` is not `left_join()`
It cannot change the row count of its input. On DM + EX, `left_join` gives 643
rows; `derive_vars_merged` gives 306. Duplicate keys without `order` are a hard
error; `order` + `mode` is the explicit deduplication rule.

## Quoting rules are inconsistent — and that is the main beginner error
List-valued arguments take `exprs()`: `by_vars`, `order`, `new_vars`,
`source_vars`, `min_dates`. Conditions are bare: `filter_add`, `condition`,
`filter`. Single names are bare symbols: `new_var`. `new_vars = exprs(NEW =
OLD)` renames like `rename()`, and the right-hand side may be an expression
that calls your own function.

## `exprs()` carries no environment
`rlang::exprs()` returns bare calls; `quos()` captures an environment. So a
helper referenced inside `new_vars` is resolved against the global
environment. Sourcing a program into `new.env()` breaks it with "could not
find function", while `Rscript programs/01_adsl.R` works. Documented in
`run_all.R`.

## Three-valued exist flags
`derive_var_merged_exist_flag()` distinguishes "condition met", "in source but
never met", "absent from source". dplyr's `%in%` collapses the last two.

## `restrict_derivation()` versus filter–mutate–bind
Applies a derivation to a subset while keeping all rows' values. Unlike the
hand-rolled version it cannot drop rows through a wrong filter — but it does
reorder rows (`bind_rows`), so never depend on order after any admiral call.

## Fixed variable names by convention
`derive_var_chg()` and `derive_var_pchg()` take no arguments: AVAL, BASE, CHG,
PCHG are fixed by ADaM convention. `derive_var_trtdurd()` assumes TRTSDT/TRTEDT.

## Similar names, different jobs
`derive_var_extreme_flag()` flags a row; `derive_vars_extreme_event()` adds
variables from events across datasets (ADSL); `derive_extreme_event()` adds
records (BDS). Deprecated and to avoid: `derive_var_dthcaus()`,
`derive_var_extreme_dt()/dtm()`, `dthcaus_source()`, `date_source()`.

## Templates
`use_ad_template("ADSL", save_path = ..., overwrite = TRUE)` — the `open`
argument was removed in 1.5.0. Read templates; do not copy them blindly.
