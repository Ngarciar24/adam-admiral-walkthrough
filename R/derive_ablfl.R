# ---------------------------------------------------------------------------
# derive_ablfl.R -- the baseline rule, as one function shared by the ADLB
# program and its unit tests.
#
# Rule (see README "Choices made here"): the baseline record for a subject and
# parameter is the LAST record with a non-missing AVAL dated on or before the
# first dose (ADT <= TRTSDT), ties on ADT broken by LBSEQ. BASE is that
# record's AVAL, carried onto every row of the subject/parameter.
#
# This lives in its own file so that programs/02_adlb.R and
# tests/testthat/test-adlb.R call the SAME code. Before this refactor the test
# file held a hand-copied version of these two calls, which could drift from
# the program without any test noticing.
#
# Requirements on `dat`: STUDYID, USUBJID, PARAMCD, ADT, LBSEQ, AVAL, TRTSDT.
# Adds ABLFL and BASE. Never drops or duplicates a row. Row ORDER is not
# preserved (restrict_derivation() binds the flagged rows back first), so the
# caller must arrange() afterwards if order matters.
# ---------------------------------------------------------------------------

derive_ablfl_base <- function(dat) {
  dat %>%
    admiral::restrict_derivation(
      derivation = admiral::derive_var_extreme_flag,
      args = admiral::params(
        by_vars = rlang::exprs(STUDYID, USUBJID, PARAMCD),
        order   = rlang::exprs(ADT, LBSEQ),
        new_var = ABLFL,
        mode    = "last"
      ),
      filter = !is.na(AVAL) & ADT <= TRTSDT
    ) %>%
    admiral::derive_var_base(
      by_vars    = rlang::exprs(STUDYID, USUBJID, PARAMCD),
      source_var = AVAL,
      new_var    = BASE
    )
}
