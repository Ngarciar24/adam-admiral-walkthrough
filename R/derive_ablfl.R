# -----------------------------------------------------------------------------
# Program    : derive_ablfl.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Baseline rule for ADLB, shared by programs/02_adlb.R and
#              tests/testthat/test-adlb.R so both run the same code
# Inputs     : Data frame with STUDYID, USUBJID, PARAMCD, ADT, LBSEQ, AVAL, TRTSDT
# Outputs    : Same rows with ABLFL and BASE added
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-17
# Change log : 2026-09-17  IGR  Initial version (moved out of 02_adlb.R)
#              2026-09-25  IGR  Standard header
# Notes      : Rule: the baseline record is the last record with non-missing
#              AVAL dated on or before first dose (ADT <= TRTSDT), ties on ADT
#              broken by LBSEQ. BASE is its AVAL, carried onto every row of the
#              subject/parameter. Rows are neither dropped nor duplicated, but
#              their order is not preserved.
# -----------------------------------------------------------------------------

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
