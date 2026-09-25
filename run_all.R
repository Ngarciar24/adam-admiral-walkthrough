# -----------------------------------------------------------------------------
# Program    : run_all.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Rebuild all ADaM datasets, XPT files, define.xml, tables and
#              figures in dependency order, compare with the published pilot
#              ADaM, then run the test suite
# Usage      : Rscript run_all.R   (from the project root)
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-25  IGR  Standard header; comments condensed
#              2026-09-25  IGR  ADTTE, ADQSADAS, efficacy/TTE tables,
#                               define.xml and pilot comparison added
# -----------------------------------------------------------------------------

stopifnot(file.exists("programs/00_setup.R"))

core <- c(
  "programs/01_adsl.R",
  "programs/02_adlb.R",
  "programs/03_adae.R",
  "programs/04_adtte.R",
  "programs/05_adqsadas.R"
)

# Exports, tables, figures, define.xml and the pilot comparison.
# Skipped with a message if not present.
stretch <- c(
  "programs/90_export_xpt.R",
  "programs/91_tables.R",
  "programs/92_figures.R",
  "programs/93_tables_tte_eff.R",
  "programs/94_define.R",
  "programs/95_compare_pilot.R"
)

run_one <- function(path) {
  if (!file.exists(path)) {
    message("SKIP (not present): ", path)
    return(invisible(NULL))
  }
  message("\n==> ", path)
  # Sourced into the global environment on purpose: functions used inside
  # admiral exprs() (e.g. format_eosstt() in 01_adsl.R) are looked up there.
  source(path, echo = FALSE)
}

invisible(lapply(core, run_one))
invisible(lapply(stretch, run_one))

# --- Tests ----------------------------------------------------------------
message("\n==> tests")
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)

message("\nAll programs run and all tests passed.")
