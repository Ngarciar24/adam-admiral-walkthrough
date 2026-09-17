# ---------------------------------------------------------------------------
# run_all.R -- rebuild every ADaM dataset, then run the test suite.
#
#   Rscript run_all.R
#
# Programs are numbered and run in dependency order: ADSL first, because every
# other dataset merges ADSL variables onto itself. This mirrors how a real study
# is organised -- one program per dataset, run in a fixed, documented order --
# rather than one monolithic script.
# ---------------------------------------------------------------------------

stopifnot(file.exists("programs/00_setup.R"))

core <- c(
  "programs/01_adsl.R",
  "programs/02_adlb.R",
  "programs/03_adae.R"
)

# Stretch programs: exports and tables. Skipped silently if not present.
stretch <- c(
  "programs/90_export_xpt.R",
  "programs/91_tables.R",
  "programs/92_figures.R"
)

run_one <- function(path) {
  if (!file.exists(path)) {
    message("SKIP (not present): ", path)
    return(invisible(NULL))
  }
  message("\n==> ", path)
  # NOTE -- these are sourced into the GLOBAL environment, deliberately.
  #
  # The obvious choice is source(path, local = new.env()) for isolation, and it
  # FAILS here. admiral's list-valued arguments are built with rlang::exprs(),
  # which returns a bare `call` with NO environment attached (rlang::quos() is
  # the variant that captures one -- verify with
  #   attr(exprs(f(x))[[1]], ".Environment")  -> NULL
  #   quo_get_env(quos(f(x))[[1]])            -> <environment>
  # ).
  #
  # So when derive_vars_merged() evaluates a user function inside a new_vars
  # expression -- here new_vars = exprs(EOSSTT = format_eosstt(DSDECOD)) in
  # 01_adsl.R -- the lookup resolves against the global environment, not the
  # environment the program was sourced into. Sourcing locally produces
  #   Error in format_eosstt(): could not find function "format_eosstt"
  # which is confusing precisely because running the same program directly with
  # Rscript works fine.
  #
  # This is a real property of the admiral idiom worth knowing: any helper
  # function referenced inside exprs() must be visible globally.
  source(path, echo = FALSE)
}

invisible(lapply(core, run_one))
invisible(lapply(stretch, run_one))

# --- Tests ----------------------------------------------------------------
message("\n==> tests")
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)

message("\nAll programs run and all tests passed.")
