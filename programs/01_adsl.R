# ---------------------------------------------------------------------------
# 01_adsl.R -- ADSL, the Subject-Level Analysis Dataset
#
# Structure: exactly ONE ROW PER SUBJECT. ADSL is the only ADaM dataset that is
# always required. Every other ADaM dataset merges a subset of ADSL onto itself,
# so treatment assignment and analysis populations are defined exactly once and
# can never disagree between two tables.
#
# Source SDTM: DM (demographics), EX (exposure), DS (disposition).
# ---------------------------------------------------------------------------

source("programs/00_setup.R")

# ===========================================================================
# 1. Exposure: impute TIME only, never the date
# ===========================================================================
# derive_vars_dtm() builds a datetime (--DTM) from an ISO 8601 --DTC string.
#
# The defaults here are asymmetric and are a classic interview question:
#   derive_vars_dt()  has highest_imputation = "n"  -> imputes NOTHING
#   derive_vars_dtm() has highest_imputation = "h"  -> imputes TIME by default
#
# EXSTDTC in this study is a plain date ("2012-07-09") with no time component.
# Leaving the default "h" means: keep the date exactly as collected, and fill the
# missing clock time with 00:00:00 for the start and 23:59:59 for the end. That
# is deliberate -- it makes "first dose" and "last dose" orderable without ever
# inventing a date that was not observed.
#
# EXSTTMF / EXENTMF are the imputation FLAGS. A flag variable exists so a
# reviewer can see which values were imputed and which were collected. That is
# traceability: the imputation is visible in the data, not buried in the code.
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

# ===========================================================================
# 2. Disposition: no imputation at all
# ===========================================================================
# DSSTDTC is complete (all 850 records are 10-character dates), so the default
# highest_imputation = "n" is correct. If a partial date appeared, ADT would
# become NA rather than silently guessing -- fail loudly, not quietly.
ds_ext <- ds %>%
  derive_vars_dt(
    dtc = DSSTDTC,
    new_vars_prefix = "DSST"
  )

# Map the DS controlled terminology to the ADaM end-of-study status.
# DSDECOD is CDISC controlled terminology; EOSSTT is ADaM controlled terminology.
# They are different vocabularies, so an explicit mapping is required.
format_eosstt <- function(x) {
  case_when(
    x == "COMPLETED" ~ "COMPLETED",
    !is.na(x)        ~ "DISCONTINUED",
    TRUE             ~ NA_character_
  )
}

# ===========================================================================
# 3. Build ADSL
# ===========================================================================
adsl <- dm %>%
  select(-DOMAIN) %>% # DOMAIN is an SDTM housekeeping variable; ADaM has no use for it

  # -- Treatment variables -------------------------------------------------
  # TRT01P = PLANNED treatment for period 01, TRT01A = ACTUAL treatment.
  #
  # CAVEAT on this mapping: ARM/ACTARM are copied UNCONDITIONALLY, so the 52
  # screen failures get TRT01P = TRT01A = "Screen Failure" -- a treatment-arm
  # variable holding a non-treatment value. They were never randomised, so
  # strictly nothing was "planned" for them. Sponsors differ on whether TRT01P
  # should be null when ARMNRS is populated; this repo keeps the DM value and
  # relies on ITTFL/SAFFL to exclude them. A real study fixes this in the spec.
  # Both exist because efficacy is normally analysed as-randomised (ITT, planned)
  # while safety is analysed as-treated (actual). In THIS study they differ for
  # exactly 12 subjects: planned Xanomeline High Dose, actual Xanomeline Low Dose.
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM
  ) %>%

  # -- First dose ----------------------------------------------------------
  # This is NOT a left_join. derive_vars_merged() cannot change the row count of
  # its input: EX has up to 3 records per subject, and a dplyr::left_join() here
  # would turn 306 rows into 643. The order + mode pair is the DEDUPLICATION
  # rule -- "of all the qualifying EX records for this subject, take the first
  # by EXSTDTM then EXSEQ". Omit `order` and admiral raises a hard error on
  # duplicates rather than quietly picking one.
  #
  # filter_add encodes the definition of a dose that counts. EXDOSE > 0 is a real
  # dose; the placebo arm is dosed at 0 mg by design, so a zero dose of PLACEBO
  # must also count or every placebo subject would look untreated.
  derive_vars_merged(
    dataset_add = ex_ext,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars    = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order       = exprs(EXSTDTM, EXSEQ),
    mode        = "first"
  ) %>%

  # -- Last dose -----------------------------------------------------------
  derive_vars_merged(
    dataset_add = ex_ext,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXENDTM),
    new_vars    = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order       = exprs(EXENDTM, EXSEQ),
    mode        = "last"
  ) %>%

  # -- Dates from datetimes ------------------------------------------------
  # Analysis is by day, not by second. ADaM keeps BOTH: --DTM for ordering and
  # --DT for analysis and display.
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%

  # -- Treatment duration --------------------------------------------------
  # TRTDURD = TRTEDT - TRTSDT + 1. The "+1" is the clinical convention: a subject
  # dosed and stopped on the same day was treated for 1 day, not 0.
  derive_var_trtdurd() %>%

  # -- Randomisation date --------------------------------------------------
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = DSDECOD == "RANDOMIZED",
    new_vars    = exprs(RANDDT = DSSTDT)
  ) %>%

  # -- End of study --------------------------------------------------------
  # Screen failures are excluded here: they have a DISPOSITION EVENT record with
  # DSDECOD == "SCREEN FAILURE", but they never entered the study, so an
  # end-of-study status is not meaningful for them. EOSDT/EOSSTT stay NA.
  #
  # new_vars can compute: format_eosstt(DSDECOD) is evaluated inside the merge.
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE",
    new_vars    = exprs(
      EOSDT   = DSSTDT,
      EOSSTT  = format_eosstt(DSDECOD),
      # DCSREAS is "Reason for Discontinuation from Study". A subject who
      # COMPLETED did not discontinue, so DCSREAS must be NULL for them --
      # populating it with "COMPLETED" is a conformance error a validator flags,
      # and it would corrupt any frequency table of discontinuation reasons.
      DCSREAS = if_else(DSDECOD == "COMPLETED", NA_character_, DSDECOD)
    ),
    order = exprs(DSSTDT, DSSEQ),
    mode  = "last"
  ) %>%

  # -- Death ---------------------------------------------------------------
  # DTHDTC is complete in this study, so nothing is imputed and DTHDTF is all NA.
  # In a real study death dates are frequently partial (month/year only) and
  # highest_imputation = "M" with a visible DTHDTF flag is the usual approach.
  derive_vars_dt(
    dtc             = DTHDTC,
    new_vars_prefix = "DTH",
    highest_imputation = "M"
  ) %>%

  # -- Safety population flag ----------------------------------------------
  # SAFFL: received at least one dose of study treatment.
  #
  # derive_var_merged_exist_flag() is THREE-valued, not two, and this is the
  # subtle part. The three cases are:
  #   true_value    -- the condition is TRUE for >=1 record in EX
  #   false_value   -- the subject IS in EX but the condition is never TRUE
  #   missing_value -- the subject is ABSENT from EX entirely (the 52 screen failures)
  # Setting false_value and missing_value both to "N" deliberately collapses
  # "in EX but never dosed" and "never in EX" into a single N.
  #
  # TOY-DATA CAVEAT: in THIS study only the third case occurs. All 254 subjects
  # present in EX satisfy the condition, so the false_value branch has ZERO
  # instances and all 52 "N" values come from missing_value. The three-way
  # distinction is real and worth knowing, but this data does not exercise it.
  derive_var_merged_exist_flag(
    dataset_add   = ex,
    by_vars       = exprs(STUDYID, USUBJID),
    new_var       = SAFFL,
    condition     = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))),
    false_value   = "N",
    missing_value = "N"
  ) %>%

  # -- Intent-to-treat flag ------------------------------------------------
  # There is NO admiral function for this, on purpose. admiral's documented
  # position is that population flags are company- and study-specific, so only
  # SAFFL ships in the template. Every other population flag is hand-written
  # against the Statistical Analysis Plan.
  #
  # ITTFL here = "randomised", which in this study means "not a screen failure".
  # ITT is analysed AS RANDOMISED, which is why it keys off ARMCD (planned), not
  # ACTARMCD (actual). That distinction is the whole point of the ITT principle.
  mutate(
    ITTFL = if_else(!is.na(ARMCD) & ARMCD != "Scrnfail", "Y", "N")
  ) %>%

  # -- Age grouping --------------------------------------------------------
  # The CUT POINTS THEMSELVES come from metadata/adsl_agegr1.csv (AGE_LOW /
  # AGE_HIGH), not just the labels. That distinction matters: if the cut points
  # were literals in a case_when here and the CSV only supplied label strings,
  # the spec would be decorative and could silently disagree with the code.
  # Changing a band now means editing the CSV alone.
  #
  # AGEGR1N exists because tables need a sort order that is not alphabetical
  # ("<65" must precede "65-80", which sorts after it). Paired --GR1/--GR1N
  # variables are an ADaM convention.
  #
  # NA handling is explicit. A bare `TRUE ~ 3` fall-through would silently put a
  # subject with missing AGE into the oldest band. AGE is complete in this study
  # (306/306, range 50-89), so this branch never fires here -- it is defensive.
  mutate(
    AGEGR1N = {
      idx <- vapply(
        AGE,
        function(a) {
          if (is.na(a)) return(NA_integer_)
          hit <- which(a >= adsl_agegr1$AGE_LOW & a <= adsl_agegr1$AGE_HIGH)
          if (length(hit) == 1L) as.integer(hit) else NA_integer_
        },
        integer(1)
      )
      adsl_agegr1$AGEGR1N[idx]
    },
    AGEGR1 = adsl_agegr1$AGEGR1[match(AGEGR1N, adsl_agegr1$AGEGR1N)]
  )

# ===========================================================================
# 4. Save
# ===========================================================================
saveRDS(adsl, file.path(adam_dir, "adsl.rds"))

message("ADSL: ", nrow(adsl), " rows x ", ncol(adsl), " columns")
