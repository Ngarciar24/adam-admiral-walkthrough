# -----------------------------------------------------------------------------
# Program    : 01_adsl.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Create ADSL, the subject-level analysis dataset (one row per
#              subject): treatment variables, treatment dates, disposition,
#              population flags and age groups
# Inputs     : SDTM DM, EX, DS (via programs/00_setup.R)
#              metadata/adsl_agegr1.csv
# Outputs    : data/adam/adsl.rds
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-25  IGR  Standard header; comments condensed
# Notes      : Derivation rules and data checks: docs/implementation-notes.md
# -----------------------------------------------------------------------------

source("programs/00_setup.R")

# =============================================================================
# 1. Exposure datetimes
# =============================================================================
# EXSTDTC/EXENDTC are complete dates without time. Only the time is imputed
# (00:00:00 start, 23:59:59 end), never the date; EXSTTMF/EXENTMF flag it.
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

# =============================================================================
# 2. Disposition dates
# =============================================================================
# DSSTDTC is complete for all records, so no imputation.
ds_ext <- ds %>%
  derive_vars_dt(
    dtc = DSSTDTC,
    new_vars_prefix = "DSST"
  )

# Map DS controlled terminology (DSDECOD) to ADaM EOSSTT.
format_eosstt <- function(x) {
  case_when(
    x == "COMPLETED" ~ "COMPLETED",
    !is.na(x)        ~ "DISCONTINUED",
    TRUE             ~ NA_character_
  )
}

# =============================================================================
# 3. Build ADSL
# =============================================================================
adsl <- dm %>%
  select(-DOMAIN) %>%

  # -- Planned and actual treatment ----------------------------------------
  # Copied from ARM/ACTARM, so screen failures carry "Screen Failure"; they
  # are excluded from analyses through ITTFL/SAFFL.
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM
  ) %>%

  # -- First dose ------------------------------------------------------------
  # A dose counts if EXDOSE > 0, or EXDOSE = 0 on placebo (placebo is 0 mg).
  # order/mode select one EX record per subject; ties broken by EXSEQ.
  derive_vars_merged(
    dataset_add = ex_ext,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars    = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order       = exprs(EXSTDTM, EXSEQ),
    mode        = "first"
  ) %>%

  # -- Last dose -------------------------------------------------------------
  derive_vars_merged(
    dataset_add = ex_ext,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXENDTM),
    new_vars    = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order       = exprs(EXENDTM, EXSEQ),
    mode        = "last"
  ) %>%

  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%

  # TRTDURD = TRTEDT - TRTSDT + 1
  derive_var_trtdurd() %>%

  # -- Randomisation date ----------------------------------------------------
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = DSDECOD == "RANDOMIZED",
    new_vars    = exprs(RANDDT = DSSTDT)
  ) %>%

  # -- End of study ------------------------------------------------------------
  # Not derived for screen failures (EOSDT/EOSSTT stay missing).
  # DCSREAS is missing for subjects who completed.
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE",
    new_vars    = exprs(
      EOSDT   = DSSTDT,
      EOSSTT  = format_eosstt(DSDECOD),
      DCSREAS = if_else(DSDECOD == "COMPLETED", NA_character_, DSDECOD)
    ),
    order = exprs(DSSTDT, DSSEQ),
    mode  = "last"
  ) %>%

  # -- Death date --------------------------------------------------------------
  # Complete in this study; month/day imputation (flagged in DTHDTF) would
  # apply to partial dates.
  derive_vars_dt(
    dtc             = DTHDTC,
    new_vars_prefix = "DTH",
    highest_imputation = "M"
  ) %>%

  # -- Safety population -------------------------------------------------------
  # Y = at least one qualifying dose. Subjects never in EX (screen failures)
  # get "N" through missing_value.
  derive_var_merged_exist_flag(
    dataset_add   = ex,
    by_vars       = exprs(STUDYID, USUBJID),
    new_var       = SAFFL,
    condition     = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))),
    false_value   = "N",
    missing_value = "N"
  ) %>%

  # -- Intent-to-treat population ----------------------------------------------
  # Y = randomised, i.e. assigned to a planned arm other than screen failure.
  mutate(
    ITTFL = if_else(!is.na(ARMCD) & ARMCD != "Scrnfail", "Y", "N")
  ) %>%

  # -- Age groups --------------------------------------------------------------
  # Cut points and labels from metadata/adsl_agegr1.csv. Missing AGE gives
  # missing AGEGR1/AGEGR1N rather than a default band.
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

# =============================================================================
# 4. Save
# =============================================================================
saveRDS(adsl, file.path(adam_dir, "adsl.rds"))

message("ADSL: ", nrow(adsl), " rows x ", ncol(adsl), " columns")
