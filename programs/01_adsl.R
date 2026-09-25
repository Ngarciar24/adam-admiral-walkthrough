# -----------------------------------------------------------------------------
# Program    : 01_adsl.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Create ADSL, the subject-level analysis dataset (one row per
#              subject): treatment variables, treatment dates, disposition,
#              population flags, pooled site and age groups
# Inputs     : SDTM DM, EX, DS, SV, QS (via programs/00_setup.R)
#              metadata/adsl_agegr1.csv, metadata/adsl_trt.csv
# Outputs    : data/adam/adsl.rds
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-25  IGR  Standard header; comments condensed
#              2026-09-25  IGR  Treatment variables missing for screen
#                               failures; TRT01PN/TRT01AN, SITEGR1, RFENDT,
#                               EFFFL and COMP24FL added; TRTEDT from the
#                               end-of-study date when the last dose record
#                               has no end date
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
  # From ARM/ACTARM; missing for screen failures, who were never assigned a
  # treatment. Numeric codes are the daily dose in mg (metadata/adsl_trt.csv).
  mutate(
    TRT01P  = if_else(ARMCD == "Scrnfail", NA_character_, ARM),
    TRT01A  = if_else(ACTARMCD == "Scrnfail", NA_character_, ACTARM),
    TRT01PN = adsl_trt$TRTN[match(TRT01P, adsl_trt$TRT)],
    TRT01AN = adsl_trt$TRTN[match(TRT01A, adsl_trt$TRT)]
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
  # End of the last qualifying dose record (by start); missing if that record
  # has no end date (completed below from the end-of-study date).
  derive_vars_merged(
    dataset_add = ex_ext,
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars    = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order       = exprs(EXSTDTM, EXSEQ),
    mode        = "last"
  ) %>%

  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%

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

  # -- Last dose date when the last dose record has no end date ----------------
  # The end-of-study date is used, then the duration is derived.
  mutate(TRTEDT = if_else(is.na(TRTEDT) & !is.na(TRTSDT), EOSDT, TRTEDT)) %>%

  # TRTDURD = TRTEDT - TRTSDT + 1
  derive_var_trtdurd() %>%

  # -- Reference end date (end of study participation) -------------------------
  derive_vars_dt(
    dtc             = RFENDTC,
    new_vars_prefix = "RFEN"
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

  # -- Efficacy population -----------------------------------------------------
  # Y = in the safety population with at least one post-baseline (VISITNUM > 3)
  # ADAS-Cog and at least one post-baseline CIBIC+ assessment.
  derive_var_merged_exist_flag(
    dataset_add   = qs,
    by_vars       = exprs(STUDYID, USUBJID),
    new_var       = ADASPBFL,
    condition     = QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE" & VISITNUM > 3
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add   = qs,
    by_vars       = exprs(STUDYID, USUBJID),
    new_var       = CIBCPBFL,
    condition     = QSCAT == "CLINICIAN'S INTERVIEW-BASED IMPRESSION OF CHANGE (CIBIC+)" &
      VISITNUM > 3
  ) %>%
  mutate(
    EFFFL = if_else(SAFFL == "Y" & ADASPBFL %in% "Y" & CIBCPBFL %in% "Y", "Y", "N")
  ) %>%
  select(-ADASPBFL, -CIBCPBFL) %>%

  # -- Week 24 completers ------------------------------------------------------
  # Y = the Week 24 visit (SV VISITNUM 12) took place on or before the end of
  # study participation.
  derive_vars_merged(
    dataset_add = derive_vars_dt(sv, dtc = SVSTDTC, new_vars_prefix = "SVST"),
    by_vars     = exprs(STUDYID, USUBJID),
    filter_add  = VISITNUM == 12,
    new_vars    = exprs(WK24DT = SVSTDT)
  ) %>%
  mutate(
    COMP24FL = if_else(!is.na(WK24DT) & !is.na(RFENDT) & RFENDT >= WK24DT, "Y", "N")
  ) %>%
  select(-WK24DT) %>%

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

# -- Pooled site ----------------------------------------------------------------
# A site is pooled into "900" when any planned arm has fewer than 3 ITT
# subjects there; otherwise SITEGR1 = SITEID. Used as a model covariate.
small_sites <- adsl %>%
  filter(ITTFL == "Y") %>%
  count(SITEID, TRT01P) %>%
  tidyr::complete(SITEID, TRT01P, fill = list(n = 0L)) %>%
  group_by(SITEID) %>%
  summarise(pooled = any(n < 3), .groups = "drop") %>%
  filter(pooled) %>%
  pull(SITEID)

adsl <- adsl %>%
  mutate(SITEGR1 = if_else(SITEID %in% small_sites, "900", SITEID))

# =============================================================================
# 4. Save
# =============================================================================
saveRDS(adsl, file.path(adam_dir, "adsl.rds"))

message("ADSL: ", nrow(adsl), " rows x ", ncol(adsl), " columns")
