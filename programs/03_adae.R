# -----------------------------------------------------------------------------
# Program    : 03_adae.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Create ADAE (OCCDS, one row per collected adverse event):
#              analysis dates with imputation flags, study days, duration,
#              treatment-emergent flag, dermatologic customised query and
#              occurrence flags
# Inputs     : SDTM AE (via programs/00_setup.R); data/adam/adsl.rds
# Outputs    : data/adam/adae.rds
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-25  IGR  Standard header; comments condensed
#              2026-09-25  IGR  TRTA/TRTAN, CQ01NAM and AOCC01FL added
# Notes      : Source date profile (checked before writing the imputation):
#              AESTDTC: 1165 complete, 15 year-month, 11 year-only, 0 missing.
#              AEENDTC: 718 complete, 473 missing, no partial dates.
#              Neither carries a time, so the program works with dates only.
#              Rules and verified counts: docs/implementation-notes.md
# -----------------------------------------------------------------------------

source("programs/00_setup.R")

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))

# =============================================================================
# 1. ADSL variables
# =============================================================================
adae <- ae %>%
  select(-DOMAIN) %>%
  derive_vars_merged(
    dataset_add = adsl,
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(TRT01P, TRT01A, TRT01AN, TRTSDT, TRTEDT, SAFFL, AGEGR1, SEX)
  ) %>%

  # Safety analyses are by actual treatment.
  mutate(
    TRTA  = TRT01A,
    TRTAN = TRT01AN
  ) %>%

  # =============================================================================
  # 2. Analysis start and end dates
  # =============================================================================
  # ASTDT: partial dates imputed to the first of the month/year (ASTDTF = D/M);
  # a missing year is not imputed. min_dates = TRTSDT: if first dose falls
  # within the range a partial date allows, impute to first dose, so a
  # possibly treatment-emergent event is not moved before treatment.
  derive_vars_dt(
    new_vars_prefix    = "AST",
    dtc                = AESTDTC,
    highest_imputation = "M",
    date_imputation    = "first",
    min_dates          = exprs(TRTSDT)
  ) %>%

  # AENDT: no imputation. Missing end dates are ongoing events and stay missing.
  derive_vars_dt(
    new_vars_prefix    = "AEN",
    dtc                = AEENDTC,
    highest_imputation = "n"
  ) %>%

  # =============================================================================
  # 3. Study days
  # =============================================================================
  # Relative to TRTSDT, no day 0. Recomputed rather than copied from SDTM
  # AESTDY/AEENDY, which are relative to RFSTDTC.
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars    = exprs(ASTDT, AENDT)
  ) %>%

  # Analysis day of an occurrence record is its onset day.
  mutate(ADY = ASTDY) %>%

  # =============================================================================
  # 4. Duration
  # =============================================================================
  # ADURN = AENDT - ASTDT + 1 days; missing for ongoing events. Computed also
  # when ASTDT is imputed (identify those records with ASTDTF).
  derive_vars_duration(
    new_var      = ADURN,
    new_var_unit = ADURU,
    start_date   = ASTDT,
    end_date     = AENDT,
    in_unit      = "days",
    out_unit     = "DAYS",
    add_one      = TRUE,
    trunc_out    = FALSE
  ) %>%

  # =============================================================================
  # 5. Analysis severity and causality
  # =============================================================================
  # Copies of the collected values (no recoding in this study); ASEVN gives
  # the clinical sort order MILD < MODERATE < SEVERE.
  mutate(
    ASEV  = AESEV,
    AREL  = AEREL,
    ASEVN = as.integer(factor(ASEV, levels = c("MILD", "MODERATE", "SEVERE")))
  ) %>%

  # =============================================================================
  # 6. Treatment-emergent flag
  # =============================================================================
  # TRTEMFL = "Y" if onset is on or after first dose and no later than 30 days
  # after last dose; otherwise missing (admiral returns "Y" or NA, never "N").
  # Date variables are passed explicitly: the admiral defaults are datetimes,
  # which this study does not have.
  derive_var_trtemfl(
    new_var        = TRTEMFL,
    start_date     = ASTDT,
    end_date       = AENDT,
    trt_start_date = TRTSDT,
    trt_end_date   = TRTEDT,
    end_window     = 30
  ) %>%

  # =============================================================================
  # 7. Dermatologic events (adverse events of special interest)
  # =============================================================================
  # Customised query as defined for the pilot study: preferred terms containing
  # APPLICATION, DERMATITIS, ERYTHEMA or BLISTER, or any term in the skin SOC
  # except cold sweat, hyperhidrosis and alopecia.
  mutate(
    CQ01NAM = if_else(
      str_detect(AEDECOD, "APPLICATION|DERMATITIS|ERYTHEMA|BLISTER") |
        (AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
           !AEDECOD %in% c("COLD SWEAT", "HYPERHIDROSIS", "ALOPECIA")),
      "DERMATOLOGIC EVENTS",
      NA_character_
    )
  ) %>%

  # =============================================================================
  # 8. Occurrence flags
  # =============================================================================
  # First treatment-emergent event per subject (AOCCFL), per subject and SOC
  # (AOCCSFL), and per subject, SOC and PT (AOCCPFL), so incidence tables
  # count flagged rows. Ties on onset date broken by AESEQ.
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID),
      order   = exprs(ASTDT, AESEQ),
      new_var = AOCCFL,
      mode    = "first"
    ),
    filter = TRTEMFL == "Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID, AEBODSYS),
      order   = exprs(ASTDT, AESEQ),
      new_var = AOCCSFL,
      mode    = "first"
    ),
    filter = TRTEMFL == "Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID, AEBODSYS, AEDECOD),
      order   = exprs(ASTDT, AESEQ),
      new_var = AOCCPFL,
      mode    = "first"
    ),
    filter = TRTEMFL == "Y"
  ) %>%
  # First treatment-emergent dermatologic event per subject (source of the
  # ADTTE event).
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID),
      order   = exprs(ASTDT, AESEQ),
      new_var = AOCC01FL,
      mode    = "first"
    ),
    filter = TRTEMFL == "Y" & CQ01NAM == "DERMATOLOGIC EVENTS"
  ) %>%

  # =============================================================================
  # 9. Sequence number and sort order
  # =============================================================================
  # ASEQ follows onset order and differs from AESEQ (collection order).
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order   = exprs(ASTDT, AESEQ),
    new_var = ASEQ
  ) %>%
  arrange(STUDYID, USUBJID, ASTDT, AESEQ) %>%

  # =============================================================================
  # 10. Variable order
  # =============================================================================
  # Coded terms: AEDECOD = MedDRA preferred term, AEBODSYS = primary SOC.
  # SDTM variables at the end are kept for traceability back to AE.
  select(
    # -- keys and subject-level context
    STUDYID, USUBJID, ASEQ,
    TRTA, TRTAN, TRT01P, TRT01A, TRTSDT, TRTEDT, SAFFL, AGEGR1, SEX,
    # -- coded terms and customised query
    AETERM, AEDECOD, AEBODSYS, CQ01NAM,
    # -- timing
    ASTDT, ASTDTF, ASTDY, AENDT, AENDY, ADY, ADURN, ADURU,
    # -- severity, seriousness, causality, outcome
    ASEV, ASEVN, AESEV, AESER, AREL, AEREL, AEOUT,
    # -- analysis flags
    TRTEMFL, AOCCFL, AOCCSFL, AOCCPFL, AOCC01FL,
    # -- traceability to SDTM AE
    AESEQ, AESTDTC, AEENDTC, AESTDY, AEENDY
  )

# =============================================================================
# 11. Save
# =============================================================================
saveRDS(adae, file.path(adam_dir, "adae.rds"))

message("ADAE: ", nrow(adae), " rows x ", ncol(adae), " columns")
message("  TRTEMFL == 'Y': ", sum(adae$TRTEMFL == "Y", na.rm = TRUE),
        "  |  subjects with >=1 TE AE (AOCCFL == 'Y'): ",
        sum(adae$AOCCFL == "Y", na.rm = TRUE))
message("  imputed start dates (ASTDTF populated): ", sum(!is.na(adae$ASTDTF)))
