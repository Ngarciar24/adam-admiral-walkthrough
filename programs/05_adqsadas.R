# -----------------------------------------------------------------------------
# Program    : 05_adqsadas.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Create ADQSADAS (BDS) for the ADAS-Cog(11) total score, the
#              primary efficacy variable: analysis visit windows, record
#              selection, baseline, LOCF imputation and change from baseline
# Inputs     : SDTM QS (via programs/00_setup.R); data/adam/adsl.rds
#              metadata/adqsadas_windows.csv
# Outputs    : data/adam/adqsadas.rds
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-25
# Change log : 2026-09-25  IGR  Initial version
# Notes      : Windows are on study day relative to first dose. Within a
#              window the record closest to the target day is analysed
#              (ANL01FL); ties go to the later record. Missing Week 8/16/24
#              values are imputed by LOCF from the last analysed record,
#              baseline included (DTYPE = "LOCF"). ITT subjects only.
# -----------------------------------------------------------------------------

source("programs/00_setup.R")

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))

windows <- read_csv("metadata/adqsadas_windows.csv", show_col_types = FALSE) %>%
  mutate(
    lo = coalesce(AWLO, -Inf),
    hi = coalesce(AWHI, Inf)
  )

# =============================================================================
# 1. Source records and ADSL variables
# =============================================================================
adqs <- qs %>%
  filter(QSCAT == "ALZHEIMER'S DISEASE ASSESSMENT SCALE", QSTESTCD == "ACTOT") %>%
  derive_vars_merged(
    dataset_add = adsl,
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(TRT01P, TRT01PN, TRTSDT, TRTEDT, ITTFL, EFFFL, COMP24FL,
                        SITEGR1, AGE, AGEGR1, SEX)
  ) %>%
  filter(ITTFL == "Y") %>%
  mutate(
    TRTP    = TRT01P,
    TRTPN   = TRT01PN,
    PARAMCD = "ACTOT",
    PARAM   = "ADAS-Cog(11) Total Score",
    PARAMN  = 1,
    AVAL    = QSSTRESN
  ) %>%
  derive_vars_dt(
    dtc             = QSDTC,
    new_vars_prefix = "A"
  ) %>%
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars    = exprs(ADT)
  )

# =============================================================================
# 2. Analysis windows
# =============================================================================
# Each record falls in exactly one window by ADY.
adqs <- adqs %>%
  left_join(
    windows,
    by = join_by(between(ADY, lo, hi))
  ) %>%
  select(-lo, -hi) %>%
  mutate(
    AWU     = "DAYS",
    AWTDIFF = abs(ADY - AWTARGET)
  ) %>%

  # Record analysed per window: closest to target, then the later one.
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID, PARAMCD, AVISITN),
      order   = exprs(AWTDIFF, desc(ADT), QSSEQ),
      new_var = ANL01FL,
      mode    = "first"
    ),
    filter = !is.na(AVAL)
  ) %>%

  # =============================================================================
  # 3. Baseline
  # =============================================================================
  mutate(ABLFL = if_else(AVISITN == 0 & ANL01FL == "Y", "Y", NA_character_)) %>%
  derive_var_base(
    by_vars    = exprs(STUDYID, USUBJID, PARAMCD),
    source_var = AVAL,
    new_var    = BASE
  )

# =============================================================================
# 4. LOCF
# =============================================================================
# Expected post-baseline visits for every subject with a baseline value; a
# visit without an analysed record takes the last analysed value, which is
# the baseline value if no post-baseline value exists yet.
expected <- adqs %>%
  filter(ABLFL == "Y") %>%
  distinct(STUDYID, USUBJID, PARAMCD, PARAM, PARAMN) %>%
  cross_join(windows %>% filter(AVISITN > 0) %>% select(AVISIT, AVISITN))

adqs_anl <- adqs %>% filter(ANL01FL == "Y")

locf <- derive_locf_records(
  dataset     = adqs_anl,
  dataset_ref = expected,
  by_vars     = exprs(STUDYID, USUBJID, PARAMCD),
  order       = exprs(AVISITN),
  keep_vars   = exprs(
    TRTP, TRTPN, TRT01P, TRT01PN, TRTSDT, TRTEDT, ITTFL, EFFFL, COMP24FL,
    SITEGR1, AGE, AGEGR1, SEX, BASE, ADT, ADY, QSSEQ, VISIT, VISITNUM
  )
) %>%
  filter(DTYPE == "LOCF") %>%
  select(-any_of(c("AWLO", "AWHI", "AWTARGET", "AWRANGE"))) %>%
  left_join(windows %>% select(AVISITN, AWLO, AWHI, AWTARGET, AWRANGE), by = "AVISITN") %>%
  mutate(ANL01FL = "Y", AWU = "DAYS")

adqsadas <- bind_rows(adqs, locf) %>%

  # =============================================================================
  # 5. Change from baseline
  # =============================================================================
  restrict_derivation(
    derivation = derive_var_chg,
    filter     = AVISITN > 0
  ) %>%
  restrict_derivation(
    derivation = derive_var_pchg,
    filter     = AVISITN > 0
  ) %>%

  # =============================================================================
  # 6. Sequence number and variable order
  # =============================================================================
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order   = exprs(PARAMCD, AVISITN, ADT, QSSEQ, DTYPE),
    new_var = ASEQ
  ) %>%
  arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT, DTYPE) %>%
  select(
    STUDYID, USUBJID, ASEQ,
    TRTP, TRTPN, TRTSDT, TRTEDT, ITTFL, EFFFL, COMP24FL, SITEGR1, AGE, AGEGR1, SEX,
    PARAMCD, PARAM, PARAMN,
    AVAL, BASE, CHG, PCHG, ABLFL, ANL01FL, DTYPE,
    AVISIT, AVISITN, ADT, ADY,
    AWRANGE, AWTARGET, AWTDIFF, AWLO, AWHI, AWU,
    QSSEQ, VISIT, VISITNUM
  )

# =============================================================================
# 7. Save
# =============================================================================
saveRDS(adqsadas, file.path(adam_dir, "adqsadas.rds"))

message("ADQSADAS: ", nrow(adqsadas), " rows x ", ncol(adqsadas), " columns; LOCF records: ",
        sum(adqsadas$DTYPE %in% "LOCF"))
