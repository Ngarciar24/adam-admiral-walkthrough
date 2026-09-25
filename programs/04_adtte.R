# -----------------------------------------------------------------------------
# Program    : 04_adtte.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Create ADTTE (BDS, one row per subject and parameter) with the
#              time to first treatment-emergent dermatologic adverse event
#              (PARAMCD TTDE), safety population
# Inputs     : data/adam/adsl.rds, data/adam/adae.rds
# Outputs    : data/adam/adtte.rds
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-25
# Change log : 2026-09-25  IGR  Initial version
# Notes      : Event: first ADAE record with TRTEMFL = "Y" and CQ01NAM =
#              "DERMATOLOGIC EVENTS" (the record flagged AOCC01FL). Censoring:
#              end of study participation (RFENDT). Origin: first dose (TRTSDT).
#              AVAL = ADT - STARTDT + 1 days.
# -----------------------------------------------------------------------------

source("programs/00_setup.R")

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))
adae <- readRDS(file.path(adam_dir, "adae.rds"))

adsl_saf <- adsl %>% filter(SAFFL == "Y")

# =============================================================================
# 1. Event and censoring definitions
# =============================================================================
derm_event <- event_source(
  dataset_name  = "adae",
  filter        = TRTEMFL == "Y" & CQ01NAM == "DERMATOLOGIC EVENTS",
  date          = ASTDT,
  order         = exprs(AESEQ),  # same-day events: lowest AESEQ, as AOCC01FL
  set_values_to = exprs(
    EVNTDESC = "DERMATOLOGIC EVENT",
    SRCDOM   = "ADAE",
    SRCVAR   = "ASTDT",
    SRCSEQ   = AESEQ
  )
)

end_of_study <- censor_source(
  dataset_name  = "adsl",
  date          = RFENDT,
  set_values_to = exprs(
    EVNTDESC = "END OF STUDY",
    SRCDOM   = "ADSL",
    SRCVAR   = "RFENDT"
  )
)

# =============================================================================
# 2. Parameter
# =============================================================================
# The earliest qualifying event is used; subjects without one are censored.
adtte <- derive_param_tte(
  dataset_adsl      = adsl_saf,
  source_datasets   = list(adsl = adsl_saf, adae = adae),
  start_date        = TRTSDT,
  event_conditions  = list(derm_event),
  censor_conditions = list(end_of_study),
  set_values_to     = exprs(
    PARAMCD = "TTDE",
    PARAM   = "Time to First Dermatologic Event (days)",
    PARAMN  = 1
  )
) %>%

  # AVAL in days, counting the day of first dose as day 1.
  derive_vars_duration(
    new_var    = AVAL,
    start_date = STARTDT,
    end_date   = ADT,
    add_one    = TRUE
  ) %>%
  mutate(AVALU = "DAYS") %>%

  # =============================================================================
  # 3. ADSL variables and treatment
  # =============================================================================
  derive_vars_merged(
    dataset_add = adsl_saf,
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(TRT01P, TRT01PN, TRT01A, TRT01AN, TRTSDT, TRTEDT,
                        SAFFL, AGE, AGEGR1, SEX, SITEGR1)
  ) %>%
  mutate(
    TRTP  = TRT01P,
    TRTPN = TRT01PN,
    TRTA  = TRT01A,
    TRTAN = TRT01AN
  ) %>%

  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order   = exprs(PARAMCD),
    new_var = ASEQ
  ) %>%
  arrange(STUDYID, USUBJID, PARAMCD) %>%
  select(
    STUDYID, USUBJID, ASEQ,
    TRTP, TRTPN, TRTA, TRTAN, TRTSDT, TRTEDT, SAFFL, AGE, AGEGR1, SEX, SITEGR1,
    PARAMCD, PARAM, PARAMN,
    AVAL, AVALU, STARTDT, ADT, CNSR, EVNTDESC, SRCDOM, SRCVAR, SRCSEQ
  )

# =============================================================================
# 4. Save
# =============================================================================
saveRDS(adtte, file.path(adam_dir, "adtte.rds"))

message("ADTTE: ", nrow(adtte), " rows x ", ncol(adtte), " columns; events: ",
        sum(adtte$CNSR == 0), ", censored: ", sum(adtte$CNSR == 1))
