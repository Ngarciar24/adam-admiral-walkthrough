# -----------------------------------------------------------------------------
# Program    : hta_ae_tte.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Time to first event for the standard adverse-event sets of an
#              HTA dossier: any TEAE, serious TEAE, severe TEAE, study
#              discontinuation due to an adverse event, and the dermatologic
#              TEAE already held in ADTTE
# Inputs     : ADSL and ADAE data frames (as read from data/adam/*.xpt)
# Outputs    : function derive_ae_tte(), returning an ADTTE-like data frame
#              with one row per subject and parameter
# Author     : Ignacio Garcia Ribelles
# Created    : 2026-11-25
# Change log : 2026-11-25  IGR  Initial version
# Notes      : Same rules as programs/04_adtte.R, so that TTDE reproduces the
#              committed ADTTE exactly (tested in test-hta-ae-tte.R).
#              Origin: first dose (TRTSDT). Censoring: end of study
#              participation (RFENDT). AVAL = ADT - STARTDT + 1 days.
#              Discontinuation uses the study discontinuation reason
#              (DCSREAS) and date (EOSDT). ADSL holds no reason for stopping
#              treatment alone. Severity is the pilot's MILD / MODERATE /
#              SEVERE scale, not CTCAE grades.
# -----------------------------------------------------------------------------

derive_ae_tte <- function(adsl, adae) {
  adsl_saf <- dplyr::filter(adsl, SAFFL == "Y")

  # One event source per AE set: first qualifying ADAE record by date, then
  # lowest AESEQ, as in programs/04_adtte.R.
  ae_event <- function(cond, desc) {
    admiral::event_source(
      dataset_name  = "adae",
      filter        = !!cond,
      date          = ASTDT,
      order         = admiral::exprs(AESEQ),
      set_values_to = admiral::exprs(EVNTDESC = !!desc, SRCDOM = "ADAE",
                                     SRCVAR = "ASTDT", SRCSEQ = AESEQ)
    )
  }
  end_of_study <- admiral::censor_source(
    dataset_name  = "adsl",
    date          = RFENDT,
    set_values_to = admiral::exprs(EVNTDESC = "END OF STUDY", SRCDOM = "ADSL",
                                   SRCVAR = "RFENDT")
  )
  dc_event <- admiral::event_source(
    dataset_name  = "adsl",
    filter        = DCSREAS == "ADVERSE EVENT",
    date          = EOSDT,
    set_values_to = admiral::exprs(EVNTDESC = "DISCONTINUED DUE TO AE", SRCDOM = "ADSL",
                                   SRCVAR = "EOSDT")
  )

  params <- list(
    list("TTAE",    "Time to First TEAE (days)",
         ae_event(rlang::expr(TRTEMFL == "Y"), "TEAE")),
    list("TTSAE",   "Time to First Serious TEAE (days)",
         ae_event(rlang::expr(TRTEMFL == "Y" & AESER == "Y"), "SERIOUS TEAE")),
    list("TTSEVAE", "Time to First Severe TEAE (days)",
         ae_event(rlang::expr(TRTEMFL == "Y" & ASEV == "SEVERE"), "SEVERE TEAE")),
    list("TTDCAE",  "Time to Study Discontinuation due to AE (days)", dc_event),
    list("TTDE",    "Time to First Dermatologic Event (days)",
         ae_event(rlang::expr(TRTEMFL == "Y" & CQ01NAM == "DERMATOLOGIC EVENTS"),
                  "DERMATOLOGIC EVENT"))
  )

  out <- lapply(params, function(p) {
    admiral::derive_param_tte(
      dataset_adsl      = adsl_saf,
      source_datasets   = list(adsl = adsl_saf, adae = adae),
      start_date        = TRTSDT,
      event_conditions  = list(p[[3]]),
      censor_conditions = list(end_of_study),
      set_values_to     = admiral::exprs(PARAMCD = !!p[[1]], PARAM = !!p[[2]])
    )
  })

  dplyr::bind_rows(out) |>
    dplyr::mutate(AVAL = as.numeric(ADT - STARTDT) + 1, AVALU = "DAYS") |>
    dplyr::left_join(
      dplyr::select(adsl_saf, USUBJID, TRTA = TRT01A, AGE, AGEGR1, SEX, SITEGR1),
      by = "USUBJID"
    )
}
