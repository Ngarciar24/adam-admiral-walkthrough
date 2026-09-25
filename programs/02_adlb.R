# -----------------------------------------------------------------------------
# Program    : 02_adlb.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Create ADLB (BDS) for the five parameters in
#              metadata/adlb_params.csv: analysis values, baseline, change from
#              baseline, reference-range indicators and shifts
# Inputs     : SDTM LB (via programs/00_setup.R); data/adam/adsl.rds
#              metadata/adlb_params.csv
# Outputs    : data/adam/adlb.rds
# Functions  : R/derive_ablfl.R (baseline flag and BASE)
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-17  IGR  Baseline rule moved to R/derive_ablfl.R
#              2026-09-25  IGR  Standard header; comments condensed
# Notes      : Keys: (USUBJID, ASEQ) and (USUBJID, PARAMCD, ADT, LBSEQ).
#              (USUBJID, PARAMCD, AVISIT) is not unique because all unscheduled
#              visits share one AVISIT; see docs/implementation-notes.md.
# -----------------------------------------------------------------------------

source("programs/00_setup.R")
source("R/derive_ablfl.R")

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))

# =============================================================================
# 1. Parameters and ADSL variables
# =============================================================================
# PARAMCD/PARAM/PARAMN/PARCAT1 come from the spec, not from LBTESTCD.
adlb <- lb %>%
  inner_join(adlb_params, by = "LBTESTCD") %>%

  # Subset of ADSL needed for subsetting, display and derivations.
  derive_vars_merged(
    dataset_add = adsl,
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(TRT01P, TRT01A, TRTSDT, TRTEDT, SAFFL, ITTFL, AGEGR1, SEX)
  ) %>%

  # Single-period study: record-level treatment equals period-01 treatment.
  mutate(
    TRTP = TRT01P,
    TRTA = TRT01A
  ) %>%

  # =============================================================================
  # 2. Timing
  # =============================================================================
  # ADT is the date part of LBDTC; no imputation needed (LBDTC complete).
  derive_vars_dt(
    dtc             = LBDTC,
    new_vars_prefix = "A"
  ) %>%

  # Study day relative to first dose; no day 0.
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars    = exprs(ADT)
  ) %>%

  # =============================================================================
  # 3. Analysis value, ranges and visits
  # =============================================================================
  # AVAL from the standardised result. AVALC keeps below-LLOQ results
  # (e.g. BILI "<3.42") that have no numeric value.
  # All unscheduled visits are grouped into one AVISIT (AVISITN 999); no
  # visit windows are applied.
  mutate(
    AVAL  = LBSTRESN,
    AVALC = LBSTRESC,
    AVALU = LBSTRESU,

    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI,

    AVISIT = case_when(
      str_detect(VISIT, "UNSCHEDULED") ~ "UNSCHEDULED",
      TRUE                             ~ VISIT
    ),
    AVISITN = if_else(str_detect(VISIT, "UNSCHEDULED"), 999, VISITNUM)
  ) %>%

  # =============================================================================
  # 4. Baseline
  # =============================================================================
  # ABLFL: last non-missing AVAL on or before TRTSDT, ties broken by LBSEQ.
  # BASE: AVAL of the ABLFL record, on every row of the subject/parameter.
  # Row order is not preserved; the dataset is sorted in section 7.
  derive_ablfl_base() %>%

  # =============================================================================
  # 5. Change from baseline
  # =============================================================================
  # Populated on every row with AVAL and BASE, including pre-baseline rows.
  derive_var_chg() %>%
  derive_var_pchg() %>%

  # =============================================================================
  # 6. Reference-range indicators
  # =============================================================================
  derive_var_anrind() %>%
  derive_var_base(
    by_vars    = exprs(STUDYID, USUBJID, PARAMCD),
    source_var = ANRIND,
    new_var    = BNRIND
  ) %>%

  # Shift from baseline category to current category. missing_value is left
  # at the admiral default ("NULL"), so the 5 BILI rows without ANRIND read
  # "NORMAL to NULL".
  derive_var_shift(
    new_var  = SHIFT1,
    from_var = BNRIND,
    to_var   = ANRIND
  ) %>%

  # =============================================================================
  # 7. Sequence number and sort order
  # =============================================================================
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order   = exprs(PARAMCD, ADT, LBSEQ),
    new_var = ASEQ
  ) %>%
  arrange(STUDYID, USUBJID, PARAMCD, ADT, LBSEQ) %>%

  # SDTM variables are kept for traceability back to LB (USUBJID + LBSEQ).
  select(
    STUDYID, USUBJID, ASEQ,
    TRTP, TRTA, TRT01P, TRT01A, TRTSDT, TRTEDT, SAFFL, ITTFL, AGEGR1, SEX,
    PARAMCD, PARAM, PARAMN, PARCAT1,
    AVAL, AVALC, AVALU, ABLFL, BASE, CHG, PCHG,
    ANRLO, ANRHI, ANRIND, BNRIND, SHIFT1,
    AVISIT, AVISITN, ADT, ADY,
    LBSEQ, LBTESTCD, LBTEST, LBCAT, LBSTRESN, LBBLFL, VISIT, VISITNUM, LBDTC
  )

# =============================================================================
# 8. Save
# =============================================================================
saveRDS(adlb, file.path(adam_dir, "adlb.rds"))

message("ADLB: ", nrow(adlb), " rows x ", ncol(adlb), " columns")
