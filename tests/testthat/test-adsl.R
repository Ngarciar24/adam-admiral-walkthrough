# -----------------------------------------------------------------------------
# Program    : test-adsl.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Dataset tests for ADSL: structure, treatment dates, population
#              flags, age groups and end-of-study status
# Inputs     : data/adam/adsl.rds; metadata/adsl_agegr1.csv; SDTM DM, EX
# Usage      : testthat::test_dir("tests/testthat")   (from the project root)
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-25  IGR  Standard header; comments condensed
#              2026-09-25  IGR  Treatment codes, TRTEDT rule, EFFFL, COMP24FL
#                               and SITEGR1 tests added
# Notes      : Two kinds of assertion: structural invariants that hold in any
#              study, and pinned counts for this extract (306 subjects, 52
#              screen failures) that act as regression guards. Where possible
#              the expected value is re-derived from SDTM with plain dplyr.
#              Labels, lengths, codelists and define.xml are checked by
#              python/validate_define.py and python/adam_conformance.py.
# -----------------------------------------------------------------------------

library(testthat)
library(dplyr)
library(stringr)

# --- Inputs ------------------------------------------------------------------
# test_path() resolves the same file from the project root and from
# tests/testthat.
adsl <- readRDS(testthat::test_path("../../data/adam/adsl.rds"))
agegr1_spec <- readr::read_csv(
  testthat::test_path("../../metadata/adsl_agegr1.csv"),
  show_col_types = FALSE
)

# SDTM re-read independently of the programs, with the same blank-to-NA step.
dm_src <- admiral::convert_blanks_to_na(pharmaversesdtm::dm)
ex_src <- admiral::convert_blanks_to_na(pharmaversesdtm::ex)


# =============================================================================
# 1. Structure
# =============================================================================
# ADSL is the denominator for every table and the merge source for every
# other ADaM dataset: no duplicated and no missing subjects.
test_that("ADSL has exactly one row per subject and covers every DM subject", {
  expect_equal(nrow(adsl), 306)
  expect_equal(nrow(adsl), nrow(dm_src))

  # Unique and non-missing key.
  expect_equal(dplyr::n_distinct(adsl$USUBJID), nrow(adsl))
  expect_false(any(is.na(adsl$USUBJID)))

  # Same subjects as DM, checked in both directions.
  expect_equal(setdiff(dm_src$USUBJID, adsl$USUBJID), character(0))
  expect_equal(setdiff(adsl$USUBJID, dm_src$USUBJID), character(0))
})


# =============================================================================
# 2. Treatment start is not after treatment end
# =============================================================================
# TRTSDT/TRTEDT bound the treatment-emergent window used in ADAE.
test_that("TRTSDT is never after TRTEDT where both are present", {
  both <- adsl %>% filter(!is.na(TRTSDT), !is.na(TRTEDT))

  # Guard against a vacuous pass on an empty subset.
  expect_gt(nrow(both), 0)
  expect_true(all(both$TRTSDT <= both$TRTEDT))
})


# =============================================================================
# 3. Treatment duration
# =============================================================================
# Inclusive-day convention, consistent with study day (no day 0).
test_that("TRTDURD equals TRTEDT - TRTSDT + 1, and is NA when either date is", {
  both <- adsl %>% filter(!is.na(TRTSDT), !is.na(TRTEDT))
  expect_gt(nrow(both), 0)

  expect_equal(
    as.numeric(both$TRTDURD),
    as.numeric(both$TRTEDT - both$TRTSDT) + 1
  )

  # No duration without both dates.
  one_missing <- adsl %>% filter(is.na(TRTSDT) | is.na(TRTEDT))
  expect_true(all(is.na(one_missing$TRTDURD)))
})


# =============================================================================
# 4. Safety population
# =============================================================================
# Expected SAFFL built directly from EX, independently of
# derive_var_merged_exist_flag(). The dose condition is shared with
# 01_adsl.R, so this checks the merge and the Y/N assignment, not the
# condition itself. The placebo clause (EXDOSE = 0 on placebo counts as a
# dose) does not discriminate in this extract: all EX records qualify.
test_that("SAFFL is Y exactly for subjects with a qualifying EX record", {
  dosed_usubjid <- ex_src %>%
    filter(EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) %>%
    pull(USUBJID) %>%
    unique()

  expected <- adsl %>%
    transmute(
      USUBJID,
      SAFFL,
      SAFFL_EXPECTED = if_else(USUBJID %in% dosed_usubjid, "Y", "N")
    )

  expect_equal(expected$SAFFL, expected$SAFFL_EXPECTED)

  # Both directions reported separately.
  expect_equal(
    sort(expected$USUBJID[expected$SAFFL == "Y"]),
    sort(unique(dosed_usubjid))
  )
  expect_equal(
    length(intersect(expected$USUBJID[expected$SAFFL == "N"], dosed_usubjid)),
    0L
  )

  # Subjects absent from EX are "N", not NA.
  never_in_ex <- setdiff(adsl$USUBJID, ex_src$USUBJID)
  expect_gt(length(never_in_ex), 0)
  expect_true(all(adsl$SAFFL[adsl$USUBJID %in% never_in_ex] == "N"))
})


# =============================================================================
# 5. Population flags are Y/N, never missing
# =============================================================================
# A missing flag drops subjects silently from filter(FL == "Y").
test_that("SAFFL and ITTFL contain only Y or N, with no missing values", {
  expect_false(any(is.na(adsl$SAFFL)))
  expect_false(any(is.na(adsl$ITTFL)))
  expect_true(all(adsl$SAFFL %in% c("Y", "N")))
  expect_true(all(adsl$ITTFL %in% c("Y", "N")))

  # Both values occur.
  expect_setequal(unique(adsl$SAFFL), c("Y", "N"))
  expect_setequal(unique(adsl$ITTFL), c("Y", "N"))
})


# =============================================================================
# 6. Screen failures
# =============================================================================
# Screen failures (planned arm code "Scrnfail") are in no population and have
# no treatment start or randomisation date.
test_that("screen failures carry no population flags, treatment start or randomisation date", {
  sf <- adsl %>% filter(ARMCD == "Scrnfail")

  expect_equal(nrow(sf), 52)
  expect_true(all(sf$SAFFL == "N"))
  expect_true(all(sf$ITTFL == "N"))
  expect_true(all(is.na(sf$TRTSDT)))
  expect_true(all(is.na(sf$RANDDT)))

  # Every other subject is in the ITT population.
  non_sf <- adsl %>% filter(ARMCD != "Scrnfail")
  expect_true(all(non_sf$ITTFL == "Y"))
  expect_true(all(!is.na(non_sf$RANDDT)))
})


# =============================================================================
# 7. Actual treatment in the safety population
# =============================================================================
# Safety is summarised by actual treatment; TRT01P and TRT01A differ for 12
# subjects in this study.
test_that("TRT01A is populated for every subject in the safety population", {
  treated <- adsl %>% filter(SAFFL == "Y")

  expect_gt(nrow(treated), 0)
  expect_false(any(is.na(treated$TRT01A)))
  expect_false(any(treated$TRT01A == ""))

  # Arm counts add up to the population (follows from the checks above).
  expect_equal(sum(table(treated$TRT01A)), nrow(treated))
})

test_that("treatment variables are missing for screen failures and coded by dose", {
  sf <- adsl %>% filter(ARMCD == "Scrnfail")
  expect_true(all(is.na(sf$TRT01P) & is.na(sf$TRT01A) & is.na(sf$TRT01PN) & is.na(sf$TRT01AN)))
  itt <- adsl %>% filter(ITTFL == "Y")
  dose <- c(Placebo = 0, "Xanomeline Low Dose" = 54, "Xanomeline High Dose" = 81)
  expect_equal(itt$TRT01PN, unname(dose[itt$TRT01P]))
  expect_equal(itt$TRT01AN, unname(dose[itt$TRT01A]))
  expect_equal(sum(itt$TRT01P != itt$TRT01A), 12L)
})

test_that("every dosed subject has a last-dose date", {
  # When the last EX record has no end date, the end-of-study date is used.
  saf <- adsl %>% filter(SAFFL == "Y")
  expect_false(anyNA(saf$TRTEDT))
  open_last <- pharmaversesdtm::ex %>%
    group_by(USUBJID) %>%
    slice_max(EXSTDTC, n = 1, with_ties = FALSE) %>%
    filter(is.na(EXENDTC) | EXENDTC == "") %>%
    pull(USUBJID)
  chk <- saf %>% filter(USUBJID %in% open_last)
  expect_equal(nrow(chk), 6L)
  expect_equal(chk$TRTEDT, chk$EOSDT)
})

test_that("EFFFL and COMP24FL are Y/N subsets of the safety population", {
  expect_setequal(unique(adsl$EFFFL), c("Y", "N"))
  expect_setequal(unique(adsl$COMP24FL), c("Y", "N"))
  expect_true(all(adsl$SAFFL[adsl$EFFFL == "Y"] == "Y"))
  expect_true(all(adsl$SAFFL[adsl$COMP24FL == "Y"] == "Y"))
  expect_equal(sum(adsl$EFFFL == "Y"), 234L)
  expect_equal(sum(adsl$COMP24FL == "Y"), 118L)
})

test_that("SITEGR1 pools sites with fewer than 3 ITT subjects in any arm", {
  itt <- adsl %>% filter(ITTFL == "Y")
  counts <- table(itt$SITEID, factor(itt$TRT01P))
  small <- rownames(counts)[apply(counts, 1, min) < 3]
  expect_equal(itt$SITEGR1, if_else(itt$SITEID %in% small, "900", itt$SITEID))
  expect_true("900" %in% itt$SITEGR1)
})


# =============================================================================
# 8. Age groups follow the metadata
# =============================================================================
test_that("AGEGR1 and AGEGR1N pair 1:1 and follow the metadata cut points", {
  # -- one label per code and one code per label -----------------------------
  pairs <- adsl %>% distinct(AGEGR1N, AGEGR1)
  expect_equal(nrow(pairs), dplyr::n_distinct(adsl$AGEGR1N))
  expect_equal(nrow(pairs), dplyr::n_distinct(adsl$AGEGR1))
  expect_false(any(is.na(adsl$AGEGR1N)))
  expect_false(any(is.na(adsl$AGEGR1)))

  # -- labels are exactly the spec labels ------------------------------------
  # Requires every band to be populated (all three are here).
  expect_equal(
    pairs %>% arrange(AGEGR1N) %>% pull(AGEGR1),
    agegr1_spec %>% arrange(AGEGR1N) %>% pull(AGEGR1)
  )

  # -- each subject falls in exactly one band --------------------------------
  # Computed from AGE_LOW/AGE_HIGH, which also checks the bands neither
  # overlap nor leave gaps over the observed ages.
  n_bands <- vapply(
    adsl$AGE,
    function(a) sum(a >= agegr1_spec$AGE_LOW & a <= agegr1_spec$AGE_HIGH),
    integer(1)
  )
  expect_true(all(n_bands == 1))

  expected_agegr1n <- vapply(
    adsl$AGE,
    function(a) agegr1_spec$AGEGR1N[a >= agegr1_spec$AGE_LOW & a <= agegr1_spec$AGE_HIGH][1],
    numeric(1)
  )
  expect_equal(as.numeric(adsl$AGEGR1N), expected_agegr1n)
})


# =============================================================================
# 9. End-of-study status
# =============================================================================
# EOSSTT is missing by design for screen failures, and only for them.
test_that("EOSSTT is COMPLETED/DISCONTINUED and is missing exactly for screen failures", {
  expect_true(all(adsl$EOSSTT %in% c("COMPLETED", "DISCONTINUED") | is.na(adsl$EOSSTT)))
  expect_setequal(
    unique(adsl$EOSSTT[!is.na(adsl$EOSSTT)]),
    c("COMPLETED", "DISCONTINUED")
  )

  # Same subjects, not just the same count.
  eosstt_missing <- sort(adsl$USUBJID[is.na(adsl$EOSSTT)])
  scrnfail <- sort(adsl$USUBJID[adsl$ARMCD == "Scrnfail"])

  expect_equal(length(eosstt_missing), 52)
  expect_equal(eosstt_missing, scrnfail)
})
