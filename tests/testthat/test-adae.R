# -----------------------------------------------------------------------------
# Program    : test-adae.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Dataset tests for ADAE: structure, dates and imputation flags,
#              study days, duration, treatment-emergent flag, occurrence flags,
#              traceability to SDTM AE
# Inputs     : data/adam/adae.rds, data/adam/adsl.rds
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-adae.R")'
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-25  IGR  Standard header; comments condensed
# -----------------------------------------------------------------------------

library(testthat)
library(dplyr)

# Works from the project root or from tests/testthat.
root <- if (file.exists("data/adam/adae.rds")) "." else "../.."
adae <- readRDS(file.path(root, "data/adam/adae.rds"))
adsl <- readRDS(file.path(root, "data/adam/adsl.rds"))

# ===========================================================================
# Structure: OCCDS, one row per collected adverse event
# ===========================================================================
test_that("ADAE is one row per source AE record", {
  # Same row count as SDTM AE: non-emergent events are flagged, not dropped.
  expect_equal(nrow(adae), 1191L)
  expect_equal(anyDuplicated(adae[c("USUBJID", "AESEQ")]), 0L)
  # ASEQ is the ADaM within-subject key: USUBJID + ASEQ must identify one row.
  expect_equal(anyDuplicated(adae[c("USUBJID", "ASEQ")]), 0L)
})

test_that("ADAE is not BDS", {
  # OCCDS carries no BDS analysis variables.
  expect_false(any(c("AVAL", "AVALC", "PARAMCD", "PARAM", "BASE", "CHG") %in%
                     names(adae)))
})

test_that("every ADAE subject exists in ADSL", {
  # An AE subject missing from ADSL has no treatment arm.
  orphans <- setdiff(adae$USUBJID, adsl$USUBJID)
  expect_equal(orphans, character(0))
  # 225 of the 306 ADSL subjects reported at least one AE.
  expect_equal(n_distinct(adae$USUBJID), 225L)
})

# ===========================================================================
# Dates
# ===========================================================================
test_that("ASTDT is never after AENDT", {
  # Catches an imputation that moves a partial start date past the end date
  # (includes the four records with an imputed start and a collected end).
  bad <- adae %>% filter(!is.na(ASTDT), !is.na(AENDT), ASTDT > AENDT)
  expect_equal(nrow(bad), 0L)
})

test_that("ASTDTF marks exactly the records whose AESTDTC was partial", {
  # Flag set on every imputed date and on no complete date.
  expect_equal(sum(!is.na(adae$ASTDTF)), 26L)            # 11 year-only + 15 year-month
  expect_setequal(na.omit(adae$ASTDTF), c("D", "M"))
  expect_true(all(nchar(adae$AESTDTC[!is.na(adae$ASTDTF)]) < 10))
  expect_true(all(nchar(adae$AESTDTC[is.na(adae$ASTDTF)]) == 10))
})

test_that("AENDT is populated exactly when AEENDTC was collected", {
  # End dates are not imputed; the 473 ongoing events stay missing.
  expect_equal(sum(is.na(adae$AENDT)), 473L)
  expect_true(all(is.na(adae$AENDT) == is.na(adae$AEENDTC)))
})

test_that("study days follow the no-day-zero convention", {
  # Day of first dose is Day 1, the day before is Day -1.
  expect_false(any(adae$ASTDY == 0, na.rm = TRUE))
  expect_false(any(adae$AENDY == 0, na.rm = TRUE))
  # ADY is the record's analysis day, which for an occurrence record is onset.
  expect_equal(adae$ADY, adae$ASTDY)
  # Recompute ASTDY independently of admiral.
  manual <- with(adae, ifelse(ASTDT >= TRTSDT,
                              as.integer(ASTDT - TRTSDT) + 1L,
                              as.integer(ASTDT - TRTSDT)))
  expect_equal(adae$ASTDY, manual)
})

test_that("ADURN equals AENDT - ASTDT + 1 and carries its unit", {
  d <- adae %>% filter(!is.na(ADURN))
  expect_equal(d$ADURN, as.numeric(d$AENDT - d$ASTDT) + 1)
  expect_true(all(d$ADURU == "DAYS"))
  # The unit variable must be populated when and only when the value is.
  expect_true(all(is.na(adae$ADURU) == is.na(adae$ADURN)))
  expect_equal(sum(is.na(adae$ADURN)), 473L)  # ongoing events, not zero-duration ones
})

# ===========================================================================
# TRTEMFL
# ===========================================================================
test_that("TRTEMFL takes only 'Y' or NA", {
  # "Y" or NA by design (NA: not emergent or not determinable). Downstream
  # code filters on TRTEMFL == "Y", never != "N".
  expect_setequal(unique(adae$TRTEMFL), c("Y", NA))
  expect_equal(sum(adae$TRTEMFL == "Y", na.rm = TRUE), 1122L)
})

test_that("no event with a COLLECTED onset before first dose is treatment-emergent", {
  # Restricted to collected (not imputed) onset dates.
  bad <- adae %>% filter(TRTEMFL == "Y", is.na(ASTDTF), ASTDT < TRTSDT)
  expect_equal(nrow(bad), 0L)
})

test_that("the non-emergent records are non-emergent for a stated reason", {
  # The 69 unflagged records split exactly into onset before first dose (65)
  # and onset more than 30 days after last dose (4).
  nonte <- adae %>% filter(is.na(TRTEMFL))
  expect_equal(nrow(nonte), 69L)
  expect_true(all(nonte$ASTDT < nonte$TRTSDT | nonte$ASTDT > nonte$TRTEDT + 30))
  expect_equal(sum(nonte$ASTDT < nonte$TRTSDT), 65L)
  expect_equal(sum(nonte$ASTDT > nonte$TRTEDT + 30), 4L)
})

test_that("every treatment-emergent event belongs to a safety-population subject", {
  # A treatment-emergent event implies a dosed subject (SAFFL = "Y").
  te <- adae %>% filter(TRTEMFL == "Y")
  expect_true(all(te$SAFFL == "Y"))
  expect_equal(n_distinct(te$USUBJID), 217L)
})

# ===========================================================================
# Occurrence flags
# ===========================================================================
test_that("occurrence flags mark one record per counting unit", {
  # One flagged record per subject, per subject/SOC and per subject/SOC/PT.
  te <- adae %>% filter(TRTEMFL == "Y")
  expect_equal(sum(adae$AOCCFL  == "Y", na.rm = TRUE), n_distinct(te$USUBJID))
  expect_equal(sum(adae$AOCCSFL == "Y", na.rm = TRUE),
               nrow(distinct(te, USUBJID, AEBODSYS)))
  expect_equal(sum(adae$AOCCPFL == "Y", na.rm = TRUE),
               nrow(distinct(te, USUBJID, AEBODSYS, AEDECOD)))
})

test_that("occurrence flags never land on a non-emergent record", {
  # Flags are NA outside the treatment-emergent subset.
  nonte <- adae %>% filter(is.na(TRTEMFL))
  expect_true(all(is.na(nonte$AOCCFL) & is.na(nonte$AOCCSFL) & is.na(nonte$AOCCPFL)))
  # AOCCFL sits on the subject's earliest emergent onset.
  chk <- adae %>%
    filter(TRTEMFL == "Y") %>%
    group_by(USUBJID) %>%
    summarise(ok = ASTDT[which(AOCCFL == "Y")] == min(ASTDT), .groups = "drop")
  expect_true(all(chk$ok))
})

# ===========================================================================
# Traceability
# ===========================================================================
test_that("the SDTM source variables survive into ADAE", {
  # Predecessor variables are kept.
  expect_true(all(c("AESEQ", "AESTDTC", "AEENDTC", "AESTDY", "AEENDY",
                    "AESEV", "AEREL", "AEOUT", "AETERM", "AEDECOD",
                    "AEBODSYS") %in% names(adae)))
  # ASEV/AREL are unrecoded copies in this study.
  expect_equal(adae$ASEV, adae$AESEV)
  expect_equal(adae$AREL, adae$AEREL)
  expect_equal(adae$ASEVN,
               as.integer(factor(adae$AESEV,
                                 levels = c("MILD", "MODERATE", "SEVERE"))))
})

test_that("ASTDY is recomputed rather than copied from SDTM AESTDY", {
  # ASTDY (relative to TRTSDT) and SDTM AESTDY (relative to RFSTDTC) agree on
  # all complete dates except 01-716-1063 AESEQ 1: onset equals RFSTDTC and
  # TRTSDT, so the study day is 1, but SDTM AESTDY is 366 (a source data error).
  cmp <- adae %>% filter(is.na(ASTDTF), !is.na(AESTDY))
  expect_equal(nrow(cmp), 1165L)
  expect_equal(sum(cmp$ASTDY != cmp$AESTDY), 1L)
  expect_equal(cmp$USUBJID[cmp$ASTDY != cmp$AESTDY], "01-716-1063")
})
