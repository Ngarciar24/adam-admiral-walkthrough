# -----------------------------------------------------------------------------
# Program    : test-adtte.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Dataset tests for ADTTE (time to first dermatologic event):
#              structure, event and censoring rules re-derived from ADAE and
#              ADSL, analysis value arithmetic
# Inputs     : data/adam/adtte.rds, data/adam/adae.rds, data/adam/adsl.rds
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-adtte.R")'
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-25
# Change log : 2026-09-25  IGR  Initial version
# -----------------------------------------------------------------------------

library(testthat)
library(dplyr)

adtte <- readRDS(testthat::test_path("../../data/adam/adtte.rds"))
adae  <- readRDS(testthat::test_path("../../data/adam/adae.rds"))
adsl  <- readRDS(testthat::test_path("../../data/adam/adsl.rds"))

test_that("one TTDE record per subject in the safety population", {
  expect_equal(unique(adtte$PARAMCD), "TTDE")
  expect_equal(anyDuplicated(adtte$USUBJID), 0L)
  expect_setequal(adtte$USUBJID, adsl$USUBJID[adsl$SAFFL == "Y"])
})

test_that("events are the first treatment-emergent dermatologic AE", {
  # Expected event per subject, re-derived from ADAE without admiral.
  ev <- adae %>%
    filter(TRTEMFL == "Y", CQ01NAM == "DERMATOLOGIC EVENTS") %>%
    arrange(USUBJID, ASTDT, AESEQ) %>%
    distinct(USUBJID, .keep_all = TRUE) %>%
    select(USUBJID, ASTDT, AESEQ)
  got <- adtte %>% filter(CNSR == 0)
  expect_setequal(got$USUBJID, ev$USUBJID)
  chk <- inner_join(got, ev, by = "USUBJID")
  expect_equal(chk$ADT, chk$ASTDT)
  expect_equal(chk$SRCSEQ, chk$AESEQ)
  expect_true(all(got$SRCDOM == "ADAE" & got$SRCVAR == "ASTDT"))
  expect_equal(nrow(got), 152L)
})

test_that("subjects without an event are censored at end of study", {
  cen <- adtte %>% filter(CNSR == 1) %>%
    left_join(adsl %>% select(USUBJID, RFENDT), by = "USUBJID")
  expect_equal(cen$ADT, cen$RFENDT)
  expect_true(all(cen$SRCDOM == "ADSL" & is.na(cen$SRCSEQ)))
  expect_setequal(unique(adtte$CNSR), c(0L, 1L))
})

test_that("AVAL = ADT - STARTDT + 1 with STARTDT = first dose", {
  expect_equal(adtte$STARTDT, adtte$TRTSDT)
  expect_equal(adtte$AVAL, as.numeric(adtte$ADT - adtte$STARTDT) + 1)
  expect_true(all(adtte$AVAL >= 1))
  expect_true(all(adtte$AVALU == "DAYS"))
})
