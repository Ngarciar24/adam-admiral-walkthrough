# -----------------------------------------------------------------------------
# Program    : test-adqsadas.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Dataset tests for ADQSADAS (ADAS-Cog(11) total): windows, record
#              selection, baseline, LOCF and change from baseline
# Inputs     : data/adam/adqsadas.rds, metadata/adqsadas_windows.csv
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-adqsadas.R")'
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-25
# Change log : 2026-09-25  IGR  Initial version
# -----------------------------------------------------------------------------

library(testthat)
library(dplyr)

adqs <- readRDS(testthat::test_path("../../data/adam/adqsadas.rds"))
win  <- readr::read_csv(testthat::test_path("../../metadata/adqsadas_windows.csv"),
                        show_col_types = FALSE)

obs  <- adqs %>% filter(is.na(DTYPE))
anl  <- adqs %>% filter(ANL01FL == "Y")

test_that("each observed record lies inside its analysis window", {
  chk <- obs %>% select(ADY, AVISITN) %>%
    left_join(win %>% select(AVISITN, AWLO, AWHI), by = "AVISITN")
  expect_true(all(is.na(chk$AWLO) | chk$ADY >= chk$AWLO))
  expect_true(all(is.na(chk$AWHI) | chk$ADY <= chk$AWHI))
  expect_equal(obs$AWTDIFF, abs(obs$ADY - obs$AWTARGET))
})

test_that("one analysed record per subject and window, closest to target", {
  expect_equal(anyDuplicated(anl[c("USUBJID", "PARAMCD", "AVISITN")]), 0L)
  best <- obs %>% group_by(USUBJID, AVISITN) %>% summarise(min_diff = min(AWTDIFF), .groups = "drop")
  chk <- anl %>% filter(is.na(DTYPE)) %>% inner_join(best, by = c("USUBJID", "AVISITN"))
  expect_equal(chk$AWTDIFF, chk$min_diff)
})

test_that("baseline is the analysed Baseline-window record", {
  bl <- adqs %>% filter(ABLFL == "Y")
  expect_true(all(bl$AVISITN == 0 & bl$ANL01FL == "Y"))
  expect_equal(anyDuplicated(bl$USUBJID), 0L)
  chk <- adqs %>% inner_join(bl %>% select(USUBJID, BL = AVAL), by = "USUBJID")
  expect_equal(chk$BASE, chk$BL)
})

test_that("every subject with a baseline has Week 8, 16 and 24 analysed records", {
  subj <- unique(adqs$USUBJID[adqs$ABLFL %in% "Y"])
  grid <- anl %>% filter(AVISITN > 0) %>% count(USUBJID)
  expect_setequal(grid$USUBJID, subj)
  expect_true(all(grid$n == 3))
})

test_that("LOCF records carry the last analysed value of an earlier window", {
  locf <- adqs %>% filter(DTYPE == "LOCF")
  expect_true(all(locf$ANL01FL == "Y" & locf$AVISITN > 0))
  prev <- anl %>% filter(is.na(DTYPE)) %>% select(USUBJID, PVIS = AVISITN, PVAL = AVAL)
  chk <- locf %>%
    inner_join(prev, by = "USUBJID", relationship = "many-to-many") %>%
    filter(PVIS < AVISITN) %>%
    group_by(USUBJID, AVISITN) %>%
    slice_max(PVIS, n = 1) %>%
    ungroup()
  expect_equal(nrow(chk), nrow(locf))
  expect_equal(chk$AVAL, chk$PVAL)
  expect_equal(nrow(locf), 222L)
})

test_that("CHG = AVAL - BASE after baseline and missing at baseline", {
  post <- adqs %>% filter(AVISITN > 0)
  expect_equal(post$CHG, post$AVAL - post$BASE)
  expect_true(all(is.na(adqs$CHG[adqs$AVISITN == 0])))
})
