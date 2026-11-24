# -----------------------------------------------------------------------------
# Program    : test-hta-subgroups.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Check hta_subgroups() (R/hta_subgroups.R) and derive_ae_tte()
#              (R/hta_ae_tte.R): subgroup hazard ratios against direct
#              coxph() calls and against {tern}, interaction tests, the
#              IQWiG size rule, and the adverse-event time-to-event data
#              against the committed ADTTE and a re-derivation without
#              {admiral}
# Inputs     : data/adam/adsl.xpt, adae.xpt, adtte.xpt, R/hta_subgroups.R,
#              R/hta_ae_tte.R
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-hta-subgroups.R")'
# Author     : Ignacio Garcia Ribelles
# Created    : 2026-11-25
# Change log : 2026-11-25  IGR  Initial version
# Notes      : Numerical checks require an absolute difference below 1e-8.
# -----------------------------------------------------------------------------

library(testthat)
library(dplyr)
library(haven)
library(survival)

source(test_path("../../R/hta_subgroups.R"))
source(test_path("../../R/hta_ae_tte.R"))

tol <- 1e-8
expect_same <- function(a, b) expect_lt(max(abs(as.numeric(a) - as.numeric(b))), tol)

read_adam <- function(name) {
  read_xpt(test_path("../../data/adam", paste0(name, ".xpt"))) %>%
    mutate(across(where(is.character), ~ na_if(.x, "")))
}
adsl  <- read_adam("adsl")
adae  <- read_adam("adae")
adtte <- read_adam("adtte") %>%
  mutate(AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
         SEX = factor(SEX, levels = c("F", "M")))

hd   <- "Xanomeline High Dose"
subs <- c(AGEGR1 = "Age group", SEX = "Sex")
sg   <- hta_subgroups(adtte, "TTDE", hd, subgroups = subs)
two  <- adtte %>% filter(TRTA %in% c("Placebo", hd)) %>% mutate(ACT = TRTA == hd, EVENT = 1 - CNSR)

test_that("overall and per-level hazard ratios equal direct coxph() fits", {
  ci <- summary(coxph(Surv(AVAL, EVENT) ~ ACT, data = two))$conf.int
  expect_same(c(sg$overall$hr, sg$overall$lo, sg$overall$hi), ci[1, c(1, 3, 4)])
  for (i in seq_len(nrow(sg$table))) {
    v  <- names(subs)[subs == sg$table$variable[i]]
    d  <- two[two[[v]] == sg$table$level[i], ]
    ci <- summary(coxph(Surv(AVAL, EVENT) ~ ACT, data = d))$conf.int
    expect_same(c(sg$table$hr[i], sg$table$lo[i], sg$table$hi[i]), ci[1, c(1, 3, 4)])
    expect_equal(sg$table$n[i], nrow(d))
    expect_equal(sg$table$events_active[i] + sg$table$events_control[i], sum(d$EVENT))
  }
})

test_that("interaction p-values are likelihood-ratio tests of the interaction", {
  for (v in names(subs)) {
    m0 <- coxph(as.formula(paste("Surv(AVAL, EVENT) ~ ACT +", v)), data = two)
    m1 <- coxph(as.formula(paste("Surv(AVAL, EVENT) ~ ACT *", v)), data = two)
    lr <- 2 * (m1$loglik[2] - m0$loglik[2])
    df <- length(coef(m1)) - length(coef(m0))
    expect_same(unique(sg$table$p_int[sg$table$variable == subs[[v]]]),
                pchisq(lr, df, lower.tail = FALSE))
  }
})

test_that("hazard ratios equal tern::extract_survival_subgroups()", {
  skip_if_not_installed("tern")
  d <- two %>% mutate(ARM = factor(TRTA, levels = c("Placebo", hd)), is_event = EVENT == 1)
  t <- tern::extract_survival_subgroups(
    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM",
                     subgroups = names(subs)), data = d)$hr
  expect_same(t$hr[1], sg$overall$hr)
  expect_same(t$hr[-1], sg$table$hr)
  expect_same(c(t$lcl[-1], t$ucl[-1]), c(sg$table$lo, sg$table$hi))
})

test_that("the IQWiG size rule is applied per subgroup variable", {
  ev <- sg$table$events_active + sg$table$events_control
  ok <- sg$table$n >= 10 & ave(ev, sg$table$variable, FUN = max) >= 10
  expect_equal(sg$table$iqwig_size_ok, ok)
  expect_true(all(sg$table$iqwig_size_ok))
})

test_that("hta_subgroups() returns a ggplot and rejects an unknown parameter", {
  expect_s3_class(sg$plot, "ggplot")
  expect_error(hta_subgroups(adtte, "NOPE", hd, subgroups = subs), "No records")
})

ae_tte <- derive_ae_tte(adsl, adae)

# check.attributes = FALSE: the transport files carry variable labels, the derived data not.
test_that("derive_ae_tte() reproduces the committed ADTTE for TTDE", {
  de <- ae_tte %>% filter(PARAMCD == "TTDE") %>% arrange(USUBJID)
  a  <- adtte %>% arrange(USUBJID)
  expect_equal(de$USUBJID, a$USUBJID, check.attributes = FALSE)
  expect_equal(de$AVAL, a$AVAL, check.attributes = FALSE)
  expect_equal(de$CNSR, a$CNSR, check.attributes = FALSE)
  expect_equal(as.numeric(de$ADT), as.numeric(a$ADT))
})

test_that("each adverse-event set has one record per safety subject", {
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  for (pc in c("TTAE", "TTSAE", "TTSEVAE", "TTDCAE", "TTDE")) {
    d <- ae_tte %>% filter(PARAMCD == pc)
    expect_setequal(d$USUBJID, saf)
    expect_equal(anyDuplicated(d$USUBJID), 0L)
  }
})

test_that("severe-TEAE events re-derived without admiral match", {
  ev <- adae %>%
    filter(TRTEMFL == "Y", ASEV == "SEVERE") %>%
    arrange(USUBJID, ASTDT, AESEQ) %>%
    distinct(USUBJID, .keep_all = TRUE)
  got <- ae_tte %>% filter(PARAMCD == "TTSEVAE", CNSR == 0) %>% arrange(USUBJID)
  expect_equal(got$USUBJID, ev$USUBJID, check.attributes = FALSE)
  expect_equal(as.numeric(got$ADT), as.numeric(ev$ASTDT))
})

test_that("discontinuation events are study discontinuations due to an AE", {
  dc  <- adsl %>% filter(SAFFL == "Y", DCSREAS == "ADVERSE EVENT") %>% arrange(USUBJID)
  got <- ae_tte %>% filter(PARAMCD == "TTDCAE", CNSR == 0) %>% arrange(USUBJID)
  expect_equal(got$USUBJID, dc$USUBJID, check.attributes = FALSE)
  expect_equal(as.numeric(got$ADT), as.numeric(dc$EOSDT))
  expect_equal(nrow(got), 92L)
})
