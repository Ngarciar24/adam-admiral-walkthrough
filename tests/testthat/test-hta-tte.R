# -----------------------------------------------------------------------------
# Program    : test-hta-tte.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Check the hand calculations of hta/01-tte-safety.qmd against
#              {survival}, {survRM2} and {metafor}: Kaplan-Meier, log-rank,
#              restricted mean survival time and the risk ratio, odds ratio
#              and risk difference for "at least one TEAE"
# Inputs     : data/adam/adtte.xpt, adsl.xpt, adae.xpt (committed transport
#              files), R/hta_methods.R
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-hta-tte.R")'
# Author     : Ignacio Garcia Ribelles
# Created    : 2026-11-24
# Change log : 2026-11-24  IGR  Initial version
# Notes      : Each check requires an absolute difference below 1e-8.
# -----------------------------------------------------------------------------

library(testthat)
library(dplyr)
library(haven)
library(survival)
library(survRM2)
library(metafor)

source(test_path("../../R/hta_methods.R"))

tol <- 1e-8
expect_same <- function(hand, pkg) {
  expect_lt(max(abs(as.numeric(hand) - as.numeric(pkg))), tol)
}

trt_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
tte <- read_xpt(test_path("../../data/adam/adtte.xpt")) %>%
  filter(PARAMCD == "TTDE", SAFFL == "Y") %>%
  mutate(EVENT = 1 - CNSR)
adsl <- read_xpt(test_path("../../data/adam/adsl.xpt"))
adae <- read_xpt(test_path("../../data/adam/adae.xpt"))

test_that("Kaplan-Meier by hand equals survfit in every arm", {
  for (arm in trt_levels) {
    d    <- tte[tte$TRTA == arm, ]
    hand <- km_hand(d$AVAL, d$EVENT)
    pkg  <- summary(survfit(Surv(AVAL, EVENT) ~ 1, data = d))
    expect_equal(hand$time, pkg$time)
    expect_equal(hand$n.risk, pkg$n.risk)
    expect_equal(hand$n.event, pkg$n.event)
    expect_same(hand$surv, pkg$surv)
  }
})

test_that("log-rank by hand equals survdiff for each arm vs placebo", {
  for (arm in trt_levels[-1]) {
    d    <- tte[tte$TRTA %in% c("Placebo", arm), ]
    act  <- d$TRTA == arm
    hand <- logrank_hand(d$AVAL, d$EVENT, act)
    pkg  <- survdiff(Surv(AVAL, EVENT) ~ act, data = d)
    expect_same(hand$chisq, pkg$chisq)
    expect_same(hand$o_minus_e, pkg$obs[2] - pkg$exp[2])
    expect_same(hand$var, pkg$var[2, 2])
  }
})

test_that("RMST and its difference by hand equal survRM2 at tau = 168", {
  for (arm in trt_levels[-1]) {
    d    <- tte[tte$TRTA %in% c("Placebo", arm), ]
    act  <- d$TRTA == arm
    hand <- rmst_diff_hand(d$AVAL, d$EVENT, act, tau = 168)
    pkg  <- rmst2(d$AVAL, d$EVENT, as.integer(act), tau = 168)
    expect_same(hand$rmst1, pkg$RMST.arm1$rmst["Est."])
    expect_same(hand$rmst0, pkg$RMST.arm0$rmst["Est."])
    expect_same(rmst_hand(d$AVAL[act], d$EVENT[act], 168)$se, pkg$RMST.arm1$rmst["se"])
    expect_same(rmst_hand(d$AVAL[!act], d$EVENT[!act], 168)$se, pkg$RMST.arm0$rmst["se"])
    expect_same(c(hand$diff, hand$lower, hand$upper, hand$p),
                pkg$unadjusted.result[1, ])
  }
})

test_that("RR, OR and RD for any TEAE by hand equal metafor::escalc", {
  any_teae <- adsl %>%
    filter(SAFFL == "Y") %>%
    mutate(ANY = USUBJID %in% adae$USUBJID[adae$TRTEMFL == "Y"]) %>%
    group_by(TRT01A) %>%
    summarise(n = sum(ANY), N = n(), .groups = "drop")
  pbo <- any_teae[any_teae$TRT01A == "Placebo", ]
  expect_equal(c(pbo$n, pbo$N), c(65L, 86L))

  for (arm in trt_levels[-1]) {
    a    <- any_teae[any_teae$TRT01A == arm, ]
    hand <- effect_2x2_hand(a$n, a$N, pbo$n, pbo$N)
    for (i in 1:3) {
      m   <- c("RR", "OR", "RD")[i]
      es  <- escalc(m, ai = a$n, n1i = a$N, ci = pbo$n, n2i = pbo$N)
      ci  <- summary(es)
      back <- if (m == "RD") identity else exp
      expect_same(hand$yi[i], es$yi)
      expect_same(hand$vi[i], es$vi)
      expect_same(c(hand$lower[i], hand$upper[i]), back(c(ci$ci.lb, ci$ci.ub)))
    }
  }
})
