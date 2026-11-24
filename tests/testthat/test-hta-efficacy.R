# -----------------------------------------------------------------------------
# Program    : test-hta-efficacy.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Check the hand calculations of hta/01b-efficacy-dossier.qmd:
#              Hedges' g for the ADAS-Cog(11) change to Week 24 against
#              metafor::escalc(), responder risk ratio and risk difference
#              against escalc(), and the ANCOVA mean difference against
#              Table 4 of this repository
# Inputs     : data/adam/adqsadas.xpt, outputs/t4_adascog_wk24_ancova.csv,
#              R/hta_methods.R
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-hta-efficacy.R")'
# Author     : Ignacio Garcia Ribelles
# Created    : 2026-11-25
# Change log : 2026-11-25  IGR  Initial version
# Notes      : Each numerical check requires an absolute difference below
#              1e-8. Responder threshold: 15% of the 0-70 scale range
#              (IQWiG General Methods 7.0, section 9.3.3).
# -----------------------------------------------------------------------------

library(testthat)
library(dplyr)
library(haven)
library(metafor)

source(test_path("../../R/hta_methods.R"))

tol <- 1e-8
expect_same <- function(a, b) expect_lt(max(abs(as.numeric(a) - as.numeric(b))), tol)

wk24 <- read_xpt(test_path("../../data/adam/adqsadas.xpt")) %>%
  filter(PARAMCD == "ACTOT", EFFFL == "Y", AVISITN == 24, ANL01FL == "Y")
pbo  <- wk24$CHG[wk24$TRTP == "Placebo"]
arms <- c("Xanomeline Low Dose", "Xanomeline High Dose")
threshold <- 0.15 * 70

test_that("the analysis set is the one of Table 4", {
  expect_equal(as.vector(table(wk24$TRTP)[c("Placebo", arms)]), c(79L, 81L, 74L))
  expect_equal(threshold, 10.5)
})

test_that("Hedges' g by hand equals escalc(measure = 'SMD')", {
  for (arm in arms) {
    x <- wk24$CHG[wk24$TRTP == arm]
    g <- hedges_g_hand(x, pbo)
    e <- escalc("SMD", m1i = mean(x), sd1i = sd(x), n1i = length(x),
                m2i = mean(pbo), sd2i = sd(pbo), n2i = length(pbo))
    s <- summary(e)
    expect_same(g$g, e$yi)
    expect_same(g$vi, e$vi)
    expect_same(c(g$lower, g$upper), c(s$ci.lb, s$ci.ub))
  }
})

test_that("responder RR and RD by hand equal escalc", {
  for (arm in arms) {
    x  <- wk24$CHG[wk24$TRTP == arm]
    a  <- sum(x >= threshold)
    c0 <- sum(pbo >= threshold)
    h  <- effect_2x2_hand(a, length(x), c0, length(pbo))
    rr <- escalc("RR", ai = a, n1i = length(x), ci = c0, n2i = length(pbo))
    rd <- escalc("RD", ai = a, n1i = length(x), ci = c0, n2i = length(pbo))
    expect_same(c(h$yi[1], h$vi[1]), c(rr$yi, rr$vi))
    expect_same(c(h$yi[3], h$vi[3]), c(rd$yi, rd$vi))
  }
})

test_that("the ANCOVA mean difference reproduces Table 4", {
  d   <- wk24 %>% mutate(TRTP = factor(TRTP, levels = c("Placebo", arms)))
  fit <- lm(CHG ~ TRTP + SITEGR1 + BASE, data = d)
  ci  <- confint(fit)["TRTPXanomeline High Dose", ]
  est <- coef(fit)[["TRTPXanomeline High Dose"]]
  t4  <- read.csv(test_path("../../outputs/t4_adascog_wk24_ancova.csv"))
  cell <- t4$V3[grepl("^LS mean difference", t4$row_name)]
  expect_equal(cell, sprintf("%.1f (%.1f, %.1f)", est, ci[1], ci[2]))
})
