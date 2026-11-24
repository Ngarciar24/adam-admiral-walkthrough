# -----------------------------------------------------------------------------
# Program    : test-hta-meta.R
# Study      : Published trial results (metadat::dat.bcg, Colditz et al. 1994)
# Purpose    : Check the hand calculations of hta/02-meta-analysis.qmd against
#              {metafor}: per-trial log risk ratios and variances,
#              fixed-effect pooling, REML tau^2, Hartung-Knapp interval,
#              prediction interval, Q, I^2 and H^2, and the random-effects
#              model refitted as a mixed model with rma.mv()
# Inputs     : metadat::dat.bcg, R/hta_methods.R
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-hta-meta.R")'
# Author     : Ignacio Garcia Ribelles
# Created    : 2026-11-24
# Change log : 2026-11-24  IGR  Initial version
# Notes      : Hand calculations: absolute difference below 1e-8. The
#              rma.mv() refit compares two numerical optimisers, not a hand
#              calculation, and uses 1e-6.
# -----------------------------------------------------------------------------

library(testthat)
library(metafor)

source(test_path("../../R/hta_methods.R"))

tol <- 1e-8
expect_same <- function(hand, pkg, tolerance = tol) {
  expect_lt(max(abs(as.numeric(hand) - as.numeric(pkg))), tolerance)
}

bcg <- metadat::dat.bcg
es  <- escalc("RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = bcg)
tight <- list(threshold = 1e-12, maxiter = 1000)

test_that("the data are the 13 BCG trials", {
  expect_equal(nrow(bcg), 13L)
  expect_true(all(c(bcg$tpos, bcg$cpos) > 0))  # no zero cells, no correction
})

test_that("log RR and its variance by hand equal escalc", {
  n1 <- bcg$tpos + bcg$tneg
  n0 <- bcg$cpos + bcg$cneg
  expect_same(log(bcg$tpos / n1) - log(bcg$cpos / n0), es$yi)
  expect_same(1 / bcg$tpos - 1 / n1 + 1 / bcg$cpos - 1 / n0, es$vi)
  for (i in seq_len(nrow(bcg))) {
    h <- effect_2x2_hand(bcg$tpos[i], n1[i], bcg$cpos[i], n0[i])
    expect_same(c(h$yi[1], h$vi[1]), c(es$yi[i], es$vi[i]))
  }
})

test_that("fixed-effect pooling by hand equals rma(method = 'FE')", {
  hand <- fe_hand(es$yi, es$vi)
  pkg  <- rma(yi, vi, data = es, method = "FE")
  expect_same(hand$est, pkg$beta)
  expect_same(hand$se, pkg$se)
  expect_same(c(hand$lower, hand$upper), c(pkg$ci.lb, pkg$ci.ub))
  expect_same(hand$zval, pkg$zval)
  expect_same(hand$Q, pkg$QE)
  expect_same(hand$Qp, pkg$QEp)
})

test_that("REML tau^2 by Fisher scoring equals rma(method = 'REML')", {
  pkg <- rma(yi, vi, data = es, method = "REML", control = tight)
  expect_same(reml_tau2_hand(es$yi, es$vi), pkg$tau2)
})

test_that("random effects with Hartung-Knapp by hand equals rma(test = 'knha')", {
  pkg  <- rma(yi, vi, data = es, method = "REML", test = "knha", control = tight)
  hand <- re_hand(es$yi, es$vi, reml_tau2_hand(es$yi, es$vi), knha = TRUE)
  pred <- predict(pkg)
  expect_same(hand$est, pkg$beta)
  expect_same(hand$se, pkg$se)
  expect_same(c(hand$lower, hand$upper), c(pkg$ci.lb, pkg$ci.ub))
  expect_same(hand$pval, pkg$pval)
  expect_same(c(hand$pi_lower, hand$pi_upper), c(pred$pi.lb, pred$pi.ub))
})

test_that("random effects without Hartung-Knapp by hand equals rma(test = 'z')", {
  pkg  <- rma(yi, vi, data = es, method = "REML", control = tight)
  hand <- re_hand(es$yi, es$vi, reml_tau2_hand(es$yi, es$vi), knha = FALSE)
  expect_same(c(hand$est, hand$se, hand$lower, hand$upper, hand$pval),
              c(pkg$beta, pkg$se, pkg$ci.lb, pkg$ci.ub, pkg$pval))
})

test_that("Q, I^2 and H^2 by hand equal rma", {
  pkg  <- rma(yi, vi, data = es, method = "REML", control = tight)
  hand <- heterogeneity_hand(es$yi, es$vi, pkg$tau2)
  expect_same(c(hand$Q, hand$I2, hand$H2), c(pkg$QE, pkg$I2, pkg$H2))
  # With the DerSimonian-Laird tau^2, I^2 equals (Q - df) / Q.
  dl <- rma(yi, vi, data = es, method = "DL")
  expect_same(heterogeneity_hand(es$yi, es$vi, dl$tau2)$I2, dl$I2)
  expect_same(hand$I2_from_Q, dl$I2)
})

test_that("rma.mv with a random intercept per trial reproduces rma (REML)", {
  re <- rma(yi, vi, data = es, method = "REML", control = tight)
  mv <- rma.mv(yi, vi, random = ~ 1 | trial, data = es, method = "REML",
               control = list(optimizer = "optim", optmethod = "BFGS",
                              optCtrl = list(reltol = 1e-14)))
  expect_same(mv$sigma2, re$tau2, tolerance = 1e-6)
  expect_same(mv$beta, re$beta, tolerance = 1e-6)
  expect_same(mv$se, re$se, tolerance = 1e-6)
})
