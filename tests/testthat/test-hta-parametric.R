# -----------------------------------------------------------------------------
# Program    : test-hta-parametric.R
# Study      : German Breast Cancer Study Group (flexsurv::bc)
# Purpose    : Check the hand calculations of hta/01c-parametric-survival.qmd
#              against {flexsurv}: survival functions, log-likelihoods, AIC,
#              the exponential maximum likelihood estimate and restricted
#              mean survival from fitted models
# Inputs     : flexsurv::bc, R/hta_methods.R
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-hta-parametric.R")'
# Author     : Ignacio Garcia Ribelles
# Created    : 2026-11-25
# Change log : 2026-11-25  IGR  Initial version
# Notes      : Hand calculations: absolute difference below 1e-8. The
#              comparison of two numerical integrals (hand against
#              {flexsurv}) uses 1e-6, and the exact integral of the
#              exponential model uses 1e-8.
# -----------------------------------------------------------------------------

library(testthat)
library(flexsurv)

source(test_path("../../R/hta_methods.R"))

tol <- 1e-8
expect_same <- function(a, b, tolerance = tol) {
  expect_lt(max(abs(as.numeric(a) - as.numeric(b))), tolerance)
}
est <- function(fit) setNames(fit$res[, "est"], rownames(fit$res))

bc <- flexsurv::bc
bc$time3  <- pmin(bc$recyrs, 3)
bc$event3 <- as.integer(bc$censrec == 1 & bc$recyrs <= 3)
dists <- c("exp", "weibull", "gompertz", "llogis", "lnorm")
fits  <- lapply(dists, function(x) flexsurvreg(Surv(time3, event3) ~ 1, data = bc, dist = x))
names(fits) <- dists

test_that("the exercise uses 686 patients and 224 events by 3 years", {
  expect_equal(nrow(bc), 686L)
  expect_equal(sum(bc$censrec), 299L)
  expect_equal(sum(bc$event3), 224L)
})

test_that("hand log-likelihood and AIC equal flexsurv for five models", {
  for (x in dists) {
    f  <- fits[[x]]
    ll <- param_loglik_hand(x, bc$time3, bc$event3, est(f))
    expect_same(ll, f$loglik)
    expect_same(-2 * ll + 2 * length(est(f)), AIC(f))
  }
})

test_that("hand survival functions equal flexsurv at 1, 3, 5 and 7 years", {
  for (x in dists) {
    f <- fits[[x]]
    expect_same(param_surv_hand(x, c(1, 3, 5, 7), est(f))$S,
                summary(f, t = c(1, 3, 5, 7), ci = FALSE)[[1]]$est)
  }
})

test_that("the exponential rate is events divided by time at risk", {
  expect_same(exp_rate_hand(bc$time3, bc$event3), est(fits$exp)[["rate"]])
})

test_that("restricted mean from a fitted model: hand integral is right", {
  rate <- est(fits$exp)[["rate"]]
  # Exact for the exponential: (1 - exp(-rate tau)) / rate.
  expect_same(param_rmst_hand("exp", c(rate = rate), 7), (1 - exp(-rate * 7)) / rate)
  for (x in dists) {
    f <- fits[[x]]
    expect_same(param_rmst_hand(x, est(f), 7),
                summary(f, t = 7, type = "rmst", ci = FALSE)[[1]]$est, tolerance = 1e-6)
  }
})

test_that("an unknown distribution is rejected", {
  expect_error(param_surv_hand("weibul", 1, c(shape = 1, scale = 1)), "Unknown distribution")
})
