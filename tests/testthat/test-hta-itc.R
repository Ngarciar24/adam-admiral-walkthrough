# -----------------------------------------------------------------------------
# Program    : test-hta-itc.R
# Study      : Published trial results (netmeta::smokingcessation,
#              Hasselblad 1998)
# Purpose    : Check the hand calculations of hta/03-indirect-comparison.qmd
#              against {netmeta} and {metafor}: per-study log odds ratios
#              with the zero-cell correction, fixed-effect pooling per
#              comparison and the Bucher indirect comparison
# Inputs     : netmeta::smokingcessation, R/hta_methods.R
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-hta-itc.R")'
# Author     : Ignacio Garcia Ribelles
# Created    : 2026-11-24
# Change log : 2026-11-24  IGR  Initial version
# Notes      : Each check requires an absolute difference below 1e-8.
#              A = no contact, B = self-help, C = individual counselling.
# -----------------------------------------------------------------------------

library(testthat)
library(metafor)
library(netmeta)

source(test_path("../../R/hta_methods.R"))

tol <- 1e-8
expect_same <- function(hand, pkg) {
  expect_lt(max(abs(as.numeric(hand) - as.numeric(pkg))), tol)
}

data(smokingcessation, package = "netmeta")
smk <- smokingcessation
smk$study <- seq_len(nrow(smk))
two <- smk[smk$treat3 == "", ]
ab  <- two[two$treat1 == "A" & two$treat2 == "B", ]
ac  <- two[two$treat1 == "A" & two$treat2 == "C", ]

hand_ab <- log_or_hand(ab$event2, ab$n2, ab$event1, ab$n1)  # B vs A
hand_ac <- log_or_hand(ac$event2, ac$n2, ac$event1, ac$n1)  # C vs A

pw <- pairwise(treat = list(treat1, treat2, treat3), event = list(event1, event2, event3),
               n = list(n1, n2, n3), data = smk, studlab = study, sm = "OR")

test_that("the Bucher inputs are 3 and 14 two-arm studies", {
  expect_equal(nrow(smk), 24L)
  expect_equal(c(nrow(ab), nrow(ac)), c(3L, 14L))
  expect_equal(sum(hand_ac$corrected), 1L)  # one study with a zero cell
})

test_that("Bucher function reproduces the worked example by hand", {
  # log HR A vs B -0.40 (SE 0.10), C vs B -0.10 (SE 0.15)
  b <- bucher_hand(-0.40, 0.10, -0.10, 0.15)
  expect_same(b$est, -0.30)
  expect_same(b$se, sqrt(0.10^2 + 0.15^2))
})

test_that("per-study log OR by hand equals pairwise() and escalc()", {
  for (x in list(list(d = ab, h = hand_ab), list(d = ac, h = hand_ac))) {
    p <- pw[pw$studlab %in% x$d$study & pw$treat1 == "A", ]
    p <- p[match(x$d$study, p$studlab), ]
    # pairwise() reports treat1 (A) vs treat2, the hand result treat2 vs A.
    expect_same(x$h$yi, -p$TE)
    expect_same(sqrt(x$h$vi), p$seTE)
    es <- escalc("OR", ai = event2, n1i = n2, ci = event1, n2i = n1, data = x$d)
    expect_same(x$h$yi, es$yi)
    expect_same(x$h$vi, es$vi)
  }
})

test_that("fixed-effect pooling per comparison by hand equals rma", {
  for (h in list(hand_ab, hand_ac)) {
    fe  <- fe_hand(h$yi, h$vi)
    pkg <- rma(h$yi, h$vi, method = "FE")
    expect_same(c(fe$est, fe$se, fe$Q), c(pkg$beta, pkg$se, pkg$QE))
  }
})

test_that("Bucher by hand equals netmeta on the star network", {
  fe_ab <- fe_hand(hand_ab$yi, hand_ab$vi)
  fe_ac <- fe_hand(hand_ac$yi, hand_ac$vi)
  bu    <- bucher_hand(fe_ab$est, fe_ab$se, fe_ac$est, fe_ac$se)
  star  <- netmeta(subset(pw, studlab %in% c(ab$study, ac$study)),
                   common = TRUE, random = FALSE, reference.group = "A")
  expect_same(bu$est, star$TE.common["B", "C"])
  expect_same(bu$se, star$seTE.common["B", "C"])
  expect_same(c(bu$lower, bu$upper),
              c(star$lower.common["B", "C"], star$upper.common["B", "C"]))
  expect_same(bu$pval, star$pval.common["B", "C"])
})

test_that("direct self-help vs no contact in the full network equals hand pooling", {
  # All direct B vs A evidence comes from the three two-arm studies.
  nma <- netmeta(pw, common = TRUE, random = FALSE, reference.group = "A")
  ns  <- netsplit(nma)
  fe  <- fe_hand(hand_ab$yi, hand_ab$vi)
  i   <- which(ns$comparison == "B:A")
  expect_same(ns$direct.common$TE[i], fe$est)
  expect_same(ns$direct.common$seTE[i], fe$se)
})
