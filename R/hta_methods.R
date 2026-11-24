# -----------------------------------------------------------------------------
# Program    : hta_methods.R
# Study      : HTA part (hta/): CDISCPILOT01 and published trial results
# Purpose    : Hand calculations for the HTA chapters: Kaplan-Meier, log-rank,
#              restricted mean survival time, effect measures from a 2x2
#              table, inverse-variance pooling, REML between-trial variance,
#              Hartung-Knapp interval, heterogeneity statistics, prediction
#              interval and the Bucher indirect comparison
# Inputs     : vectors passed by the caller
# Outputs    : functions only. Each returns a named list or a data frame
# Author     : Ignacio Garcia Ribelles
# Created    : 2026-11-24
# Change log : 2026-11-24  IGR  Initial version
# Notes      : Base R only, written from the formulas, so that each result
#              can be checked against an established package
#              (tests/testthat/test-hta-*.R, tolerance 1e-8). The chapters in
#              hta/ source this file and use the same functions.
# -----------------------------------------------------------------------------

# =============================================================================
# 1. Time to event
# =============================================================================

# Kaplan-Meier estimate at each distinct event time.
# S(t) = product over event times t_j <= t of (1 - d_j / n_j).
km_hand <- function(time, event) {
  t_ev <- sort(unique(time[event == 1]))
  n_risk  <- vapply(t_ev, function(t) sum(time >= t), numeric(1))
  n_event <- vapply(t_ev, function(t) sum(time == t & event == 1), numeric(1))
  data.frame(time = t_ev, n.risk = n_risk, n.event = n_event,
             surv = cumprod(1 - n_event / n_risk))
}

# Two-sample log-rank test. At each distinct event time the expected number
# of events in group 1 under H0 is d * n1 / n, with hypergeometric variance.
logrank_hand <- function(time, event, group1) {
  t_ev <- sort(unique(time[event == 1]))
  o_e <- 0
  v   <- 0
  for (t in t_ev) {
    n  <- sum(time >= t)
    n1 <- sum(time >= t & group1)
    d  <- sum(time == t & event == 1)
    d1 <- sum(time == t & event == 1 & group1)
    o_e <- o_e + d1 - d * n1 / n
    if (n > 1) v <- v + d * (n1 / n) * (1 - n1 / n) * (n - d) / (n - 1)
  }
  chisq <- o_e^2 / v
  list(o_minus_e = o_e, var = v, chisq = chisq,
       p = pchisq(chisq, df = 1, lower.tail = FALSE))
}

# Restricted mean survival time up to tau: the area under the Kaplan-Meier
# step function from 0 to tau. Variance: sum over event times t_j <= tau of
# A_j^2 * d_j / (n_j * (n_j - d_j)), where A_j is the area from t_j to tau
# (the delta method applied to Greenwood's formula).
rmst_hand <- function(time, event, tau) {
  km <- km_hand(time, event)
  km <- km[km$time <= tau, ]
  grid  <- c(0, km$time, tau)
  s     <- c(1, km$surv)                  # S(t) on each interval [grid_i, grid_i+1)
  areas <- diff(grid) * s
  rmst  <- sum(areas)
  # Area from each event time to tau.
  a_j   <- rev(cumsum(rev(areas)))[-1]
  g_j   <- ifelse(km$n.risk - km$n.event == 0, 0,
                  km$n.event / (km$n.risk * (km$n.risk - km$n.event)))
  list(rmst = rmst, se = sqrt(sum(a_j^2 * g_j)))
}

# Difference in RMST between two arms (active minus control), Wald interval.
rmst_diff_hand <- function(time, event, active, tau, level = 0.95) {
  r1 <- rmst_hand(time[active], event[active], tau)
  r0 <- rmst_hand(time[!active], event[!active], tau)
  est <- r1$rmst - r0$rmst
  se  <- sqrt(r1$se^2 + r0$se^2)
  z   <- qnorm(1 - (1 - level) / 2)
  list(rmst1 = r1$rmst, rmst0 = r0$rmst, diff = est, se = se,
       lower = est - z * se, upper = est + z * se,
       p = 2 * pnorm(-abs(est / se)))
}

# =============================================================================
# 2. Effect measures from a 2x2 table
# =============================================================================
# a events out of n1 in the active arm, c events out of n0 in the control arm.
# Ratio measures are estimated on the log scale, where the sampling
# distribution is closer to normal, and back-transformed.
effect_2x2_hand <- function(a, n1, c, n0, level = 0.95) {
  b  <- n1 - a
  d  <- n0 - c
  p1 <- a / n1
  p0 <- c / n0
  z  <- qnorm(1 - (1 - level) / 2)

  log_rr <- log(p1 / p0)
  var_log_rr <- 1 / a - 1 / n1 + 1 / c - 1 / n0
  log_or <- log((a * d) / (b * c))
  var_log_or <- 1 / a + 1 / b + 1 / c + 1 / d
  rd     <- p1 - p0
  var_rd <- p1 * (1 - p1) / n1 + p0 * (1 - p0) / n0

  data.frame(
    measure = c("Risk ratio", "Odds ratio", "Risk difference"),
    yi = c(log_rr, log_or, rd),           # analysis scale
    vi = c(var_log_rr, var_log_or, var_rd),
    estimate = c(exp(log_rr), exp(log_or), rd),
    lower = c(exp(log_rr - z * sqrt(var_log_rr)), exp(log_or - z * sqrt(var_log_or)),
              rd - z * sqrt(var_rd)),
    upper = c(exp(log_rr + z * sqrt(var_log_rr)), exp(log_or + z * sqrt(var_log_or)),
              rd + z * sqrt(var_rd))
  )
}

# =============================================================================
# 3. Meta-analysis
# =============================================================================

# Inverse-variance (fixed-effect, also called common-effect) pooling.
# Weight w_i = 1 / v_i. Q is Cochran's heterogeneity statistic.
fe_hand <- function(yi, vi, level = 0.95) {
  w   <- 1 / vi
  est <- sum(w * yi) / sum(w)
  se  <- sqrt(1 / sum(w))
  z   <- qnorm(1 - (1 - level) / 2)
  q   <- sum(w * (yi - est)^2)
  k   <- length(yi)
  list(est = est, se = se, lower = est - z * se, upper = est + z * se,
       zval = est / se, pval = 2 * pnorm(-abs(est / se)),
       Q = q, df = k - 1, Qp = pchisq(q, df = k - 1, lower.tail = FALSE))
}

# REML estimate of the between-trial variance tau^2 by Fisher scoring.
# With W = diag(1 / (v_i + tau^2)) and P = W - W 1 1' W / sum(W), the REML
# score is (y'PPy - tr(P)) / 2 and the expected information is tr(PP) / 2.
# Each step adds score / information. tau^2 is kept at zero or above.
reml_tau2_hand <- function(yi, vi, tol = 1e-12, maxit = 1000) {
  tau2 <- 0
  for (i in seq_len(maxit)) {
    w  <- 1 / (vi + tau2)
    W  <- diag(w)
    P  <- W - (w %o% w) / sum(w)
    Py <- P %*% yi
    step <- (sum(Py^2) - sum(diag(P))) / sum(P * P)
    new  <- max(0, tau2 + step)
    if (abs(new - tau2) < tol) return(new)
    tau2 <- new
  }
  stop("REML did not converge")
}

# Random-effects pooling for a given tau^2. Weights w_i = 1 / (v_i + tau^2).
# Hartung-Knapp: the variance of the pooled estimate is scaled by the
# weighted residual variance sum(w_i (y_i - mu)^2) / (k - 1), and the interval
# uses a t distribution with k - 1 degrees of freedom.
re_hand <- function(yi, vi, tau2, knha = TRUE, level = 0.95) {
  k   <- length(yi)
  w   <- 1 / (vi + tau2)
  est <- sum(w * yi) / sum(w)
  if (knha) {
    s2   <- sum(w * (yi - est)^2) / (k - 1)
    se   <- sqrt(s2 / sum(w))
    crit <- qt(1 - (1 - level) / 2, df = k - 1)
    pval <- 2 * pt(-abs(est / se), df = k - 1)
  } else {
    se   <- sqrt(1 / sum(w))
    crit <- qnorm(1 - (1 - level) / 2)
    pval <- 2 * pnorm(-abs(est / se))
  }
  # Prediction interval: where the true effect of a new, similar trial is
  # expected to lie. It adds tau^2 to the uncertainty of the mean.
  pi_se <- sqrt(tau2 + se^2)
  list(est = est, se = se, lower = est - crit * se, upper = est + crit * se,
       pval = pval, pi_lower = est - crit * pi_se, pi_upper = est + crit * pi_se)
}

# Heterogeneity for a random-effects model with between-trial variance tau2.
# I^2 = tau^2 / (tau^2 + s^2), where s^2 is the "typical" within-trial
# variance (k - 1) * sum(w) / ((sum(w))^2 - sum(w^2)), w = 1 / v_i
# (Higgins and Thompson 2002). H^2 = (tau^2 + s^2) / s^2. With the
# DerSimonian-Laird tau^2 this I^2 equals (Q - df) / Q.
heterogeneity_hand <- function(yi, vi, tau2) {
  w  <- 1 / vi
  k  <- length(yi)
  s2 <- (k - 1) * sum(w) / (sum(w)^2 - sum(w^2))
  fe <- fe_hand(yi, vi)
  list(Q = fe$Q, df = fe$df, Qp = fe$Qp, tau2 = tau2, s2 = s2,
       I2 = 100 * tau2 / (tau2 + s2), H2 = (tau2 + s2) / s2,
       I2_from_Q = max(0, 100 * (fe$Q - fe$df) / fe$Q))
}

# =============================================================================
# 4. Indirect comparison (Bucher et al. 1997)
# =============================================================================
# d_xb: effect of X against the common comparator B, on the log scale.
# The indirect effect of A against C is d_ab - d_cb. The two estimates come
# from different trials, so they are independent and their variances add.
bucher_hand <- function(d_ab, se_ab, d_cb, se_cb, level = 0.95) {
  est <- d_ab - d_cb
  se  <- sqrt(se_ab^2 + se_cb^2)
  z   <- qnorm(1 - (1 - level) / 2)
  list(est = est, se = se, lower = est - z * se, upper = est + z * se,
       pval = 2 * pnorm(-abs(est / se)))
}
