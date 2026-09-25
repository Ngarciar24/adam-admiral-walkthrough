# -----------------------------------------------------------------------------
# Program    : 93_tables_tte_eff.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Table 3: time to first dermatologic event (safety population,
#              actual treatment). Table 4: ADAS-Cog(11) change from baseline to
#              Week 24, LOCF, ANCOVA (efficacy population, planned treatment)
# Inputs     : data/adam/adtte.rds, data/adam/adqsadas.rds
# Outputs    : outputs/t3_ttde.txt, .csv
#              outputs/t4_adascog_wk24_ancova.txt, .csv
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-25
# Change log : 2026-09-25  IGR  Initial version
# Notes      : Table 3: Kaplan-Meier medians (log-log 95% CI), log-rank test
#              and Cox hazard ratio for each active arm vs placebo.
#              Table 4: ANCOVA of CHG with treatment, pooled site (SITEGR1) and
#              baseline score; LS means from {emmeans}; dose-response test with
#              the dose (TRTPN, mg) as a continuous term. See docs/sap.md.
# -----------------------------------------------------------------------------

source("programs/00_setup.R")

library(rtables)
library(survival)
library(emmeans)

adtte    <- readRDS(file.path(adam_dir, "adtte.rds"))
adqsadas <- readRDS(file.path(adam_dir, "adqsadas.rds"))

out_dir <- "outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

trt_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

fmt_p <- function(p) if (is.na(p)) "NA" else if (p < 0.001) "<0.001" else sprintf("%.3f", p)
fmt_ci <- function(est, lo, hi, d = 1) {
  f <- function(x) if (is.na(x)) "NE" else formatC(x, format = "f", digits = d)
  paste0(f(est), " (", f(lo), ", ", f(hi), ")")
}

# Write a table as text and CSV (the CSV is the tidy result data).
write_table <- function(tbl, stem) {
  export_as_txt(tbl, file = file.path(out_dir, paste0(stem, ".txt")), paginate = FALSE)
  write_csv(as_result_df(tbl, data_format = "strings"), file.path(out_dir, paste0(stem, ".csv")))
  print(tbl)
  cat("\n\n")
}

# =============================================================================
# TABLE 3 -- Time to first dermatologic event
# =============================================================================
tte <- adtte %>%
  filter(PARAMCD == "TTDE", SAFFL == "Y") %>%
  mutate(TRTA = factor(TRTA, levels = trt_levels), EVENT = 1 - CNSR)

km  <- survfit(Surv(AVAL, EVENT) ~ TRTA, data = tte, conf.type = "log-log")
kmt <- summary(km)$table

# Kaplan-Meier event-free probability at a day, as a percentage.
km_at <- function(day) {
  s <- summary(km, times = day, extend = TRUE)
  sprintf("%.1f", 100 * s$surv)
}

# Log-rank p-value and Cox hazard ratio for one active arm vs placebo.
vs_placebo <- function(arm) {
  d  <- tte %>% filter(TRTA %in% c("Placebo", arm)) %>% droplevels()
  lr <- survdiff(Surv(AVAL, EVENT) ~ TRTA, data = d)
  cx <- summary(coxph(Surv(AVAL, EVENT) ~ TRTA, data = d))
  list(
    p  = pchisq(lr$chisq, df = 1, lower.tail = FALSE),
    hr = fmt_ci(cx$conf.int[1, 1], cx$conf.int[1, 3], cx$conf.int[1, 4], d = 2)
  )
}
cmp <- lapply(trt_levels[-1], vs_placebo)

n_arm   <- as.integer(kmt[, "records"])
n_event <- as.integer(kmt[, "events"])
pct     <- function(x) sprintf("%d (%.1f%%)", x, 100 * x / n_arm)

t3 <- rtable(
  header = rheader(rrow("", trt_levels[1], trt_levels[2], trt_levels[3]),
                   rrow("", paste0("(N=", n_arm, ")")[1], paste0("(N=", n_arm, ")")[2],
                        paste0("(N=", n_arm, ")")[3])),
  rrow("Subjects with event, n (%)", pct(n_event)[1], pct(n_event)[2], pct(n_event)[3]),
  rrow("Subjects censored, n (%)", pct(n_arm - n_event)[1], pct(n_arm - n_event)[2],
       pct(n_arm - n_event)[3]),
  rrow("Median time to event, days (95% CI)",
       fmt_ci(kmt[1, "median"], kmt[1, "0.95LCL"], kmt[1, "0.95UCL"], d = 0),
       fmt_ci(kmt[2, "median"], kmt[2, "0.95LCL"], kmt[2, "0.95UCL"], d = 0),
       fmt_ci(kmt[3, "median"], kmt[3, "0.95LCL"], kmt[3, "0.95UCL"], d = 0)),
  rrow("Event-free at day 28, %", km_at(28)[1], km_at(28)[2], km_at(28)[3]),
  rrow("Event-free at day 84, %", km_at(84)[1], km_at(84)[2], km_at(84)[3]),
  rrow("Hazard ratio vs placebo (95% CI)", "", cmp[[1]]$hr, cmp[[2]]$hr),
  rrow("Log-rank p-value vs placebo", "", fmt_p(cmp[[1]]$p), fmt_p(cmp[[2]]$p))
)
main_title(t3) <- "Table 3. Time to First Treatment-Emergent Dermatologic Adverse Event"
subtitles(t3)  <- "Safety Population (SAFFL = 'Y'); actual treatment (TRTA)"
main_footer(t3) <- c(
  "Event: first TEAE with CQ01NAM = 'DERMATOLOGIC EVENTS'. Censored at end of study (RFENDT).",
  "Time = ADT - TRTSDT + 1 days. Kaplan-Meier estimates; 95% CI by log-log transformation.",
  "NE = not estimable. Hazard ratio from a Cox model with treatment only."
)
prov_footer(t3) <- "Program: programs/93_tables_tte_eff.R | Source: ADTTE (PARAMCD = 'TTDE')"

write_table(t3, "t3_ttde")

# =============================================================================
# TABLE 4 -- ADAS-Cog(11): change from baseline to Week 24 (LOCF), ANCOVA
# =============================================================================
wk24 <- adqsadas %>%
  filter(PARAMCD == "ACTOT", EFFFL == "Y", AVISITN == 24, ANL01FL == "Y") %>%
  mutate(TRTP = factor(TRTP, levels = trt_levels))

stopifnot(
  nrow(wk24) == n_distinct(wk24$USUBJID),  # one analysed Week 24 record per subject
  !anyNA(wk24$CHG)
)

by_arm <- wk24 %>%
  group_by(TRTP, .drop = FALSE) %>%
  summarise(
    n      = n(),
    base   = sprintf("%.1f (%.2f)", mean(BASE), sd(BASE)),
    wk     = sprintf("%.1f (%.2f)", mean(AVAL), sd(AVAL)),
    chg    = sprintf("%.1f (%.2f)", mean(CHG), sd(CHG)),
    n_locf = sum(DTYPE %in% "LOCF"),
    .groups = "drop"
  )

fit  <- lm(CHG ~ TRTP + SITEGR1 + BASE, data = wk24)
emm  <- emmeans(fit, ~ TRTP)
lsm  <- summary(emm)
diffs <- summary(contrast(emm, method = "trt.vs.ctrl", ref = 1, adjust = "none"),
                 infer = c(TRUE, TRUE))

dose_fit <- lm(CHG ~ TRTPN + SITEGR1 + BASE, data = wk24)
p_dose   <- summary(dose_fit)$coefficients["TRTPN", "Pr(>|t|)"]

t4 <- rtable(
  header = rheader(rrow("", trt_levels[1], trt_levels[2], trt_levels[3]),
                   rrow("", paste0("(N=", by_arm$n[1], ")"), paste0("(N=", by_arm$n[2], ")"),
                        paste0("(N=", by_arm$n[3], ")"))),
  rrow("Baseline, mean (SD)", by_arm$base[1], by_arm$base[2], by_arm$base[3]),
  rrow("Week 24, mean (SD)", by_arm$wk[1], by_arm$wk[2], by_arm$wk[3]),
  rrow("Change from baseline, mean (SD)", by_arm$chg[1], by_arm$chg[2], by_arm$chg[3]),
  rrow("  of which Week 24 value by LOCF, n", by_arm$n_locf[1], by_arm$n_locf[2],
       by_arm$n_locf[3]),
  rrow("LS mean change (SE)",
       sprintf("%.1f (%.2f)", lsm$emmean[1], lsm$SE[1]),
       sprintf("%.1f (%.2f)", lsm$emmean[2], lsm$SE[2]),
       sprintf("%.1f (%.2f)", lsm$emmean[3], lsm$SE[3])),
  rrow("LS mean difference vs placebo (95% CI)", "",
       fmt_ci(diffs$estimate[1], diffs$lower.CL[1], diffs$upper.CL[1]),
       fmt_ci(diffs$estimate[2], diffs$lower.CL[2], diffs$upper.CL[2])),
  rrow("p-value vs placebo", "", fmt_p(diffs$p.value[1]), fmt_p(diffs$p.value[2])),
  rrow("Dose-response p-value", fmt_p(p_dose), "", "")
)
main_title(t4) <- "Table 4. ADAS-Cog(11) Total Score: Change from Baseline to Week 24 (LOCF)"
subtitles(t4)  <- "Efficacy Population (EFFFL = 'Y'); planned treatment (TRTP)"
main_footer(t4) <- c(
  "ANCOVA: CHG = treatment + pooled site (SITEGR1) + baseline score. LS means from emmeans;",
  "differences and p-values unadjusted for multiplicity. Dose-response: same model with dose",
  "(0, 54, 81 mg) as a continuous term. Week 24 window: study day > 140; LOCF from the last",
  "analysed value (ANL01FL = 'Y'). Higher ADAS-Cog scores indicate worse cognition."
)
prov_footer(t4) <- "Program: programs/93_tables_tte_eff.R | Source: ADQSADAS (PARAMCD = 'ACTOT')"

write_table(t4, "t4_adascog_wk24_ancova")
