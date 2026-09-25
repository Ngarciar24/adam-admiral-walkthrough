# -----------------------------------------------------------------------------
# Program    : 92_figures.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Figure 1: mean change from baseline in ALT by visit and planned
#              treatment, +/- 1 standard error
#              Figure 2: Kaplan-Meier plot of time to first dermatologic event
# Inputs     : data/adam/adlb.rds; outputs/t2_alt_change_by_visit.csv
#              data/adam/adtte.rds
# Outputs    : outputs/f1_alt_chg_by_visit.png, .csv
#              outputs/f2_ttde_km.png
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-17
# Change log : 2026-09-17  IGR  Initial version
#              2026-09-25  IGR  Standard header
#              2026-09-25  IGR  Figure 2 (Kaplan-Meier) added
# Notes      : Same records as Table 2 (safety population, ALT, scheduled
#              visits). Only visits with at least 10 subjects in every arm are
#              drawn; Table 2 keeps all scheduled visits. The program stops if
#              a plotted mean differs from the Table 2 cell. Colours are
#              Okabe-Ito (colour-blind safe) and each arm has its own shape.
# -----------------------------------------------------------------------------

source("programs/00_setup.R")
library(ggplot2)

adlb <- readRDS(file.path(adam_dir, "adlb.rds"))

out_dir <- "outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

trt_levels  <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
trt_colours <- c(
  "Placebo"              = "#009E73",
  "Xanomeline Low Dose"  = "#0072B2",
  "Xanomeline High Dose" = "#D55E00"
)
trt_shapes <- c("Placebo" = 16, "Xanomeline Low Dose" = 17, "Xanomeline High Dose" = 15)
min_n <- 10

adlb_alt <- adlb %>%
  filter(PARAMCD == "ALT", SAFFL == "Y", AVISIT != "UNSCHEDULED", !is.na(CHG)) %>%
  mutate(TRT01P = factor(TRT01P, levels = trt_levels))

# Visit order follows AVISITN, exactly as in Table 2.
visit_order <- adlb_alt %>% distinct(AVISIT, AVISITN) %>% arrange(AVISITN) %>% pull(AVISIT)

fig_dat <- adlb_alt %>%
  group_by(TRT01P, AVISIT, AVISITN) %>%
  summarise(
    n        = n(),
    mean_chg = mean(CHG),
    se_chg   = sd(CHG) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  group_by(AVISIT) %>%
  filter(all(n >= min_n), n_distinct(TRT01P) == length(trt_levels)) %>%
  ungroup() %>%
  mutate(AVISIT = factor(AVISIT, levels = visit_order))

# Guard: the plotted means must equal the Table 2 "Mean CHG" cells to the
# printed precision. If the two programs ever disagree, stop here.
t2 <- read_csv(file.path(out_dir, "t2_alt_change_by_visit.csv"), show_col_types = FALSE) %>%
  filter(row_name == "Mean CHG")
for (i in seq_len(nrow(fig_dat))) {
  cell <- t2[[as.character(fig_dat$TRT01P[i])]][t2$group1_level == as.character(fig_dat$AVISIT[i])]
  stopifnot(length(cell) == 1, abs(round(fig_dat$mean_chg[i], 1) - as.numeric(cell)) < 1e-9)
}

fig1 <- ggplot(fig_dat, aes(x = AVISIT, y = mean_chg, colour = TRT01P,
                            shape = TRT01P, group = TRT01P)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.4) +
  geom_errorbar(aes(ymin = mean_chg - se_chg, ymax = mean_chg + se_chg),
                width = 0.15, linewidth = 0.5, alpha = 0.7) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = trt_colours, breaks = trt_levels) +
  scale_shape_manual(values = trt_shapes, breaks = trt_levels) +
  labs(
    title    = "Figure 1. ALT (U/L): mean change from baseline by visit",
    subtitle = sprintf("Safety population (SAFFL = 'Y'); scheduled visits with n >= %d per arm; error bars are +/- 1 SE", min_n),
    x        = "Analysis visit (AVISIT)",
    y        = "Mean CHG (U/L)",
    colour   = "Planned treatment (TRT01P)",
    shape    = "Planned treatment (TRT01P)",
    caption  = paste0(
      "Source: ADLB (data/adam/adlb.rds), same records as Table 2. ",
      "Program: programs/92_figures.R | ggplot2 ", packageVersion("ggplot2")
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(angle = 30, hjust = 1),
    plot.title.position = "plot",
    plot.caption       = element_text(hjust = 0, colour = "grey40")
  )

ggsave(file.path(out_dir, "f1_alt_chg_by_visit.png"), fig1,
       width = 9, height = 5, dpi = 150, bg = "white")
write_csv(fig_dat, file.path(out_dir, "f1_alt_chg_by_visit.csv"))

message("Wrote: outputs/f1_alt_chg_by_visit.png, outputs/f1_alt_chg_by_visit.csv")

# =============================================================================
# Figure 2 -- Kaplan-Meier: time to first dermatologic event
# =============================================================================
library(survival)

adtte <- readRDS(file.path(adam_dir, "adtte.rds")) %>%
  filter(PARAMCD == "TTDE", SAFFL == "Y") %>%
  mutate(TRTA = factor(TRTA, levels = trt_levels), EVENT = 1 - CNSR)

km <- survfit(Surv(AVAL, EVENT) ~ TRTA, data = adtte)

# Step curves start at 1 on day 0; censored times are marked.
km_dat <- broom::tidy(km) %>%
  mutate(TRTA = factor(sub("^TRTA=", "", strata), levels = trt_levels)) %>%
  select(TRTA, time, estimate, n.censor)
km_dat <- bind_rows(
  tibble(TRTA = factor(trt_levels, levels = trt_levels), time = 0, estimate = 1, n.censor = 0),
  km_dat
)

# Numbers at risk every 28 days.
risk_times <- seq(0, 196, by = 28)
at_risk <- summary(km, times = risk_times, extend = TRUE)
risk_dat <- tibble(
  TRTA   = factor(sub("^TRTA=", "", at_risk$strata), levels = trt_levels),
  time   = at_risk$time,
  n.risk = at_risk$n.risk
)

fig2 <- ggplot(km_dat, aes(x = time, y = estimate, colour = TRTA)) +
  geom_step(linewidth = 0.8) +
  geom_point(data = filter(km_dat, n.censor > 0), aes(shape = TRTA), size = 1.8) +
  scale_colour_manual(values = trt_colours, breaks = trt_levels) +
  scale_shape_manual(values = c(3, 3, 3), breaks = trt_levels, guide = "none") +
  scale_x_continuous(breaks = risk_times, limits = c(0, max(adtte$AVAL) + 2)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title    = "Figure 2. Time to first treatment-emergent dermatologic adverse event",
    subtitle = "Kaplan-Meier estimate; + = censored at end of study. Safety population, actual treatment",
    x        = "Days since first dose",
    y        = "Event-free",
    colour   = "Actual treatment (TRTA)",
    caption  = paste0(
      "Source: ADTTE (PARAMCD = 'TTDE'). Program: programs/92_figures.R | ggplot2 ",
      packageVersion("ggplot2")
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position     = "top",
    panel.grid.minor    = element_blank(),
    plot.title.position = "plot",
    plot.caption        = element_text(hjust = 0, colour = "grey40")
  )

risk_tbl <- ggplot(risk_dat, aes(x = time, y = TRTA, label = n.risk, colour = TRTA)) +
  geom_text(size = 3.3) +
  scale_colour_manual(values = trt_colours, guide = "none") +
  scale_x_continuous(breaks = risk_times, limits = c(0, max(adtte$AVAL) + 2)) +
  scale_y_discrete(limits = rev(trt_levels)) +
  labs(x = NULL, y = NULL, title = "Number at risk") +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(size = 10))

fig2_out <- cowplot::plot_grid(fig2, risk_tbl, ncol = 1, rel_heights = c(4, 1.2), align = "v",
                               axis = "lr")

ggsave(file.path(out_dir, "f2_ttde_km.png"), fig2_out,
       width = 9, height = 6, dpi = 150, bg = "white")

message("Wrote: outputs/f2_ttde_km.png")
