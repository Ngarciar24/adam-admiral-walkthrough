# ---------------------------------------------------------------------------
# 92_figures.R -- Figure 1: mean change from baseline in ALT by visit and arm
#
# Same slice of ADLB as Table 2 (programs/91_tables.R): safety population,
# PARAMCD == "ALT", scheduled visits only. The figure and the table are two
# views of the same rows, so every mean plotted here can be read off the table
# and the table's n is the figure's n. Nothing is derived in this program; it
# only summarises columns that already exist in ADLB.
#
# Error bars are +/- 1 standard error of the mean CHG within visit x arm. They
# describe the precision of each plotted mean; they are not a hypothesis test.
#
# Only visits with at least 10 subjects in every arm are drawn. Table 2 keeps
# every scheduled visit, including two ("AMBUL ECG REMOVAL", "RETRIEVAL") that
# hold a single Low Dose record; a one-subject "mean" plotted as a point with
# no error bar reads as a trend, so the figure applies a minimum n and says so.
# The threshold is a presentation choice, not an analysis rule.
#
# Colours are three hues from the Okabe-Ito colour-blind-safe palette in a
# fixed order per arm, and each arm also has its own point shape, so arm
# identity never depends on colour alone.
# ---------------------------------------------------------------------------

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
