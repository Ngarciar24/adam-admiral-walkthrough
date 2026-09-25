# -----------------------------------------------------------------------------
# Program    : 91_tables.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Table 1 (demographics, ITT population), Table 2 (ALT observed
#              value and change from baseline by visit, safety population) and
#              a traceability trace of one Table 2 cell back to SDTM LB
# Inputs     : data/adam/adsl.rds, data/adam/adlb.rds
#              SDTM LB (read only to print the source record in the trace)
# Outputs    : outputs/t1_demographics.txt, .csv
#              outputs/t2_alt_change_by_visit.txt, .csv
#              outputs/t2_traceability_chain.txt
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-25  IGR  Standard header; comments condensed
#              2026-09-25  IGR  Table 2 restricted to post-baseline records
# Notes      : No derivation is done here; every cell is a filter and summary of
#              existing ADaM columns. Packages: {rtables} for layout, {tern} for
#              the standard statistics.
# -----------------------------------------------------------------------------

source("programs/00_setup.R")

library(rtables)
library(tern)

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))
adlb <- readRDS(file.path(adam_dir, "adlb.rds"))

out_dir <- "outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# rtables takes rows and columns from factor levels (empty levels still print,
# level order is display order), so display factors are set here, not in ADaM.

# Treatment columns in ascending dose order.
trt_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

# =============================================================================
# TABLE 1 -- Demographics and baseline characteristics (ITT population)
# =============================================================================

# ITT population; ADSL is one row per subject, so rows = subjects.
# RACE levels are taken from all of ADSL (no RACE codelist in metadata/), so
# ASIAN prints as 0: its only 2 subjects are screen failures.
adsl_itt <- adsl %>%
  filter(ITTFL == "Y") %>%
  mutate(
    TRT01P = factor(TRT01P, levels = trt_levels),
    # Order from AGEGR1N, labels from the metadata.
    AGEGR1 = factor(AGEGR1, levels = adsl_agegr1$AGEGR1[order(adsl_agegr1$AGEGR1N)]),
    SEX    = factor(SEX,  levels = c("F", "M")),
    RACE   = factor(RACE, levels = sort(unique(adsl$RACE)))
  )

stopifnot(
  nrow(adsl_itt) == nrow(distinct(adsl_itt, USUBJID)), # ADSL is one row per subject
  !any(is.na(adsl_itt$TRT01P))                         # every ITT subject has a planned arm
)

# analyze_vars() picks statistics by variable class: n, mean (SD), median,
# range for AGE; n and count (%) for the factors. Percentages use the
# non-missing count in the arm ("n" row) as denominator, not the column N;
# the two are equal here because no demographic value is missing.
lyt_t1 <- basic_table(
  title = "Table 1. Demographics and Baseline Characteristics",
  subtitles = "Intent-to-Treat Population (ITTFL = 'Y')",
  main_footer = c(
    "Percentages use the number of subjects with a non-missing value in the arm (the 'n' row)",
    "as denominator. No demographic value is missing here, so n equals the column N.",
    "Source: ADSL. Planned treatment (TRT01P) -- ITT is analysed as randomised."
  ),
  prov_footer = paste0(
    "Program: programs/91_tables.R | admiral ", packageVersion("admiral"),
    " | rtables ", packageVersion("rtables"), " | tern ", packageVersion("tern")
  )
) %>%
  split_cols_by("TRT01P", show_colcounts = TRUE) %>%
  analyze_vars(
    vars = c("AGE", "AGEGR1", "SEX", "RACE"),
    var_labels = c(
      "Age (years)",
      "Age group",
      "Sex",
      "Race"
    ),
    show_labels = "visible",
    .stats = c("n", "mean_sd", "median", "range", "count_fraction")
  )

tbl_t1 <- build_table(lyt_t1, adsl_itt)

# =============================================================================
# TABLE 2 -- ALT: observed value and change from baseline by visit
#            (safety population)
# =============================================================================
# AVAL, BASE and CHG are read from the same ADLB row, so no join to the
# baseline record is needed. CHG exists only after first dose, so the table
# shows post-baseline visits; the baseline mean appears as Mean BASE.

# Visit order from AVISITN.
visit_order <- adlb %>%
  filter(PARAMCD == "ALT", AVISIT != "UNSCHEDULED", ADT > TRTSDT) %>%
  distinct(AVISIT, AVISITN) %>%
  arrange(AVISITN) %>%
  pull(AVISIT)

# Analysis subset: ALT, safety population, scheduled post-baseline visits.
# (SAFFL removes no rows in this study: every subject with labs was dosed.)
adlb_alt <- adlb %>%
  filter(
    PARAMCD == "ALT",
    SAFFL   == "Y",
    AVISIT  != "UNSCHEDULED",
    ADT     >  TRTSDT
  ) %>%
  mutate(
    TRT01P = factor(TRT01P, levels = trt_levels),
    AVISIT = factor(AVISIT, levels = visit_order)
  )

# Confirm the table only needs existing columns.
stopifnot(
  # CHG equals AVAL - BASE wherever both exist.
  with(
    filter(adlb_alt, !is.na(AVAL), !is.na(BASE)),
    all(abs(CHG - (AVAL - BASE)) < 1e-9)
  ),
  # BASE is constant within subject for this parameter.
  adlb_alt %>%
    group_by(USUBJID) %>%
    summarise(k = n_distinct(BASE), .groups = "drop") %>%
    pull(k) %>%
    max() == 1
)

# Custom analysis function: one n and three means per visit and arm.
# tern::summarize_change() is not used because it recomputes the change from
# a baseline flag and expects baseline to be its own visit. The first
# argument is named `df` so rtables passes the whole cell data frame.
mean_or_na <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) NA_real_ else mean(v)
}

afun_chg <- function(df, ...) {
  in_rows(
    # n = records with a non-missing change.
    "n"         = rcell(sum(!is.na(df$CHG)),   format = "xx"),
    "Mean BASE" = rcell(mean_or_na(df$BASE),   format = "xx.x"),
    "Mean AVAL" = rcell(mean_or_na(df$AVAL),   format = "xx.x"),
    "Mean CHG"  = rcell(mean_or_na(df$CHG),    format = "xx.x")
  )
}

lyt_t2 <- basic_table(
  title = "Table 2. Alanine Aminotransferase (U/L): Observed Value and Change from Baseline by Visit",
  subtitles = c(
    "Safety Population (SAFFL = 'Y'); PARAMCD = 'ALT'",
    "Post-baseline records (ADT after first dose); unscheduled visits excluded"
  ),
  main_footer = c(
    "BASE = value at the ABLFL='Y' record; CHG = AVAL - BASE, both taken directly from ADLB.",
    "n = records with a non-missing CHG. Mean AVAL may be based on more records than Mean CHG",
    "when a subject has a post-baseline value but no baseline.",
    "Column N = subjects contributing at least one ALT record, NOT the number of records.",
    "Visit order follows AVISITN. Visits with a single record are retained as collected."
  ),
  prov_footer = paste0(
    "Program: programs/91_tables.R | Source: data/adam/adlb.rds (no derivation performed in this program)"
  )
) %>%
  split_cols_by("TRT01P", show_colcounts = TRUE) %>%
  # One row block per visit; visits without records are dropped.
  split_rows_by("AVISIT", label_pos = "visible", split_fun = drop_split_levels) %>%
  analyze("AVAL", afun = afun_chg, show_labels = "hidden")

tbl_t2 <- build_table(lyt_t2, adlb_alt)

# Column N: rtables counts records; replace with the number of subjects.
# .drop = FALSE keeps the vector aligned with the columns.
col_counts(tbl_t2) <- adlb_alt %>%
  distinct(TRT01P, USUBJID) %>%
  count(TRT01P, .drop = FALSE) %>%
  arrange(TRT01P) %>%
  pull(n)

# =============================================================================
# Write the outputs
# =============================================================================
# Text tables unpaginated (short tables, easier to diff); CSV with the
# formatted strings as printed.
export_as_txt(tbl_t1, file = file.path(out_dir, "t1_demographics.txt"), paginate = FALSE)
export_as_txt(tbl_t2, file = file.path(out_dir, "t2_alt_change_by_visit.txt"), paginate = FALSE)

write_csv(
  as_result_df(tbl_t1, data_format = "strings"),
  file.path(out_dir, "t1_demographics.csv")
)
write_csv(
  as_result_df(tbl_t2, data_format = "strings"),
  file.path(out_dir, "t2_alt_change_by_visit.csv")
)

print(tbl_t1)
cat("\n\n")
print(tbl_t2)

# =============================================================================
# Traceability: one Table 2 cell back to its SDTM record
# =============================================================================
# Table cell -> ADLB row (USUBJID + ASEQ) -> SDTM LB row (USUBJID + LBSEQ).
# Origins printed match data/adam/define.xml.
trace_cell_visit <- "WEEK 8"
trace_cell_arm   <- "Xanomeline High Dose"

trace_rows <- adlb_alt %>%
  filter(AVISIT == trace_cell_visit, TRT01P == trace_cell_arm, !is.na(CHG)) %>%
  arrange(USUBJID, ASEQ)

trace <- trace_rows %>% slice(1)

# Source LB record (display only).
lb_src <- lb %>%
  filter(USUBJID == trace$USUBJID, LBSEQ == trace$LBSEQ) %>%
  select(STUDYID, USUBJID, LBSEQ, LBTESTCD, LBTEST, VISIT, LBDTC, LBORRES,
         LBORRESU, LBSTRESN, LBSTRESU, LBBLFL)

# Baseline record that supplied BASE.
base_row <- adlb %>%
  filter(USUBJID == trace$USUBJID, PARAMCD == "ALT", ABLFL == "Y")

trace_txt <- c(
  "TRACEABILITY CHAIN -- one cell of Table 2, followed back to SDTM",
  "================================================================",
  "",
  sprintf("CELL      Table 2, row block '%s', row 'Mean CHG', column '%s'",
          trace_cell_visit, trace_cell_arm),
  sprintf("          printed value = %.1f, computed from n = %d non-missing CHG values",
          mean(trace_rows$CHG), nrow(trace_rows)),
  "",
  "ONE CONTRIBUTING RECORD",
  sprintf("  ADaM  ADLB  USUBJID = %s   ASEQ = %d        (USUBJID + ASEQ is the unique key)",
          trace$USUBJID, trace$ASEQ),
  sprintf("              PARAMCD = %s   PARAM = %s", trace$PARAMCD, trace$PARAM),
  sprintf("              AVISIT  = %s   AVISITN = %s   ADT = %s",
          as.character(trace$AVISIT), trace$AVISITN, format(trace$ADT)),
  sprintf("              TRT01P  = %s   SAFFL = %s",
          as.character(trace$TRT01P), trace$SAFFL),
  sprintf("              AVAL    = %s   BASE = %s   CHG = %s   (CHG = AVAL - BASE = %s - %s)",
          trace$AVAL, trace$BASE, trace$CHG, trace$AVAL, trace$BASE),
  "",
  "  BASE came from this subject's baseline record in the SAME dataset:",
  sprintf("        ADLB  USUBJID = %s   ASEQ = %d   ABLFL = Y   AVISIT = %s   AVAL = %s",
          base_row$USUBJID, base_row$ASEQ, base_row$AVISIT, base_row$AVAL),
  sprintf("              that AVAL (%s) is the BASE carried onto every ALT row of this subject.",
          base_row$AVAL),
  "",
  sprintf("  SDTM  LB    USUBJID = %s   LBSEQ = %d       (LBSEQ kept in ADLB for this hop)",
          lb_src$USUBJID, lb_src$LBSEQ),
  sprintf("              LBTESTCD = %s   LBTEST = %s", lb_src$LBTESTCD, lb_src$LBTEST),
  sprintf("              VISIT    = %s   LBDTC = %s", lb_src$VISIT, lb_src$LBDTC),
  sprintf("              LBORRES  = %s %s    (as reported by the local lab)",
          lb_src$LBORRES, lb_src$LBORRESU),
  sprintf("              LBSTRESN = %s %s    (standardised -- this is what AVAL copies)",
          lb_src$LBSTRESN, lb_src$LBSTRESU),
  sprintf("              LBBLFL   = %s", ifelse(is.na(lb_src$LBBLFL), "<NA>", lb_src$LBBLFL)),
  "",
  "SO:",
  sprintf("  LB.LBSTRESN = %s  ->  ADLB.AVAL = %s  (origin: Predecessor, LB.LBSTRESN)",
          lb_src$LBSTRESN, trace$AVAL),
  sprintf("  ADLB.BASE   = %s     (origin: Derived, AVAL where ABLFL='Y')", trace$BASE),
  sprintf("  ADLB.CHG    = %s     (origin: Derived, AVAL - BASE)", trace$CHG),
  sprintf("  -> one of the %d values averaged into Table 2 / %s / Mean CHG / %s",
          nrow(trace_rows), trace_cell_visit, trace_cell_arm),
  "",
  "No step in 91_tables.R altered any of these values; the program filtered and",
  "averaged existing columns. That is the ADaM 'analysis-ready' contract."
)

# Check the arithmetic stated in the trace. as.numeric() drops the SDTM label
# attribute so values, not attributes, are compared.
stopifnot(
  isTRUE(all.equal(as.numeric(trace$AVAL), as.numeric(lb_src$LBSTRESN))),
  abs(trace$CHG - (trace$AVAL - trace$BASE)) < 1e-9,
  isTRUE(all.equal(as.numeric(trace$BASE), as.numeric(base_row$AVAL)))
)

writeLines(trace_txt, file.path(out_dir, "t2_traceability_chain.txt"))
cat("\n\n"); writeLines(trace_txt)

message(
  "Wrote: ",
  paste(file.path(out_dir, c(
    "t1_demographics.txt", "t1_demographics.csv",
    "t2_alt_change_by_visit.txt", "t2_alt_change_by_visit.csv",
    "t2_traceability_chain.txt"
  )), collapse = ", ")
)
