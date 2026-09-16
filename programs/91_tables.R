# ---------------------------------------------------------------------------
# 91_tables.R -- the last link in the chain: ADaM -> A RESULT
#
# 01_adsl.R and 02_adlb.R built analysis datasets. Nothing in this program
# derives anything. That is the point. A table program that has to derive is a
# table program that can disagree with the dataset a reviewer was given, and
# then nobody can tell which number is right.
#
# Three outputs:
#   Table 1 -- demographics from ADSL, ITT population, columns = planned treatment
#   Table 2 -- ALT change from baseline by visit, safety population
#   plus a written traceability chain for one cell of Table 2 (end of file)
#
# Source ADaM: ADSL, ADLB. Source SDTM: none (LB is read at the very end ONLY to
# print the traceability chain, not to compute any cell).
#
# Packages: {rtables} 0.6.16 builds the table object; {tern} 0.9.11 supplies the
# standard clinical statistic functions that plug into it.
# ---------------------------------------------------------------------------

source("programs/00_setup.R")

library(rtables)
library(tern)

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))
adlb <- readRDS(file.path(adam_dir, "adlb.rds"))

out_dir <- "outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ===========================================================================
# 0. Why anything becomes a factor here
# ===========================================================================
# ADSL and ADLB store TRT01P, SEX, RACE, AGEGR1 and AVISIT as CHARACTER. That is
# correct for the datasets -- ADaM does not require factors, and a factor level
# set is a display decision, not a data decision.
#
# rtables is the opposite: it takes the ROW AND COLUMN STRUCTURE of the table
# from the FACTOR LEVELS, not from the values present in the data. Two
# consequences that a dplyr user will not expect:
#
#   1. A level with ZERO observations still gets a row/column, showing 0.
#      dplyr::count() silently drops it. Table 1 below has a live example: the
#      RACE row "ASIAN" prints 0 in all three arms, because the only 2 Asian
#      subjects in the study are screen failures (ITTFL == "N") and so are not in
#      the ITT population. That row MUST appear: an absent row is
#      indistinguishable from a programming error.
#      BE HONEST ABOUT WHERE THAT LEVEL COMES FROM. The code below takes the
#      RACE levels from sort(unique(adsl$RACE)) -- i.e. from the WHOLE of ADSL,
#      screen failures included -- so "ASIAN" only survives into the table
#      because 2 non-ITT subjects happen to be Asian. That is a stand-in, not
#      the real thing. In a real study the level set comes from the protocol /
#      controlled terminology via the spec, so a category with nobody in it
#      anywhere still prints its 0 row. Doing it from metadata (as AGEGR1 is
#      done, below) would be the correct pattern; it is not done here because
#      metadata/ carries no RACE codelist.
#   2. Level order IS display order. Character values sort alphabetically, which
#      would print AGEGR1 as "<65", ">80", "65-80". AGEGR1N exists in ADSL for
#      exactly this reason, and is used here to set the level order.
#
# So the factor conversion is a PRESENTATION step, done in the table program,
# deliberately not baked into the analysis dataset.
# ---------------------------------------------------------------------------

# Dose-ascending column order. This is a convention, not a rule: the SAP fixes
# it, and the reason it matters is that a reader scanning left to right should
# see a dose-response if there is one.
trt_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

# ===========================================================================
# TABLE 1 -- Demographics and baseline characteristics (ITT population)
# ===========================================================================

# -- Analysis population ----------------------------------------------------
# ITTFL == "Y" is applied HERE, in the table program, and it is a simple filter
# on a flag that already exists. This is the payoff for having derived ITTFL
# once in ADSL: every table that claims to be "ITT" is filtering on the same
# column, so two tables cannot silently use two different definitions of ITT.
#
# ADSL is one row per subject, so nrow() after the filter IS the subject count.
# No distinct(), no group_by. That invariant is what ADSL is for.
adsl_itt <- adsl %>%
  filter(ITTFL == "Y") %>%
  mutate(
    TRT01P = factor(TRT01P, levels = trt_levels),
    # AGEGR1N drives the order; the labels come from the metadata-driven AGEGR1.
    AGEGR1 = factor(AGEGR1, levels = adsl_agegr1$AGEGR1[order(adsl_agegr1$AGEGR1N)]),
    SEX    = factor(SEX,  levels = c("F", "M")),
    RACE   = factor(RACE, levels = sort(unique(adsl$RACE)))
  )

stopifnot(
  nrow(adsl_itt) == nrow(distinct(adsl_itt, USUBJID)), # ADSL is one row per subject
  !any(is.na(adsl_itt$TRT01P))                         # every ITT subject has a planned arm
)

# -- The layout -------------------------------------------------------------
# rtables separates the LAYOUT (a recipe: which splits, which statistics) from
# the TABLE (the recipe applied to data). The layout is a plain object that can
# be printed, stored, reviewed and reused against a different dataset. This is
# the structural difference from every "make a summary table" idiom in dplyr,
# where the shape of the output only exists once the data has been summarised.
#
# analyze_vars() is tern's, not rtables'. It reads the CLASS of each variable
# and picks the matching statistics from .stats:
#   AGE is numeric  -> n, mean (SD), median, min - max
#   the factors     -> n, then count (%) per level
# One call therefore produces both halves of a standard Table 1. Passing a
# statistic that does not apply to a variable's type is not an error; it is
# dropped for that variable. Convenient, but worth knowing: a typo in .stats is
# also silently dropped, so check the printed output rather than trusting it.
#
# NOTE on the percentage denominator, and check this rather than assuming it:
# tern's count_fraction divides by "n", the number of NON-MISSING values in that
# cell (tern:::s_summary.factor has denom = c("n", "N_col", "N_row") and takes
# the FIRST as its default). It does NOT divide by the column N. The two
# coincide in this table only because SEX, RACE and AGEGR1 have no missing
# values in the ITT population, so n == N == the arm size in every column --
# which is exactly why the printed "n" row is worth keeping: it IS the
# denominator. Verified, not assumed: blanking SEX on 6 placebo subjects leaves
# the header at (N=86) but drops the n row to 80, and tern then prints F as
# 50 (62.5%) -- 50/80. Had the denominator been the column N it would have
# printed 50 (58.1%) -- 50/86.
# Many SAPs specify the column N instead (so that "% of subjects randomised" is
# stable across variables). Getting that would mean passing denom = "N_col"
# through, not relying on the default. Which denominator the SAP asks for is a
# standard interview question.
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
  # show_colcounts = TRUE prints the "(N=xx)" header line. In a clinical table
  # the column N is not decoration: every percentage below it is relative to it.
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

# ===========================================================================
# TABLE 2 -- ALT: change from baseline by visit (safety population)
# ===========================================================================
#
# ***** THE CENTRAL TEACHING POINT OF THIS FILE *****
#
# Every cell of Table 2 is computed from columns that ALREADY EXIST on a single
# row of ADLB. Named exactly:
#
#     AVAL    the analysis value at this visit      -> "Mean AVAL"
#     BASE    that subject/parameter's baseline     -> "Mean BASE"
#     CHG     AVAL - BASE, precomputed in 02_adlb.R -> "Mean CHG"
#     AVISIT  the analysis visit                    -> the ROW split
#     TRT01P  planned treatment                     -> the COLUMN split
#     SAFFL   safety population flag                -> the row FILTER
#     PARAMCD selects the parameter (ALT)           -> the row FILTER
#
# Because BDS carries BASE and CHG on EVERY row -- including post-baseline rows
# -- this program needs NO JOIN back to the baseline record and NO WINDOW
# FUNCTION over the subject. The dplyr instinct for a change-from-baseline table
# is some version of
#
#     lb %>% group_by(USUBJID, PARAMCD) %>%
#            mutate(BASE = AVAL[ABLFL == "Y"][1], CHG = AVAL - BASE) %>%
#            group_by(TRT01P, AVISIT) %>% summarise(mean(CHG))
#
# and that grouped mutate is precisely the thing ADaM is designed to remove from
# the table program. THAT IS WHAT "ANALYSIS-READY" MEANS IN THE ADaM IG: the
# dataset is structured so the analysis can be produced by subsetting and
# summarising alone, with no further restructuring or derivation. If the table
# program had to recompute BASE, the table and the dataset could disagree, and
# a reviewer replicating the table from ADLB would get a different number.
#
# The corollary is where the risk moved to: BASE and CHG are now the
# RESPONSIBILITY OF 02_adlb.R and are tested there (tests/testthat/test-adlb.R
# asserts CHG == AVAL - BASE and exactly one ABLFL == "Y" per subject-parameter).
# The table program cannot catch a wrong baseline; it can only faithfully print
# it.
# ---------------------------------------------------------------------------

# -- Visit order ------------------------------------------------------------
# AVISITN is the numeric companion to AVISIT, derived in 02_adlb.R. Sorting the
# levels by AVISITN is what makes "WEEK 2" precede "WEEK 12" -- alphabetically
# it does not. Same paired --N variable trick as AGEGR1/AGEGR1N.
#
# READ THE FIRST ROW BLOCK CAREFULLY. It is "SCREENING 1", not "BASELINE", and
# its Mean CHG is 0.1 rather than 0.0. That is not a bug, and the real reason is
# worth stating precisely because an interviewer will ask.
#
# This study has no dedicated baseline analysis visit. 254 subjects have an ALT
# baseline record; 230 of those ABLFL == "Y" records sit at SCREENING 1 and the
# other 24 sit at an UNSCHEDULED visit. Correspondingly, of the 252 ALT records
# at SCREENING 1, 230 carry ABLFL == "Y" and 22 do not -- and each of those 22
# subjects has exactly ONE screening draw, whose baseline was taken later, at an
# unscheduled pre-dose visit. So those 22 screening values are compared against
# a LATER baseline and carry a real, non-zero CHG (20 of the 22 are non-zero,
# spanning -17 to +15). Averaged in with 230 exact zeros, the block mean lands
# near zero but not on it.
#
# TWO CONSEQUENCES OF THE UNSCHEDULED FILTER BELOW, both of which this table
# does not hide:
#   * for those 24 subjects the baseline RECORD itself is excluded from the
#     table, even though its value still reaches every row of theirs via BASE;
#   * "Mean BASE" in the SCREENING 1 block is therefore not identical to
#     "Mean AVAL", which is what makes Mean CHG non-zero.
#
# A real study would define an analysis visit "BASELINE" in the SAP with a
# window that captures the pre-dose draw wherever it was collected, so that row
# would be exactly 0.0 by construction and a non-zero value there would be a
# finding. Do not present this table as if SCREENING 1 were the baseline row.
visit_order <- adlb %>%
  filter(PARAMCD == "ALT", AVISIT != "UNSCHEDULED") %>%
  distinct(AVISIT, AVISITN) %>%
  arrange(AVISITN) %>%
  pull(AVISIT)

# -- The analysis subset ----------------------------------------------------
# Three filters, all on existing flags/keys, all of them one-liners.
#
# HONEST NOTE ON THE TOY DATA: in THIS study the SAFFL == "Y" filter removes
# ZERO rows, because every subject with a lab record in the pilot LB domain was
# also dosed (all 9,079 ADLB rows have SAFFL == "Y"). The filter is written
# anyway because it is the definition of the population the table claims to
# describe, and because in a real study it removes screen failures and
# randomised-but-never-dosed subjects. Do not claim it did work here; it did
# not. It is a stated population, verified to be a no-op.
#
# AVISIT != "UNSCHEDULED" drops the 44 ALT records that 02_adlb.R collapsed into
# a single unscheduled category. Unscheduled visits are excluded from a by-visit
# summary because they are not a timepoint -- they happen when something goes
# wrong, so averaging them would mix a clinical trigger into a scheduled trend.
adlb_alt <- adlb %>%
  filter(
    PARAMCD == "ALT",
    SAFFL   == "Y",
    AVISIT  != "UNSCHEDULED"
  ) %>%
  mutate(
    TRT01P = factor(TRT01P, levels = trt_levels),
    AVISIT = factor(AVISIT, levels = visit_order)
  )

# Prove the "no derivation happened here" claim rather than asserting it.
stopifnot(
  # CHG really is AVAL - BASE on every row that has both -- i.e. nothing below
  # needs to recompute it.
  with(
    filter(adlb_alt, !is.na(AVAL), !is.na(BASE)),
    all(abs(CHG - (AVAL - BASE)) < 1e-9)
  ),
  # BASE is constant within subject for this parameter (the subset is ALT only,
  # so grouping by USUBJID alone is the subject-parameter group). That constancy
  # is the denormalisation that removes the join.
  adlb_alt %>%
    group_by(USUBJID) %>%
    summarise(k = n_distinct(BASE), .groups = "drop") %>%
    pull(k) %>%
    max() == 1
)

# -- The analysis function --------------------------------------------------
# tern ships summarize_change(), which is the obvious-looking choice and is NOT
# used here. Two reasons, both worth being able to state:
#   (a) it requires `variables = list(value = ..., baseline_flag = ...)` where
#       baseline_flag must be LOGICAL, and it asserts that the flag has a single
#       unique value within each cell -- it is built for a layout where baseline
#       is its own AVISIT level. In this study the baseline records sit inside
#       "SCREENING 1" alongside non-baseline screening records, so that
#       assertion fails. Both failures were actually run, not assumed:
#         ABLFL as-is (character) -> "Assertion on 'df[[variables$baseline_flag]]'
#             failed: Must be of type 'logical', not 'character'."
#         ABLFL == "Y" (logical)  -> "Assertion on 'unique(df[[variables$
#             baseline_flag]])' failed: Must have length <= 1, but has length 2.
#             occured at (row) path: AVISIT[SCREENING 1]"
#   (b) it reconstructs the change value itself from the baseline flag. That
#       re-derives in the table program the exact quantity ADLB already carries
#       in CHG, which is the opposite of the point being made above.
# tern::analyze_vars(vars = c("BASE", "AVAL", "CHG")) would also work and would
# be the shorter code, but it emits a separate "n" for each of the three
# variables. The layout asked for is one n and three means, so a custom analysis
# function is used.
#
# rtables dispatches on the FIRST FORMAL ARGUMENT NAME of the analysis function:
#   afun(x, ...)   receives the single analysed VECTOR
#   afun(df, ...)  receives the whole cell's DATA FRAME
# `df` is required here because one cell reads three different columns (BASE,
# AVAL, CHG) off the same rows. This naming-is-the-API dispatch is undocumented-
# looking but is the standard rtables idiom.
#
# in_rows() returns the cell block: one entry per row of the table body.
mean_or_na <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) NA_real_ else mean(v)
}

afun_chg <- function(df, ...) {
  in_rows(
    # n counts records with a usable CHANGE, not records present. In general a
    # subject with a post-baseline ALT but no baseline contributes to "Mean
    # AVAL" and not to "Mean CHG", so one n cannot be correct for all three
    # means. VERIFIED FOR THIS SUBSET: the two counts coincide here -- there are
    # ZERO ALT rows with a non-missing AVAL and a missing CHG -- so the choice
    # changes nothing in this output. It is written this way because the
    # divergence is real in any study with missing baselines, and because a
    # footer that names its denominator is the difference between a table a
    # reviewer can check and one they have to trust.
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
    "Unscheduled visits excluded"
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
  # split_rows_by() on a factor gives one block per level, in level order.
  # label_pos = "hidden" would suppress the visit label; it is kept visible
  # because the visit IS the row identifier here.
  #
  # split_fun = drop_split_levels is the DELIBERATE EXCEPTION to the "empty
  # levels still print" rule set out at the top of this file. It drops levels
  # with no records instead of printing an all-NA block. Two things to be
  # straight about: (i) it is a no-op here, because visit_order was built from
  # this same filtered subset, so all 12 levels have at least one record; (ii)
  # it is the right default for a VISIT split and the wrong one for a
  # demographic category. A protocol category with nobody in it is information;
  # a visit at which nothing was collected is usually just noise. Keeping it
  # here means adding a lab parameter later cannot produce phantom visit blocks.
  split_rows_by("AVISIT", label_pos = "visible", split_fun = drop_split_levels) %>%
  # vars = "AVAL" names the variable the block is *about*; because afun takes
  # `df`, the value actually used is whatever afun_chg reads out of the columns.
  # show_labels = "hidden" stops "AVAL" being printed as a heading above every
  # visit block -- the four row labels come from in_rows() instead.
  analyze("AVAL", afun = afun_chg, show_labels = "hidden")

tbl_t2 <- build_table(lyt_t2, adlb_alt)

# -- Fix the column N -------------------------------------------------------
# show_colcounts = TRUE makes rtables print nrow() of each column's data as
# "(N=xx)". On ADSL (Table 1) that is the subject count, because ADSL is one row
# per subject. On a BDS dataset it is the RECORD count -- ADLB has ~7 ALT rows
# per subject, so the automatic header reads (N=707) for placebo where only 86
# subjects contributed. Printing that unchallenged in a clinical table would be
# read as a subject count and would be wrong.
#
# Nothing in Table 2 is a percentage, so the column N is informational only;
# the correct informational number is the number of SUBJECTS contributing. It is
# overwritten here. col_counts(tbl) <- assigns positionally, so the vector must
# be in the same order as the columns -- .drop = FALSE keeps an arm that
# contributed no records from silently shifting every subsequent count.
col_counts(tbl_t2) <- adlb_alt %>%
  distinct(TRT01P, USUBJID) %>%
  count(TRT01P, .drop = FALSE) %>%
  arrange(TRT01P) %>%
  pull(n)

# ===========================================================================
# WRITE THE OUTPUTS
# ===========================================================================
# .txt is the deliverable a statistical programmer actually ships: a monospaced,
# paginated listing. export_as_txt() does the pagination and the width handling;
# capture.output(print(tbl)) would give the same glyphs but no page control.
#
# paginate = FALSE here because both tables are short and a single unbroken
# block is easier to diff in git. Set paginate = TRUE (and lpp/cpp) for a real
# submission deliverable.
export_as_txt(tbl_t1, file = file.path(out_dir, "t1_demographics.txt"), paginate = FALSE)
export_as_txt(tbl_t2, file = file.path(out_dir, "t2_alt_change_by_visit.txt"), paginate = FALSE)

# .csv from as_result_df(): the table as tidy data rather than as glyphs. This
# is the machine-readable counterpart and is what an ARD (Analysis Results Data)
# workflow consumes. data_format = "strings" writes the FORMATTED values, i.e.
# exactly the characters printed in the .txt, so the two files cannot drift.
# Use "full_precision" instead if the numbers are to be re-used in a downstream
# calculation rather than read.
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

# ===========================================================================
# TRACEABILITY: one cell of Table 2, back to the SDTM record it came from
# ===========================================================================
# A regulatory reviewer is entitled to point at any number in any table and ask
# "where did this come from". The chain below is the answer for ONE record, and
# it is computed from the data rather than typed in, so it cannot go stale.
#
# The chain is: table cell -> ADLB row (USUBJID + ASEQ) -> SDTM LB row
# (USUBJID + LBSEQ) -> the collected result LBSTRESN. USUBJID + ASEQ is the
# unique key of an ADLB row; LBSEQ was deliberately kept as a column in ADLB by
# 02_adlb.R for exactly this hop, and would be documented with origin
# "Predecessor" in the study's define.xml. (This repository writes .xpt in
# 90_export_xpt.R but does NOT generate a define.xml -- the origin labels printed
# below say what the define WOULD record, they are not read from one.)
trace_cell_visit <- "WEEK 8"
trace_cell_arm   <- "Xanomeline High Dose"

trace_rows <- adlb_alt %>%
  filter(AVISIT == trace_cell_visit, TRT01P == trace_cell_arm, !is.na(CHG)) %>%
  arrange(USUBJID, ASEQ)

trace <- trace_rows %>% slice(1)

# The SDTM side. lb comes from 00_setup.R; it is read ONLY to show the source
# record, and contributes nothing to any cell above.
lb_src <- lb %>%
  filter(USUBJID == trace$USUBJID, LBSEQ == trace$LBSEQ) %>%
  select(STUDYID, USUBJID, LBSEQ, LBTESTCD, LBTEST, VISIT, LBDTC, LBORRES,
         LBORRESU, LBSTRESN, LBSTRESU, LBBLFL)

# The baseline record this row's BASE was copied from.
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

# Assert the arithmetic the chain claims, so the text can never describe
# something the data does not do.
#
# as.numeric() is not cosmetic: identical(trace$AVAL, lb_src$LBSTRESN) is FALSE
# even though both are 33, and plain all.equal() on the two is FALSE as well.
# pharmaversesdtm ships SDTM with SAS variable LABELS attached as R attributes,
# so lb$LBSTRESN carries label = "Numeric Result/Finding in Standard Units",
# while ADLB's AVAL carries no attributes at all. The VALUES match; the
# attributes do not, and identical()/all.equal() compare both.
#
# WHERE THE LABEL ACTUALLY GOES, because the obvious answer is wrong: it is NOT
# mutate(AVAL = LBSTRESN) in 02_adlb.R -- a plain dplyr mutate copies the vector
# with its attributes intact (checked). The give-away is that NOT ONE of the 40
# columns in adlb.rds carries a label, including LBSTRESN itself, which is only
# passed through. The stripper is dplyr::bind_rows(), which drops column
# attributes when it combines frames, and which admiral's restrict_derivation()
# uses internally to glue the filtered-in and filtered-out parts back together.
# Any one such call de-labels the whole dataset from that point on.
#
# Worth knowing because {xportr} re-attaches labels from the spec when writing
# the submission .xpt (see 90_export_xpt.R) -- so the labels are restored from
# metadata, not preserved through the pipeline -- and because identical() on
# labelled columns is a reliable way to confuse yourself.
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
