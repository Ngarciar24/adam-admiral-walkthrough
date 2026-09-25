# -----------------------------------------------------------------------------
# Program    : test-adlb.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Tests for ADLB. Part A: structural and consistency checks on the
#              built dataset. Part B: unit tests of the baseline rule
#              (R/derive_ablfl.R) on hand-built data covering edge cases the
#              study data does not contain.
# Inputs     : data/adam/adlb.rds, data/adam/adsl.rds, metadata/adlb_params.csv
# Usage      : Rscript -e 'testthat::test_file("tests/testthat/test-adlb.R")'
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-17  IGR  Part B calls the shared R/derive_ablfl.R
#              2026-09-25  IGR  Standard header; comments condensed
#              2026-09-25  IGR  CHG/PCHG/SHIFT1 post-baseline only; TRTPN/TRTAN
# -----------------------------------------------------------------------------

library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(admiral)
  library(readr)
})

# --- Project files (works from the root or from tests/testthat) --------------
proj_file <- function(relpath) {
  candidates <- c(relpath, file.path("..", "..", relpath))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stop("Cannot find ", relpath, " from working directory ", getwd(),
         ". Build it first with: Rscript programs/02_adlb.R")
  }
  hit[1]
}

adlb <- readRDS(proj_file("data/adam/adlb.rds"))
adsl <- readRDS(proj_file("data/adam/adsl.rds"))
adlb_params <- read_csv(proj_file("metadata/adlb_params.csv"), show_col_types = FALSE)

# Tolerance for comparing doubles (PCHG involves a division).
TOL <- 1e-8

# Failure message listing the first offending rows.
offenders <- function(dat, n = 5) {
  paste0(
    nrow(dat), " offending row(s); first ", min(n, nrow(dat)), ":\n",
    paste(utils::capture.output(print(as.data.frame(head(dat, n)))), collapse = "\n")
  )
}


# ===========================================================================
# PART A -- structural checks on the real ADLB
# ===========================================================================

# ---------------------------------------------------------------------------
# 1. Grain
# ---------------------------------------------------------------------------
# (USUBJID, ASEQ) is the record key; (USUBJID, PARAMCD, ADT, LBSEQ) is the
# order used to assign ASEQ, so it must be unique for ASEQ to be stable.
test_that("grain: (USUBJID, ASEQ) is unique", {
  dups <- adlb %>% count(USUBJID, ASEQ) %>% filter(n > 1)
  expect_equal(nrow(dups), 0L, info = offenders(dups))
  expect_false(any(is.na(adlb$ASEQ)))
})

test_that("grain: (USUBJID, PARAMCD, ADT, LBSEQ) is unique", {
  dups <- adlb %>% count(USUBJID, PARAMCD, ADT, LBSEQ) %>% filter(n > 1)
  expect_equal(nrow(dups), 0L, info = offenders(dups))
})

# ---------------------------------------------------------------------------
# 2. Parameter metadata
# ---------------------------------------------------------------------------
# PARAMCD comes from the spec, and PARAMCD <-> PARAM is 1:1 in both directions.
test_that("PARAMCD is restricted to the parameters in the spec", {
  expect_true(all(adlb$PARAMCD %in% adlb_params$PARAMCD))
  # Every spec parameter has rows.
  expect_setequal(unique(adlb$PARAMCD), adlb_params$PARAMCD)
  expect_false(any(is.na(adlb$PARAMCD)))
})

test_that("PARAMCD <-> PARAM is a 1:1 mapping, and matches the spec", {
  map <- distinct(adlb, PARAMCD, PARAM)
  expect_equal(nrow(map), n_distinct(adlb$PARAMCD))  # one PARAM per PARAMCD
  expect_equal(nrow(map), n_distinct(adlb$PARAM))    # one PARAMCD per PARAM
  # Pairs match the spec.
  expect_equal(
    map %>% arrange(PARAMCD) %>% as.data.frame(),
    adlb_params %>% select(PARAMCD, PARAM) %>% arrange(PARAMCD) %>% as.data.frame()
  )
})

# ---------------------------------------------------------------------------
# 3. ABLFL is at most one record per subject/parameter, and it qualifies
# ---------------------------------------------------------------------------
# At most one: a subject with no pre-dose result has no baseline.
test_that("ABLFL: at most one 'Y' per (USUBJID, PARAMCD)", {
  # Record-level flag: "Y" or missing, never "N" (unlike population flags).
  expect_setequal(unique(adlb$ABLFL), c("Y", NA))

  too_many <- adlb %>%
    group_by(USUBJID, PARAMCD) %>%
    summarise(n_bl = sum(!is.na(ABLFL) & ABLFL == "Y"), .groups = "drop") %>%
    filter(n_bl > 1)
  expect_equal(nrow(too_many), 0L, info = offenders(too_many))
})

test_that("ABLFL: every flagged record is pre-dose and has a non-missing AVAL", {
  bl <- adlb %>% filter(!is.na(ABLFL) & ABLFL == "Y")

  # Baseline rule: non-missing value on or before first dose (ADT <= TRTSDT).
  bad_date <- bl %>% filter(is.na(TRTSDT) | ADT > TRTSDT)
  expect_equal(nrow(bad_date), 0L, info = offenders(bad_date %>% select(USUBJID, PARAMCD, ADT, TRTSDT)))

  bad_aval <- bl %>% filter(is.na(AVAL))
  expect_equal(nrow(bad_aval), 0L, info = offenders(bad_aval %>% select(USUBJID, PARAMCD, ADT, AVAL)))
})

# ---------------------------------------------------------------------------
# 4. BASE is a faithful broadcast of the baseline record
# ---------------------------------------------------------------------------
# BASE is constant within subject/parameter and equals the baseline AVAL.
test_that("BASE is constant within (USUBJID, PARAMCD) and equals the ABLFL row's AVAL", {
  grp <- adlb %>%
    group_by(USUBJID, PARAMCD) %>%
    summarise(
      n_distinct_base = n_distinct(BASE),
      base_seen       = BASE[1],
      # NA for a group without a baseline record.
      aval_at_ablfl   = AVAL[!is.na(ABLFL) & ABLFL == "Y"][1],
      has_ablfl       = any(!is.na(ABLFL) & ABLFL == "Y"),
      .groups = "drop"
    )

  varying <- grp %>% filter(n_distinct_base > 1)
  expect_equal(nrow(varying), 0L, info = offenders(varying))

  with_bl <- grp %>% filter(has_ablfl)
  expect_lt(max(abs(with_bl$base_seen - with_bl$aval_at_ablfl)), TOL)

  # No baseline record => BASE is NA.
  without_bl <- grp %>% filter(!has_ablfl, !is.na(base_seen))
  expect_equal(nrow(without_bl), 0L, info = offenders(without_bl))
})

test_that("BNRIND is the ANRIND of the baseline record", {
  # Separate derive_var_base() call, so checked separately.
  grp <- adlb %>%
    group_by(USUBJID, PARAMCD) %>%
    summarise(
      n_distinct_bnrind = n_distinct(BNRIND),
      bnrind_seen       = BNRIND[1],
      anrind_at_ablfl   = ANRIND[!is.na(ABLFL) & ABLFL == "Y"][1],
      .groups = "drop"
    )
  expect_true(all(grp$n_distinct_bnrind == 1))
  expect_identical(grp$bnrind_seen, grp$anrind_at_ablfl)
})

# ---------------------------------------------------------------------------
# 5. CHG / PCHG arithmetic
# ---------------------------------------------------------------------------
# Arithmetic after first dose where AVAL and BASE exist; missing otherwise.
test_that("CHG == AVAL - BASE and PCHG == 100*(AVAL-BASE)/BASE", {
  ok <- adlb %>% filter(!is.na(AVAL), !is.na(BASE), ADT > TRTSDT)
  expect_gt(nrow(ok), 0L)
  expect_lt(max(abs(ok$CHG - (ok$AVAL - ok$BASE))), TOL)

  # PCHG undefined for BASE = 0 (none in this study).
  okp <- ok %>% filter(BASE != 0)
  expect_lt(max(abs(okp$PCHG - 100 * (okp$AVAL - okp$BASE) / okp$BASE)), TOL)

  # Missing values propagate.
  expect_true(all(is.na(adlb$CHG[is.na(adlb$AVAL) | is.na(adlb$BASE)])))
  expect_true(all(is.na(adlb$PCHG[is.na(adlb$AVAL) | is.na(adlb$BASE)])))
})

test_that("CHG, PCHG and SHIFT1 are missing on and before the day of first dose", {
  pre <- adlb %>% filter(ADT <= TRTSDT)
  expect_gt(nrow(pre), 0L)
  expect_true(all(is.na(pre$CHG) & is.na(pre$PCHG) & is.na(pre$SHIFT1)))
  # Every post-baseline record with AVAL and BASE has a CHG.
  post <- adlb %>% filter(ADT > TRTSDT, !is.na(AVAL), !is.na(BASE))
  expect_false(anyNA(post$CHG))
})

test_that("SHIFT1 is 'BNRIND to ANRIND' after first dose when both exist", {
  expect_false(any(grepl("NULL", adlb$SHIFT1, fixed = TRUE)))
  want <- if_else(adlb$ADT > adlb$TRTSDT & !is.na(adlb$BNRIND) & !is.na(adlb$ANRIND),
                  paste(adlb$BNRIND, "to", adlb$ANRIND), NA_character_)
  expect_equal(adlb$SHIFT1, want)
})

test_that("TRTPN/TRTAN are the numeric codes of TRTP/TRTA", {
  dose <- c(Placebo = 0, "Xanomeline Low Dose" = 54, "Xanomeline High Dose" = 81)
  expect_equal(adlb$TRTPN, unname(dose[adlb$TRTP]))
  expect_equal(adlb$TRTAN, unname(dose[adlb$TRTA]))
})

# ---------------------------------------------------------------------------
# 6. ANRIND
# ---------------------------------------------------------------------------
# Controlled values, recomputed from AVAL vs ANRLO/ANRHI. Limits are
# inclusive: AVAL equal to a limit is NORMAL.
test_that("ANRIND is in the controlled set and agrees with AVAL vs ANRLO/ANRHI", {
  expect_setequal(unique(adlb$ANRIND), c("LOW", "NORMAL", "HIGH", NA))

  chk <- adlb %>%
    mutate(
      expected = case_when(
        is.na(AVAL) | is.na(ANRLO) | is.na(ANRHI) ~ NA_character_,
        AVAL < ANRLO                              ~ "LOW",
        AVAL > ANRHI                              ~ "HIGH",
        TRUE                                      ~ "NORMAL"
      )
    )
  # xor() catches disagreement about missingness, which != would drop as NA.
  bad <- chk %>% filter(xor(is.na(expected), is.na(ANRIND)) |
                          (!is.na(expected) & !is.na(ANRIND) & expected != ANRIND))
  expect_equal(nrow(bad), 0L,
               info = offenders(bad %>% select(USUBJID, PARAMCD, AVAL, ANRLO, ANRHI, ANRIND, expected)))
})

# ---------------------------------------------------------------------------
# 7. Study day has no day zero
# ---------------------------------------------------------------------------
# ADY = ADT - TRTSDT + 1 on or after first dose, ADT - TRTSDT before it.
test_that("ADY is never 0", {
  zeros <- adlb %>% filter(!is.na(ADY), ADY == 0)
  expect_equal(nrow(zeros), 0L, info = offenders(zeros %>% select(USUBJID, PARAMCD, ADT, TRTSDT, ADY)))
  # ADY populated exactly when both dates are.
  expect_true(all(is.na(adlb$ADY) == (is.na(adlb$ADT) | is.na(adlb$TRTSDT))))
})

# ---------------------------------------------------------------------------
# 8. Referential integrity with ADSL
# ---------------------------------------------------------------------------
# One-way check: ADSL subjects without labs (the 52 screen failures) are
# expected.
test_that("every ADLB USUBJID exists in ADSL", {
  orphans <- setdiff(adlb$USUBJID, adsl$USUBJID)
  expect_equal(length(orphans), 0L,
               info = paste("USUBJIDs not in ADSL:", paste(head(orphans, 5), collapse = ", ")))
  # Merged ADSL variables populated on every row.
  expect_false(any(is.na(adlb$TRT01P)))
  expect_false(any(is.na(adlb$SAFFL)))
})

# ---------------------------------------------------------------------------
# 9. Analysis visit collapsing
# ---------------------------------------------------------------------------
# Rule: every unscheduled VISIT becomes AVISIT "UNSCHEDULED" (AVISITN 999);
# scheduled visits keep VISIT/VISITNUM. Checked in both directions.
test_that("AVISIT == 'UNSCHEDULED' exactly when VISIT contains 'UNSCHEDULED', with AVISITN 999", {
  is_unsch_avisit <- adlb$AVISIT == "UNSCHEDULED"
  is_unsch_visit  <- grepl("UNSCHEDULED", adlb$VISIT, fixed = TRUE)
  expect_equal(sum(xor(is_unsch_avisit, is_unsch_visit)), 0L)

  expect_true(all(adlb$AVISITN[is_unsch_avisit] == 999))
  # 999 is reserved for unscheduled.
  expect_true(all(adlb$AVISITN[!is_unsch_avisit] != 999))
  expect_true(all(adlb$AVISITN[!is_unsch_avisit] == adlb$VISITNUM[!is_unsch_avisit]))
})

# ---------------------------------------------------------------------------
# 10. ABLFL vs SDTM LBBLFL
# ---------------------------------------------------------------------------
# The flags answer different questions: LBBLFL marks SCREENING 1, ABLFL
# follows the rule (last value on or before first dose), which often picks a
# later unscheduled pre-dose record. Counts are pinned and the structure of
# the difference is checked: every SDTM-only record is eligible and earlier
# than the ADaM pick.
test_that("ABLFL and SDTM LBBLFL disagree in the expected, explainable way", {
  x <- adlb %>%
    mutate(
      adam_bl = !is.na(ABLFL)  & ABLFL  == "Y",
      sdtm_bl = !is.na(LBBLFL) & LBBLFL == "Y"
    )

  adam_only <- x %>% filter(adam_bl, !sdtm_bl)
  sdtm_only <- x %>% filter(!adam_bl, sdtm_bl)

  expect_equal(nrow(adam_only), 121L)
  expect_equal(nrow(sdtm_only), 106L)

  # SDTM-only records are eligible and passed over for a later record.
  expect_true(all(sdtm_only$ADT <= sdtm_only$TRTSDT))
  expect_true(all(!is.na(sdtm_only$AVAL)))

  passed_over <- sdtm_only %>%
    select(USUBJID, PARAMCD, ADT_sdtm = ADT) %>%
    left_join(
      x %>% filter(adam_bl) %>% select(USUBJID, PARAMCD, ADT_adam = ADT),
      by = c("USUBJID", "PARAMCD")
    )
  expect_false(any(is.na(passed_over$ADT_adam)))
  expect_true(all(passed_over$ADT_adam > passed_over$ADT_sdtm))

  # 15 groups have an ADaM baseline where SDTM flagged nothing at all.
  no_sdtm_flag <- x %>%
    group_by(USUBJID, PARAMCD) %>%
    summarise(n_sdtm = sum(sdtm_bl), n_adam = sum(adam_bl), .groups = "drop") %>%
    filter(n_sdtm == 0)
  expect_equal(nrow(no_sdtm_flag), 15L)
  expect_true(all(no_sdtm_flag$n_adam == 1))

  # 85 groups would have a different BASE under the SDTM flag.
  both <- x %>%
    group_by(USUBJID, PARAMCD) %>%
    summarise(
      base_adam = AVAL[adam_bl][1],
      base_sdtm = AVAL[sdtm_bl][1],
      .groups = "drop"
    ) %>%
    filter(!is.na(base_adam), !is.na(base_sdtm))
  expect_equal(sum(abs(both$base_adam - both$base_sdtm) > TOL), 85L)
})


# ===========================================================================
# PART B -- unit tests of the baseline-selection logic
# ===========================================================================
# Hand-built data with answers known by inspection, run through the same
# function as programs/02_adlb.R.
source(proj_file("R/derive_ablfl.R"))

# First dose 2013-01-10 for every fixture subject.
TRT_START <- as.Date("2013-01-10")

# One subject per edge case, in one frame so grouping by subject is tested too.
mini <- tribble(
  ~USUBJID,    ~ADT,         ~LBSEQ, ~AVAL,  ~case,
  # (a) two pre-dose records -> the LATER one wins
  "TWO-PRE",   "2013-01-01",  1L,     10,    "early pre-dose",
  "TWO-PRE",   "2013-01-05",  2L,     20,    "LATER pre-dose -> baseline",
  "TWO-PRE",   "2013-02-01",  3L,     30,    "post-dose",
  # (b) tie on ADT -> broken by LBSEQ, deterministically
  "TIE",       "2013-01-03",  1L,     11,    "tie, lower LBSEQ",
  "TIE",       "2013-01-03",  2L,     22,    "tie, higher LBSEQ -> baseline",
  "TIE",       "2013-02-03",  3L,     33,    "post-dose",
  # (c) the latest pre-dose record has AVAL NA -> skipped for an earlier one
  "NA-PRE",    "2013-01-02",  1L,     40,    "earlier, non-missing -> baseline",
  "NA-PRE",    "2013-01-06",  2L,     NA,    "latest pre-dose but AVAL missing",
  "NA-PRE",    "2013-02-02",  3L,     60,    "post-dose",
  # (d) no pre-dose record at all -> no ABLFL, BASE stays NA
  "NO-PRE",    "2013-01-20",  1L,     70,    "post-dose",
  "NO-PRE",    "2013-02-20",  2L,     80,    "post-dose",
  # (e) never dosed (TRTSDT NA): no record qualifies
  "NO-TRTSDT", "2013-01-02",  1L,     90,    "pre-dose date but no first dose",
  "NO-TRTSDT", "2013-01-08",  2L,    100,    "pre-dose date but no first dose",
  # (f) the ON-OR-BEFORE boundary: a record dated exactly TRTSDT.
  "ON-DOSE",   "2013-01-08",  1L,    110,    "before dosing day",
  "ON-DOSE",   "2013-01-10",  2L,    120,    "ON dosing day -> baseline"
) %>%
  mutate(
    STUDYID = "TESTSTUDY",
    PARAMCD = "ALT",
    ADT     = as.Date(ADT),
    TRTSDT  = if_else(USUBJID == "NO-TRTSDT", as.Date(NA), TRT_START)
  )

mini_out <- derive_ablfl_base(mini)

# Helpers: flagged row(s) and all rows for one subject.
flagged <- function(out, subject) {
  out %>% filter(USUBJID == subject, !is.na(ABLFL), ABLFL == "Y")
}
subj <- function(out, subject) out %>% filter(USUBJID == subject) %>% arrange(LBSEQ)

test_that("the derivation preserves every input row", {
  # Same set of rows (order is not preserved, so tests sort explicitly).
  expect_equal(nrow(mini_out), nrow(mini))
  expect_setequal(mini_out$LBSEQ[mini_out$USUBJID == "TWO-PRE"], c(1L, 2L, 3L))
})

test_that("(a) with two pre-dose records the LATER one is baseline", {
  fl <- flagged(mini_out, "TWO-PRE")
  expect_equal(nrow(fl), 1L)
  expect_equal(fl$LBSEQ, 2L)                  # 2013-01-05, not 2013-01-01
  expect_equal(fl$ADT, as.Date("2013-01-05"))
  # BASE on all rows, including post-dose.
  expect_equal(subj(mini_out, "TWO-PRE")$BASE, c(20, 20, 20))
})

test_that("(b) a tie on ADT is broken by LBSEQ, deterministically", {
  fl <- flagged(mini_out, "TIE")
  expect_equal(nrow(fl), 1L)
  # Highest LBSEQ on the latest date.
  expect_equal(fl$LBSEQ, 2L)
  expect_equal(subj(mini_out, "TIE")$BASE, c(22, 22, 22))

  # Same result for all 6 input row orders. Without LBSEQ in `order` the
  # result varies with row order (checked by mutation).
  tie_rows <- mini %>% filter(USUBJID == "TIE")
  perms <- list(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
  picked <- vapply(perms, function(p) derive_ablfl_base(tie_rows[p, ]) %>%
                     filter(!is.na(ABLFL), ABLFL == "Y") %>% pull(LBSEQ), integer(1))
  expect_equal(picked, rep(2L, 6L))

  # admiral warns when `order` does not make records unique; the clean run
  # must be warning-free.
  expect_no_warning(derive_ablfl_base(tie_rows))
})

test_that("the tie-break rule is not exercised by the real study data at all", {
  # No (USUBJID, PARAMCD, ADT) ties among eligible records in the study data,
  # which is why the tie-break is tested on the fixture. Descriptive: update
  # if a data refresh introduces ties.
  ties <- adlb %>%
    filter(!is.na(AVAL), !is.na(TRTSDT), ADT <= TRTSDT) %>%
    count(USUBJID, PARAMCD, ADT) %>%
    filter(n > 1)
  expect_equal(nrow(ties), 0L, info = offenders(ties))
})

test_that("(c) a pre-dose record with AVAL NA is skipped for an earlier non-missing one", {
  fl <- flagged(mini_out, "NA-PRE")
  expect_equal(nrow(fl), 1L)
  # Latest pre-dose record (LBSEQ 2) has missing AVAL, so LBSEQ 1 is used.
  expect_equal(fl$LBSEQ, 1L)
  expect_equal(fl$AVAL, 40)
  # BASE on every row, including the one with missing AVAL.
  expect_equal(subj(mini_out, "NA-PRE")$BASE, c(40, 40, 40))
})

test_that("(d) a subject with no pre-dose record gets no ABLFL and BASE stays NA", {
  fl <- flagged(mini_out, "NO-PRE")
  expect_equal(nrow(fl), 0L)
  rows <- subj(mini_out, "NO-PRE")
  expect_true(all(is.na(rows$ABLFL)))
  # NA, not 0.
  expect_true(all(is.na(rows$BASE)))
  # Rows kept, unflagged.
  expect_equal(nrow(rows), 2L)
})

test_that("(e) a subject with no first-dose date gets no baseline", {
  # ADT <= NA is NA and must not qualify.
  fl <- flagged(mini_out, "NO-TRTSDT")
  expect_equal(nrow(fl), 0L)
  expect_true(all(is.na(subj(mini_out, "NO-TRTSDT")$BASE)))
})

test_that("(f) the boundary is ON OR BEFORE first dose, not strictly before", {
  # A record dated on the first-dose day qualifies. Only this test separates
  # <= from < (checked by mutation).
  fl <- flagged(mini_out, "ON-DOSE")
  expect_equal(nrow(fl), 1L)
  expect_equal(fl$LBSEQ, 2L)
  expect_equal(fl$ADT, TRT_START)
  expect_equal(subj(mini_out, "ON-DOSE")$BASE, c(120, 120))

  # ADT and TRTSDT are dates, so a same-day post-dose draw would also qualify.
  # LB has times (LBDTC), but EX dosing dates have none, so a datetime
  # comparison is not possible. 1 of 1270 baseline records is on TRTSDT.
  expect_s3_class(adlb$ADT, "Date")      # the shipped data, not just the fixture
  expect_s3_class(adlb$TRTSDT, "Date")
  expect_s3_class(mini$ADT, "Date")
  expect_s3_class(mini$TRTSDT, "Date")
})

test_that("grouping is by PARAMCD as well as subject", {
  # Two parameters interleaved in time: one baseline per parameter.
  two_param <- tribble(
    ~PARAMCD, ~ADT,         ~LBSEQ, ~AVAL,
    "ALT",    "2013-01-02",  1L,     1,
    "HGB",    "2013-01-03",  2L,     2,
    "ALT",    "2013-01-04",  3L,     3,
    "HGB",    "2013-01-05",  4L,     4,
    "ALT",    "2013-02-01",  5L,     5
  ) %>%
    mutate(
      STUDYID = "TESTSTUDY", USUBJID = "MULTI",
      ADT = as.Date(ADT), TRTSDT = TRT_START
    )

  out <- derive_ablfl_base(two_param)
  fl <- out %>% filter(!is.na(ABLFL), ABLFL == "Y") %>% arrange(PARAMCD)
  expect_equal(nrow(fl), 2L)
  expect_equal(fl$PARAMCD, c("ALT", "HGB"))
  expect_equal(fl$LBSEQ, c(3L, 4L))   # last pre-dose WITHIN each parameter
  expect_equal(out %>% arrange(LBSEQ) %>% pull(BASE), c(3, 4, 3, 4, 3))
})
