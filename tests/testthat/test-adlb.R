# ---------------------------------------------------------------------------
# test-adlb.R -- tests for data/adam/adlb.rds (built by programs/02_adlb.R)
#
# Two kinds of test live in this file, and the distinction matters:
#
#   PART A -- STRUCTURAL / CONFORMANCE checks run against the real built
#             dataset. These answer "is the thing I produced a legal ADaM BDS
#             dataset, and is it internally consistent?" They are the analogue
#             of the checks a sponsor's Pinnacle 21 run would make, hand-written
#             here so it is visible WHAT is being asserted.
#
#   PART B -- UNIT tests of the baseline-selection LOGIC, run against tiny
#             hand-built tibbles whose correct answer is known by inspection.
#             These answer "does my ABLFL rule actually implement the SAP
#             sentence, including at its edges?"
#
# Part A alone is a snapshot: it would pass just as happily on a subtly wrong
# rule, because it only checks that the output is self-consistent. Part B is
# what pins the rule itself down. Edge cases (ties, missing values, subjects
# with no qualifying record) are exactly where a baseline derivation goes wrong
# in a real study, and they are usually NOT exercised by the study data you
# happen to have in front of you.
#
# Run from the project root with:
#   Rscript -e 'testthat::test_file("tests/testthat/test-adlb.R")'
# ---------------------------------------------------------------------------

library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(admiral)
  library(readr)
})

# --- Locating the project files -------------------------------------------
# testthat may run this file with the working directory set either to the
# project root or to tests/testthat/, depending on how it is invoked. Resolve
# both rather than hard-coding one and having the file "only work when I run it
# the way I ran it".
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

# Numeric tolerance. AVAL, BASE, CHG and PCHG are doubles. CHG happens to be
# exact here (it is one subtraction), but PCHG involves a division and is only
# equal to the recomputed value to about 1e-14. Asserting == on doubles is the
# classic way to write a test that passes on your machine and fails on the
# validation server.
TOL <- 1e-8

# A failure message that lists the offending keys is worth more than a bare
# "expected 0, got 7": the point of a data test is to tell you WHICH rows.
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
# A BDS dataset has to state its grain and then actually hold to it. Two keys
# are checked because they mean different things:
#   (USUBJID, ASEQ)                 -- the ADaM record identifier. ASEQ is the
#                                      variable a reviewer's query points at, so
#                                      a duplicate here makes the dataset
#                                      unciteable.
#   (USUBJID, PARAMCD, ADT, LBSEQ)  -- the derivation's natural key. This is the
#                                      `order` used for ASEQ, so if it is not
#                                      unique then ASEQ was assigned by an
#                                      arbitrary tie-break and is not stable
#                                      across re-runs.
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
# PARAMCD must come from the spec, not from whatever happened to be in LB. And
# PARAMCD -> PARAM must be 1:1 IN BOTH DIRECTIONS. The reverse direction is the
# one people forget: two different PARAMCDs sharing one PARAM string would make
# a table's row labels ambiguous even though every individual row looks fine.
test_that("PARAMCD is restricted to the parameters in the spec", {
  expect_true(all(adlb$PARAMCD %in% adlb_params$PARAMCD))
  # setequal, not just subset: if a parameter in the spec produced no rows at
  # all that is a silent data problem, not a pass.
  expect_setequal(unique(adlb$PARAMCD), adlb_params$PARAMCD)
  expect_false(any(is.na(adlb$PARAMCD)))
})

test_that("PARAMCD <-> PARAM is a 1:1 mapping, and matches the spec", {
  map <- distinct(adlb, PARAMCD, PARAM)
  expect_equal(nrow(map), n_distinct(adlb$PARAMCD))  # one PARAM per PARAMCD
  expect_equal(nrow(map), n_distinct(adlb$PARAM))    # one PARAMCD per PARAM
  # And the pairs are the ones the spec says, not merely self-consistent ones.
  expect_equal(
    map %>% arrange(PARAMCD) %>% as.data.frame(),
    adlb_params %>% select(PARAMCD, PARAM) %>% arrange(PARAMCD) %>% as.data.frame()
  )
})

# ---------------------------------------------------------------------------
# 3. ABLFL is at most one record per subject/parameter, and it qualifies
# ---------------------------------------------------------------------------
# "At most one" rather than "exactly one" is deliberate: a subject with no
# pre-dose result legitimately has no baseline. Asserting "exactly one" here
# would be asserting a property of THIS dataset, not of the derivation.
test_that("ABLFL: at most one 'Y' per (USUBJID, PARAMCD)", {
  # ABLFL is a RECORD-LEVEL flag, and the ADaM convention for those is "Y" or
  # null -- never "N" and never "". Do NOT over-generalise that to all *FL
  # variables: the POPULATION flags in this repo's own ADSL (SAFFL, ITTFL) are
  # genuinely "Y"/"N", 254/52. Worth pinning for ABLFL specifically, because a
  # stray "N" would leave `ABLFL == "Y"` filters behaving while `!is.na(ABLFL)`
  # filters silently break.
  expect_setequal(unique(adlb$ABLFL), c("Y", NA))

  too_many <- adlb %>%
    group_by(USUBJID, PARAMCD) %>%
    summarise(n_bl = sum(!is.na(ABLFL) & ABLFL == "Y"), .groups = "drop") %>%
    filter(n_bl > 1)
  expect_equal(nrow(too_many), 0L, info = offenders(too_many))
})

test_that("ABLFL: every flagged record is pre-dose and has a non-missing AVAL", {
  bl <- adlb %>% filter(!is.na(ABLFL) & ABLFL == "Y")

  # This is the SAP sentence restated as an assertion: "last NON-MISSING value
  # ON OR BEFORE first dose". ADT <= TRTSDT, not ADT < TRTSDT -- a lab drawn on
  # the morning of dosing is a baseline. ADT and TRTSDT are both class Date, so
  # a same-day POST-dose draw cannot be told apart from a pre-dose one and would
  # be accepted. Test (f) below states exactly why that is a missing DOSE-time
  # problem rather than a missing lab-time one; the distinction matters, because
  # the lazy version of this caveat is factually wrong about this study.
  bad_date <- bl %>% filter(is.na(TRTSDT) | ADT > TRTSDT)
  expect_equal(nrow(bad_date), 0L, info = offenders(bad_date %>% select(USUBJID, PARAMCD, ADT, TRTSDT)))

  bad_aval <- bl %>% filter(is.na(AVAL))
  expect_equal(nrow(bad_aval), 0L, info = offenders(bad_aval %>% select(USUBJID, PARAMCD, ADT, AVAL)))
})

# ---------------------------------------------------------------------------
# 4. BASE is a faithful broadcast of the baseline record
# ---------------------------------------------------------------------------
# BASE is denormalised: the same number repeated on every row of the group.
# Two things can go wrong and both are checked, because either one alone would
# still look plausible in a listing:
#   - BASE varies within the group (the broadcast joined on the wrong key)
#   - BASE is constant but is not the ABLFL row's AVAL (the broadcast picked
#     the wrong record)
test_that("BASE is constant within (USUBJID, PARAMCD) and equals the ABLFL row's AVAL", {
  grp <- adlb %>%
    group_by(USUBJID, PARAMCD) %>%
    summarise(
      n_distinct_base = n_distinct(BASE),
      base_seen       = BASE[1],
      # [1] on a zero-length vector gives NA rather than erroring, which is what
      # we want for a group that has no baseline record.
      aval_at_ablfl   = AVAL[!is.na(ABLFL) & ABLFL == "Y"][1],
      has_ablfl       = any(!is.na(ABLFL) & ABLFL == "Y"),
      .groups = "drop"
    )

  varying <- grp %>% filter(n_distinct_base > 1)
  expect_equal(nrow(varying), 0L, info = offenders(varying))

  with_bl <- grp %>% filter(has_ablfl)
  expect_lt(max(abs(with_bl$base_seen - with_bl$aval_at_ablfl)), TOL)

  # And the converse: no baseline record => BASE must be NA, not 0 and not
  # carried over from a neighbouring group.
  without_bl <- grp %>% filter(!has_ablfl, !is.na(base_seen))
  expect_equal(nrow(without_bl), 0L, info = offenders(without_bl))
})

test_that("BNRIND is the ANRIND of the baseline record", {
  # Same broadcast mechanism as BASE, applied to the categorical variable.
  # Checked separately because it is a second derive_var_base() call and a
  # copy-paste error in its arguments would not show up in the BASE test.
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
# "Wherever both are non-missing": rows with a missing AVAL or a missing BASE
# must have a missing CHG, and that is asserted too. Testing only the populated
# rows would let a derivation that invented CHG = 0 for missing AVAL pass.
test_that("CHG == AVAL - BASE and PCHG == 100*(AVAL-BASE)/BASE", {
  ok <- adlb %>% filter(!is.na(AVAL), !is.na(BASE))
  expect_gt(nrow(ok), 0L)
  expect_lt(max(abs(ok$CHG - (ok$AVAL - ok$BASE))), TOL)

  # PCHG is undefined when BASE is 0 (division by zero). There are no BASE == 0
  # rows in this dataset, but the filter is written anyway so the test does not
  # become a landmine if the data changes.
  okp <- ok %>% filter(BASE != 0)
  expect_lt(max(abs(okp$PCHG - 100 * (okp$AVAL - okp$BASE) / okp$BASE)), TOL)

  # Missingness must propagate, not be filled in.
  expect_true(all(is.na(adlb$CHG[is.na(adlb$AVAL) | is.na(adlb$BASE)])))
  expect_true(all(is.na(adlb$PCHG[is.na(adlb$AVAL) | is.na(adlb$BASE)])))
})

# ---------------------------------------------------------------------------
# 6. ANRIND
# ---------------------------------------------------------------------------
# Two separate assertions: the value is in the controlled set, AND it agrees
# with the numbers it claims to summarise. The second is the real one -- a
# hard-coded ANRIND <- "NORMAL" would pass the first.
#
# Boundary convention: AVAL exactly equal to ANRLO or ANRHI is NORMAL. The
# range is inclusive at both ends, so only strict inequalities produce LOW/HIGH.
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
  # An offender is a row where the derived and recomputed values disagree,
  # INCLUDING a disagreement about missingness -- hence the xor() term. A plain
  # `expected != ANRIND` comparison returns NA for those rows and filter()
  # drops NA, so the rows that matter most would quietly escape the check.
  bad <- chk %>% filter(xor(is.na(expected), is.na(ANRIND)) |
                          (!is.na(expected) & !is.na(ANRIND) & expected != ANRIND))
  expect_equal(nrow(bad), 0L,
               info = offenders(bad %>% select(USUBJID, PARAMCD, AVAL, ANRLO, ANRHI, ANRIND, expected)))
})

# ---------------------------------------------------------------------------
# 7. Study day has no day zero
# ---------------------------------------------------------------------------
# ADY = ADT - TRTSDT + 1 on or after first dose, ADT - TRTSDT before it. The
# day before first dose is Day -1, never Day 0. An ADY of 0 is the fingerprint
# of a plain date subtraction that forgot the +1, which would silently shift
# every post-baseline day by one in a by-day listing.
test_that("ADY is never 0", {
  zeros <- adlb %>% filter(!is.na(ADY), ADY == 0)
  expect_equal(nrow(zeros), 0L, info = offenders(zeros %>% select(USUBJID, PARAMCD, ADT, TRTSDT, ADY)))
  # ADY must be populated exactly when both dates it needs are populated.
  expect_true(all(is.na(adlb$ADY) == (is.na(adlb$ADT) | is.na(adlb$TRTSDT))))
})

# ---------------------------------------------------------------------------
# 8. Referential integrity with ADSL
# ---------------------------------------------------------------------------
# ADSL is the single source of truth for who is in the study. A USUBJID in a BDS
# dataset that is absent from ADSL means either a merge key problem or a subject
# who is in a lab file but not in the study -- both are hard errors at submission.
# NOTE this is a one-way check: ADSL subjects with no lab data are normal (the
# 52 screen failures contribute no LB records), so the reverse is NOT asserted.
test_that("every ADLB USUBJID exists in ADSL", {
  orphans <- setdiff(adlb$USUBJID, adsl$USUBJID)
  expect_equal(length(orphans), 0L,
               info = paste("USUBJIDs not in ADSL:", paste(head(orphans, 5), collapse = ", ")))
  # The merged-in ADSL columns must be non-missing for every row, which is the
  # practical symptom of a failed merge even when the key technically matches.
  expect_false(any(is.na(adlb$TRT01P)))
  expect_false(any(is.na(adlb$SAFFL)))
})

# ---------------------------------------------------------------------------
# 9. Analysis visit collapsing
# ---------------------------------------------------------------------------
# AVISIT is an ANALYSIS visit and is allowed to differ from the collected VISIT.
# The rule implemented in 02_adlb.R is the simplest possible one: collapse every
# unscheduled visit into a single "UNSCHEDULED" category with AVISITN = 999.
# The test states the rule as an if-and-only-if, so that neither an unscheduled
# visit that escaped collapsing nor a scheduled visit that got swept in can pass.
test_that("AVISIT == 'UNSCHEDULED' exactly when VISIT contains 'UNSCHEDULED', with AVISITN 999", {
  is_unsch_avisit <- adlb$AVISIT == "UNSCHEDULED"
  is_unsch_visit  <- grepl("UNSCHEDULED", adlb$VISIT, fixed = TRUE)
  expect_equal(sum(xor(is_unsch_avisit, is_unsch_visit)), 0L)

  expect_true(all(adlb$AVISITN[is_unsch_avisit] == 999))
  # 999 is reserved for unscheduled: no scheduled visit may borrow it.
  expect_true(all(adlb$AVISITN[!is_unsch_avisit] != 999))
  # And scheduled visits keep the collected VISITNUM as AVISITN.
  expect_true(all(adlb$AVISITN[!is_unsch_avisit] == adlb$VISITNUM[!is_unsch_avisit]))
})

# ---------------------------------------------------------------------------
# 10. ABLFL vs SDTM's LBBLFL -- they DISAGREE, and that is correct
# ---------------------------------------------------------------------------
# This is the most interesting test in Part A, because a naive reviewer would
# call the disagreement a bug.
#
# LBBLFL and ABLFL answer different questions:
#   LBBLFL (SDTM) -- "which collected record did the data-management process
#                     designate as the baseline record?" In this study it is
#                     set on the SCREENING 1 record, i.e. it follows the VISIT
#                     LABEL.
#   ABLFL  (ADaM) -- "which record does THIS analysis use as baseline?" The SAP
#                     here says: the last non-missing value ON OR BEFORE the
#                     date of first dose. It follows the DOSING DATE.
#
# Those two definitions coincide only when nothing is collected between the
# screening visit and first dose. In this study things ARE collected in between
# (unscheduled visits 1.1/1.2/1.3), so the two flags part company. Verified
# breakdown of the 227 disagreeing records:
#   * 106 records are flagged by SDTM only. Every one of them is a SCREENING 1
#     record, every one is on or before TRTSDT, and in every case the ADaM flag
#     sits on a STRICTLY LATER pre-dose record. So ADaM did not miss them -- it
#     deliberately preferred a value closer to dosing.
#   * 121 records are flagged by ADaM only: 120 at unscheduled visits and 1 at
#     BASELINE. Of these, 15 belong to subject/parameter groups where SDTM set
#     no LBBLFL at all.
#   * 85 of the 1255 groups where both flags exist would give a numerically
#     DIFFERENT baseline value. This is not cosmetic: it moves CHG and PCHG.
#
# The test asserts the exact counts so that a future change to the ABLFL rule
# cannot pass unnoticed, and asserts the STRUCTURE of the disagreement (SDTM-only
# records are always pre-dose and always earlier than the ADaM pick), which is
# the part that proves the divergence is the SAP rule working rather than a bug.
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

  # -- the disagreement is systematic, not random ---------------------------
  # Every SDTM-only record is itself eligible under the SAP (pre-dose, non-
  # missing) and was passed over only because a LATER eligible record exists.
  # If this fails, the ADaM rule really is dropping records it should have kept.
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

  # The consequence: 85 groups would report a different BASE under the two
  # definitions. Asserted so nobody can claim the difference is immaterial.
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
# Everything below runs on hand-built data whose correct answer is obvious by
# eye. That is the point: the assertions are derived from the SAP sentence, not
# from the output of the program.
#
# The helper under test is the SAME function programs/02_adlb.R calls,
# sourced from R/derive_ablfl.R. An earlier version of this file carried a
# hand-copied version of the two admiral calls; that copy could drift from
# the program without any test noticing, so it was lifted into one file.
source(proj_file("R/derive_ablfl.R"))

# First dose is 2013-01-10 for every subject in the fixture, so "pre-dose" can
# be read straight off the dates below.
TRT_START <- as.Date("2013-01-10")

# One tibble, five subjects, each one isolating a single edge case. Building
# them in one frame rather than five is deliberate: it also proves the by_vars
# grouping keeps subjects from contaminating each other.
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
  # (e) never dosed (TRTSDT NA), e.g. a screen failure with labs.
  #     ADT <= NA evaluates to NA, and the filter must treat that as "does not
  #     qualify" rather than as TRUE or as an error.
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

# Helper: pull the single row (or zero rows) flagged for one subject.
flagged <- function(out, subject) {
  out %>% filter(USUBJID == subject, !is.na(ABLFL), ABLFL == "Y")
}
subj <- function(out, subject) out %>% filter(USUBJID == subject) %>% arrange(LBSEQ)

test_that("the derivation preserves every input row", {
  # restrict_derivation()'s contract, and the reason it exists instead of
  # filter |> mutate |> bind_rows. Note it preserves the SET of rows but NOT
  # their ORDER -- the rows matching the filter come back first. 02_adlb.R
  # hides that behind a final arrange(); these tests sort explicitly rather
  # than relying on positional alignment.
  expect_equal(nrow(mini_out), nrow(mini))
  expect_setequal(mini_out$LBSEQ[mini_out$USUBJID == "TWO-PRE"], c(1L, 2L, 3L))
})

test_that("(a) with two pre-dose records the LATER one is baseline", {
  fl <- flagged(mini_out, "TWO-PRE")
  expect_equal(nrow(fl), 1L)
  expect_equal(fl$LBSEQ, 2L)                  # 2013-01-05, not 2013-01-01
  expect_equal(fl$ADT, as.Date("2013-01-05"))
  # ...and BASE is broadcast to all three rows, including the post-dose one.
  expect_equal(subj(mini_out, "TWO-PRE")$BASE, c(20, 20, 20))
})

test_that("(b) a tie on ADT is broken by LBSEQ, deterministically", {
  fl <- flagged(mini_out, "TIE")
  expect_equal(nrow(fl), 1L)
  # mode = "last" with order = exprs(ADT, LBSEQ) means the HIGHEST LBSEQ within
  # the latest date. Stating the direction explicitly is the whole point: "the
  # tie is broken somehow" is not a specification.
  expect_equal(fl$LBSEQ, 2L)
  expect_equal(subj(mini_out, "TIE")$BASE, c(22, 22, 22))

  # -- determinism, proved exhaustively rather than sampled -----------------
  # The property being asserted is: the answer does not depend on the order the
  # rows arrive in. The honest way to assert that on a 3-row fixture is to try
  # ALL 3! = 6 input orders, not one shuffle.
  #
  # This matters -- it is not belt-and-braces. Re-running these tests with LBSEQ
  # removed from `order` gives flagged LBSEQ = 2,2,1,1,2,1 across the six
  # permutations: the derivation silently returns a DIFFERENT baseline, and
  # therefore a different BASE, CHG and PCHG, depending on input row order.
  # A single-shuffle test hits a passing permutation half the time, so it would
  # let that bug through. An interviewer asking "how do you know your tie-break
  # works?" is asking for exactly this.
  tie_rows <- mini %>% filter(USUBJID == "TIE")
  perms <- list(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
  picked <- vapply(perms, function(p) derive_ablfl_base(tie_rows[p, ]) %>%
                     filter(!is.na(ABLFL), ABLFL == "Y") %>% pull(LBSEQ), integer(1))
  expect_equal(picked, rep(2L, 6L))

  # -- admiral's own guard --------------------------------------------------
  # derive_var_extreme_flag() defaults to check_type = "warning" and warns
  # "Dataset contains duplicate records with respect to ..." whenever `order`
  # does not uniquely order the records within by_vars. So an incomplete `order`
  # is not merely silently wrong -- admiral tells you. Asserting the clean run
  # is warning-free turns that warning into a test failure instead of a line of
  # console output nobody reads. (Verified: removing LBSEQ from `order` makes
  # this expectation fail.)
  expect_no_warning(derive_ablfl_base(tie_rows))
})

test_that("the tie-break rule is not exercised by the real study data at all", {
  # The justification for Part B, stated as an assertion rather than as a claim.
  # There are ZERO (USUBJID, PARAMCD, ADT) ties among the baseline-eligible
  # records in this study, so every Part A test above would pass unchanged even
  # if the tie-break were wrong or absent. The edge cases that break a baseline
  # derivation in a real study are simply not present in this one -- which is
  # why they have to be constructed.
  #
  # This expectation is descriptive, not normative: if a future data refresh
  # introduced ties it would fail, and the correct response would be to update
  # the comment, not to "fix" the data.
  ties <- adlb %>%
    filter(!is.na(AVAL), !is.na(TRTSDT), ADT <= TRTSDT) %>%
    count(USUBJID, PARAMCD, ADT) %>%
    filter(n > 1)
  expect_equal(nrow(ties), 0L, info = offenders(ties))
})

test_that("(c) a pre-dose record with AVAL NA is skipped for an earlier non-missing one", {
  fl <- flagged(mini_out, "NA-PRE")
  expect_equal(nrow(fl), 1L)
  # The LATEST pre-dose record is LBSEQ 2 (2013-01-06) but its AVAL is missing.
  # The baseline rule adopted in 02_adlb.R (there is no SAP for pilot data; this
  # is the repo's own choice) is "last NON-MISSING value on or before first dose", so the flag
  # falls back to LBSEQ 1. This is the case that distinguishes a correct
  # implementation from `slice_max(ADT)`, which would flag the missing record
  # and produce BASE = NA for the whole subject.
  expect_equal(fl$LBSEQ, 1L)
  expect_equal(fl$AVAL, 40)
  # BASE is 40 on every row, including the row whose own AVAL is missing.
  expect_equal(subj(mini_out, "NA-PRE")$BASE, c(40, 40, 40))
})

test_that("(d) a subject with no pre-dose record gets no ABLFL and BASE stays NA", {
  fl <- flagged(mini_out, "NO-PRE")
  expect_equal(nrow(fl), 0L)
  rows <- subj(mini_out, "NO-PRE")
  expect_true(all(is.na(rows$ABLFL)))
  # NA, not 0. A silent 0 here would make CHG == AVAL and quietly corrupt every
  # change-from-baseline summary for this subject. Missing must stay missing.
  expect_true(all(is.na(rows$BASE)))
  # The subject's rows survive: they are still in the dataset, just unflagged.
  expect_equal(nrow(rows), 2L)
})

test_that("(e) a subject with no first-dose date gets no baseline", {
  # ADT <= NA is NA. The filter must treat NA as "does not qualify"; if it
  # treated NA as TRUE, an untreated subject would acquire a baseline and a
  # change-from-baseline, which is meaningless.
  fl <- flagged(mini_out, "NO-TRTSDT")
  expect_equal(nrow(fl), 0L)
  expect_true(all(is.na(subj(mini_out, "NO-TRTSDT")$BASE)))
})

test_that("(f) the boundary is ON OR BEFORE first dose, not strictly before", {
  # The adopted rule says "on or before the date of first dose", so ADT == TRTSDT
  # qualifies: a lab drawn on dosing day is a baseline. This single character
  # (<= vs <) is a real decision, not a typo-level detail -- with `<` this
  # subject's baseline moves from 120 to 110 and every CHG for them shifts.
  #
  # Without this case NOTHING in this file distinguishes <= from <: the real
  # study data is checked for ADT <= TRTSDT on flagged records, which a `<`
  # implementation also satisfies. Verified by mutation: changing the filter to
  # `<` fails only this test.
  fl <- flagged(mini_out, "ON-DOSE")
  expect_equal(nrow(fl), 1L)
  expect_equal(fl$LBSEQ, 2L)
  expect_equal(fl$ADT, TRT_START)
  expect_equal(subj(mini_out, "ON-DOSE")$BASE, c(120, 120))

  # CAVEAT worth saying out loud in an interview -- stated precisely, because
  # the obvious version of it is wrong for this study. ADT and TRTSDT are both
  # class Date, so a sample drawn at 16:00 on dosing day, hours AFTER the dose,
  # is indistinguishable from one drawn at 08:00 before it, and both are
  # accepted as baseline.
  #
  # The reason is NOT that lab times are missing. LB collects them: LBDTC is a
  # full "YYYY-MM-DDThh:mm" on 9045 of the 9079 records carried into ADLB, and
  # 02_adlb.R deliberately keeps only the date part via derive_vars_dt(). The
  # binding constraint is the DOSE time: EXSTDTC is date-only on all 591 EX
  # records, so TRTSDTM cannot be derived at all. With no dose time to compare
  # against, ADTM <= TRTSDTM is impossible however precise the lab timestamps
  # are. Never say "this dataset has no lab times" in the interview -- the
  # programmer across the table can open LB and see that it does.
  #
  # Exposure here is real but small: exactly 1 of the 1270 flagged baseline
  # records is dated on TRTSDT itself (1 of 1398 eligible pre-dose records).
  expect_s3_class(adlb$ADT, "Date")      # the shipped data, not just the fixture
  expect_s3_class(adlb$TRTSDT, "Date")
  expect_s3_class(mini$ADT, "Date")
  expect_s3_class(mini$TRTSDT, "Date")
})

test_that("grouping is by PARAMCD as well as subject", {
  # One subject, two parameters, with the eligible records deliberately
  # interleaved in time. If PARAMCD were dropped from by_vars the derivation
  # would flag one record for the subject overall instead of one per parameter,
  # and BASE would leak across analytes -- a mistake that is invisible in a
  # listing sorted by subject.
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
