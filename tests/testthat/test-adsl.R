# ---------------------------------------------------------------------------
# test-adsl.R -- invariant tests for data/adam/adsl.rds
#
# Run with:  testthat::test_dir("tests/testthat")   (from the project root)
#
# WHAT THIS FILE IS FOR
# ---------------------
# These are not unit tests of admiral. admiral has its own test suite. These are
# DATASET tests: they assert properties that must be true of ADSL once it is
# built, independently of HOW it was built. That distinction matters, and it is
# the first thing a statistical programmer will probe:
#
#   A test that re-runs the derivation and compares it to itself tests nothing.
#
# Two different kinds of assertion are mixed in here, and it is worth being able
# to say which is which out loud rather than being asked:
#
#   * STRUCTURAL INVARIANTS -- one row per subject, TRTSDT <= TRTEDT, a
#     population flag is never NA. These follow from the ADaM IG and from what
#     the variables mean, and would have to hold in any study.
#   * PINNED COUNTS -- nrow == 306, 52 screen failures. These are properties of
#     THIS extract, not requirements of anything. They are regression guards:
#     they catch a silently changed input or a filter that quietly started
#     dropping rows. There is no protocol or SAP behind them -- the pilot data
#     ships with neither -- so the numbers were read off the delivered dataset.
#     Do not describe them as spec compliance; they are characterisation tests.
#
# So wherever a derivation can be re-expressed a second, simpler way (SAFFL is
# the clearest case), this file derives the expected answer FROM THE SDTM SOURCE
# with plain dplyr and compares. Where that is not possible, it asserts a
# structural property that would have to hold under any correct implementation.
#
# In a real study this role is filled by (a) double programming -- a second
# programmer independently writes the dataset from the spec and the two are
# compared with PROC COMPARE -- and (b) automated CDISC conformance checking
# against define.xml (Pinnacle 21). This file is a small stand-in for (a) only.
# Nothing here checks variable labels, lengths, types, controlled-terminology
# codelists or any define.xml conformance rule, so it does not replace (b).
# ---------------------------------------------------------------------------

library(testthat)
library(dplyr)
library(stringr)

# --- Inputs ---------------------------------------------------------------
# test_path() is testthat's path helper: under test_dir() the working directory
# is tests/testthat, but when the file is source()d interactively from the
# project root it is not. test_path() resolves both to the same file, so the
# tests behave identically in the console and in CI.
adsl <- readRDS(testthat::test_path("../../data/adam/adsl.rds"))
agegr1_spec <- readr::read_csv(
  testthat::test_path("../../metadata/adsl_agegr1.csv"),
  show_col_types = FALSE
)

# The SDTM source is re-read here, NOT taken from 00_setup.R's environment and
# NOT taken from the ADSL being tested. The point is to compare the delivered
# dataset against the raw input, so the input has to come in by its own door.
# convert_blanks_to_na() is repeated for the same reason: the test must make the
# same "" -> NA assumption the production program makes, or a blank ARMCD would
# be compared against an NA ARMCD and the test would fail for the wrong reason.
dm_src <- admiral::convert_blanks_to_na(pharmaversesdtm::dm)
ex_src <- admiral::convert_blanks_to_na(pharmaversesdtm::ex)


# ===========================================================================
# 1. Structure
# ===========================================================================
# IF THIS FAILS IN THE REAL WORLD: ADSL is the subject denominator for every
# table in the submission and the merge source for every other ADaM dataset. A
# duplicated USUBJID double-counts that subject in every "N" in the study, and
# fans out the row count of ADAE/ADLB when they merge treatment variables on
# (dplyr::left_join() would do this silently; derive_vars_merged() would abort,
# which is the safer failure but still a broken deliverable). A DM subject
# MISSING from ADSL disappears from the disposition and demographic tables
# entirely -- a screen failure that is never reported is a protocol deviation
# nobody can see.
test_that("ADSL has exactly one row per subject and covers every DM subject", {
  expect_equal(nrow(adsl), 306)
  expect_equal(nrow(adsl), nrow(dm_src))

  # One row per subject = USUBJID is a unique key. Both halves are asserted:
  # uniqueness alone would pass on a dataset that lost subjects.
  expect_equal(dplyr::n_distinct(adsl$USUBJID), nrow(adsl))
  expect_false(any(is.na(adsl$USUBJID)))

  # Set equality in BOTH directions. setdiff() is not symmetric, and testing one
  # direction only is a classic way to miss an invented subject.
  expect_equal(setdiff(dm_src$USUBJID, adsl$USUBJID), character(0))
  expect_equal(setdiff(adsl$USUBJID, dm_src$USUBJID), character(0))
})


# ===========================================================================
# 2. Treatment start is not after treatment end
# ===========================================================================
# IF THIS FAILS IN THE REAL WORLD: TRTSDT/TRTEDT bound the treatment-emergent
# window. An inverted interval makes derive_var_trtemfl() classify AEs against a
# window that cannot contain anything, so treatment-emergent AE counts silently
# collapse; exposure-adjusted incidence rates get a negative person-time
# denominator. It is also the signature of a specific real bug: taking "first
# dose" and "last dose" with the wrong mode = "first"/"last" pairing, which is
# easy to typo and produces a dataset that otherwise looks completely normal.
test_that("TRTSDT is never after TRTEDT where both are present", {
  both <- adsl %>% filter(!is.na(TRTSDT), !is.na(TRTEDT))

  # Guard the guard: if a future edit dropped TRTEDT entirely, `both` would be
  # empty and all(logical(0)) is TRUE -- the test would pass vacuously.
  expect_gt(nrow(both), 0)
  expect_true(all(both$TRTSDT <= both$TRTEDT))
})


# ===========================================================================
# 3. TRTDURD uses the inclusive-day (+1) convention
# ===========================================================================
# IF THIS FAILS IN THE REAL WORLD: total exposure is a required safety summary
# and is compared across arms. An off-by-one is systematic, not random: it
# shifts every subject's exposure by the same day, so it does not cancel out and
# it survives every mean/median. The +1 is the same no-day-zero convention that
# makes the day of first dose Day 1 in ADY -- if TRTDURD and ADY disagree about
# whether day zero exists, a "days on treatment" listing and a study-day listing
# for the same subject will not line up, and a reviewer WILL notice.
test_that("TRTDURD equals TRTEDT - TRTSDT + 1, and is NA when either date is", {
  both <- adsl %>% filter(!is.na(TRTSDT), !is.na(TRTEDT))
  expect_gt(nrow(both), 0)

  # as.numeric() on a difftime of two Dates gives whole days; the subtraction is
  # done here rather than trusting derive_var_trtdurd()'s own arithmetic.
  expect_equal(
    as.numeric(both$TRTDURD),
    as.numeric(both$TRTEDT - both$TRTSDT) + 1
  )

  # The other side of the convention: a duration must not be invented for a
  # subject with an unknown start or end. This is the bit that a naive
  # `TRTEDT - TRTSDT + 1` in dplyr gets right by accident (NA propagates) but a
  # coalesce()-happy implementation gets wrong on purpose.
  one_missing <- adsl %>% filter(is.na(TRTSDT) | is.na(TRTEDT))
  expect_true(all(is.na(one_missing$TRTDURD)))
})


# ===========================================================================
# 4. SAFFL matches an independently derived safety population
# ===========================================================================
# IF THIS FAILS IN THE REAL WORLD: this is the single most consequential flag in
# the dataset. SAFFL is the denominator of every adverse event, lab and vital
# signs table. Exclude a dosed subject and their AEs vanish from the summary
# while remaining in the listings -- an inconsistency that is caught late and
# expensively. Include an undosed subject and every AE rate is understated.
#
# The specific trap is the PLACEBO clause. The placebo arm is dosed at EXDOSE = 0
# by design. Write the flag as EXDOSE > 0 alone -- which is what "received at
# least one dose of study drug" sounds like in English -- and every placebo
# subject drops out of the safety population, leaving the comparator arm looking
# event-free. That is the reason the condition is written the way it is in
# 01_adsl.R, and the reason it is worth a test.
#
# The expected value is built here with plain dplyr straight off EX, without
# calling derive_var_merged_exist_flag(). Be precise about what that does and
# does not buy, because this is exactly where an interviewer will push:
#   * INDEPENDENTLY CHECKED -- the merge-and-flag machinery. That "any qualifying
#     record makes the subject Y", that the result is one value per subject, and
#     that the three value slots (true/false/missing) are set as intended.
#   * NOT INDEPENDENTLY CHECKED -- the dosing condition itself, which is copied
#     verbatim from 01_adsl.R. A test cannot ratify the clause it inherits. Only
#     a second programmer reading the SAP can, which is what double programming
#     is for. What this test does prove is that DELETING the PLACEBO clause from
#     01_adsl.R alone would be caught, because the two sides would then disagree.
#
# TOY DATA FLAG: in this extract the PLACEBO clause never actually discriminates.
# EXDOSE is 0 only on PLACEBO records and 54 or 81 only on XANOMELINE records, so
# every EX record qualifies and "SAFFL == Y" collapses to "the subject appears in
# EX at all". A real study separates the two the first time an active-arm record
# carries EXDOSE = 0 (dose held, or a visit where nothing was administered), and
# that is where the clause earns its keep. Do not claim this test exercises that
# case -- the data does not contain it.
test_that("SAFFL is Y exactly for subjects with a qualifying EX record", {
  dosed_usubjid <- ex_src %>%
    filter(EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) %>%
    pull(USUBJID) %>%
    unique()

  expected <- adsl %>%
    transmute(
      USUBJID,
      SAFFL,
      SAFFL_EXPECTED = if_else(USUBJID %in% dosed_usubjid, "Y", "N")
    )

  expect_equal(expected$SAFFL, expected$SAFFL_EXPECTED)

  # Assert the "if and only if" as two explicit, separately-reported halves, so
  # a failure says which direction broke rather than just "not equal".
  expect_equal(
    sort(expected$USUBJID[expected$SAFFL == "Y"]),
    sort(unique(dosed_usubjid))
  )
  expect_equal(
    length(intersect(expected$USUBJID[expected$SAFFL == "N"], dosed_usubjid)),
    0L
  )

  # Subjects with NO EX record at all must be N, not NA. This is the third arm
  # of derive_var_merged_exist_flag(): true_value / false_value / missing_value.
  # 01_adsl.R deliberately collapses "in EX but never dosed" and "never in EX"
  # into a single N; if missing_value were left at its NA default the flag would
  # be three-valued and filter(SAFFL == "Y") would still work, so the bug would
  # hide until someone counted the N arm.
  never_in_ex <- setdiff(adsl$USUBJID, ex_src$USUBJID)
  expect_gt(length(never_in_ex), 0)
  expect_true(all(adsl$SAFFL[adsl$USUBJID %in% never_in_ex] == "N"))
})


# ===========================================================================
# 5. Population flags are two-valued, never missing
# ===========================================================================
# IF THIS FAILS IN THE REAL WORLD: NA is the dangerous value here, not a wrong
# letter. filter(SAFFL == "Y") evaluates NA == "Y" to NA, and dplyr::filter()
# drops NA rows without warning. So a population flag that is accidentally NA
# removes subjects from the analysis population silently, and the table simply
# reports a smaller N with no error anywhere in the log. ADaM controlled
# terminology for a population flag is "Y"/"N"; anything else is also a
# define.xml conformance finding.
test_that("SAFFL and ITTFL contain only Y or N, with no missing values", {
  expect_false(any(is.na(adsl$SAFFL)))
  expect_false(any(is.na(adsl$ITTFL)))
  expect_true(all(adsl$SAFFL %in% c("Y", "N")))
  expect_true(all(adsl$ITTFL %in% c("Y", "N")))

  # Both values must actually occur. A flag that is constant "Y" would satisfy
  # every assertion above while meaning the population logic never fired.
  expect_setequal(unique(adsl$SAFFL), c("Y", "N"))
  expect_setequal(unique(adsl$ITTFL), c("Y", "N"))
})


# ===========================================================================
# 6. Screen failures are excluded from everything
# ===========================================================================
# IF THIS FAILS IN THE REAL WORLD: a screen failure never entered the study. If
# one carries a randomisation date, the CONSORT diagram reports more randomised
# subjects than were randomised. If one carries SAFFL = "Y" or a treatment start
# date, a person who was never dosed sits in the safety denominator. Both are
# the kind of finding that gets a submission questioned, because they mean the
# study population itself is described wrongly.
#
# ARMCD == "Scrnfail" is the PLANNED arm code, so this keys off randomisation
# intent, which is the correct axis for "did this subject enter the study".
test_that("screen failures carry no population flags, treatment start or randomisation date", {
  sf <- adsl %>% filter(ARMCD == "Scrnfail")

  expect_equal(nrow(sf), 52)
  expect_true(all(sf$SAFFL == "N"))
  expect_true(all(sf$ITTFL == "N"))
  expect_true(all(is.na(sf$TRTSDT)))
  expect_true(all(is.na(sf$RANDDT)))

  # And the converse, which is the half that actually constrains ITTFL: every
  # non-screen-failure IS in the ITT population. Without this, an ITTFL that was
  # hard-coded to "N" for everyone would pass the assertions above.
  non_sf <- adsl %>% filter(ARMCD != "Scrnfail")
  expect_true(all(non_sf$ITTFL == "Y"))
  expect_true(all(!is.na(non_sf$RANDDT)))
})


# ===========================================================================
# 7. Every treated subject has an actual treatment
# ===========================================================================
# IF THIS FAILS IN THE REAL WORLD: safety is summarised BY ACTUAL TREATMENT
# (TRT01A), not by planned treatment. A subject who is in the safety population
# but has no TRT01A cannot be put in a column: group_by() gives them an <NA>
# column that nobody expects, or the table program filters them out and the arm
# Ns stop adding up to the population N. In this study TRT01P and TRT01A differ
# for 12 subjects -- planned high dose, actually low dose -- so the distinction
# is live here, not hypothetical.
test_that("TRT01A is populated for every subject in the safety population", {
  treated <- adsl %>% filter(SAFFL == "Y")

  expect_gt(nrow(treated), 0)
  expect_false(any(is.na(treated$TRT01A)))
  expect_false(any(treated$TRT01A == ""))

  # The arm Ns reconstitute the population N -- the arithmetic a reviewer does by
  # eye on the first page of the safety tables. Honest caveat, because it would
  # be embarrassing to be caught overselling it: given the no-NA assertion above,
  # this one CANNOT fail on its own. table() drops NA, so once TRT01A has no NAs
  # the arm counts necessarily sum to the row count. It is kept because it states
  # the property being relied on, not because it adds independent coverage.
  expect_equal(sum(table(treated$TRT01A)), nrow(treated))
})


# ===========================================================================
# 8. AGEGR1 / AGEGR1N agree with each other and with the metadata spec
# ===========================================================================
# IF THIS FAILS IN THE REAL WORLD: the paired --GR1 / --GR1N convention exists
# because tables sort on the numeric and print the label. If the pairing is not
# 1:1, a demographic table shows the wrong label against a row of counts, and
# because both columns are individually plausible nothing looks broken. The
# second half matters more: metadata/adsl_agegr1.csv is the SPEC, and the spec
# is what becomes define.xml. Code that drifts from the spec means the submitted
# define.xml describes a dataset that was not produced -- a documentation defect
# that is found by audit rather than by any statistical check.
test_that("AGEGR1 and AGEGR1N pair 1:1 and follow the metadata cut points", {
  # -- the pairing is a bijection -----------------------------------------
  pairs <- adsl %>% distinct(AGEGR1N, AGEGR1)
  expect_equal(nrow(pairs), dplyr::n_distinct(adsl$AGEGR1N))
  expect_equal(nrow(pairs), dplyr::n_distinct(adsl$AGEGR1))
  expect_false(any(is.na(adsl$AGEGR1N)))
  expect_false(any(is.na(adsl$AGEGR1)))

  # -- the labels are the spec's labels, not lookalikes ---------------------
  # String equality, so "65-80" vs "65-80 " or an en-dash would be caught.
  # Note what else this quietly demands: every band in the spec must be POPULATED
  # in the data, or the two vectors differ in length. All three are here
  # (<65 = 42, 65-80 = 172, >80 = 92). On a study where a band happened to be
  # empty this would fail without anything being wrong -- worth knowing before
  # the file is reused, and not the same thing as a defect.
  expect_equal(
    pairs %>% arrange(AGEGR1N) %>% pull(AGEGR1),
    agegr1_spec %>% arrange(AGEGR1N) %>% pull(AGEGR1)
  )

  # -- every subject lands in exactly one spec band -------------------------
  # Computed from AGE_LOW/AGE_HIGH, NOT by re-running the case_when() in
  # 01_adsl.R. case_when() is ordered and falls through, so it cannot express
  # an overlap or a gap; the spec's closed intervals can. Counting the matching
  # bands first therefore tests the SPEC (are the bands disjoint and complete
  # over the observed ages?) before testing the data against it.
  n_bands <- vapply(
    adsl$AGE,
    function(a) sum(a >= agegr1_spec$AGE_LOW & a <= agegr1_spec$AGE_HIGH),
    integer(1)
  )
  expect_true(all(n_bands == 1))

  expected_agegr1n <- vapply(
    adsl$AGE,
    function(a) agegr1_spec$AGEGR1N[a >= agegr1_spec$AGE_LOW & a <= agegr1_spec$AGE_HIGH][1],
    numeric(1)
  )
  expect_equal(as.numeric(adsl$AGEGR1N), expected_agegr1n)
})


# ===========================================================================
# 9. EOSSTT domain, and the exact set of subjects allowed to be missing it
# ===========================================================================
# IF THIS FAILS IN THE REAL WORLD: the disposition table ("completed /
# discontinued / reason") is built by tabulating EOSSTT, so an unmapped DSDECOD
# leaking through creates a row nobody wrote a spec for, and a value wrongly set
# to NA drops a subject out of the numerator while leaving them in the
# denominator -- the percentages then do not sum to 100 and the cause is not
# obvious from the table.
#
# The NA is not a defect here, it is the design: screen failures have a DS
# record with DSDECOD == "SCREEN FAILURE" but never entered the study, so an
# end-of-study status is not defined for them. Asserting that the NAs are
# EXACTLY the screen failures is what separates "deliberately missing" from
# "the merge lost some subjects", which look identical in a frequency table.
test_that("EOSSTT is COMPLETED/DISCONTINUED and is missing exactly for screen failures", {
  expect_true(all(adsl$EOSSTT %in% c("COMPLETED", "DISCONTINUED") | is.na(adsl$EOSSTT)))
  expect_setequal(
    unique(adsl$EOSSTT[!is.na(adsl$EOSSTT)]),
    c("COMPLETED", "DISCONTINUED")
  )

  # Set equality of the missing subjects against the screen failures, not just a
  # count match. Equal counts with different subjects is a real failure mode of
  # a many-to-one merge and a count-only test would pass it.
  eosstt_missing <- sort(adsl$USUBJID[is.na(adsl$EOSSTT)])
  scrnfail <- sort(adsl$USUBJID[adsl$ARMCD == "Scrnfail"])

  expect_equal(length(eosstt_missing), 52)
  expect_equal(eosstt_missing, scrnfail)
})
