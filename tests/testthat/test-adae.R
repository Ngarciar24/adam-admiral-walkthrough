# ---------------------------------------------------------------------------
# test-adae.R -- checks on data/adam/adae.rds built by programs/03_adae.R
#
# These are not unit tests of admiral (admiral tests itself). They are DATASET
# tests: they assert the properties the SAP and the ADaM IG require of the
# finished dataset, which is what a validation programmer would independently
# re-derive. Each one is written so that a failure names the records at fault
# rather than just saying FALSE.
#
# Run from the project root:
#   Rscript -e 'testthat::test_file("tests/testthat/test-adae.R")'
# ---------------------------------------------------------------------------

library(testthat)
library(dplyr)

# testthat may run this with the working directory set to the test file's own
# folder or to the project root, so resolve the data location rather than
# assuming one of them.
root <- if (file.exists("data/adam/adae.rds")) "." else "../.."
adae <- readRDS(file.path(root, "data/adam/adae.rds"))
adsl <- readRDS(file.path(root, "data/adam/adsl.rds"))

# ===========================================================================
# Structure: OCCDS, one row per collected adverse event
# ===========================================================================
test_that("ADAE is one row per source AE record", {
  # 1191 is the AE domain row count. ADAE neither drops nor duplicates events:
  # non-emergent events are FLAGGED (TRTEMFL) and kept, never filtered out, so
  # that a reviewer can see what was excluded from a table and why.
  expect_equal(nrow(adae), 1191L)
  expect_equal(anyDuplicated(adae[c("USUBJID", "AESEQ")]), 0L)
  # ASEQ is the ADaM within-subject key: USUBJID + ASEQ must identify one row.
  expect_equal(anyDuplicated(adae[c("USUBJID", "ASEQ")]), 0L)
})

test_that("ADAE is not BDS", {
  # Guards the structural claim made at the top of 03_adae.R. If someone later
  # adds AVAL or PARAMCD to this dataset they have misunderstood the structure,
  # and this fails loudly instead of the mistake surviving into a table.
  expect_false(any(c("AVAL", "AVALC", "PARAMCD", "PARAM", "BASE", "CHG") %in%
                     names(adae)))
})

test_that("every ADAE subject exists in ADSL", {
  # ADSL is the population of record. A USUBJID in ADAE that is absent from ADSL
  # means the AE cannot be attributed to a treatment arm and the safety table
  # denominator is wrong.
  orphans <- setdiff(adae$USUBJID, adsl$USUBJID)
  expect_equal(orphans, character(0))
  # 225 of the 306 ADSL subjects reported at least one AE.
  expect_equal(n_distinct(adae$USUBJID), 225L)
})

# ===========================================================================
# Dates
# ===========================================================================
test_that("ASTDT is never after AENDT", {
  # An event cannot end before it starts. This is the check that catches a bad
  # imputation rule: imputing a partial start date forward (to the last of the
  # month, or up to first dose via min_dates) can push ASTDT past a collected
  # AENDT and create a negative duration. It passes here, including for the
  # four records that have both an imputed start and a collected end date.
  bad <- adae %>% filter(!is.na(ASTDT), !is.na(AENDT), ASTDT > AENDT)
  expect_equal(nrow(bad), 0L)
})

test_that("ASTDTF marks exactly the records whose AESTDTC was partial", {
  # The imputation flag must be neither over- nor under-populated: a flag on a
  # complete date would be a false alarm, and a missing flag on an imputed date
  # would hide manufactured data.
  expect_equal(sum(!is.na(adae$ASTDTF)), 26L)            # 11 year-only + 15 year-month
  expect_setequal(na.omit(adae$ASTDTF), c("D", "M"))
  expect_true(all(nchar(adae$AESTDTC[!is.na(adae$ASTDTF)]) < 10))
  expect_true(all(nchar(adae$AESTDTC[is.na(adae$ASTDTF)]) == 10))
})

test_that("AENDT is populated exactly when AEENDTC was collected", {
  # No end date is imputed (see section 2 of 03_adae.R): AEENDTC is either a
  # complete date or entirely missing, and the 473 missing ones stay missing
  # rather than being invented for ongoing events.
  expect_equal(sum(is.na(adae$AENDT)), 473L)
  expect_true(all(is.na(adae$AENDT) == is.na(adae$AEENDTC)))
})

test_that("study days follow the no-day-zero convention", {
  # Day of first dose is Day 1 and the day before is Day -1; a zero would mean
  # the +1 offset was dropped on one side of the comparison.
  expect_false(any(adae$ASTDY == 0, na.rm = TRUE))
  expect_false(any(adae$AENDY == 0, na.rm = TRUE))
  # ADY is the record's analysis day, which for an occurrence record is onset.
  expect_equal(adae$ADY, adae$ASTDY)
  # Recompute ASTDY independently of admiral.
  manual <- with(adae, ifelse(ASTDT >= TRTSDT,
                              as.integer(ASTDT - TRTSDT) + 1L,
                              as.integer(ASTDT - TRTSDT)))
  expect_equal(adae$ASTDY, manual)
})

test_that("ADURN equals AENDT - ASTDT + 1 and carries its unit", {
  d <- adae %>% filter(!is.na(ADURN))
  expect_equal(d$ADURN, as.numeric(d$AENDT - d$ASTDT) + 1)
  expect_true(all(d$ADURU == "DAYS"))
  # The unit variable must be populated when and only when the value is.
  expect_true(all(is.na(adae$ADURU) == is.na(adae$ADURN)))
  expect_equal(sum(is.na(adae$ADURN)), 473L)  # ongoing events, not zero-duration ones
})

# ===========================================================================
# TRTEMFL
# ===========================================================================
test_that("TRTEMFL takes only 'Y' or NA", {
  # DOCUMENTED CHOICE: admiral's derive_var_trtemfl() returns "Y" or NA, with no
  # "N" branch, and 03_adae.R deliberately does NOT recode NA to "N" -- NA means
  # "not treatment-emergent, or not determinable because TRTSDT is unknown".
  # Consequence for downstream code: filter on TRTEMFL == "Y", never != "N".
  expect_setequal(unique(adae$TRTEMFL), c("Y", NA))
  expect_equal(sum(adae$TRTEMFL == "Y", na.rm = TRUE), 1122L)
})

test_that("no event with a COLLECTED onset before first dose is treatment-emergent", {
  # The core of the definition. Restricted to records where ASTDT was actually
  # collected (ASTDTF is NA), because an imputed date that lands before TRTSDT
  # is an artefact of the imputation rule, not evidence about the real onset --
  # judging the rule by those records would be judging it by its own output.
  bad <- adae %>% filter(TRTEMFL == "Y", is.na(ASTDTF), ASTDT < TRTSDT)
  expect_equal(nrow(bad), 0L)
})

test_that("the non-emergent records are non-emergent for a stated reason", {
  # 69 records are not flagged, and they must decompose exactly into the two
  # rules that were written down: onset before first dose, or onset more than
  # end_window = 30 days after last dose. Any record failing to fall in one of
  # those groups would mean the flag is being driven by something unintended.
  nonte <- adae %>% filter(is.na(TRTEMFL))
  expect_equal(nrow(nonte), 69L)
  expect_true(all(nonte$ASTDT < nonte$TRTSDT | nonte$ASTDT > nonte$TRTEDT + 30))
  expect_equal(sum(nonte$ASTDT < nonte$TRTSDT), 65L)
  expect_equal(sum(nonte$ASTDT > nonte$TRTEDT + 30), 4L)
})

test_that("every treatment-emergent event belongs to a safety-population subject", {
  # A treatment-emergent event implies the subject was dosed, which is the
  # definition of SAFFL == "Y" in 01_adsl.R. If this failed, either the safety
  # flag or the emergence rule is wrong, and the AE tables would report events
  # for subjects who are not in their own denominator.
  te <- adae %>% filter(TRTEMFL == "Y")
  expect_true(all(te$SAFFL == "Y"))
  expect_equal(n_distinct(te$USUBJID), 217L)
})

# ===========================================================================
# Occurrence flags
# ===========================================================================
test_that("occurrence flags mark one record per counting unit", {
  # These are what make a subject-level incidence table a simple row count.
  te <- adae %>% filter(TRTEMFL == "Y")
  expect_equal(sum(adae$AOCCFL  == "Y", na.rm = TRUE), n_distinct(te$USUBJID))
  expect_equal(sum(adae$AOCCSFL == "Y", na.rm = TRUE),
               nrow(distinct(te, USUBJID, AEBODSYS)))
  expect_equal(sum(adae$AOCCPFL == "Y", na.rm = TRUE),
               nrow(distinct(te, USUBJID, AEBODSYS, AEDECOD)))
})

test_that("occurrence flags never land on a non-emergent record", {
  # restrict_derivation(filter = TRTEMFL == "Y") is what guarantees this: the
  # flags must be NA everywhere outside the treatment-emergent subset, so a
  # table filtered on AOCCFL == "Y" alone still counts only emergent events.
  nonte <- adae %>% filter(is.na(TRTEMFL))
  expect_true(all(is.na(nonte$AOCCFL) & is.na(nonte$AOCCSFL) & is.na(nonte$AOCCPFL)))
  # The flagged record is the earliest emergent one in its group, so AOCCFL == "Y"
  # must sit on the subject's minimum emergent ASTDT.
  chk <- adae %>%
    filter(TRTEMFL == "Y") %>%
    group_by(USUBJID) %>%
    summarise(ok = ASTDT[which(AOCCFL == "Y")] == min(ASTDT), .groups = "drop")
  expect_true(all(chk$ok))
})

# ===========================================================================
# Traceability
# ===========================================================================
test_that("the SDTM source variables survive into ADAE", {
  # Origin "Predecessor" in define.xml only means something if the predecessor
  # is actually there to compare against.
  expect_true(all(c("AESEQ", "AESTDTC", "AEENDTC", "AESTDY", "AEENDY",
                    "AESEV", "AEREL", "AEOUT", "AETERM", "AEDECOD",
                    "AEBODSYS") %in% names(adae)))
  # ASEV / AREL are analysis copies; on this study nothing is recoded, so they
  # must be identical to their source. This test is what would start failing --
  # correctly -- the day a SAP introduces a recode.
  expect_equal(adae$ASEV, adae$AESEV)
  expect_equal(adae$AREL, adae$AEREL)
  expect_equal(adae$ASEVN,
               as.integer(factor(adae$AESEV,
                                 levels = c("MILD", "MODERATE", "SEVERE"))))
})

test_that("ASTDY is recomputed rather than copied from SDTM AESTDY", {
  # Deliberately asserts a DISAGREEMENT, the same way test-adlb.R records the
  # ABLFL vs LBBLFL difference. ADaM anchors study day on TRTSDT (first dose,
  # from EX) while SDTM anchors on RFSTDTC. On the 1165 records with a complete
  # start date they agree on all but ONE: subject 01-716-1063 AESEQ 1, where the
  # AE started on the reference start date itself so ASTDY is 1, and SDTM says
  # 366. That is an error in the public pilot data which the ADaM derivation
  # exposes. If this test ever reports 0 mismatches, pharmaversesdtm has fixed
  # its data and the comment in 03_adae.R must be updated.
  cmp <- adae %>% filter(is.na(ASTDTF), !is.na(AESTDY))
  expect_equal(nrow(cmp), 1165L)
  expect_equal(sum(cmp$ASTDY != cmp$AESTDY), 1L)
  expect_equal(cmp$USUBJID[cmp$ASTDY != cmp$AESTDY], "01-716-1063")
})
