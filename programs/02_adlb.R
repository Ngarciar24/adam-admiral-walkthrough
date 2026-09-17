# ---------------------------------------------------------------------------
# 02_adlb.R -- ADLB, a Basic Data Structure (BDS) dataset
#
# Structure: one row per SUBJECT x PARAMETER x ANALYSIS TIMEPOINT.
#
# HONEST CAVEAT, because an interviewer will test exactly this: that statement is
# the BDS ideal, and this program does not fully achieve it. Collapsing every
# unscheduled visit into AVISIT = "UNSCHEDULED" (see section 3) means
# (USUBJID, PARAMCD, AVISIT) is NOT unique: 19 such keys cover 42 rows.
# The dataset IS uniquely keyed by (USUBJID, PARAMCD, ADT, LBSEQ) and by
# (USUBJID, ASEQ). A real study resolves this with SAP-defined analysis visit
# windows plus DTYPE to mark records derived when a window holds more than one
# result -- not by bucketing, which is done here only to keep the repo readable.
#
# BDS is the "tall" ADaM shape. Where SDTM LB has one row per lab result with
# the test identified by LBTESTCD, BDS identifies it by PARAMCD/PARAM and puts
# the result in a single generic column, AVAL. That is what makes it analysis-
# ready: one table can serve every lab endpoint, and a table program does not
# need to know which analyte it is summarising.
#
# Source SDTM: LB. Source ADaM: ADSL.
# ---------------------------------------------------------------------------

source("programs/00_setup.R")
source("R/derive_ablfl.R")

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))

# ===========================================================================
# 1. Restrict to the parameters in the spec
# ===========================================================================
# Only the 5 analytes in metadata/adlb_params.csv are carried forward. A real
# ADLB would carry every scheduled analyte; this is trimmed to stay readable.
#
# PARAMCD is NOT simply a copy of LBTESTCD. It happens to coincide here, but
# they are different things:
#   LBTESTCD -- SDTM controlled terminology for the test that was performed
#   PARAMCD  -- an ANALYSIS parameter defined by the SAP
# They diverge as soon as the analysis needs something the lab did not measure
# directly: a derived parameter (e.g. a ratio), the same analyte in two units as
# two separate parameters, or a fasting/non-fasting split. Mapping through an
# explicit lookup rather than assigning PARAMCD <- LBTESTCD keeps that honest.
adlb <- lb %>%
  inner_join(adlb_params, by = "LBTESTCD") %>%

  # -- Carry a SUBSET of ADSL ---------------------------------------------
  # Not all of ADSL. A BDS dataset takes the treatment variables, the population
  # flags it needs for subsetting, and the reference dates the derivations need
  # (TOY-DATA CAVEAT: SAFFL and ITTFL are constant "Y" across all 9079 ADLB rows,
  # because no screen-failure subject has a single LB record in this study. They
  # subset nothing here. In a real study screening labs on screen failures make
  # these flags load-bearing, and the ADT <= TRTSDT baseline rule has to cope
  # with TRTSDT being NA -- a path this dataset never reaches.)
  # (TRTSDT drives ADY and the baseline definition). Copying all 46 ADSL columns
  # onto 9,000 rows would bloat the dataset without adding information.
  derive_vars_merged(
    dataset_add = adsl,
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(TRT01P, TRT01A, TRTSDT, TRTEDT, SAFFL, ITTFL, AGEGR1, SEX)
  ) %>%

  # TRTP / TRTA are the BDS analysis treatment variables. TRT01P / TRT01A are
  # ADSL PERIOD-level variables; a BDS dataset is expected to carry TRTP/TRTA,
  # which is what a table program splits columns by. In a single-period study
  # they are a straight copy, and that is exactly why it is worth knowing they
  # are different variables: in a crossover or multi-period study TRTP varies by
  # record while TRT01P does not.
  mutate(
    TRTP = TRT01P,
    TRTA = TRT01A
  ) %>%

  # ===========================================================================
  # 2. Timing variables
  # ===========================================================================
  # LBDTC here is mostly a full datetime ("2013-01-15T09:30"); derive_vars_dt()
  # takes the date part. No imputation (highest_imputation defaults to "n"),
  # because LBDTC is complete in this study.
  derive_vars_dt(
    dtc             = LBDTC,
    new_vars_prefix = "A"
  ) %>%

  # ADY = study day relative to first dose. The convention has NO DAY ZERO:
  # the day of first dose is Day 1, and the day before it is Day -1. So
  # ADY = ADT - TRTSDT + 1 when ADT >= TRTSDT, and ADT - TRTSDT when before.
  # Screening labs therefore get negative ADY, which is correct and expected.
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars    = exprs(ADT)
  ) %>%

  # ===========================================================================
  # 3. The analysis value
  # ===========================================================================
  # AVAL takes the STANDARDISED result (LBSTRESN), never the original (LBORRES).
  # LBORRES is whatever the local lab reported in whatever unit it used;
  # LBSTRESN has been converted to a single standard unit per analyte by the
  # SDTM programmer. Analysing LBORRES across sites would mix units.
  # AVALC is a character copy of AVAL on 9074 of 9079 rows and adds information
  # on only 5: the BILI results reported as "<3.42", which are below the limit of
  # quantification and therefore have AVAL = NA but a meaningful AVALC. That is
  # the one case AVALC exists for. AVALU is strictly redundant here because each
  # PARAMCD has exactly one unit and the unit is already inside PARAM -- when the
  # same analyte arrives in two units the ADaM answer is TWO PARAMCDs, not one
  # PARAMCD plus AVALU. Both are kept so the redundancy is visible and arguable.
  mutate(
    AVAL  = LBSTRESN,
    AVALC = LBSTRESC,
    AVALU = LBSTRESU,

    # Analysis reference ranges. Named ANRLO/ANRHI rather than reused as
    # LBSTNRLO/LBSTNRHI because the SAP is allowed to define analysis ranges
    # that differ from the lab's own (e.g. protocol-specified toxicity limits).
    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI,

    # AVISIT / AVISITN are ANALYSIS visits, which are not required to equal the
    # collected VISIT. Here all unscheduled visits collapse into one category so
    # they do not each become a column in a by-visit table. A real study would
    # apply analysis visit WINDOWS instead: "any assessment between day 8 and
    # day 21 is analysed as Week 2", defined in the SAP.
    AVISIT = case_when(
      str_detect(VISIT, "UNSCHEDULED") ~ "UNSCHEDULED",
      TRUE                             ~ VISIT
    ),
    AVISITN = if_else(str_detect(VISIT, "UNSCHEDULED"), 999, VISITNUM)
  ) %>%

  # ===========================================================================
  # 4. Baseline flag (ABLFL)
  # ===========================================================================
  # SAP definition used here: the LAST non-missing value on or before the date
  # of first dose.
  #
  # restrict_derivation() is admiral's answer to "apply this derivation to only
  # some rows, but keep all rows". The dplyr instinct is
  #   filter(...) |> mutate(flag) |> bind_rows(the rest)
  # which silently drops rows if the filter is wrong and reorders the dataset.
  # restrict_derivation() runs the derivation on the matching rows only and
  # leaves every other row's VALUES untouched.
  #
  # Precise correction, because the sloppy version of this claim is wrong:
  # it does NOT preserve row ORDER. admiral 1.5.0 implements it as
  # filter -> derive -> bind_rows(), so the restricted rows come back first and
  # the rest follow. That is why this program ends with an explicit arrange().
  # Never rely on row order surviving an admiral call.
  #
  # mode = "last" with order = exprs(ADT, LBSEQ): LBSEQ is the tie-breaker, so
  # two results on the same date resolve deterministically instead of depending
  # on row order. Ties MUST be broken explicitly -- a baseline that changes when
  # the input is re-sorted is not reproducible.
  #
  # NOTE: SDTM already carries LBBLFL, its own baseline flag. ADaM derives ABLFL
  # independently, because SDTM's flag answers "which record did the lab
  # consider baseline" while ABLFL answers "which record does THIS analysis use
  # as baseline". They are compared in tests/testthat/test-adlb.R; they do not
  # have to agree, and where they disagree the SAP wins.
  # The two calls that implement this rule -- restrict_derivation() around
  # derive_var_extreme_flag(), then derive_var_base() -- live in
  # R/derive_ablfl.R so the unit tests in tests/testthat/test-adlb.R exercise
  # exactly the code this program runs, rather than a copy of it.
  #
  # BASE copies the AVAL of the ABLFL == "Y" record onto EVERY row of that
  # subject/parameter, including the baseline row itself and the pre-baseline
  # rows. That denormalisation is the point of BDS: a change-from-baseline table
  # needs no join and no window function, just AVAL - BASE on a single row.
  #
  # In dplyr you would write
  #   group_by(USUBJID, PARAMCD) |> mutate(BASE = AVAL[ABLFL == "Y"][1])
  # which fails silently (returns NA, or errors on length 0) when a subject has
  # no baseline record. derive_var_base() makes the filter explicit and leaves
  # BASE as NA for those subjects, which is the correct, inspectable outcome.
  derive_ablfl_base() %>%

  # ===========================================================================
  # 5. Change from baseline
  # ===========================================================================
  # CHG = AVAL - BASE, PCHG = 100 * (AVAL - BASE) / BASE.
  # Both take no arguments: admiral fixes the variable names by ADaM convention.
  # WHERE CHG IS POPULATED -- the part most likely to be probed:
  #   * on the baseline row itself, where it is 0 by construction;
  #   * AND on every PRE-baseline row. 128 rows in this dataset were collected
  #     strictly BEFORE their own baseline record and still carry a CHG, i.e. a
  #     change measured against a value drawn later in time.
  # admiral's own ad_adlb.R template wraps these in restrict_derivation() with
  # filter = ADY > 0 so only post-treatment records get CHG. This program does
  # not, so the reader can SEE the unrestricted behaviour. Which of the two a
  # study uses is an SAP decision; both are defensible, but it must be stated.
  # This choice is documented in README.md.
  derive_var_chg() %>%
  derive_var_pchg() %>%

  # ===========================================================================
  # 6. Reference range indicators
  # ===========================================================================
  # ANRIND classifies AVAL against ANRLO/ANRHI as LOW / NORMAL / HIGH.
  # BNRIND is the same classification carried from the baseline record, exactly
  # as BASE carries the baseline AVAL.
  derive_var_anrind() %>%
  derive_var_base(
    by_vars    = exprs(STUDYID, USUBJID, PARAMCD),
    source_var = ANRIND,
    new_var    = BNRIND
  ) %>%

  # SHIFT1 = "baseline category to current category", e.g. "NORMAL to HIGH".
  #
  # WATCH THE DEFAULT: derive_var_shift()'s missing_value argument defaults to
  # the four-character STRING "NULL", not to NA. So the 5 BILI rows with a
  # missing ANRIND come out as SHIFT1 == "NORMAL to NULL" -- a literal string
  # that reads like a bug in a shift table. Left at the default deliberately so
  # it is visible; a real study would set missing_value = NA_character_ or
  # define a below-limit-of-quantification imputation rule in the SAP.
  # Shift tables are a standard safety output: how many subjects moved from a
  # normal baseline to an abnormal post-baseline value.
  derive_var_shift(
    new_var  = SHIFT1,
    from_var = BNRIND,
    to_var   = ANRIND
  ) %>%

  # ===========================================================================
  # 7. Sequence number and final ordering
  # ===========================================================================
  # ASEQ uniquely identifies a record WITHIN a subject. Together with USUBJID it
  # is the key that lets a reviewer point at exactly one row of ADLB.
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order   = exprs(PARAMCD, ADT, LBSEQ),
    new_var = ASEQ
  ) %>%
  arrange(STUDYID, USUBJID, PARAMCD, ADT, LBSEQ) %>%

  # -- Variable order ------------------------------------------------------
  # The SDTM source variables (LBSEQ, LBTESTCD, LBTEST, LBCAT, VISIT, VISITNUM,
  # LBDTC, LBSTRESN, LBBLFL) are kept even though the analysis does not need
  # them. THIS IS TRACEABILITY. A reviewer holding an ADLB row can go straight
  # back to the exact LB record it came from via USUBJID + LBSEQ, and can see
  # that AVAL was copied from LBSTRESN rather than computed. In define.xml these
  # carry origin "Predecessor"; AVAL's origin names LB.LBSTRESN, while CHG's
  # origin is "Derived" with the rule AVAL - BASE.
  select(
    STUDYID, USUBJID, ASEQ,
    TRTP, TRTA, TRT01P, TRT01A, TRTSDT, TRTEDT, SAFFL, ITTFL, AGEGR1, SEX,
    PARAMCD, PARAM, PARAMN, PARCAT1,
    AVAL, AVALC, AVALU, ABLFL, BASE, CHG, PCHG,
    ANRLO, ANRHI, ANRIND, BNRIND, SHIFT1,
    AVISIT, AVISITN, ADT, ADY,
    LBSEQ, LBTESTCD, LBTEST, LBCAT, LBSTRESN, LBBLFL, VISIT, VISITNUM, LBDTC
  )

# ===========================================================================
# 8. Save
# ===========================================================================
saveRDS(adlb, file.path(adam_dir, "adlb.rds"))

message("ADLB: ", nrow(adlb), " rows x ", ncol(adlb), " columns")
