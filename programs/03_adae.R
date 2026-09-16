# ---------------------------------------------------------------------------
# 03_adae.R -- ADAE, an Occurrence Data Structure (OCCDS) dataset
#
# STRUCTURE: one row per ADVERSE EVENT AS RECORDED. Nothing else.
#
# This is NOT BDS, and the contrast with 02_adlb.R is the single most important
# thing to get straight before reading the code:
#
#   BDS (ADLB)                          OCCDS (ADAE)
#   ------------------------------      ------------------------------------
#   one row per subject x PARAM x       one row per collected EVENT; there is
#     analysis timepoint                  no timepoint and no "visit" at all
#   the thing being analysed lives      the thing being analysed is a CODED
#     in AVAL, a single numeric column    TERM (AEDECOD) -- a character value
#   PARAMCD/PARAM say what AVAL is      there is NO PARAMCD and NO PARAM
#   BASE / CHG / PCHG are meaningful    "change from baseline" is meaningless:
#     because the same quantity is        an event either happened or it did
#     measured repeatedly                 not
#   analysis = summarise a number       analysis = COUNT SUBJECTS who had at
#     by visit and treatment              least one event, by term and treatment
#
# The practical consequence of that last line is the occurrence flags in
# section 7. In BDS you count rows; in OCCDS counting rows answers the wrong
# question ("how many events") when the table asks "how many subjects".
#
# The ADaM model defines three STANDARD structures -- ADSL (one row per
# subject), BDS, and OCCDS -- plus an "other" category for datasets that fit
# none of them. ADAE is the canonical OCCDS dataset; ADCM (concomitant meds)
# and ADMH (medical history) are the same shape. (ADTTE, despite being about
# events, is BDS: its AVAL is a time.)
#
# Source SDTM: AE. Source ADaM: ADSL.
# ---------------------------------------------------------------------------

source("programs/00_setup.R")

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))

# ===========================================================================
# 0. What the source dates actually look like -- CHECKED, not assumed
# ===========================================================================
# Before writing any imputation code, the character length of the --DTC strings
# was tabulated. ISO 8601 partial dates are detectable by length alone:
# 10 = "YYYY-MM-DD" (complete), 7 = "YYYY-MM" (no day), 4 = "YYYY" (year only).
#
#   table(nchar(ae$AESTDTC))  ->  4: 11    7: 15    10: 1165    NA: 0
#   table(nchar(ae$AEENDTC))  ->                    10:  718    NA: 473
#
# Two different situations, handled two different ways below:
#   * AESTDTC has 26 genuinely PARTIAL dates -> imputation is required and it
#     really fires (26 records get an imputed ASTDT and a populated ASTDTF).
#   * AEENDTC is either complete or entirely absent -- there is not a single
#     partial end date. Imputation there would be dead code, so it is not
#     written. A wholly missing AEENDTC means the event was still ongoing at
#     the last contact, and inventing an end date for it would be fabrication.
#
# Neither variable carries a time component (no value contains a "T" separator;
# the complete dates are exactly 10 characters), so this program stays in Date
# space and never builds ASTDTM/AENDTM. See the note in section 6 about what
# that means for derive_var_trtemfl() defaults.

# ===========================================================================
# 1. Carry the ADSL subset this dataset needs
# ===========================================================================
# Same principle as ADLB: take the treatment variables, the population flag the
# AE tables subset on, the reference dates the derivations need, and the two
# subgroup variables. Not all 46 ADSL columns.
#
# TRTSDT is load-bearing three times over: it is the reference date for ASTDY,
# the anchor for the imputation guard in section 2, and the definition of
# treatment-emergence in section 6. TRTEDT closes the post-treatment window.
#
# derive_vars_merged() here CANNOT change the row count (ADSL is one row per
# subject and USUBJID is its key), so the 1191 AE rows stay 1191. That is the
# same guarantee dplyr::left_join() does not give you.
#
# Every AE subject exists in ADSL and every one of them has a non-missing
# TRTSDT (verified: 0 AE rows with missing TRTSDT). A real study is not so
# tidy -- an AE recorded for a subject who was randomised but never dosed has
# no TRTSDT, ASTDY is then NA, and TRTEMFL is NA rather than "N". The code
# below handles that case correctly, but this data never exercises it.
adae <- ae %>%
  select(-DOMAIN) %>%
  derive_vars_merged(
    dataset_add = adsl,
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(TRT01P, TRT01A, TRTSDT, TRTEDT, SAFFL, AGEGR1, SEX)
  ) %>%

  # =========================================================================
  # 2. Analysis start and end dates
  # =========================================================================
  # -- ASTDT: imputation is REQUIRED here and it fires -----------------------
  # highest_imputation = "M" means "impute at most the month" -- i.e. a missing
  # DAY may be filled, and a missing MONTH may be filled, but a missing YEAR
  # may not (that would be highest_imputation = "Y"). A --DTC with no year at
  # all therefore stays NA rather than being guessed.
  #
  # date_imputation = "first": missing day -> 01, missing month -> January.
  # Result on this data: 15 records get flag ASTDTF = "D" (day imputed) and 11
  # get ASTDTF = "M" (month and day imputed). The flag is not decoration -- it
  # is how a reviewer, and the test file, can separate collected dates from
  # manufactured ones. Any analysis rule that says "ignore imputed dates" is
  # only implementable because this flag exists.
  #
  # THE IMPUTATION TRAP (this is the interview question):
  #   Take an AE recorded as "2013-05" for a subject first dosed 2013-05-20.
  #     impute to the FIRST -> ASTDT = 2013-05-01, which is BEFORE first dose,
  #       so the event is scored not-treatment-emergent and disappears from
  #       every safety table. A genuinely emergent event has been LOST.
  #     impute to the LAST  -> ASTDT = 2013-05-31, which is AFTER first dose,
  #       so a pre-treatment event may be MANUFACTURED into an emergent one.
  #   Neither direction is safe in general. The convention in safety analyses
  #   is to be conservative -- when it cannot be ruled out that the event was
  #   treatment-emergent, count it -- which is what min_dates does below.
  #
  # min_dates = exprs(TRTSDT) is admiral's resolution: impute to the first of
  # the period, BUT if the subject's first dose date falls inside the range of
  # dates the partial string could represent, use the first dose date instead.
  # "2013-05" + TRTSDT 2013-05-20 therefore imputes to 2013-05-20, not
  # 2013-05-01, and the event stays emergent. Dates outside the possible range
  # are ignored, so "2012" + TRTSDT 2013-05-20 still imputes to 2012-01-01 --
  # admiral will not move a date into a year the string rules out.
  #
  # HONEST NOTE, verified by running it both ways: on THIS data min_dates
  # changes exactly 0 records, because no partial AESTDTC value's possible date
  # range contains that subject's TRTSDT. It is written because it is the
  # correct rule and because the moment one such record appears the result is
  # wrong without it -- but it is not exercised here, and the owner should not
  # claim it is. Likewise, imputing to "last" instead of "first" changes all 26
  # imputed dates yet changes TRTEMFL for none of them, for the same reason.
  derive_vars_dt(
    new_vars_prefix    = "AST",
    dtc                = AESTDTC,
    highest_imputation = "M",
    date_imputation    = "first",
    min_dates          = exprs(TRTSDT)
  ) %>%

  # -- AENDT: no imputation, because there is nothing to impute --------------
  # highest_imputation = "n" is the default and it is stated explicitly here so
  # the reader sees it was a decision, not an oversight. Section 0 showed every
  # AEENDTC is either a complete 10-character date or absent entirely.
  #
  # Because nothing is imputed, flag_imputation = "auto" creates NO AENDTF
  # variable at all. That is worth noticing: the presence of a --DTF variable in
  # a dataset is itself information.
  #
  # The 473 missing end dates stay missing. The admiral ADAE template imputes
  # them to the last of the period with max_dates = exprs(DTHDT, EOSDT); that is
  # a defensible SAP rule for duration analyses, but it invents an end date for
  # events that were ongoing, and it is not needed for anything derived here.
  derive_vars_dt(
    new_vars_prefix    = "AEN",
    dtc                = AEENDTC,
    highest_imputation = "n"
  ) %>%

  # =========================================================================
  # 3. Study days
  # =========================================================================
  # Same no-day-zero convention as ADLB: day of first dose is Day 1, the day
  # before is Day -1. derive_vars_dy() names the outputs by swapping the "DT"
  # suffix for "DY", so ASTDT -> ASTDY and AENDT -> AENDY in one call.
  #
  # ASTDY runs from -13469 to 194 here. The extreme negative is the single AE
  # recorded as "1977" (subject 01-710-1077, AESEQ 4) -- a condition the subject
  # has had since long before the study, recorded as an AE. The EVENT is real;
  # the exact -13469 is not, because it depends on that year-only date being
  # imputed to 1977-01-01 (ASTDTF = "M"). AENDY is NA for the 473 ongoing events.
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars    = exprs(ASTDT, AENDT)
  ) %>%

  # ADY is the generic ADaM "analysis relative day" for the record. For an
  # occurrence dataset the record IS the event, so its analysis day is the
  # onset day: ADY = ASTDY, by definition and not by coincidence. ASTDY/AENDY
  # are the names a spec and define.xml would lead with for ADAE; ADY is added
  # because it is the variable generic tooling looks for.
  mutate(ADY = ASTDY) %>%

  # -- Traceability check worth knowing about ------------------------------
  # SDTM already carries AESTDY/AEENDY. They are NOT copied into ASTDY/AENDY,
  # for a real reason: SDTM's --STDY is relative to RFSTDTC (the DM reference
  # start date), while ADaM's ASTDY is relative to TRTSDT (first dose, derived
  # from EX in 01_adsl.R). Those two anchors usually coincide and here they
  # almost always do -- but recomputing rather than copying catches the case
  # where they do not. On the 1165 records with a complete start date, ASTDY
  # and AESTDY disagree on exactly ONE: subject 01-716-1063 AESEQ 1, where
  # AESTDTC == RFSTDTC == TRTSDT == 2013-05-09 so the study day must be 1, and
  # SDTM says 366. That is an error in the public pilot data, and the ADaM
  # derivation is what exposes it. AESTDY is kept in the output so the
  # discrepancy stays visible instead of being quietly overwritten.

  # =========================================================================
  # 4. Event duration
  # =========================================================================
  # ADURN/ADURU are a value/unit PAIR -- ADaM never encodes a unit inside a
  # value or a variable name. add_one = TRUE applies the same clinical
  # convention as TRTDURD in ADSL: an event that started and ended on the same
  # day lasted 1 day, not 0.
  #
  # Result: 718 records get a duration (range 1 to 444 days), 473 are NA
  # because the event is ongoing. NA here means "not calculable", not "zero" --
  # a mean duration computed over the non-missing rows is biased SHORT, since
  # the events excluded are precisely the ones that had not finished yet. That
  # is informative censoring and a real analysis would say so in the SAP.
  #
  # CHOICE, recorded here: duration is computed even when the start date was
  # imputed. Four records are affected (subject 01-716-1418, AESTDTC "2013-07"
  # imputed to 2013-07-01), so their ADURN is an upper bound rather than a
  # measurement. Some SAPs blank ADURN whenever ASTDTF is populated; this
  # program does not, and ASTDTF is carried in the output so the four records
  # can be identified and excluded downstream.
  derive_vars_duration(
    new_var      = ADURN,
    new_var_unit = ADURU,
    start_date   = ASTDT,
    end_date     = AENDT,
    in_unit      = "days",
    out_unit     = "DAYS",
    add_one      = TRUE,
    trunc_out    = FALSE
  ) %>%

  # =========================================================================
  # 5. Analysis copies of severity and causality
  # =========================================================================
  # ASEV = AESEV and AREL = AEREL look like pointless duplication, and on THIS
  # data they are literally identical copies. They exist because the analysis
  # variable and the collected variable are allowed to diverge: a SAP that
  # collapses AEREL's four-level scale (NONE/REMOTE/POSSIBLE/PROBABLE) into a
  # binary "related / not related" writes that into AREL and leaves AEREL
  # untouched, so the collected value remains auditable. Deriving the copy now
  # means the table programs reference ASEV/AREL from the start and nothing
  # downstream has to change if the SAP later adds a recode.
  #
  # ASEVN is the numeric companion, and it exists for exactly the reason AGEGR1N
  # exists in ADSL: "SEVERE" must sort after "MODERATE", which alphabetical
  # order gets wrong. It is also what orders the most-severe-event flag if one
  # is ever needed. as.integer(factor(..., levels = ...)) with EXPLICIT levels,
  # never factor() on its own -- the whole point is to not accept the default
  # alphabetical ordering.
  #
  # Note AEREL has 4 missing values in this data, which propagate to AREL as NA.
  mutate(
    ASEV  = AESEV,
    AREL  = AEREL,
    ASEVN = as.integer(factor(ASEV, levels = c("MILD", "MODERATE", "SEVERE")))
  ) %>%

  # =========================================================================
  # 6. TRTEMFL -- treatment-emergent flag
  # =========================================================================
  # DEFINITION. An adverse event is treatment-emergent if its onset is ON OR
  # AFTER the first dose of study treatment, and (in a real study, within a
  # window the SAP specifies) not later than a
  # specified window after the last dose. Events that started before first dose
  # are pre-existing conditions, not effects of the drug; events that start long
  # after the drug has been cleared are not plausibly attributable to it either.
  # Almost every AE table in a submission is restricted to TRTEMFL == "Y", so
  # this one flag decides what the safety section of the CSR shows.
  #
  # THE WINDOW IS A SAP DECISION, NOT A STANDARD. end_window = 30 is used here:
  # 30 days after last dose is the most common convention for AE follow-up and
  # it is what the admiral ADAE template uses. It is a choice about drug
  # clearance and follow-up length, and a study with a long-half-life biologic
  # would use 90 days or more. The alternatives, run on this data:
  #     end_window = 30  -> 1122 events flagged "Y"   (used here)
  #     no window at all -> 1126   (onset >= first dose, no upper bound)
  #     end_window = 0   -> 1086   (strictly on-treatment onset)
  # So the window is worth 40 events out of 1191 in this study. State the rule
  # before looking at the numbers, never after.
  #
  # ADMIRAL DEFAULTS ARE DATETIME. The signature is
  #   start_date = ASTDTM, end_date = AENDTM, trt_start_date = TRTSDTM
  # -- all --DTM, none of which exist in this program, because AE and EX dates
  # in this study carry no time. Calling derive_var_trtemfl() on its defaults
  # here fails loudly:
  #   "Required variables `ASTDTM`, `AENDTM`, and `TRTSDTM` are missing"
  # which is the good outcome. The fix is to pass the Date variables explicitly,
  # as below; admiral accepts Date or POSIXct as long as the comparison is
  # between like types. Mixing them -- an ASTDT date against a TRTSDTM datetime
  # -- is the silent-wrong-answer version of this mistake: midnight compares
  # earlier than a same-day dosing time, so same-day events would drop out.
  # ignore_time_for_trt_end is irrelevant here for the same reason (no times),
  # but in a datetime study it decides whether "30 days after last dose" means
  # 30 calendar days or 30x24 hours.
  #
  # WHAT admiral ACTUALLY RETURNS: "Y" or NA. There is no "N". The internal
  # case_when() has no fallback branch, so every non-emergent record is NA,
  # including records with a missing TRTSDT. This program keeps that two-valued
  # flag rather than recoding NA to "N", because "N" would assert "this event is
  # not treatment-emergent" for a subject whose first dose date is unknown and
  # where the truth is "cannot be determined". The test file asserts the flag is
  # in c("Y", NA) precisely so that this choice cannot be changed by accident.
  # Downstream code must therefore filter TRTEMFL == "Y", never TRTEMFL != "N".
  #
  # Two branches of that case_when() are subtle and worth being able to recite:
  #   * AENDT < TRTSDT  -> NA even if the start date is missing: an event that
  #     had already ENDED before first dose cannot be emergent (4 records here).
  #   * ASTDT missing but end date not before first dose -> "Y": with no onset
  #     date, admiral takes the conservative side and counts the event. No
  #     record here has a missing ASTDT, so this branch never fires in this run.
  #
  # Result: 1122 "Y", 69 NA. The 69 split as 65 events with onset before first
  # dose and 4 with onset more than 30 days after last dose.
  derive_var_trtemfl(
    new_var        = TRTEMFL,
    start_date     = ASTDT,
    end_date       = AENDT,
    trt_start_date = TRTSDT,
    trt_end_date   = TRTEDT,
    end_window     = 30
  ) %>%

  # =========================================================================
  # 7. Occurrence flags -- the derivation that only OCCDS needs
  # =========================================================================
  # A standard AE summary table reads "n (%) of SUBJECTS with at least one
  # treatment-emergent event", broken down by System Organ Class and Preferred
  # Term. A subject who had three episodes of headache counts ONCE in the
  # headache row, once in the Nervous Disorders row, and once in the overall
  # row. Counting ADAE rows would report 3 and overstate the incidence.
  #
  # The occurrence flags mark the single record per subject that such a table
  # should count:
  #   AOCCFL  -- first treatment-emergent event for the subject overall
  #   AOCCSFL -- first within each System Organ Class
  #   AOCCPFL -- first within each Preferred Term (within its SOC)
  # A table then counts rows where the relevant flag is "Y", with no distinct()
  # and no grouping logic in the table program. Pushing the de-duplication into
  # ADaM is deliberate: the rule is then written once, in a dataset a reviewer
  # can inspect, instead of being reimplemented in every table and figure.
  #
  # restrict_derivation(filter = TRTEMFL == "Y") is the same idiom as ABLFL in
  # 02_adlb.R: derive on the qualifying rows only, leave every other row in
  # place with the flag NA. Restricting to treatment-emergent records matters --
  # if a subject's earliest headache is pre-treatment, the flag must land on
  # their earliest EMERGENT headache, not on the pre-treatment one.
  #
  # order = exprs(ASTDT, AESEQ): AESEQ breaks ties so two events with the same
  # onset date resolve deterministically instead of by input row order.
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID),
      order   = exprs(ASTDT, AESEQ),
      new_var = AOCCFL,
      mode    = "first"
    ),
    filter = TRTEMFL == "Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID, AEBODSYS),
      order   = exprs(ASTDT, AESEQ),
      new_var = AOCCSFL,
      mode    = "first"
    ),
    filter = TRTEMFL == "Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID, AEBODSYS, AEDECOD),
      order   = exprs(ASTDT, AESEQ),
      new_var = AOCCPFL,
      mode    = "first"
    ),
    filter = TRTEMFL == "Y"
  ) %>%

  # =========================================================================
  # 8. Sequence number
  # =========================================================================
  # ASEQ identifies a record uniquely within a subject, so USUBJID + ASEQ points
  # at exactly one ADAE row. AESEQ is kept as well and is NOT the same variable:
  # AESEQ is the SDTM key and is what a reviewer uses to jump back to the source
  # AE record.
  #
  # They do NOT coincide, and being able to say why is the point. ASEQ is
  # numbered AFTER sorting by onset date; AESEQ is the order the records were
  # collected in, which is not onset order. On this data ASEQ != AESEQ on 339
  # of the 1191 rows, across 72 subjects. Concretely, subject 01-701-1023 has
  # AESEQ 3 starting 2012-08-26 and AESEQ 4 starting 2012-08-07, so sorting by
  # ASTDT swaps them and they come out as ASEQ 4 and ASEQ 3. Nothing downstream
  # may treat the two numbers as interchangeable.
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order   = exprs(ASTDT, AESEQ),
    new_var = ASEQ
  ) %>%
  arrange(STUDYID, USUBJID, ASTDT, AESEQ) %>%

  # =========================================================================
  # 9. Final variable order
  # =========================================================================
  # MedDRA -- what the coded terms actually are, and why the version matters.
  #
  #   AETERM   the verbatim text the investigator wrote. Free text. Never
  #            analysed, always kept, because it is the only record of what was
  #            actually reported.
  #            TOY-DATA FLAG, do not over-claim this one: in the pilot data
  #            AETERM is character-for-character IDENTICAL to AEDECOD on all
  #            1191 rows (242 distinct values in each, zero rows differ, no
  #            free-text punctuation anywhere). The events ship already coded,
  #            so nothing in this repository actually demonstrates the coding
  #            step. In a real study AETERM is messy investigator text and
  #            AEDECOD is what a trained coder mapped it to, and reconciling
  #            the two is a whole workstream.
  #   AEDECOD  the MedDRA PREFERRED TERM (PT): the verbatim text mapped to a
  #            single standard concept by a trained coder. This is the row label
  #            in an AE table.
  #   AEBODSYS the MedDRA SYSTEM ORGAN CLASS (SOC): the top of the hierarchy,
  #            the section heading the PTs are grouped under. The full hierarchy
  #            is LLT -> PT -> HLT -> HLGT -> SOC, and SDTM carries all of them
  #            (AELLT, AEHLT, AEHLGT); AEDECOD and AEBODSYS are the two levels
  #            safety tables are built on. AESOC is a separate variable that can
  #            differ from AEBODSYS when a PT is multi-axial -- a PT can sit
  #            under several SOCs, MedDRA designates one as PRIMARY, and
  #            AEBODSYS is the SOC actually used for reporting. Here the two are
  #            identical for all 1191 records, which is not guaranteed in
  #            general.
  #
  # MedDRA is a LICENSED, VERSIONED dictionary: it is distributed by the MSSO
  # under a subscription (free for some non-profit and regulatory users, paid
  # for industry) and released twice a year, e.g. 26.1, 27.0. It is not shipped
  # in R packages, which is why this dataset carries the coding that the SDTM
  # already contains rather than coding anything itself.
  # The version is recorded in define.xml as the coding dictionary and version
  # for AEDECOD/AEBODSYS. It matters analytically because an upgrade can rename
  # a PT, move a PT to a different primary SOC, or merge two PTs -- so the same
  # events recoded under a later version can produce different counts in the
  # same table. A real study therefore FIXES the MedDRA version in the SAP, and
  # if a re-code to a newer version happens mid-study the SAP says whether the
  # whole database is re-coded for consistency.
  #
  # The SDTM variables kept at the end (AESEQ, AESTDTC, AEENDTC, AESTDY, AEENDY,
  # AESEV, AESER, AEREL, AEOUT) are TRACEABILITY, exactly as in ADLB: a reviewer
  # holding an ADAE row can see that ASTDT came from AESTDTC and where it was
  # imputed, and that ASEV is a copy rather than a computation. In define.xml
  # those carry origin "Predecessor" while TRTEMFL's origin is "Derived" with
  # the window rule written out.
  select(
    # -- keys and subject-level context
    STUDYID, USUBJID, ASEQ,
    TRT01P, TRT01A, TRTSDT, TRTEDT, SAFFL, AGEGR1, SEX,
    # -- what happened (coded terms)
    AETERM, AEDECOD, AEBODSYS,
    # -- when it happened
    ASTDT, ASTDTF, ASTDY, AENDT, AENDY, ADY, ADURN, ADURU,
    # -- how bad it was (analysis variables first, then the collected source)
    ASEV, ASEVN, AESEV, AESER, AREL, AEREL, AEOUT,
    # -- analysis flags
    TRTEMFL, AOCCFL, AOCCSFL, AOCCPFL,
    # -- traceability back to SDTM AE
    AESEQ, AESTDTC, AEENDTC, AESTDY, AEENDY
  )

# ===========================================================================
# 10. Save
# ===========================================================================
saveRDS(adae, file.path(adam_dir, "adae.rds"))

message("ADAE: ", nrow(adae), " rows x ", ncol(adae), " columns")
message("  TRTEMFL == 'Y': ", sum(adae$TRTEMFL == "Y", na.rm = TRUE),
        "  |  subjects with >=1 TE AE (AOCCFL == 'Y'): ",
        sum(adae$AOCCFL == "Y", na.rm = TRUE))
message("  imputed start dates (ASTDTF populated): ", sum(!is.na(adae$ASTDTF)))

# ===========================================================================
# 11. Out of scope here: {admiralvaccine}
# ===========================================================================
# admiral has a family of therapeutic-area extension packages that add
# derivations the core package deliberately leaves out. {admiralvaccine} is the
# vaccine one: it targets reactogenicity data -- the solicited local and
# systemic events a subject records in an e-diary for the days after each
# vaccination -- and builds ADFACE (from the SDTM FACE domain, findings about
# events) plus ADIS for immunogenicity titres. Its characteristic derivations
# are things like maximum severity per event per vaccination, day-of-onset and
# duration within the diary window, and the solicited/unsolicited split, none
# of which core admiral provides.
#
# It is NOT used in this repository and is NOT installed. This study is a
# small-molecule Alzheimer's trial with ordinary unsolicited AE collection, so
# ADAE as built above is the right structure. The note is here because the
# distinction is a fair question in a vaccines group: reactogenicity data is
# collected as FINDINGS about pre-specified events (a BDS-shaped ADFACE with
# AVAL per event per day), while unsolicited AEs remain OCCDS in ADAE -- the
# same trial can legitimately carry both, and they are not interchangeable.
#
# Related extensions, for orientation only: {admiralonco}, {admiralophtha},
# {admiralpeds}, {admiralmetabolic}.
