# ---------------------------------------------------------------------------
# 90_export_xpt.R -- write ADSL, ADLB and ADAE as SAS Transport v5 (.xpt)
#
# WHY XPT v5 AT ALL
# -----------------
# Nothing about the analysis needs it. It exists because the FDA's Study Data
# Technical Conformance Guide names SAS Transport Format Version 5 (XPORT) as
# THE transport format for study data in an eCTD submission. It is a 1980s
# fixed-width binary container: 80-byte records, IBM hexadecimal floating point,
# ASCII text. It carries no compression, no UTF-8, no long names, and no types
# beyond "8-byte numeric" and "fixed-width character".
#
# Everything downstream of that sentence is a consequence, and every one of the
# consequences is a constraint you have to design the ADaM dataset around:
#
#   * variable NAMES       <= 8  characters, uppercase A-Z/0-9, first char alpha
#   * variable LABELS      <= 40 characters, ASCII only
#   * character VALUES     <= 200 bytes
#   * dataset LABEL        <= 40 characters
#   * the FILE NAME stem   <= 8  characters (adsl.xpt, adlb.xpt)
#
# This is the real reason ADaM variable names look like TRTSDT and AGEGR1N and
# not treatment_start_date. The 8-character limit is not CDISC being terse for
# its own sake -- it is the transport format leaking into the standard. A dplyr
# user meeting CDISC for the first time usually assumes the naming is stylistic;
# it is not, it is load-bearing, and saying so is a good answer to "why are the
# names like that?".
#
# WHICH CONSTRAINTS THIS REPO ALREADY SATISFIED
# ---------------------------------------------
# ALL of them, and NOTHING had to be renamed or shortened. Measured, not
# assumed -- section 3 re-derives every number below and stops the run if any of
# them is wrong. Note WHERE each number is measured from, because it is not all
# one place: names and character values are measured from the DATA, labels are
# measured from the SPEC (that is what actually gets written into the file), and
# the file stem is just the dataset name.
#
#   variable name    8 of 8   -- RFXSTDTC, RFXENDTC, RFPENDTC, ACTARMCD,
#                                ACTARMUD (ADSL); LBTESTCD, LBSTRESN, VISITNUM
#                                (ADLB). Every other name is shorter.
#   variable label  39 of 40  -- "Datetime of First Exposure to Treatment"
#   character value 32 of 200 -- RACE in ADSL, PARAM in ADLB
#   dataset label   40 of 40  -- "Laboratory Test Results Analysis Dataset"
#   file name stem   4 of 8   -- adsl, adlb
#
# That the names all fit is not luck: 01_adsl.R and 02_adlb.R took their names
# from the ADaM IG, and the IG was itself written inside these limits.
#
# Two of those are worth noticing rather than skimming. The ADLB dataset label
# is EXACTLY at the limit -- 40 of 40 -- so one more word and xportr_df_label()
# aborts with "Length of dataset label must be 40 characters or less."; it is
# left at 40 deliberately, as a live demonstration of where the ceiling is. And
# the 200-byte value limit has an enormous margin here only because this is a
# tidy pilot study; the variables that blow through 200 in real life are AE
# verbatim terms (AETERM) and free-text comments, neither of which is in this
# repo. Do not present a 32-byte maximum as evidence that the limit is easy.
#
# .xpt IS COMMITTED TO THIS REPOSITORY
# ------------------------------------
# data/adam/*.rds is gitignored, because it is reproducible: run the programs
# and you get it back byte for byte. data/adam/*.xpt is NOT gitignored, and that
# is deliberate. The .xpt is the SUBMISSION ARTIFACT -- it is the thing that
# would actually be shipped -- so a reader should be able to clone this repo,
# point haven::read_xpt() or a SAS viewer at it, and inspect exactly what the
# agency would receive, without installing R or re-running anything. Committing
# the deliverable and ignoring the intermediate is the usual split.
#
# One consequence to know before being surprised by it: an .xpt is NOT
# byte-reproducible. The XPT header carries a CREATION TIMESTAMP, so re-running
# this program produces a file with a different checksum even when every value
# in it is identical (verified: two consecutive runs give different md5sums).
# `git diff` on a committed .xpt is therefore always noisy and never
# informative. The meaningful comparison is the one section 5 does -- read both
# files back and compare CONTENT -- not a checksum of the container.
#
# Source ADaM: data/adam/adsl.rds, data/adam/adlb.rds.
#
# SCOPE: all three datasets built by programs/01-03 are exported. ADAE was
# added after ADSL and ADLB: appending its variable rows to
# metadata/adam_spec.csv and one row to metadata/adam_datasets.csv was the
# whole change, plus one more call to each of the functions below. Nothing in
# the code is specific to any one dataset; a dataset absent from the spec is
# never touched at all, so there is no path here that half-exports one.
# ---------------------------------------------------------------------------

source("programs/00_setup.R")

library(xportr)
library(haven)

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))
adlb <- readRDS(file.path(adam_dir, "adlb.rds"))
adae <- readRDS(file.path(adam_dir, "adae.rds"))

spec_path    <- "metadata/adam_spec.csv"
dsspec_path  <- "metadata/adam_datasets.csv"

# ===========================================================================
# 1. The spec is DATA, not code
# ===========================================================================
# xportr is driven entirely by a metadata data frame. It does not inspect your
# intentions; it applies a table. That table is the same table that, in a real
# study, generates define.xml -- so the spec and the shipped dataset cannot
# drift apart, because one produced the other.
#
# xportr 0.6.0 finds the columns by OPTION, not by position. The defaults come
# from xportr_options() and are, verbatim:
#   xportr.domain_name    = "dataset"     xportr.variable_name = "variable"
#   xportr.type_name      = "type"        xportr.label         = "label"
#   xportr.length         = "length"      xportr.order_name    = "order"
#   xportr.format_name    = "format"
# so metadata/adam_spec.csv uses exactly those lowercase names and no renaming
# or metacore object is needed. (xportr also accepts a {metacore} object and
# will pull $var_spec / $ds_spec out of it; a plain data frame is enough here
# and is far easier to review in a pull request.)
#
# TWO files, not one, and this catches people out: the DATASET-level label is
# looked up with a DIFFERENT pair of options -- xportr.df_domain_name
# ("dataset") and xportr.df_label ("label") -- and xportr_df_label() does
#   filter(metadata, dataset == domain) |> select(label) |> as.character()
# and then requires the result to be ONE string of 40 characters or less. Hand
# it the 88-row VARIABLE spec with domain = "ADSL" and the filter leaves 46
# rows -- and as.character() on a one-column tibble does NOT give a 46-element
# vector, it DEPARSES the column into a single 1,293-character string starting
#   c("Study Identifier", "Unique Subject Identifier", ...
# which then trips the 40-character test and aborts with "Length of dataset
# label must be 40 characters or less." (verified by doing it). The error names
# a length limit and the actual cause is passing the wrong table; worth knowing
# before losing twenty minutes to it. So dataset labels live in their own
# one-row-per-dataset file. xportr's own shipped examples split the same way
# (dataset_spec vs var_spec), and so does define.xml: ItemGroupDef describes
# the dataset, ItemDef describes the variable.

# -- How adam_spec.csv was FIRST created ----------------------------------
# A first draft is generated from the built datasets: names, R classes, the
# observed maximum byte length of every character column, the SAS format
# IMPLIED BY THE R CLASS (Date -> DATE9., POSIXct -> DATETIME20.), and whatever
# labels the SDTM source happened to carry through. That draft is then CURATED
# BY HAND, and the curated file is what is committed.
#
# Be precise about what "curated by hand" means here, because it is a fair
# interview question and the honest answer is narrower than it sounds. Move
# metadata/adam_spec.csv aside, let this program regenerate the draft, and diff
# the draft against the committed file: that diff IS the hand work, and re-doing
# it is how to refresh the counts below whenever a variable is added. Against
# the spec as first committed (88 variable rows across ADSL and ADLB; the 37
# ADAE rows were added later by the same draft-then-curate route):
#   * 59 label cells were EMPTY in the draft and had to be written in -- one for
#     every derived variable, because admiral attaches no label attribute;
#   * 2 labels were non-empty but WRONG, inherited from whichever SDTM variable
#     the derivation happened to read: TRT01P and TRT01A arrived carrying
#     "Description of Planned Arm" / "Description of Actual Arm", which is the
#     label on ARM/ACTARM. Both were replaced with the ADaM IG wording;
#   * 2 lengths were widened (see the next block).
# The `type`, `order` and `format` columns were NOT touched -- the draft derives
# all three correctly from the R classes. So do not claim the SAS formats were
# assigned by hand; they were not.
#
# The guard below therefore matters: re-running this program must NOT overwrite
# the curated spec with a fresh draft. Deleting metadata/adam_spec.csv and
# re-running regenerates the draft; that is the bootstrap path, not the normal
# one. A real study never regenerates the spec from the data at all, because the
# spec is written FIRST, from the SAP, and the data is checked against it.
build_spec_draft <- function(d, dataset_name) {
  data.frame(
    dataset  = dataset_name,
    variable = names(d),
    # Labels present on the .rds are inherited from SDTM via haven's label
    # attribute. Derived variables have none, so this column is mostly blank in
    # the draft and is the main thing the human has to fill in.
    label = vapply(d, function(x) {
      l <- attr(x, "label")
      if (is.null(l)) "" else as.character(l)
    }, character(1)),
    # XPT v5 has exactly two storage types. Date/POSIXct are numeric in SAS
    # (days / seconds since 1960-01-01) and are handled below via `format`.
    type = vapply(d, function(x) {
      if (is.character(x)) "character" else "numeric"
    }, character(1)),
    # A SAS numeric is always 8 bytes. A SAS character variable has a declared
    # width, and every value is padded to it, so the width is a real cost: the
    # file is nrow * sum(width) bytes of data. max(1L, ...) is needed because an
    # all-missing character column has an observed maximum of 0 and SAS has no
    # zero-length character variable.
    length = vapply(d, function(x) {
      if (is.character(x)) max(1L, max(0L, nchar(x, type = "bytes"), na.rm = TRUE)) else 8L
    }, integer(1)),
    order = seq_along(d),
    # SAS FORMAT, not storage. A SAS date is the number 19723; DATE9. is what
    # makes a viewer print it as 02JAN2014. Leave blank for everything else.
    format = vapply(d, function(x) {
      if (inherits(x, "Date")) "DATE9." else if (inherits(x, "POSIXct")) "DATETIME20." else NA_character_
    }, character(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

if (!file.exists(spec_path)) {
  message("metadata/adam_spec.csv not found -- writing a FIRST DRAFT from the data. ",
          "Labels must now be curated by hand before this is a real spec.")
  write_csv(
    rbind(build_spec_draft(adsl, "ADSL"), build_spec_draft(adlb, "ADLB"),
          build_spec_draft(adae, "ADAE")),
    spec_path,
    na = ""
  )
}

# -- Read the spec back ----------------------------------------------------
# Column types are pinned explicitly. If `format` is read by readr's guesser it
# comes back as logical NA for a dataset with no formats, and xportr_format()
# then has nothing usable. length/order must be integer, not character, because
# xportr_order() sorts on as.numeric(order) -- character "10" would sort before
# character "2".
adam_spec <- read_csv(
  spec_path,
  col_types = cols(
    dataset = col_character(), variable = col_character(), label = col_character(),
    type = col_character(), length = col_integer(), order = col_integer(),
    format = col_character()
  )
)

adam_datasets <- read_csv(
  dsspec_path,
  col_types = cols(dataset = col_character(), label = col_character())
)

# -- Where the curated spec deliberately departs from the draft ------------
# Labels aside, the curation changed exactly two LENGTHS, and the reason is the
# single most important thing to understand about deriving a spec from data:
#
#   ADSL RFICDTC  draft 1 -> spec 19   (ISO 8601 datetime width)
#   ADSL ACTARMUD draft 1 -> spec 40   (free-text arm description)
#
# Both variables are ENTIRELY MISSING in this study, so the observed maximum
# byte length is 0 and the draft floors it at 1. Shipping width 1 would be
# wrong in a way that only bites later: the first study where an informed
# consent datetime is actually collected, the transport layout silently changes
# and no longer matches the define.xml that was already filed.
#
# That generalises. A length taken from the data is a length that can change
# when the data changes. A real spec fixes every length from the standard and
# the SAP up front -- SDTM USUBJID is 200 whether or not this study's longest
# subject id is 11 characters -- and it is the DATA that gets checked against
# the spec, never the other way round. Those two are the ONLY widths set by
# hand: of the remaining 86 rows, 27 are numeric and carry the constant 8 that
# a SAS numeric always has, and 59 are character widths that are still just the
# observed maximum in THIS extract. That is the honest weakness of
# bootstrapping a spec from built datasets, and it is worth conceding before
# being asked. Section 5 verifies that whatever the spec says is what the file
# actually declares: RFICDTC really is written as CHAR(19), not CHAR(1).

# ===========================================================================
# 2. Check the spec covers the data, before touching xportr
# ===========================================================================
# xportr's own reaction to a variable that is in the data but not in the spec is
# to give it an EMPTY label and shove it to the end of the dataset. With
# verbose = "message" it says so, but it does not stop. For a submission that is
# the wrong default: an unspecified variable is a finding, not a formatting
# detail. So check it here and fail.
check_spec_covers <- function(d, dataset_name) {
  in_spec <- adam_spec$variable[adam_spec$dataset == dataset_name]
  missing_from_spec <- setdiff(names(d), in_spec)
  missing_from_data <- setdiff(in_spec, names(d))
  if (length(missing_from_spec) > 0) {
    stop(dataset_name, ": in data but NOT in spec: ", paste(missing_from_spec, collapse = ", "))
  }
  if (length(missing_from_data) > 0) {
    stop(dataset_name, ": in spec but NOT in data: ", paste(missing_from_data, collapse = ", "))
  }
  invisible(TRUE)
}
check_spec_covers(adsl, "ADSL")
check_spec_covers(adlb, "ADLB")
check_spec_covers(adae, "ADAE")

# ===========================================================================
# 3. Re-derive the XPT v5 constraints from the actual data
# ===========================================================================
# xportr::xpt_validate() runs MOST of these checks inside xportr_write(), and
# with strict_checks = TRUE it aborts. They are repeated here for three reasons:
# to print the actual measured numbers rather than only a pass/fail; so a reader
# can see WHAT is being checked without reading xportr's source; and because the
# overlap is NOT total. Read out of xportr 0.6.0's source rather than assumed:
#
#   in xpt_validate()  name length/case, label > 40 chars, non-ASCII label,
#                      format validity, character value > 200 bytes
#   in xportr_write()  file name stem > 8 chars -- and that one aborts whatever
#                      strict_checks is set to
#   in NEITHER         "every variable has a label". An EMPTY label passes
#                      xportr with strict_checks = TRUE (verified by writing
#                      such a file). That row below is therefore an ADDITION,
#                      not a repeat -- and it is the row that fails on the
#                      bootstrap path.
#
# One xpt_validate() rule is worth knowing about precisely so as not to claim
# it: it checks that a variable named *DT / *DTM / *TM carries an R date/time
# class, but the check is gated on grepl("^ad", deparse(substitute(data))) --
# the NAME of the argument it was handed. xportr_write() calls it as
# xpt_validate(.df), and ".df" does not match "^ad", so that rule never fires
# through the normal pipeline. It is not protecting this export.
audit_xpt_constraints <- function(d, dataset_name) {
  spec  <- adam_spec[adam_spec$dataset == dataset_name, ]
  nm    <- names(d)
  chars <- vapply(d, is.character, logical(1))
  maxb  <- if (any(chars)) {
    max(vapply(d[chars], function(x) max(0L, nchar(x, type = "bytes"), na.rm = TRUE), integer(1)))
  } else 0L
  ds_label <- adam_datasets$label[adam_datasets$dataset == dataset_name]

  res <- data.frame(
    constraint = c("variable name <= 8 chars", "name is uppercase alnum, alpha first",
                   "every variable has a label", "variable label <= 40 chars",
                   "label is plain ASCII", "character value <= 200 bytes",
                   "dataset label <= 40 chars", "file name stem <= 8 chars"),
    worst = c(max(nchar(nm)),
              sum(!grepl("^[A-Z][A-Z0-9]*$", nm)),
              # A freshly generated draft has empty labels for every derived
              # variable, so this row is what fails on the bootstrap path and
              # tells the human the spec is not curated yet. max() is guarded
              # with c(0L, ...) and na.rm so an all-NA label column reports 0
              # rather than propagating NA into the stop() condition.
              sum(is.na(spec$label) | !nzchar(spec$label)),
              max(c(0L, nchar(spec$label)), na.rm = TRUE),
              sum(grepl("[^ -~]", spec$label)),
              maxb,
              nchar(ds_label),
              nchar(tolower(dataset_name))),
    limit = c(8, 0, 0, 40, 0, 200, 40, 8),
    stringsAsFactors = FALSE
  )
  # Rows whose limit is 0 COUNT VIOLATIONS rather than measure a maximum, so the
  # same <= comparison works for every row.
  res$ok <- res$worst <= res$limit
  message("\n--- ", dataset_name, ": XPT v5 constraint audit ---")
  print(res, row.names = FALSE)
  if (!all(res$ok)) {
    stop(dataset_name, ": XPT v5 constraint(s) violated: ",
         paste(res$constraint[!res$ok], collapse = "; "))
  }
  invisible(res)
}
audit_xpt_constraints(adsl, "ADSL")
audit_xpt_constraints(adlb, "ADLB")
audit_xpt_constraints(adae, "ADAE")

# ===========================================================================
# 4. Write the transport files
# ===========================================================================
# THE PIPELINE. Each xportr_* verb attaches ONE attribute and returns the data
# frame, so the whole thing is a pipe and every step is inspectable:
#
#   xportr_metadata()  parks the spec + domain on the data frame as attributes,
#                      so the later verbs do not each need metadata=/domain=.
#                      Purely ergonomic; you can pass them to every call instead.
#   xportr_type()      coerces each column to the type the SPEC declares.
#   xportr_format()    sets attr "format.sas"  (display, not storage).
#   xportr_length()    sets attr "width"       (the SAS declared width).
#   xportr_label()     sets attr "label".
#   xportr_order()     reorders the columns by the spec's `order`.
#   xportr_write()     validates, then haven::write_xpt(version = 5).
#
# Two things worth saying out loud about xportr_type() in THIS repo:
#
#   (a) It is a no-op here BY CONSTRUCTION, because the `type` column of the
#       spec was generated FROM these datasets' R classes. It cannot disagree
#       with them. That is an artefact of bootstrapping the spec from the data
#       and it is exactly what a real study does not do -- there the spec is
#       written from the SAP first, and xportr_type() is the thing that catches
#       "PARAMN was built as character but the spec says numeric". Do not claim
#       this run demonstrates type enforcement. It demonstrates type agreement.
#
#   (b) Its type vocabulary is NOT R's. xportr.character_metadata_types includes
#       "date", "datetime", "time" and "posixct". So writing type = "date" in
#       the spec for TRTSDT makes xportr treat it as CHARACTER and run
#       as.character() on it -- the dates would be written as the strings
#       "2014-01-02". The correct spec entry for an ADaM numeric date is
#       type = "numeric" plus format = "DATE9.", which is what this spec does.
#       That is a trap you only find by reading xportr_options().
#
# verbose = "message" everywhere: xportr's default is "none", which means it
# silently does nothing when a variable is unspecified. "message" prints what it
# did. ("warn" and "stop" are the other two settings.)
#
# strict_checks = TRUE: the DEFAULT IS FALSE, which downgrades every validation
# failure to a warning and writes the file anyway. For a submission artifact
# that is unacceptable -- a label over 40 characters should stop the run, not
# produce a file with a silently unusable label.
export_xpt <- function(d, dataset_name) {
  path <- file.path(adam_dir, paste0(tolower(dataset_name), ".xpt"))

  # Warnings are captured rather than left to scroll past. haven::write_xpt()
  # warns (it does not error) when a declared width is narrower than an actual
  # value -- "Column `X` contains string values longer than user width 4. Width
  # set to 10 to accommodate." -- and that warning is a SPEC BUG: the shipped
  # file would no longer match define.xml. It must be surfaced, not swallowed.
  warns <- character()
  withCallingHandlers(
    d %>%
      xportr_metadata(adam_spec, domain = dataset_name, verbose = "message") %>%
      xportr_type() %>%
      xportr_format() %>%
      xportr_length() %>%
      xportr_label() %>%
      xportr_order() %>%
      xportr_write(
        path          = path,
        metadata      = adam_datasets,  # dataset-level: supplies attr(df,"label")
        domain        = dataset_name,
        strict_checks = TRUE
      ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (length(warns) > 0) {
    message("!! ", dataset_name, ": xportr/haven raised ", length(warns), " warning(s):")
    for (w in warns) message("   - ", w)
  } else {
    message("OK ", dataset_name, ": written to ", path, " with no warnings.")
  }
  path
}

adsl_xpt <- export_xpt(adsl, "ADSL")
adlb_xpt <- export_xpt(adlb, "ADLB")
adae_xpt <- export_xpt(adae, "ADAE")

# ===========================================================================
# 5. Round trip
# ===========================================================================
# Writing a file is not evidence that the file is right. Read it back.
#
# haven::read_xpt() recovers values, labels and formats, and reconstructs Date /
# POSIXct from the SAS numeric + format pair. What it does NOT return is the
# DECLARED CHARACTER WIDTH -- the "width" attribute xportr_length() set is
# consumed by the writer and not reported by the reader. So a read_xpt()-only
# check cannot prove the `length` column of the spec had any effect.
#
# read_namestr() below reads it out of the file's own header instead. XPT v5
# stores one 140-byte NAMESTR record per variable, laid out (big-endian):
#   bytes  1-2   type   1 = numeric, 2 = character
#          5-6   length (the declared width)
#          7-8   variable number
#          9-16  name   (8 chars)
#         17-56  label  (40 chars)  <- the 40-char limit is literally this field
#         57-64  format NAME only (8 chars)
#         65-66  format width      67-68  format decimals
# Seeing the 8- and 40-character limits as FIXED-WIDTH FIELDS in the header is
# the clearest possible explanation of where those limits come from: there is
# nowhere to put a 9th character.
#
# Note that a SAS format is stored SPLIT: "DATETIME20." is name "DATETIME" in
# the 8-byte name field plus width 20 in a separate 2-byte field. It has to be,
# because "DATETIME20" would not fit in 8 bytes. Reassemble it below.
read_namestr <- function(path) {
  raw <- readBin(path, "raw", n = file.size(path))
  # grepRaw, not rawToChar: XPT contains embedded NUL bytes and rawToChar errors
  # on them.
  h <- grepRaw(charToRaw("HEADER RECORD*******NAMESTR HEADER RECORD!!!!!!!"),
               raw, fixed = TRUE)[1]
  nvar <- as.integer(rawToChar(raw[(h + 54):(h + 57)]))  # count sits in that header record
  off  <- h + 80                                          # NAMESTRs start in the next record
  chr  <- function(r) trimws(rawToChar(r[r != as.raw(0)]))
  do.call(rbind, lapply(seq_len(nvar), function(k) {
    r   <- raw[(off + (k - 1) * 140):(off + k * 140 - 1)]
    s16 <- function(a, b) sum(as.integer(r[a:b]) * c(256L, 1L))
    nfl <- s16(65, 66)
    nfd <- s16(67, 68)
    data.frame(
      variable = chr(r[9:16]),
      type     = c("numeric", "character")[s16(1, 2)],
      length   = s16(5, 6),
      varnum   = s16(7, 8),
      label    = chr(r[17:56]),
      format   = paste0(chr(r[57:64]),
                        if (nfl > 0) nfl else "",
                        if (nfd > 0) paste0(".", nfd) else ""),
      stringsAsFactors = FALSE
    )
  }))
}

verify_round_trip <- function(original, path, dataset_name) {
  back <- read_xpt(path)
  ns   <- read_namestr(path)
  spec <- adam_spec[adam_spec$dataset == dataset_name, ]
  spec <- spec[order(spec$order), ]

  lab_of <- function(d) vapply(d, function(x) {
    l <- attr(x, "label"); if (is.null(l)) "" else as.character(l)
  }, character(1))

  # -- Values ---------------------------------------------------------------
  # Values are compared COLUMN BY COLUMN BY NAME, not positionally, because
  # xportr_order() legitimately reorders the columns; "are the columns in the
  # spec's order" and "do the values survive" are separate questions and are
  # answered separately.
  #
  # And they must be compared in TWO groups, because the round trip is NOT the
  # identity on character columns, for a reason that is the exact mirror of
  # convert_blanks_to_na() in 00_setup.R:
  #
  #     SAS HAS NO NULL FOR CHARACTER.
  #
  # An R NA_character_ has nowhere to go in an XPT character field, so it is
  # written as spaces and read back as "". A round trip through .xpt is
  # therefore lossy for exactly one thing: the distinction between "missing"
  # and "empty string" in character variables. Numerics are fine -- SAS has a
  # genuine numeric missing value (in fact 28 of them: . and ._ and .A-.Z), and
  # it survives the round trip as NA.
  #
  # This is not a bug to be coerced away, it is the format, and it is why every
  # admiral pipeline begins by converting blanks back to NA when it reads SDTM.
  # The check below asserts the SPECIFIC expected relationship -- NA maps to ""
  # and nothing else changes -- rather than either demanding exact identity
  # (which would fail) or dropping the check (which would hide a real corruption).
  chr_vars <- names(original)[vapply(original, is.character, logical(1))]
  num_vars <- setdiff(names(original), chr_vars)

  # as.vector() is doing real work here, not decoration: identical() and
  # all.equal() compare ATTRIBUTES as well as values, and both sides carry a
  # "label" attribute that is SUPPOSED to differ -- the .rds label is whatever
  # SDTM passed through, the .xpt label is the curated one from the spec.
  # Stripping attributes keeps this a comparison of DATA. (The labels are
  # checked separately and against the SPEC -- not against the .rds -- by the
  # "all labels match spec `label`" row of `checks` below.)
  bare <- function(x, mode) as.vector(x, mode)

  same_num <- all(vapply(num_vars, function(v) {
    # Date/POSIXct compare on their underlying numeric, which is what actually
    # crossed the format boundary.
    isTRUE(all.equal(bare(original[[v]], "numeric"), bare(back[[v]], "numeric"),
                     tolerance = 1e-8))
  }, logical(1)))

  same_chr <- all(vapply(chr_vars, function(v) {
    a <- bare(original[[v]], "character")
    identical(replace(a, is.na(a), ""), bare(back[[v]], "character"))
  }, logical(1)))

  # How many cells that asymmetry actually touched, so the claim is quantified
  # rather than asserted.
  na_to_blank <- sum(vapply(chr_vars, function(v) {
    sum(is.na(original[[v]]) & bare(back[[v]], "character") == "")
  }, integer(1)))

  # SAS pads every character value to the declared width and haven strips the
  # padding on read, so a value that genuinely ENDED in a space would come back
  # shortened. None do here; this confirms it rather than assuming it.
  no_trailing_ws <- !any(vapply(chr_vars, function(v) {
    any(grepl("[ \t]$", original[[v]]), na.rm = TRUE)
  }, logical(1)))

  checks <- data.frame(
    check = c(
      "row count matches .rds",
      "column count matches .rds",
      "same variable names (as a set)",
      "column order matches spec `order`",
      "all labels match spec `label`",
      "no label is empty",
      "declared lengths match spec `length`",
      "declared types match spec `type`",
      "date formats match spec `format`",
      "numeric/date values identical",
      "character values identical (NA -> \"\")",
      "no value ends in whitespace (padding-safe)"
    ),
    result = c(
      nrow(back) == nrow(original),
      ncol(back) == ncol(original),
      setequal(names(back), names(original)),
      identical(names(back), spec$variable),
      identical(unname(lab_of(back)), spec$label),
      all(nchar(lab_of(back)) > 0),
      identical(as.integer(ns$length), as.integer(spec$length)),
      identical(ns$type, spec$type),
      identical(ns$format[!is.na(spec$format)],
                sub("\\.$", "", toupper(spec$format[!is.na(spec$format)]))),
      same_num,
      same_chr,
      no_trailing_ws
    ),
    stringsAsFactors = FALSE
  )

  message("\n--- ", dataset_name, ": round-trip verification (", path, ") ---")
  message("  .rds: ", nrow(original), " x ", ncol(original),
          "   .xpt: ", nrow(back), " x ", ncol(back),
          "   file size: ", format(file.size(path), big.mark = ","), " bytes")
  message("  character cells where R NA became \"\" in the .xpt: ", na_to_blank,
          " (expected: SAS has no character NULL)")
  print(checks, row.names = FALSE)

  if (!all(checks$result)) {
    stop(dataset_name, ": round trip FAILED on: ",
         paste(checks$check[!checks$result], collapse = "; "))
  }
  invisible(checks)
}

verify_round_trip(adsl, adsl_xpt, "ADSL")
verify_round_trip(adlb, adlb_xpt, "ADLB")
verify_round_trip(adae, adae_xpt, "ADAE")

message("\nXPT export complete: ", adsl_xpt, ", ", adlb_xpt, ", ", adae_xpt)
