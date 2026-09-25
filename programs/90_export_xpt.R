# -----------------------------------------------------------------------------
# Program    : 90_export_xpt.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Export ADSL, ADLB and ADAE as SAS Transport v5 (.xpt) with
#              attributes from the specification; check spec coverage and
#              XPT v5 limits before writing; verify each file by reading it back
# Inputs     : data/adam/adsl.rds, adlb.rds, adae.rds
#              metadata/adam_spec.csv (variable level)
#              metadata/adam_datasets.csv (dataset level)
# Outputs    : data/adam/adsl.xpt, adlb.xpt, adae.xpt
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version (ADSL, ADLB)
#              2026-09-17  IGR  ADAE added
#              2026-09-25  IGR  Standard header; comments condensed
# Notes      : XPT v5 limits: names <= 8 chars, labels <= 40 chars ASCII,
#              character values <= 200 bytes, dataset label <= 40 chars,
#              file name stem <= 8 chars. The .xpt header carries a creation
#              timestamp, so files are compared by content, not checksum.
#              Background: docs/implementation-notes.md
# -----------------------------------------------------------------------------

source("programs/00_setup.R")

library(xportr)
library(haven)

adsl <- readRDS(file.path(adam_dir, "adsl.rds"))
adlb <- readRDS(file.path(adam_dir, "adlb.rds"))
adae <- readRDS(file.path(adam_dir, "adae.rds"))

spec_path    <- "metadata/adam_spec.csv"
dsspec_path  <- "metadata/adam_datasets.csv"

# =============================================================================
# 1. Specification
# =============================================================================
# Column names follow the xportr defaults (dataset, variable, label, type,
# length, order, format). Dataset labels are kept in a separate one-row-per-
# dataset file, which is what xportr_df_label() expects.

# Bootstrap only: if the spec is missing, write a draft from the data for
# manual curation (labels in particular). The committed spec is curated and
# is never overwritten.
build_spec_draft <- function(d, dataset_name) {
  data.frame(
    dataset  = dataset_name,
    variable = names(d),
    # Labels inherited from SDTM; derived variables have none.
    label = vapply(d, function(x) {
      l <- attr(x, "label")
      if (is.null(l)) "" else as.character(l)
    }, character(1)),
    # XPT v5 has two storage types; dates are numeric with a date format.
    type = vapply(d, function(x) {
      if (is.character(x)) "character" else "numeric"
    }, character(1)),
    # Observed maximum byte length (at least 1); numerics are 8 bytes.
    length = vapply(d, function(x) {
      if (is.character(x)) max(1L, max(0L, nchar(x, type = "bytes"), na.rm = TRUE)) else 8L
    }, integer(1)),
    order = seq_along(d),
    # Display format for dates and datetimes.
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

# Column types are fixed so that format is never guessed as logical and
# order sorts numerically.
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

# =============================================================================
# 2. Spec coverage
# =============================================================================
# Every variable in the data must be in the spec and vice versa. xportr alone
# would only give an unspecified variable an empty label.
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

# =============================================================================
# 3. XPT v5 constraint audit
# =============================================================================
# Prints the measured maximum against each limit and stops on a violation.
# Adds a check xportr does not make: every variable has a non-empty label.
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
              # Count of empty labels; fails on an uncurated draft spec.
              sum(is.na(spec$label) | !nzchar(spec$label)),
              max(c(0L, nchar(spec$label)), na.rm = TRUE),
              sum(grepl("[^ -~]", spec$label)),
              maxb,
              nchar(ds_label),
              nchar(tolower(dataset_name))),
    limit = c(8, 0, 0, 40, 0, 200, 40, 8),
    stringsAsFactors = FALSE
  )
  # Rows with limit 0 count violations.
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

# =============================================================================
# 4. Write the transport files
# =============================================================================
# Each xportr step applies one attribute from the spec: type, SAS format,
# length, label, variable order; xportr_write() validates and writes XPT v5.
# strict_checks = TRUE stops on any validation failure (the default only
# warns). Dates are type "numeric" with format DATE9. in the spec; xportr
# treats type "date" as character.
export_xpt <- function(d, dataset_name) {
  path <- file.path(adam_dir, paste0(tolower(dataset_name), ".xpt"))

  # Collect warnings: a value wider than its declared length means the spec
  # and the file disagree.
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

# =============================================================================
# 5. Round-trip verification
# =============================================================================
# Read each file back. read_xpt() does not return declared character widths,
# so they are read from the file's NAMESTR header records (140 bytes per
# variable, big-endian):
#   bytes 1-2 type, 5-6 length, 7-8 varnum, 9-16 name, 17-56 label,
#   57-64 format name, 65-66 format width, 67-68 format decimals
read_namestr <- function(path) {
  raw <- readBin(path, "raw", n = file.size(path))
  # grepRaw because the file contains NUL bytes.
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

  # Values are compared by column name (xportr reorders columns). Character
  # NA is written as blank in XPT (SAS has no character missing), so NA -> ""
  # is the expected change; numerics must be identical.
  chr_vars <- names(original)[vapply(original, is.character, logical(1))]
  num_vars <- setdiff(names(original), chr_vars)

  # Strip attributes so only values are compared; labels are checked
  # against the spec below.
  bare <- function(x, mode) as.vector(x, mode)

  same_num <- all(vapply(num_vars, function(v) {
    # Date/POSIXct compare on the underlying numeric.
    isTRUE(all.equal(bare(original[[v]], "numeric"), bare(back[[v]], "numeric"),
                     tolerance = 1e-8))
  }, logical(1)))

  same_chr <- all(vapply(chr_vars, function(v) {
    a <- bare(original[[v]], "character")
    identical(replace(a, is.na(a), ""), bare(back[[v]], "character"))
  }, logical(1)))

  # Number of character cells affected by NA -> "".
  na_to_blank <- sum(vapply(chr_vars, function(v) {
    sum(is.na(original[[v]]) & bare(back[[v]], "character") == "")
  }, integer(1)))

  # Trailing blanks would be lost on read, so none may exist.
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
