# ---------------------------------------------------------------------------
# 00_setup.R -- shared setup for every ADaM program in this repository
#
# Loads the public CDISC pilot SDTM test data from {pharmaversesdtm} and applies
# the one transformation that must happen before any derivation: blanks -> NA.
# ---------------------------------------------------------------------------

library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(stringr)
library(readr)

# --- Source SDTM ----------------------------------------------------------
# Study CDISCPILOT01. This is public test data shipped inside an R package, NOT
# data from a real trial. In a real study these would be read from the SDTM
# submission datasets (usually .xpt or .sas7bdat via haven::read_xpt()).
data("dm", package = "pharmaversesdtm")
data("ex", package = "pharmaversesdtm")
data("ds", package = "pharmaversesdtm")
data("ae", package = "pharmaversesdtm")
data("lb", package = "pharmaversesdtm")

# --- Blanks to NA ---------------------------------------------------------
# SAS has no NULL for character: an unset character value is the empty string.
# When SDTM arrives via haven::read_sas()/read_xpt() those become "" in R, and
# "" is NOT caught by is.na(). Every admiral pipeline therefore starts here.
#
# NOTE for the reader: on pharmaversesdtm as shipped this is a NO-OP -- the data
# already uses NA. It is kept because it is the correct habit and because it is
# load-bearing the moment the source is a real SAS transport file.
# It converts "" only; it does NOT trim whitespace, so " " survives.
dm <- convert_blanks_to_na(dm)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)
lb <- convert_blanks_to_na(lb)

# --- Metadata (a miniature, hand-written "spec") --------------------------
# In a real study this information lives in the dataset specification that is
# also the source of define.xml. Keeping it as data, not as hard-coded case_when
# branches, is what lets the spec be reviewed independently of the code.
adlb_params <- read_csv("metadata/adlb_params.csv", show_col_types = FALSE)
adsl_agegr1 <- read_csv("metadata/adsl_agegr1.csv", show_col_types = FALSE)

# --- Output location ------------------------------------------------------
adam_dir <- "data/adam"
dir.create(adam_dir, recursive = TRUE, showWarnings = FALSE)
