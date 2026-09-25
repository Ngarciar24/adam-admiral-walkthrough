# -----------------------------------------------------------------------------
# Program    : 00_setup.R
# Study      : CDISCPILOT01 (public CDISC pilot test data, {pharmaversesdtm})
# Purpose    : Shared setup for all ADaM programs: load SDTM, convert blank
#              character values to NA, read the derivation metadata
# Inputs     : pharmaversesdtm::dm, ex, ds, ae, lb
#              metadata/adlb_params.csv, metadata/adsl_agegr1.csv
# Outputs    : SDTM and metadata data frames in the calling environment;
#              creates data/adam/
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-16
# Change log : 2026-09-16  IGR  Initial version
#              2026-09-25  IGR  Standard header; comments condensed
# -----------------------------------------------------------------------------

library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(stringr)
library(readr)

# --- Source SDTM -------------------------------------------------------------
data("dm", package = "pharmaversesdtm")
data("ex", package = "pharmaversesdtm")
data("ds", package = "pharmaversesdtm")
data("ae", package = "pharmaversesdtm")
data("lb", package = "pharmaversesdtm")

# --- Blank character values to NA --------------------------------------------
# SAS stores missing character values as "", which is.na() does not catch.
# No-op on pharmaversesdtm (already NA); required when reading SAS transport.
dm <- convert_blanks_to_na(dm)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)
lb <- convert_blanks_to_na(lb)

# --- Derivation metadata -----------------------------------------------------
adlb_params <- read_csv("metadata/adlb_params.csv", show_col_types = FALSE)
adsl_agegr1 <- read_csv("metadata/adsl_agegr1.csv", show_col_types = FALSE)

# --- Output location ---------------------------------------------------------
adam_dir <- "data/adam"
dir.create(adam_dir, recursive = TRUE, showWarnings = FALSE)
