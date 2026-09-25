# -----------------------------------------------------------------------------
# Program    : pilot_files.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Download files of the published CDISC pilot submission from the
#              PHUSE phuse-scripts repository at a pinned commit, verify their
#              SHA-256 checksums, and cache them under data/external/
# Inputs     : metadata/pilot_files.csv (file name, repository path, checksum)
# Outputs    : fetch_pilot_file(name) returns the local path of a verified file
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-25
# Change log : 2026-09-25  IGR  Initial version
# Notes      : The files are not committed (the source repository states no
#              licence). A checksum mismatch stops the run, so a changed
#              upstream file can never enter the analysis unnoticed.
# -----------------------------------------------------------------------------

pilot_commit <- "398a6d33ced9359ffb58c46650a6d488811176b1"
pilot_base_url <- paste0(
  "https://raw.githubusercontent.com/phuse-org/phuse-scripts/", pilot_commit, "/"
)

fetch_pilot_file <- function(name, cache_dir = "data/external") {
  files <- utils::read.csv("metadata/pilot_files.csv", stringsAsFactors = FALSE)
  row <- files[files$name == name, ]
  if (nrow(row) != 1) stop("Unknown pilot file: ", name)

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(cache_dir, paste0(name, ".", tools::file_ext(row$path)))

  if (!file.exists(dest) || digest::digest(file = dest, algo = "sha256") != row$sha256) {
    message("Downloading ", row$path, " (phuse-scripts@", substr(pilot_commit, 1, 7), ")")
    utils::download.file(paste0(pilot_base_url, row$path), dest, mode = "wb", quiet = TRUE)
  }

  got <- digest::digest(file = dest, algo = "sha256")
  if (got != row$sha256) {
    stop("Checksum mismatch for ", row$path, ": expected ", row$sha256, ", got ", got)
  }
  dest
}
