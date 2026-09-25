# -----------------------------------------------------------------------------
# Program    : 95_compare_pilot.R
# Study      : CDISCPILOT01 (public CDISC pilot test data)
# Purpose    : Compare this repository's ADaM datasets with the published
#              CDISC pilot ADaM datasets, variable by variable, and require
#              every difference to be explained in metadata/pilot_differences.csv
# Inputs     : data/adam/*.rds; published pilot ADSL, ADAE, ADLBC, ADLBH,
#              ADTTE, ADQSADAS (R/pilot_files.R, pinned and checksum-verified)
#              metadata/pilot_differences.csv
# Outputs    : outputs/qc/pilot_comparison.txt, outputs/qc/pilot_comparison.csv
# Author     : Ignacio G. Ribelles
# Created    : 2026-09-25
# Change log : 2026-09-25  IGR  Initial version
# Notes      : A comparison row fails when its difference count does not equal
#              the count recorded with the explanation, so both a new
#              difference and a silently removed one stop the run.
# -----------------------------------------------------------------------------

source("programs/00_setup.R")

ours <- lapply(
  c(adsl = "adsl", adae = "adae", adlb = "adlb", adtte = "adtte", adqsadas = "adqsadas"),
  function(d) readRDS(file.path(adam_dir, paste0(d, ".rds")))
)

# Published pilot datasets; blank character values to NA as for SDTM.
ref <- lapply(
  c(adsl = "ref_adsl", adae = "ref_adae", adlbc = "ref_adlbc", adlbh = "ref_adlbh",
    adtte = "ref_adtte", adqsadas = "ref_adqsadas"),
  function(f) convert_blanks_to_na(haven::read_xpt(fetch_pilot_file(f)))
)

expected <- read_csv(
  "metadata/pilot_differences.csv",
  col_types = cols(dataset = "c", item = "c", expected_differences = "i", explanation = "c")
)

# =============================================================================
# 1. Comparison function
# =============================================================================
# Joins on the keys and counts, per variable, the matched records whose
# values differ (numeric tolerance 1e-8; missing vs non-missing is a
# difference). Also counts records present on one side only.
compare_ds <- function(name, x, y, keys, vars) {
  x <- x %>% mutate(across(where(is.character), trimws))
  y <- y %>% mutate(across(where(is.character), trimws))
  j <- inner_join(x, y, by = keys, suffix = c(".ours", ".pilot"), relationship = "one-to-one")
  same <- function(a, b) {
    a <- as.vector(a); b <- as.vector(b)
    if (is.numeric(a) || is.numeric(b)) {
      a <- as.numeric(a); b <- as.numeric(b)
      (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < 1e-8)
    } else {
      (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
    }
  }
  per_var <- lapply(vars, function(v) {
    tibble(dataset = name, item = v, compared = nrow(j),
           differences = sum(!same(j[[paste0(v, ".ours")]], j[[paste0(v, ".pilot")]])))
  })
  bind_rows(
    tibble(dataset = name, item = "(records only in ours)", compared = nrow(x),
           differences = nrow(anti_join(x, y, by = keys))),
    tibble(dataset = name, item = "(records only in pilot)", compared = nrow(y),
           differences = nrow(anti_join(y, x, by = keys))),
    bind_rows(per_var)
  )
}

# =============================================================================
# 2. Dataset comparisons
# =============================================================================
# ADSL: the pilot contains randomised subjects only.
adsl_o <- ours$adsl %>% filter(ITTFL == "Y") %>% rename(TRTDUR = TRTDURD)
res_adsl <- compare_ds(
  "ADSL", adsl_o, ref$adsl, keys = "USUBJID",
  vars = c("SITEID", "SITEGR1", "AGE", "AGEGR1", "AGEGR1N", "SEX", "RACE",
           "TRT01P", "TRT01PN", "TRT01A", "TRT01AN", "TRTSDT", "TRTEDT", "TRTDUR",
           "SAFFL", "ITTFL", "EFFFL", "COMP24FL")
)

# ADAE: one record per AE in both; the pilot flags non-emergent events "N".
adae_p <- ref$adae %>% mutate(TRTEMFL = na_if(TRTEMFL, "N"))
res_adae <- compare_ds(
  "ADAE", ours$adae, adae_p, keys = c("USUBJID", "AESEQ"),
  vars = c("TRTA", "TRTAN", "ASTDT", "ASTDTF", "AENDT", "ASTDY", "AENDY", "ADURN",
           "TRTEMFL", "AOCCFL", "AOCCSFL", "AOCCPFL", "CQ01NAM", "AOCC01FL")
)

# ADLB vs ADLBC/ADLBH: source records only (the pilot's derived end-of-treatment
# records, AVISITN = 99, are excluded); pilot range indicators are L/N/H.
adlb_p <- bind_rows(ref$adlbc, ref$adlbh) %>%
  filter(PARAMCD %in% adlb_params$PARAMCD, is.na(AVISITN) | AVISITN != 99) %>%
  mutate(across(c(ANRIND, BNRIND), ~ recode(.x, L = "LOW", N = "NORMAL", H = "HIGH")))
res_adlb <- compare_ds(
  "ADLB", ours$adlb, adlb_p, keys = c("USUBJID", "PARAMCD", "LBSEQ"),
  vars = c("ADT", "ADY", "AVAL", "ABLFL", "BASE", "CHG", "ANRIND", "BNRIND")
)

# ADTTE: one record per subject in both.
res_adtte <- compare_ds(
  "ADTTE", ours$adtte, ref$adtte, keys = c("USUBJID", "PARAMCD"),
  vars = c("TRTA", "TRTAN", "STARTDT", "ADT", "AVAL", "CNSR", "SRCSEQ")
)

# ADQSADAS: analysed records (ANL01FL = "Y") of the ADAS-Cog(11) total.
anl <- function(d) d %>% filter(PARAMCD == "ACTOT", ANL01FL == "Y")
res_adqs <- bind_rows(
  compare_ds(
    "ADQSADAS", anl(ours$adqsadas), anl(ref$adqsadas), keys = c("USUBJID", "AVISITN"),
    vars = c("ADT", "ADY", "AVAL", "BASE", "CHG", "ABLFL", "DTYPE", "QSSEQ", "EFFFL", "COMP24FL")
  ),
  tibble(
    dataset = "ADQSADAS", item = "(LOCF records all)",
    compared = sum(ours$adqsadas$DTYPE %in% "LOCF"),
    differences = abs(sum(ref$adqsadas$PARAMCD == "ACTOT" & ref$adqsadas$DTYPE %in% "LOCF") -
                        sum(ours$adqsadas$DTYPE %in% "LOCF"))
  )
)

results <- bind_rows(res_adsl, res_adae, res_adlb, res_adtte, res_adqs) %>%
  left_join(expected, by = c("dataset", "item")) %>%
  mutate(
    expected_differences = coalesce(expected_differences, 0L),
    status = if_else(differences == expected_differences,
                     if_else(differences == 0, "match", "explained"), "UNEXPLAINED")
  )

# =============================================================================
# 3. Report
# =============================================================================
qc_dir <- "outputs/qc"
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(results, file.path(qc_dir, "pilot_comparison.csv"), na = "")

lines <- c(
  "COMPARISON WITH THE PUBLISHED CDISC PILOT ADaM DATASETS",
  paste0("Reference: phuse-org/phuse-scripts@", pilot_commit, ", data/adam/cdiscpilot01/"),
  "Each row: records compared and records that differ. Explanations: metadata/pilot_differences.csv",
  ""
)
for (d in unique(results$dataset)) {
  r <- results %>% filter(dataset == d)
  lines <- c(lines, d, sprintf("  %-26s %8s %8s  %-11s %s", "item", "compared", "differ", "status", "explanation"))
  lines <- c(lines, sprintf("  %-26s %8d %8d  %-11s %s", r$item, r$compared, r$differences, r$status,
                            coalesce(r$explanation, "")), "")
}
n_bad <- sum(results$status == "UNEXPLAINED")
lines <- c(lines, sprintf("Items compared: %d; matching: %d; explained differences: %d; unexplained: %d",
                          nrow(results), sum(results$status == "match"),
                          sum(results$status == "explained"), n_bad))
writeLines(lines, file.path(qc_dir, "pilot_comparison.txt"))
writeLines(lines)

if (n_bad > 0) {
  stop(n_bad, " unexplained difference(s) against the pilot; see outputs/qc/pilot_comparison.txt")
}
