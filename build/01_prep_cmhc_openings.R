#!/usr/bin/env Rscript
# ============================================================================
# 01_prep_cmhc_openings.R — Clean CMHC treatment timing
# ============================================================================
log_section("01: CMHC Openings")

outfile <- file.path(DATA_DIR, "cmhc_openings.csv")

if (file.exists(outfile) && !FORCE_REBUILD) {
  cat("  Output exists, skipping.\n")
} else {
  cmhc_openings <- read_csv(file.path(CMHC_DIR, "cmhc_openings.csv"), show_col_types = FALSE)
  cat(sprintf("  Loaded %d counties with CMHC openings\n", nrow(cmhc_openings)))
  cat("  Year distribution:\n")
  print(table(cmhc_openings$cmhc_year_exp))
  write_csv(cmhc_openings, outfile)
  cat(sprintf("  Saved: %s\n", outfile))
}
