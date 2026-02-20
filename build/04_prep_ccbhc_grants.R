#!/usr/bin/env Rscript
# ============================================================================
# 04_prep_ccbhc_grants.R — Extract CCBHC treatment timing from USAspending
# ============================================================================
log_section("04: CCBHC Grants")

outfile <- file.path(DATA_DIR, "ccbhc_treatment.csv")

if (file.exists(outfile) && !FORCE_REBUILD) {
  cat("  Output exists, skipping.\n")
} else {

  # Load 93.829 grants
  g1_files <- list.files(CCBHC_DIR, pattern = "93\\.829", full.names = TRUE)
  if (length(g1_files) == 0) stop("93.829 grant file not found in ", CCBHC_DIR)
  grants1 <- data.table::fread(g1_files[1])

  # Identify planning grant states BEFORE filtering
  planning_grant_st_fips <- grants1[
    period_of_performance_start_date == "2015-10-23",
    unique(sprintf("%02d", prime_award_summary_recipient_state_fips_code))
  ]
  cat(sprintf("  Planning grant states: %s\n", paste(planning_grant_st_fips, collapse = ", ")))

  # Drop planning grants (start year < 2017)
  grants1 <- grants1[as.integer(substr(period_of_performance_start_date, 1, 4)) >= 2017]

  # Load 93.696 grants
  g2_files <- list.files(CCBHC_DIR, pattern = "93\\.696", full.names = TRUE)
  if (length(g2_files) > 0) {
    grants2 <- data.table::fread(g2_files[1])
    grants <- rbindlist(list(grants1, grants2), fill = TRUE)
  } else {
    grants <- grants1
  }

  cat(sprintf("  Loaded %d grant records\n", nrow(grants)))

  # Extract treatment year and county FIPS
  grants[, treat_year := as.integer(substr(period_of_performance_start_date, 1, 4))]
  grants[, fips := sprintf("%05d", prime_award_summary_recipient_county_fips_code)]

  # First grant per county
  treat <- grants[!is.na(treat_year) & !grepl("NA", fips),
                  .(treat_year = min(treat_year)), by = fips]
  treat <- treat[treat_year %in% CCBHC_TREAT_YEARS]

  # Add planning grant state flag
  treat[, stfips := substr(fips, 1, 2)]
  treat[, planning_state := as.integer(stfips %in% planning_grant_st_fips)]
  treat[, waiver_state := as.integer(stfips %in% WAIVER_ST_FIPS)]

  cat(sprintf("  Unique treated counties: %d\n", nrow(treat)))
  cat("  Treatment year distribution:\n")
  print(table(treat$treat_year))

  # Save planning grant state list for use by other scripts
  write_csv(data.frame(stfips = planning_grant_st_fips),
            file.path(DATA_DIR, "planning_grant_states.csv"))

  fwrite(treat, outfile)
  cat(sprintf("  Saved: %s\n", outfile))
}
