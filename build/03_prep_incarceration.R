#!/usr/bin/env Rscript
# ============================================================================
# 03_prep_incarceration.R — Prepare incarceration data (used by both eras)
# Already handled in 02 for CMHC panel; this produces a standalone annual file.
# ============================================================================
log_section("03: Incarceration Data")

outfile <- file.path(DATA_DIR, "incarceration_annual.parquet")

if (file.exists(outfile) && !FORCE_REBUILD) {
  cat("  Output exists, skipping.\n")
} else {
  inc_file <- file.path(ROOT, "incarceration_trends_county.fst")
  if (!file.exists(inc_file)) {
    cat("  WARNING: incarceration_trends_county.fst not found. Skipping.\n")
  } else {
    inc <- fst::read_fst(inc_file, as.data.table = TRUE)
    inc[, fips := sprintf("%05d", as.integer(fips))]

    inc_annual <- inc[, .(
      total_pop           = mean(total_pop, na.rm = TRUE),
      total_pop_15to64    = mean(total_pop_15to64, na.rm = TRUE),
      total_jail_pop_rate = mean(total_jail_pop_rate, na.rm = TRUE),
      total_jail_pop      = mean(total_jail_pop, na.rm = TRUE),
      black_jail_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE),
      jail_pop_per_capita = mean(jail_pop_per_capita, na.rm = TRUE)
    ), by = .(fips, year)]

    # Clean NaN
    num_cols <- c("total_jail_pop_rate", "black_jail_pop_rate", "jail_pop_per_capita")
    inc_annual[, (num_cols) := lapply(.SD, function(x) fifelse(is.nan(x), NA_real_, x)),
                .SDcols = num_cols]

    arrow::write_parquet(inc_annual, outfile)
    cat(sprintf("  Incarceration annual: %d rows, %d counties, years %d-%d\n",
                nrow(inc_annual), uniqueN(inc_annual$fips),
                min(inc_annual$year), max(inc_annual$year)))
    cat(sprintf("  Saved: %s\n", outfile))
  }
}
