#!/usr/bin/env Rscript
# ============================================================================
# 05_prep_ccbhc_mortality.R — Load and clean WONDER mortality for CCBHC era
# ============================================================================
log_section("05: CCBHC Mortality (WONDER)")

outfile <- file.path(DATA_DIR, "ccbhc_mortality.parquet")

if (file.exists(outfile) && !FORCE_REBUILD) {
  cat("  Output exists, skipping.\n")
} else {

  na_vals <- c("", "Suppressed", "Unreliable", "Not Applicable")

  # --- All-cause mortality, ages 25-44 ---
  d1 <- fread(file.path(CCBHC_DIR, "Deaths_2010-2015_25-44_MCOD.csv"),
              na.strings = na_vals)
  d2 <- fread(file.path(CCBHC_DIR, "Deaths_2016-2020_25-44_MCOD.csv"),
              na.strings = na_vals)

  # Provisional data
  d4_file <- file.path(CCBHC_DIR, "Provisional Mortality Statistics, 2018 through Last Week.csv")
  d4 <- fread(d4_file, na.strings = na_vals, fill = TRUE)

  # Standardize column names
  setnames(d1, c("County Code", "Year Code"), c("fips", "year"))
  setnames(d2, c("County Code", "Year Code"), c("fips", "year"))
  setnames(d4, c("Residence County Code", "Year Code"), c("fips", "year"))

  d1[, year := as.integer(year)]
  d2[, year := as.integer(year)]
  d4[, year := as.integer(trimws(as.character(year)))]
  d4 <- d4[year %in% 2021:2025]

  # Combine
  d1_sub <- d1[!is.na(fips), .(fips, year,
               Deaths = as.numeric(Deaths), Population = as.numeric(Population))]
  d2_sub <- d2[!is.na(fips), .(fips, year,
               Deaths = as.numeric(Deaths), Population = as.numeric(Population))]
  d4_sub <- d4[!is.na(fips), .(fips, year,
               Deaths = as.numeric(Deaths))]
  d4_sub[, Population := NA_real_]

  mort <- rbindlist(list(d1_sub, d2_sub, d4_sub))

  # Impute population for 2021+ using county linear trend from 2010-2020
  pop_data <- mort[!is.na(Population), .(fips, year, Population)]
  pop_models <- pop_data[, {
    if (.N >= 2) {
      m <- lm(Population ~ year)
      .(intercept = coef(m)[1], slope = coef(m)[2])
    } else {
      .(intercept = NA_real_, slope = NA_real_)
    }
  }, by = fips]

  impute_grid <- CJ(fips = pop_models[!is.na(slope), fips], year = 2021:2025)
  impute_grid <- merge(impute_grid, pop_models, by = "fips")
  impute_grid[, Population_imp := intercept + slope * year]
  impute_grid[Population_imp < 0, Population_imp := NA_real_]

  mort <- merge(mort, impute_grid[, .(fips, year, Population_imp)],
                by = c("fips", "year"), all.x = TRUE)
  mort[is.na(Population) & !is.na(Population_imp), Population := Population_imp]
  mort[, Population_imp := NULL]

  cat(sprintf("  Population imputed for %d county-years (2021+)\n",
              mort[year >= 2021 & !is.na(Population), .N]))

  # Calculate death rate per 100k
  mort[, mort_rate := Deaths / Population * 100000]
  mort <- mort[!is.na(mort_rate) & Population > 0]
  mort[, fips := sprintf(fips, fmt = "%05d")]

  # --- Cause-specific mortality ---
  # ICD-F (mental/behavioral)
  load_cause <- function(hist_file, prov_file, hist_years, prov_years) {
    d_h <- tryCatch(
      fread(file.path(CCBHC_DIR, hist_file), na.strings = na_vals, fill = TRUE),
      error = function(e) NULL
    )
    d_p <- tryCatch(
      fread(file.path(CCBHC_DIR, prov_file), na.strings = na_vals, fill = TRUE),
      error = function(e) NULL
    )

    parts <- list()
    if (!is.null(d_h)) {
      setnames(d_h, c("County Code", "Year Code"), c("fips", "year"), skip_absent = TRUE)
      d_h[, year := as.integer(year)]
      d_h <- d_h[year %in% hist_years & !is.na(fips)]
      parts <- c(parts, list(d_h[, .(fips, year, Deaths = as.numeric(Deaths))]))
    }
    if (!is.null(d_p)) {
      setnames(d_p, c("Residence County Code", "Year Code"), c("fips", "year"), skip_absent = TRUE)
      d_p[, year := as.integer(trimws(as.character(year)))]
      d_p <- d_p[year %in% prov_years & !is.na(fips)]
      parts <- c(parts, list(d_p[, .(fips, year, Deaths = as.numeric(Deaths))]))
    }
    if (length(parts) > 0) rbindlist(parts) else NULL
  }

  mort_f <- load_cause("Mortality_ICD_F.csv", "Provisional_Mortality_ICD_F.csv",
                         2012:2017, 2018:2025)
  mort_uv <- load_cause("Mortality_U_V_codes.csv", "Mortality_U_V_codes_2018-25.csv",
                          2012:2017, 2018:2025)

  # Merge cause-specific onto main mortality
  if (!is.null(mort_f)) {
    mort_f[, fips := sprintf(fips, fmt = "%05d")]
    setnames(mort_f, "Deaths", "deaths_f")
    mort <- merge(mort, mort_f, by = c("fips", "year"), all.x = TRUE)
  }
  if (!is.null(mort_uv)) {
    mort_uv[, fips := sprintf(fips, fmt = "%05d")]
    setnames(mort_uv, "Deaths", "deaths_external")
    mort <- merge(mort, mort_uv, by = c("fips", "year"), all.x = TRUE)
  }

  # Compute cause-specific rates
  if ("deaths_f" %in% names(mort)) {
    mort[, mort_rate_f := deaths_f / Population * 100000]
  }
  if ("deaths_external" %in% names(mort)) {
    mort[, mort_rate_external := deaths_external / Population * 100000]
  }

  arrow::write_parquet(mort, outfile)
  cat(sprintf("  CCBHC mortality: %d county-years, %d counties, years %d-%d\n",
              nrow(mort), uniqueN(mort$fips), min(mort$year), max(mort$year)))
  cat(sprintf("  Saved: %s\n", outfile))
}
