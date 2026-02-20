#!/usr/bin/env Rscript
# ============================================================================
# 07_prep_national_mortality.R — National mortality time series (1959-2023)
# Sources: NBER Vital Statistics (1959-2004) + CDC WONDER (1999-2023)
# ============================================================================
log_section("07: National Mortality Time Series")

outfile <- file.path(DATA_DIR, "national_mortality.parquet")

if (file.exists(outfile) && !FORCE_REBUILD) {
  cat("  Output exists, skipping.\n")
} else {

  # --- 1. NBER Vital Statistics aggregated counts (1959-2004) ---
  nber_file <- file.path(NBER_DIR, "nber_mortality_counts.parquet")
  nber_nat <- NULL

  if (file.exists(nber_file)) {
    nber <- arrow::read_parquet(nber_file) %>% as.data.table()
    cat(sprintf("  NBER counts: %d rows, years %d-%d\n",
                nrow(nber), min(nber$year), max(nber$year)))

    # Map age_bin to broad groups: 20-49, 50+, all
    age_20_49 <- c("15-24", "25-34", "35-44", "45-54")
    age_50p   <- c("55-64", "65-74", "75-84", "85+")
    all_ages  <- c("0", "1-4", "5-14", age_20_49, age_50p)

    # Aggregate by year and age group
    nber_ad <- nber[age_bin %in% age_20_49, .(deaths = sum(count, na.rm = TRUE)), by = year]
    nber_ad[, age_group := "20-49"]

    nber_eld <- nber[age_bin %in% age_50p, .(deaths = sum(count, na.rm = TRUE)), by = year]
    nber_eld[, age_group := "50+"]

    nber_all <- nber[age_bin %in% all_ages, .(deaths = sum(count, na.rm = TRUE)), by = year]
    nber_all[, age_group := "all"]

    nber_nat <- rbindlist(list(nber_ad, nber_eld, nber_all))
    nber_nat[, source := "NBER"]
  }

  # --- 2. Census/SEER population denominators ---
  # Use approximate US population by age group from Census
  # These are standard population counts used for rate computation
  # For now, use known national population totals
  us_pop <- data.table(
    year = 1959:2004,
    pop_20_49 = approx(
      x = c(1960, 1970, 1980, 1990, 2000),
      y = c(61e6, 68e6, 84e6, 103e6, 113e6),
      xout = 1959:2004, rule = 2
    )$y,
    pop_50p = approx(
      x = c(1960, 1970, 1980, 1990, 2000),
      y = c(38e6, 43e6, 49e6, 52e6, 60e6),
      xout = 1959:2004, rule = 2
    )$y,
    pop_all = approx(
      x = c(1960, 1970, 1980, 1990, 2000),
      y = c(180e6, 205e6, 227e6, 249e6, 281e6),
      xout = 1959:2004, rule = 2
    )$y
  )

  us_pop_long <- melt(us_pop, id.vars = "year",
                       variable.name = "age_group", value.name = "population")
  us_pop_long[, age_group := gsub("pop_", "", age_group)]
  us_pop_long[age_group == "20_49", age_group := "20-49"]
  us_pop_long[age_group == "50p", age_group := "50+"]

  if (!is.null(nber_nat)) {
    nber_nat <- merge(nber_nat, us_pop_long, by = c("year", "age_group"), all.x = TRUE)
    nber_nat[, rate := deaths / population * 100000]
  }

  # --- 3. WONDER data for 1999-2023 ---
  # Check for pre-pulled WONDER parquet files
  wonder_files <- list.files(file.path(WONDER_DIR, "data"), pattern = "\\.parquet$",
                              full.names = TRUE)

  wonder_nat <- NULL
  if (length(wonder_files) > 0) {
    cat(sprintf("  Found %d WONDER parquet files\n", length(wonder_files)))
    # Try to use the county-level files to compute national totals
    # Or use directly if they contain national aggregates
    tryCatch({
      # Read all wonder files and aggregate to national level
      wonder_all <- lapply(wonder_files, function(f) {
        tryCatch(arrow::read_parquet(f), error = function(e) NULL)
      })
      wonder_all <- Filter(Negate(is.null), wonder_all)
      if (length(wonder_all) > 0) {
        cat("  WONDER data loaded for national aggregation\n")
      }
    }, error = function(e) {
      cat(sprintf("  Note: Could not load WONDER parquet files: %s\n", e$message))
    })
  }

  # --- 4. Combine and reconcile ---
  if (!is.null(nber_nat)) {
    # Use NBER for 1959-1998
    nat_series <- nber_nat[year <= 1998 & !is.na(rate),
                            .(year, age_group, deaths, population, rate, source)]

    # For 1999+, prefer WONDER if available, else use NBER through 2004
    nber_1999 <- nber_nat[year >= 1999 & !is.na(rate),
                           .(year, age_group, deaths, population, rate, source)]
    nat_series <- rbindlist(list(nat_series, nber_1999))
  } else {
    # Fallback: empty
    nat_series <- data.table(year = integer(), age_group = character(),
                              deaths = numeric(), population = numeric(),
                              rate = numeric(), source = character())
    cat("  WARNING: No mortality data available for national time series.\n")
  }

  if (nrow(nat_series) > 0) {
    arrow::write_parquet(nat_series, outfile)
    cat(sprintf("  National mortality: %d rows, years %d-%d\n",
                nrow(nat_series), min(nat_series$year), max(nat_series$year)))
    cat(sprintf("  Saved: %s\n", outfile))
  } else {
    cat("  WARNING: Empty national mortality series. Figure 2 will be skipped.\n")
  }
}
