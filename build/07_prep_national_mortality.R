#!/usr/bin/env Rscript
# ============================================================================
# 07_prep_national_mortality.R — National mortality time series (1959-2023)
# Sources: NBER Vital Statistics (1959-2004) + CDC WONDER (1999-2023)
# ============================================================================
log_section("07: National Mortality Time Series")

outfile <- file.path(DATA_DIR, "national_mortality.parquet")

nber_years = list(
    `0` = 1970, `1` = 1971, `2` = 1972, `3` = 1973, `4` = 1974, `5` = 1975, `6` = 1976, `7` = 1977, `8` = 1978,
    `79` = 1979, `80` = 1980, `81` = 1981, `82` = 1982, `83` = 1983, `84` = 1984, `85` = 1985, `86` = 1986, `87` = 1987,
    `88` = 1988, `89` = 1989, `90` = 1990, `91` = 1991, `92` = 1992, `93` = 1993, `94` = 1994, `95` = 1995)


if (file.exists(outfile) && !FORCE_REBUILD) {
  cat("  Output exists, skipping.\n")
} else {

  # --- 1. NBER Vital Statistics aggregated counts (1959-2004) ---
  nber_file <- file.path(NBER_DIR, "nber_mortality_counts.parquet")
  nber_nat <- NULL

  if (file.exists(nber_file)) {
    nber <- arrow::read_parquet(nber_file) %>% as.data.table()
    nber[, year := as.integer(year)]
    # replace year with nber_years mapping
    nber[as.character(year) %in% names(nber_years), 
      year := as.integer(nber_years[as.character(year)])]
    nber <- nber[!is.na(year) & year >= 1959 & year <= 2004]
    cat(sprintf("  NBER counts: %d rows, years %d-%d\n",
                nrow(nber), min(nber$year), max(nber$year)))

    # Map age_bin to broad groups: 20-49, 50+, all
    age_20_49 <- c("15-24", "25-34", "35-44", "45-54")
    age_50p   <- c("55-64", "65-74", "75-84", "85+")
    all_ages  <- c("0", "1-4", "5-14", age_20_49, age_50p)

    # Aggregate by year and age group
    nber_ad <- nber[age_bin %in% age_20_49, .(deaths = sum(deaths, na.rm = TRUE)), by = year]
    nber_ad[, age_group := "20-49"]

    nber_eld <- nber[age_bin %in% age_50p, .(deaths = sum(deaths, na.rm = TRUE)), by = year]
    nber_eld[, age_group := "50+"]

    nber_all <- nber[age_bin %in% all_ages, .(deaths = sum(deaths, na.rm = TRUE)), by = year]
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

  # --- 3. WONDER data for 1999-2025 (ages 20-49 all-cause + external causes) ---
  wonder_nat <- NULL
  
  # All-cause mortality (ages 20-49)
  all_final <- file.path(WONDER_DIR, "data/alladults.parquet")
  all_prov  <- file.path(WONDER_DIR, "data/alladults_provisional.parquet")
  
  wonder_all_parts <- list()
  if (file.exists(all_final)) {
    d <- arrow::read_parquet(all_final) %>% as.data.table()
    d[, year := as.integer(Year)]
    wonder_all_parts[["final"]] <- d[, .(deaths = sum(as.numeric(Deaths), na.rm = TRUE),
                                          population = sum(as.numeric(Population), na.rm = TRUE)), by = year]
    cat(sprintf("  WONDER all-cause final: %d years (%d-%d)\n",
                uniqueN(d$year), min(d$year), max(d$year)))
  }
  if (file.exists(all_prov)) {
    d <- arrow::read_parquet(all_prov) %>% as.data.table()
    d[, year := as.integer(Year)]
    wonder_all_parts[["prov"]] <- d[, .(deaths = sum(as.numeric(Deaths), na.rm = TRUE),
                                         population = sum(as.numeric(Population), na.rm = TRUE)), by = year]
    cat(sprintf("  WONDER all-cause prov: %d years (%d-%d)\n",
                uniqueN(d$year), min(d$year), max(d$year)))
  }
  
  if (length(wonder_all_parts) > 0) {
    wonder_all <- rbindlist(wonder_all_parts, idcol = "source")
    wonder_all <- wonder_all[order(year, match(source, c("final", "prov")))]
    wonder_all <- unique(wonder_all, by = "year", fromLast = FALSE)
    wonder_all[, source := NULL]
    wonder_all[, `:=`(rate = deaths / population * 100000,
                      age_group = "20-49",
                      source = "WONDER")]
    
    # External causes (DoD)
    dod_final <- file.path(WONDER_DIR, "data/dod_alladults.parquet")
    dod_prov  <- file.path(WONDER_DIR, "data/dod_alladults_provisional.parquet")
    
    dod_parts <- list()
    if (file.exists(dod_final)) {
      d <- arrow::read_parquet(dod_final) %>% as.data.table()
      d[, year := as.integer(Year)]
      dod_parts[["final"]] <- d[, .(deaths = sum(as.numeric(Deaths), na.rm = TRUE),
                                     population = sum(as.numeric(Population), na.rm = TRUE)), by = year]
    }
    if (file.exists(dod_prov)) {
      d <- arrow::read_parquet(dod_prov) %>% as.data.table()
      d[, year := as.integer(Year)]
      dod_parts[["prov"]] <- d[, .(deaths = sum(as.numeric(Deaths), na.rm = TRUE),
                                    population = sum(as.numeric(Population), na.rm = TRUE)), by = year]
    }
    
    if (length(dod_parts) > 0) {
      wonder_dod <- rbindlist(dod_parts, idcol = "source")
      wonder_dod <- wonder_dod[order(year, match(source, c("final", "prov")))]
      wonder_dod <- unique(wonder_dod, by = "year", fromLast = FALSE)
      wonder_dod[, source := NULL]
      wonder_dod[, `:=`(rate = deaths / population * 100000,
                        age_group = "external",
                        source = "WONDER")]
      
      wonder_nat <- rbindlist(list(wonder_all, wonder_dod), use.names = TRUE)
      cat(sprintf("  WONDER combined: %d rows (all-cause + external)\n", nrow(wonder_nat)))
    } else {
      wonder_nat <- wonder_all
      cat("  WONDER all-cause only (no external causes data)\n")
    }
  }

  # --- 4. Combine and reconcile ---
  parts <- list()
  
  if (!is.null(nber_nat)) {
    # Use NBER for 1959-1998 (20-49 age group only)
    parts[["nber"]] <- nber_nat[year <= 1998 & age_group == "20-49" & !is.na(rate),
                                  .(year, age_group, deaths, population, rate, source)]
  }
  
  if (!is.null(wonder_nat)) {
    # WONDER for 1999+
    parts[["wonder"]] <- wonder_nat[year >= 1999 & !is.na(rate),
                                      .(year, age_group, deaths, population, rate, source)]
  } else if (!is.null(nber_nat)) {
    # Fallback: use NBER through 2004 if no WONDER
    parts[["nber_late"]] <- nber_nat[year >= 1999 & year <= 2004 & age_group == "20-49" & !is.na(rate),
                                       .(year, age_group, deaths, population, rate, source)]
  }
  
  if (length(parts) > 0) {
    nat_series <- rbindlist(parts, use.names = TRUE)
  } else {
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
