#!/usr/bin/env Rscript
# ============================================================================
# 06_prep_ccbhc_panel.R — Build CCBHC-era master panel (2010-2024)
# Merges: incarceration + WONDER mortality + CCBHC grants + SAMHSA facilities
# ============================================================================
log_section("06: CCBHC Panel")

outfile <- file.path(DATA_DIR, "ccbhc_panel.parquet")

if (file.exists(outfile) && !FORCE_REBUILD) {
  cat("  Output exists, skipping.\n")
} else {

  # 1. Load CCBHC treatment timing
  treat <- fread(file.path(DATA_DIR, "ccbhc_treatment.csv"))
  cat(sprintf("  CCBHC treated counties: %d\n", nrow(treat)))

  # 2. Load incarceration data (annualized)
  inc_file <- file.path(DATA_DIR, "incarceration_annual.parquet")
  if (file.exists(inc_file)) {
    inc <- arrow::read_parquet(inc_file) %>% as.data.table()
  } else {
    inc_raw <- fst::read_fst(file.path(ROOT, "incarceration_trends_county.fst"), as.data.table = TRUE)
    inc_raw[, fips := sprintf("%05d", as.integer(fips))]
    inc <- inc_raw[, .(
      total_pop            = mean(total_pop, na.rm = TRUE),
      total_jail_pop_rate  = mean(total_jail_pop_rate, na.rm = TRUE)
    ), by = .(fips, year)]
  }

  # 3. Load WONDER mortality
  mort <- arrow::read_parquet(file.path(DATA_DIR, "ccbhc_mortality.parquet")) %>% as.data.table()

  # 4. Load SAMHSA facility directory (2017 cross-section)
  fac_file <- file.path(CCBHC_DIR, "directories", "facilities_geocoded.rds")
  services_by_county <- NULL
  if (file.exists(fac_file)) {
    fac <- readRDS(fac_file)
    setDT(fac)
    fac17 <- fac[year == 2017, .(county_fips, facility_name, services)] %>% unique()

    wide <- fac17[, .(service = unlist(tstrsplit(services, "\\s+"))), by = .I][, val := 1L] %>%
      dcast(I ~ service, value.var = "val", fill = 0L)

    fac17 <- cbind(fac17, wide[, -"I"])

    # Safely sum services (handle missing columns)
    safe_sum <- function(dt, cols) {
      existing <- intersect(cols, names(dt))
      if (length(existing) == 0) return(rep(0L, nrow(dt)))
      rowSums(dt[, ..existing], na.rm = TRUE)
    }

    services_by_county <- fac17[, .(
      n_omh_2017        = safe_sum(.SD, "OMH"),
      n_act_2017        = safe_sum(.SD, "ACT"),
      n_cmhc_2017       = safe_sum(.SD, "CMHC"),
      n_therapy_2017    = safe_sum(.SD, c("CBT", "DBT", "IPT", "GT")),
      n_case_mgmt_2017  = safe_sum(.SD, c("CM", "ICM")),
      n_crisis_2017     = safe_sum(.SD, c("CIT", "CFT", "ES")),
      n_residential_2017 = safe_sum(.SD, c("OSF", "SNR", "TCC"))
    ), by = .(fips = county_fips)]

    cat(sprintf("  SAMHSA facilities (2017): %d counties\n", nrow(services_by_county)))
  }

  # 5. Build incarceration panel component
  inc_panel <- inc[year >= CCBHC_PANEL_START, .(fips, year,
    total_pop_inc = total_pop,
    jail_pop_rate = total_jail_pop_rate)]
  inc_panel[, log_jail_pop_rate := log(jail_pop_rate)]
  inc_panel[is.infinite(log_jail_pop_rate), log_jail_pop_rate := NA_real_]

  # 6. Build mortality panel component
  mort_panel <- mort[, .(fips, year,
    total_pop_mort = Population,
    mort_rate, Deaths)]
  if ("mort_rate_f" %in% names(mort)) {
    mort_panel <- merge(mort_panel, mort[, .(fips, year, mort_rate_f)], by = c("fips", "year"), all.x = TRUE)
  }
  if ("mort_rate_external" %in% names(mort)) {
    mort_panel <- merge(mort_panel, mort[, .(fips, year, mort_rate_external)], by = c("fips", "year"), all.x = TRUE)
  }

  # 7. Merge: start with incarceration, add mortality, add treatment
  panel <- merge(inc_panel, mort_panel, by = c("fips", "year"), all = TRUE)

  # Coalesce population
  panel[, total_pop := fcoalesce(total_pop_mort, total_pop_inc)]
  panel[, c("total_pop_inc", "total_pop_mort") := NULL]

  # Merge treatment
  panel <- merge(panel, treat[, .(fips, treat_year, planning_state, waiver_state)],
                 by = "fips", all.x = TRUE)
  panel[, treated := as.integer(!is.na(treat_year))]
  panel[, stfips := substr(fips, 1, 2)]

  # Event time
  panel[, event_time := year - treat_year]
  panel[, event_time_binned := fcase(
    is.na(event_time),               -999L,
    event_time <= CCBHC_EVENT_MIN,   CCBHC_EVENT_MIN,
    event_time >= CCBHC_EVENT_MAX,   CCBHC_EVENT_MAX,
    default = as.integer(event_time)
  )]

  # Fill planning/waiver flags for control counties
  panel[is.na(planning_state), planning_state := 0L]
  panel[is.na(waiver_state), waiver_state := 0L]

  # Merge facility data
  if (!is.null(services_by_county)) {
    panel <- merge(panel, services_by_county, by = "fips", all.x = TRUE)
    fill_cols <- grep("^n_.*_2017$", names(panel), value = TRUE)
    panel[, (fill_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0L, as.integer(x))),
           .SDcols = fill_cols]
  }

  arrow::write_parquet(panel, outfile)
  cat(sprintf("  CCBHC panel: %d rows, %d counties, years %d-%d\n",
              nrow(panel), uniqueN(panel$fips),
              min(panel$year, na.rm = TRUE), max(panel$year, na.rm = TRUE)))
  cat(sprintf("  Treated: %d, Control: %d\n",
              uniqueN(panel[treated == 1, fips]),
              uniqueN(panel[treated == 0, fips])))
  cat(sprintf("  Saved: %s\n", outfile))
}
