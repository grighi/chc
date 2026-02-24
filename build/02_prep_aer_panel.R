#!/usr/bin/env Rscript
# ============================================================================
# 02_prep_aer_panel.R — Build CMHC-era master panel (1959-1988)
# Merges: AER data + CMHC openings + priority ranks + pscore data + incarceration
# ============================================================================
log_section("02: AER Panel (CMHC Era)")

outfile <- file.path(DATA_DIR, "cmhc_panel.parquet")

if (file.exists(outfile) && !FORCE_REBUILD) {
  cat("  Output exists, skipping.\n")
} else {

  # 1. Load AER base data
  cat("  Loading AER data...\n")
  data <- fst::read_fst(file.path(AER_DIR, "aer_data.fst")) %>% as_tibble()
  cat(sprintf("  AER data: %d rows, %d counties\n", nrow(data), n_distinct(data$fips)))

  # 2. Load and merge CMHC openings
  cmhc_openings <- read_csv(file.path(DATA_DIR, "cmhc_openings.csv"), show_col_types = FALSE)
  data <- data %>% left_join(cmhc_openings, by = "fips")

  # 3. Load and merge priority ranks
  priority_file <- file.path(CMHC_DIR, "priority_ranks_for_eventstudy.csv")
  if (file.exists(priority_file)) {
    priority <- read_csv(priority_file, show_col_types = FALSE)
    data <- data %>%
      left_join(priority %>% select(fips, priority_pctile, priority_tercile,
                                     high_priority, area_has_cmhc, high_priority_no_cmhc,
                                     area_cmhc_year),
                by = "fips")
    cat(sprintf("  Priority data merged: %d counties\n", nrow(priority)))
  }

  # 4. Load and merge propensity score data
  pscore_file <- file.path(AER_DIR, "aer_pscore_data.dta")
  if (file.exists(pscore_file)) {
    pscore <- haven::read_dta(pscore_file)
    # Create fips from stfips + cofips
    if (!"fips" %in% names(pscore) && all(c("stfips", "cofips") %in% names(pscore))) {
      pscore <- pscore %>% mutate(fips = stfips * 1000 + cofips)
    }
    # Keep pscore-specific variables (avoid collisions)
    pscore_vars <- setdiff(names(pscore), names(data))
    pscore_vars <- c("fips", pscore_vars)
    data <- data %>% left_join(pscore[, pscore_vars, drop = FALSE], by = "fips")
    cat(sprintf("  Propensity score data merged: %d counties\n", nrow(pscore)))
  }

  # 5. Drop NYC, LA, Chicago
  data <- data %>%
    filter(!(stfips == 36 & cofips == 61),
           !(stfips == 6  & cofips == 37),
           !(stfips == 17 & cofips == 31))

  # 6. Filter to panel end
  data <- data %>% filter(year <= CMHC_PANEL_END)

  # 7. Create treatment indicator
  data <- data %>%
    mutate(
      cmhc_year_exp_orig = cmhc_year_exp,
      cmhc_year_exp = ifelse(!is.na(cmhc_year_exp) & cmhc_year_exp <= CMHC_MAX_COHORT,
                              cmhc_year_exp, NA),
      cmhc_treated = as.integer(!is.na(cmhc_year_exp))
    )

  # 8. Create event time
  data <- data %>%
    mutate(event_time = year - cmhc_year_exp)

  # 9. Bin event time
  data <- data %>%
    mutate(
      event_time_binned = case_when(
        is.na(event_time)           ~ -999L,
        event_time <= CMHC_EVENT_MIN ~ CMHC_EVENT_MIN,
        event_time >= CMHC_EVENT_MAX ~ CMHC_EVENT_MAX,
        TRUE                         ~ as.integer(event_time)
      )
    )

  # 10. Create urban categories
  data <- data %>%
    group_by(fips) %>%
    mutate(urb60 = sum(`_60pcturban` * (year == 1960), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Durb = cut(urb60, breaks = c(-Inf, 1, 25, 50, 75, Inf),
                      labels = c("0", "1-25", "25-50", "50-75", "75+"),
                      include.lowest = TRUE))

  # 11. Population weights from 1960
  data <- data %>%
    group_by(fips) %>%
    mutate(
      popwt     = sum(copop     * (year == 1960), na.rm = TRUE),
      popwt_ad  = sum(copop_ad  * (year == 1960), na.rm = TRUE),
      popwt_eld = sum(copop_eld * (year == 1960), na.rm = TRUE)
    ) %>%
    ungroup()

  # 12. Population quintile
  pop_xsec <- data %>%
    filter(year == 1960) %>%
    mutate(pop_quintile = ntile(popwt, 5)) %>%
    select(fips, pop_quintile)
  data <- data %>% left_join(pop_xsec, by = "fips")

  # 13. Pre-treatment AMR quintile
  pre_amr <- data %>%
    filter(year >= 1959, year <= 1964) %>%
    group_by(fips) %>%
    summarize(
      pre_amr_mean    = mean(amr, na.rm = TRUE),
      pre_amr_ad_mean = mean(amr_ad, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      amr_quintile    = ntile(pre_amr_mean, 5),
      amr_ad_quintile = ntile(pre_amr_ad_mean, 5)
    )
  data <- data %>% left_join(pre_amr, by = "fips")

  # 14. CHC event time for horse race
  data <- data %>%
    mutate(
      chc_event_time = year - chc_year_exp,
      chc_event_time_binned = case_when(
        is.na(chc_event_time) ~ -999L,
        chc_event_time <= -6L ~ -6L,
        chc_event_time >= 20L ~ 20L,
        TRUE                  ~ as.integer(chc_event_time)
      ),
      ever_chc = as.integer(!is.na(chc_year_exp))
    ) %>%
    group_by(fips) %>%
    mutate(ever_chc = max(ever_chc)) %>%
    ungroup()

  # 15. Merge incarceration data (1970-1988 subset)
  inc_file <- file.path(ROOT, "incarceration_trends_county.fst")
  if (file.exists(inc_file)) {
    inc <- fst::read_fst(inc_file, as.data.table = TRUE)
    inc_annual <- inc[, .(
      jail_rate        = mean(total_jail_pop_rate, na.rm = TRUE),
      bl_jail_rate     = mean(black_jail_pop_rate, na.rm = TRUE),
      jail_pop_per_cap = mean(jail_pop_per_capita, na.rm = TRUE)
    ), by = .(fips, year)]
    inc_annual[, fips := as.integer(fips)]
    inc_annual[, `:=`(
      jail_rate        = fifelse(is.nan(jail_rate), NA_real_, jail_rate),
      bl_jail_rate     = fifelse(is.nan(bl_jail_rate), NA_real_, bl_jail_rate),
      jail_pop_per_cap = fifelse(is.nan(jail_pop_per_cap), NA_real_, jail_pop_per_cap)
    )]

    # Balanced jail sample indicator
    bal <- inc_annual[year %in% 1965:1980 & !is.na(jail_rate),
                       .(n_obs = .N), by = fips]
    bal[, jail_bal_sample := as.integer(n_obs >= 7)]
    inc_annual <- merge(inc_annual, bal[, .(fips, jail_bal_sample)], by = "fips", all.x = TRUE)
    inc_annual[is.na(jail_bal_sample), jail_bal_sample := 0L]

    data <- data %>%
      left_join(as_tibble(inc_annual), by = c("fips", "year"))
    cat(sprintf("  Incarceration data merged: %d county-years\n", nrow(inc_annual)))
  }

  # 16. Write parquet
  arrow::write_parquet(data, outfile)
  cat(sprintf("  CMHC panel: %d rows, %d counties, years %d-%d\n",
              nrow(data), n_distinct(data$fips), min(data$year), max(data$year)))
  cat(sprintf("  Treated counties (CMHC <= %d): %d\n",
              CMHC_MAX_COHORT, n_distinct(data$fips[data$cmhc_treated == 1])))
  cat(sprintf("  Saved: %s\n", outfile))
}
