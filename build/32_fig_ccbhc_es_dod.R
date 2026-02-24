#!/usr/bin/env Rscript
# ============================================================================
# 32_fig_ccbhc_es_dod.R — CCBHC Event Study: Deaths of Despair
#
# Dependent variable: dod_rate (deaths per 100k, ages 20-59)
#   Source: CDC WONDER final data (D77), DoD = suicide + drug overdose +
#   alcohol liver disease (ICD-10 X40-X45, X60-X65, X70-X84, Y10-Y15,
#   K70, K73-K74). Merged in 06_prep_ccbhc_panel.R from
#   raw/wonder_mortality/data/dod_alladults.parquet.
#
# Design mirrors 23_fig08_ccbhc_es_mort.R:
#   - Propensity-score matched sample (2:1 nearest-neighbor on 2017 facilities)
#   - TWFE event study: dod_rate ~ i(event_time_binned) + total_pop | fips + year
#   - Pop-weighted, county-clustered SEs
#
# Note: WONDER suppresses rates for county-years with <20 deaths, but we use
# the raw death count / population, which is available for counties with 10-19
# deaths. County-years with <10 deaths have dod_deaths = NA and are excluded.
# ============================================================================
log_section("32: CCBHC Event Study — Deaths of Despair")

panel <- arrow::read_parquet(file.path(DATA_DIR, "ccbhc_panel.parquet")) %>% as.data.table()

# --- Check that dod_rate exists ---
if (!"dod_rate" %in% names(panel)) {
  cat("  WARNING: dod_rate not in panel. Run 06_prep_ccbhc_panel.R with FORCE_REBUILD=TRUE.\n")
  cat("  Skipping DoD event study.\n")
} else {

  # Filter to years with WONDER coverage (2010-2020) and valid DoD rate
  es <- panel[!is.na(dod_rate) & year >= 2013 & year <= 2024]
  es <- es[(event_time_binned >= CCBHC_EVENT_MIN & event_time_binned <= CCBHC_EVENT_MAX) |
            event_time_binned == -999]

  cat(sprintf("  DoD panel: %d rows, %d counties, years %d-%d\n",
              nrow(es), uniqueN(es$fips), min(es$year), max(es$year)))
  cat(sprintf("  Treated: %d  |  Control: %d\n",
              uniqueN(es[treated == 1, fips]),
              uniqueN(es[treated == 0, fips])))

  # --- Propensity score matching (same as mortality ES) ---
  matching_data <- es[year == 2015]

  # Compute pre-trend in DoD rate (2010-2015) from full panel, not filtered es
  pretrend <- panel[!is.na(dod_rate) & year >= 2013 & year <= 2017, .(
    dod_pretrend = coef(lm(dod_rate ~ year^2, na.action = na.omit))[2],
    dod_rate_17 = dod_rate[year == 2015]
  ), by = fips]

  matching_data <- matching_data %>%
    merge(pretrend, by = "fips", all.x = TRUE) %>%
    as.data.table()
  
  # Drop counties missing pre-trend (insufficient data in 2010-2015)
  matching_data <- matching_data[!is.na(dod_pretrend)]

  fac_cols <- c("n_omh_2017", "n_act_2017", "n_cmhc_2017", "n_therapy_2017",
                "n_case_mgmt_2017", "n_crisis_2017", "n_residential_2017")
  available_fac <- intersect(fac_cols, names(matching_data))

  if (length(available_fac) >= 3) {
    # fml_match <- as.formula(paste("treated ~",
    #   paste(c(available_fac, "dod_pretrend"), collapse = " + ")))
    fml_match <- as.formula("treated ~ dod_pretrend + dod_rate + total_pop")
    m_match <- MatchIt::matchit(fml_match, data = matching_data,
                                 method = "nearest", distance = "logit", ratio = 3)
    matched_counties <- c(MatchIt::match.data(m_match)$fips)
    cat(sprintf("  Matched counties: %d\n", length(unique(matched_counties))))
  } else {
    matched_counties <- es[, unique(fips)]
    cat("  No facility data for matching — using all counties.\n")
  }

  ## Matching estimator
   es_matched <- es[fips %in% matched_counties]
  ## Keep all counties
  # es_matched <- es
  ## Keep highest pretrend counties
  # treated_and_top_quartile <- matching_data[(treated == 0 & dod_pretrend > quantile(dod_pretrend, 0.5)) | treated == 1, unique(fips)]
  # es_matched <- es[fips %in% treated_and_top_quartile]
  # above_median <- matching_data[(dod_pretrend > quantile(dod_pretrend, 0.5)), unique(fips)]
  # es_matched <- es[fips %in% above_median]

  # need to create a table that shows
  # - large counties with rising mortality rates were more treated
  # - when you match on trends, you get small counties that can't break through
  #   the pretrend; but if you match on size, you get large counties with 
  #   lower trends that lead to the pretrend

  # --- Event study regression ---
  mod <- feols(
    dod_rate ~ i(event_time_binned, ref = -1) + total_pop | fips[year],
    # fips[year]: Did mortality fall relative to what we would expect given the county’s pre-treatment linear trajectory?
    # fips + year: Did mortality fall relative to untreated counties?
    data = es_matched,
    panel.id = c("fips", "year"),
    weights = ~total_pop,
    cluster = ~fips
  )

  coef_df <- extract_es_coefs(mod, max_et = CCBHC_EVENT_MAX)

  tests <- run_joint_tests(mod,
    pre_range  = CCBHC_EVENT_MIN:-2,
    post_range = 0:CCBHC_EVENT_MAX
  )

  # --- Figure ---
  p <- plot_event_study(
    coef_df,
    title = "Effect of CCBHC on Deaths of Despair (Ages 20–59)",
    subtitle = sprintf("Matched sample. Pre-trend p = %.3f. N treated = %d.",
                        tests$pre_p %||% NA,
                        uniqueN(es_matched[treated == 1, fips])),
    xlab = "Years Relative to First CCBHC Grant",
    ylab = "Change in Deaths per 100,000",
    color = "#c0392b"
  )

  save_fig(p, "fig_ccbhc_es_dod.pdf")
  save_csv(coef_df, "ccbhc_dod_baseline_coefs.csv")

  # --- Print summary ---
  cat("\n  Event-study coefficients:\n")
  print(coef_df, row.names = FALSE)
  cat("\n")
}
