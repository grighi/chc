#!/usr/bin/env Rscript
# ============================================================================
# 23_fig08_ccbhc_es_mort.R — CCBHC Mortality Event Study, Ages 25-44
# Adapted from: ccbhc/ccbhc_grants_did.R (mortality mode)
# ============================================================================
log_section("23: Figure 8 — CCBHC Mortality Event Study")

panel <- arrow::read_parquet(file.path(DATA_DIR, "ccbhc_panel.parquet")) %>% as.data.table()

# Filter to relevant years and valid mortality
es <- panel[!is.na(mort_rate) & year > 2012]
es <- es[(event_time_binned >= CCBHC_EVENT_MIN & event_time_binned <= CCBHC_EVENT_MAX) |
          event_time_binned == -999]

cat(sprintf("  Mortality panel: %d rows, %d counties\n", nrow(es), uniqueN(es$fips)))

# --- Propensity score matching ---
matching_data <- es[year == 2015]

# Ensure facility columns exist
fac_cols <- c("n_omh_2017", "n_act_2017", "n_cmhc_2017", "n_therapy_2017",
              "n_case_mgmt_2017", "n_crisis_2017", "n_residential_2017")
available_fac <- intersect(fac_cols, names(matching_data))

if (length(available_fac) >= 3) {
  fml_match <- as.formula(paste("treated ~", paste(available_fac, collapse = " + ")))
  m_match <- MatchIt::matchit(fml_match, data = matching_data,
                               method = "nearest", distance = "logit", ratio = 2)
  matched_counties <- c(MatchIt::match.data(m_match)$fips, es[treated == 1, unique(fips)])
  cat(sprintf("  Matched counties: %d\n", length(unique(matched_counties))))
} else {
  matched_counties <- es[, unique(fips)]
  cat("  No facility data for matching — using all counties.\n")
}

es_matched <- es[fips %in% matched_counties]

# --- Run event study ---
mod <- feols(
  mort_rate ~ i(event_time_binned, ref = -1) + total_pop | fips + year,
  data = es_matched,
  weights = ~total_pop,
  cluster = ~fips
)

coef_df <- extract_es_coefs(mod, max_et = CCBHC_EVENT_MAX)

tests <- run_joint_tests(mod,
  pre_range  = CCBHC_EVENT_MIN:-2,
  post_range = 0:CCBHC_EVENT_MAX
)

p <- plot_event_study(
  coef_df,
  title = "Effect of CCBHC on Death Rate (Ages 25-44)",
  subtitle = sprintf("Matched sample. Pre-trend p = %.3f. N treated = %d.",
                      tests$pre_p %||% NA,
                      uniqueN(es_matched[treated == 1, fips])),
  xlab = "Years Relative to First CCBHC Grant",
  ylab = "Change in Deaths per 100,000"
)

save_fig(p, "fig08_ccbhc_es_mort.pdf")
save_csv(coef_df, "ccbhc_mort_baseline_coefs.csv")

# Heterogeneity: small vs large
county_pop <- es_matched[, .(avg_pop = mean(total_pop, na.rm = TRUE)), by = fips]
county_pop[, size_group := fifelse(avg_pop < 30000, "Small (<30k)", "Large (>=30k)")]
es_matched <- merge(es_matched, county_pop[, .(fips, size_group)], by = "fips", all.x = TRUE)

for (sg in c("Small (<30k)", "Large (>=30k)")) {
  m_sg <- tryCatch(
    feols(mort_rate ~ i(event_time_binned, ref = -1) + total_pop | fips + year,
          data = es_matched[size_group == sg], weights = ~total_pop, cluster = ~fips),
    error = function(e) NULL
  )
  if (!is.null(m_sg)) {
    coef_sg <- extract_es_coefs(m_sg, max_et = CCBHC_EVENT_MAX)
    save_csv(coef_sg, sprintf("ccbhc_mort_%s_coefs.csv", gsub("[^a-z]", "", tolower(sg))))
  }
}
