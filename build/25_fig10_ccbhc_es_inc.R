#!/usr/bin/env Rscript
# ============================================================================
# 25_fig10_ccbhc_es_inc.R — CCBHC Incarceration Event Study
# Adapted from: ccbhc/ccbhc_grants_did.R (incarceration mode)
# ============================================================================
log_section("25: Figure 10 — CCBHC Incarceration Event Study")

panel <- arrow::read_parquet(file.path(DATA_DIR, "ccbhc_panel.parquet")) %>% as.data.table()

# Use log jail pop rate as outcome
es <- panel[!is.na(log_jail_pop_rate) & !is.na(total_pop) & year > 2012]
es <- es[(event_time_binned >= CCBHC_EVENT_MIN & event_time_binned <= CCBHC_EVENT_MAX) |
          event_time_binned == -999]

cat(sprintf("  Incarceration panel: %d rows, %d counties\n", nrow(es), uniqueN(es$fips)))

# --- Matching (same as mortality) ---
fac_cols <- c("n_omh_2017", "n_act_2017", "n_cmhc_2017", "n_therapy_2017",
              "n_case_mgmt_2017", "n_crisis_2017", "n_residential_2017")
available_fac <- intersect(fac_cols, names(es))

matching_data <- es[year == 2015]

if (length(available_fac) >= 3 && nrow(matching_data) > 20) {
  fml_match <- as.formula(paste("treated ~", paste(available_fac, collapse = " + ")))
  m_match <- tryCatch(
    MatchIt::matchit(fml_match, data = matching_data,
                     method = "nearest", distance = "logit", ratio = 2),
    error = function(e) NULL
  )
  if (!is.null(m_match)) {
    matched_counties <- c(MatchIt::match.data(m_match)$fips, es[treated == 1, unique(fips)])
  } else {
    matched_counties <- es[, unique(fips)]
  }
} else {
  matched_counties <- es[, unique(fips)]
}

es_matched <- es[fips %in% matched_counties]

# --- Event study ---
mod <- feols(
  log_jail_pop_rate ~ i(event_time_binned, ref = -1) + total_pop | fips + year,
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
  title = "Effect of CCBHC on Jail Incarceration Rate",
  subtitle = sprintf("log(jail pop rate per 100k). Pre-trend p = %.3f. N treated = %d.",
                      tests$pre_p %||% NA,
                      uniqueN(es_matched[treated == 1, fips])),
  xlab = "Years Relative to First CCBHC Grant",
  ylab = "Change in log(Jail Pop Rate per 100k)",
  color = "darkorange"
)

save_fig(p, "fig10_ccbhc_es_inc.pdf")
save_csv(coef_df, "ccbhc_inc_baseline_coefs.csv")
