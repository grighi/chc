#!/usr/bin/env Rscript
# ============================================================================
# 15_fig05_cmhc_es_ad.R — CMHC Event Study, Ages 20-49 (Main Result)
# ============================================================================
log_section("15: Figure 5 — CMHC Event Study, Ages 20-49")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Main specification
model_es_ad <- feols(
  amr_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
    + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`
  | fips + year^Durb + year^stfips,
  data = panel,
  weights = ~popwt_ad,
  cluster = ~fips
)

cat("  Model estimated. Extracting coefficients...\n")

coef_df <- extract_es_coefs(model_es_ad, max_et = CMHC_PLOT_MAX)

# Joint tests
cat("  Joint significance tests:\n")
tests <- run_joint_tests(model_es_ad,
  pre_range  = CMHC_EVENT_MIN:-2,
  post_range = 0:CMHC_PLOT_MAX
)

# Plot
p <- plot_event_study(
  coef_df,
  title = "Effect of CMHC Opening on Mortality (Ages 20-49)",
  subtitle = sprintf("Event study, 95%% CI. Pre-trend p = %.3f. N treated = %d.",
                      tests$pre_p %||% NA,
                      n_distinct(panel$fips[panel$cmhc_treated == 1])),
  ylab = "Change in Deaths per 100,000",
  xlab = "Years Relative to CMHC Opening"
)

save_fig(p, "fig05_cmhc_es_ad.pdf")
save_csv(coef_df, "cmhc_es_ad_coefs.csv")

# Print summary
cat("\n  Post-treatment average (t=0 to t=14):\n")
post <- coef_df %>% filter(event_time >= 0)
cat(sprintf("    Mean coefficient: %.3f\n", mean(post$coefficient)))
cat(sprintf("    Min coefficient:  %.3f (t=%d)\n",
            min(post$coefficient), post$event_time[which.min(post$coefficient)]))
