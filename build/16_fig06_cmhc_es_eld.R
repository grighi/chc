#!/usr/bin/env Rscript
# ============================================================================
# 16_fig06_cmhc_es_eld.R — CMHC Event Study, Ages 50+ (Placebo)
# ============================================================================
log_section("16: Figure 6 — CMHC Event Study, Ages 50+")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

model_es_eld <- feols(
  amr_eld ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
  | fips + year^Durb + year^stfips,
  data = panel,
  weights = ~popwt_eld,
  cluster = ~fips
)

coef_df <- extract_es_coefs(model_es_eld, max_et = CMHC_PLOT_MAX)

tests <- run_joint_tests(model_es_eld,
  pre_range  = CMHC_EVENT_MIN:-2,
  post_range = 0:CMHC_PLOT_MAX
)

p <- plot_event_study(
  coef_df,
  title = "Effect of CMHC Opening on Mortality (Ages 50+) — Placebo",
  subtitle = sprintf("Event study, 95%% CI. Pre-trend p = %.3f.",
                      tests$pre_p %||% NA),
  ylab = "Change in Deaths per 100,000",
  xlab = "Years Relative to CMHC Opening",
  color = "darkred"
)

save_fig(p, "fig06_cmhc_es_eld.pdf")
save_csv(coef_df, "cmhc_es_eld_coefs.csv")
