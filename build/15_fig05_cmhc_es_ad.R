#!/usr/bin/env Rscript
# ============================================================================
# 15_fig05_cmhc_es_ad.R — CMHC Event Study, Ages 20-49 (Main Result)
# ============================================================================
log_section("15: Figure 5 — CMHC Event Study, Ages 20-49")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Main specification
model_es_ad <- feols(
  amr_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
  | fips + year + year^Durb + year^stfips,
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

print(p)

save_fig(p, "fig05_cmhc_es_ad.pdf")
save_csv(coef_df, "cmhc_es_ad_coefs.csv")

# Print summary
cat("\n  Post-treatment average (t=0 to t=14):\n")
post <- coef_df %>% filter(event_time >= 0)
cat(sprintf("    Mean coefficient: %.3f\n", mean(post$coefficient)))
cat(sprintf("    Min coefficient:  %.3f (t=%d)\n",
            min(post$coefficient), post$event_time[which.min(post$coefficient)]))

# ── Sun-Abraham (2021) heterogeneity-robust estimator ────────────────────
cat("\n  Estimating Sun-Abraham (2021) estimator...\n")
cat("  (Uses cohort x period interactions; robust to heterogeneous treatment effects)\n")

model_sa <- feols(
  amr_ad ~ sunab(cmhc_year_exp, year, ref.c = "NA", ref.p = -1) + D_tot_act_md_t + H_bpc
  | fips + year,
  data   = panel %>% 
    mutate(cmhc_year_exp = ifelse(cmhc_treated == 0, -999, cmhc_year_exp)) %>%
    filter(is.na(event_time) | event_time < 14),
  weights = ~popwt_ad,
  cluster = ~fips
)
model_sa

cat("  Sun-Abraham model estimated.\n")

# ── Aggregate ATT ─────────────────────────────────────────────────────────
sa_att_tbl  <- summary(model_sa, agg = "ATT")$coeftable
sa_att      <- sa_att_tbl[1, "Estimate"]
sa_att_se   <- sa_att_tbl[1, "Std. Error"]
twfe_post_mean <- mean(coef_df$coefficient[coef_df$event_time >= 0])

cat(sprintf("\n  ── TWFE vs Sun-Abraham ATT comparison ──\n"))
cat(sprintf("    TWFE post-period mean:  %.3f\n", twfe_post_mean))
cat(sprintf("    SA aggregate ATT:       %.3f (SE = %.3f)\n", sa_att, sa_att_se))
cat(sprintf("    SA 95%% CI:             [%.3f, %.3f]\n",
            sa_att - 1.96 * sa_att_se, sa_att + 1.96 * sa_att_se))
cat(sprintf("    Difference (SA - TWFE): %.3f\n", sa_att - twfe_post_mean))

# ── Cohort-specific ATTs ──────────────────────────────────────────────────
cat("\n  Sun-Abraham cohort-specific ATTs:\n")
sa_cohort_tbl <- summary(model_sa, agg = "cohort")$coeftable
print(round(sa_cohort_tbl, 3))

# ── Extract period-aggregated SA coefficients ─────────────────────────────
# agg = "period" row names are already event-time labels (e.g. "year::-3",
# "year::0"), NOT calendar years — parse with -? to capture negatives, then
# filter to the same window used by the TWFE event-time bins.
sa_period_ct <- summary(model_sa, agg = "period")$coeftable
sa_coef_df <- data.frame(
  event_time  = as.integer(gsub(".*::(-?[0-9]+)", "\\1", rownames(sa_period_ct))),
  coefficient = sa_period_ct[, "Estimate"],
  se          = sa_period_ct[, "Std. Error"],
  row.names   = NULL
) %>%
  mutate(
    ci_lower = coefficient - 1.96 * se,
    ci_upper = coefficient + 1.96 * se
  ) %>%
  filter(event_time >= CMHC_EVENT_MIN, event_time <= CMHC_PLOT_MAX) %>%
  bind_rows(data.frame(event_time = -1L, coefficient = 0, se = 0,
                       ci_lower = 0, ci_upper = 0)) %>%
  arrange(event_time)

# ── Overlay comparison plot ───────────────────────────────────────────────
p_compare <- ggplot(coef_df, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  # TWFE
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.15) +
  geom_line(aes(color = "TWFE"), linewidth = 0.8) +
  geom_point(aes(color = "TWFE"), size = 2) +
  # Sun-Abraham
  geom_ribbon(data = sa_coef_df,
              aes(ymin = ci_lower, ymax = ci_upper), fill = "tomato", alpha = 0.15) +
  geom_line(data  = sa_coef_df, aes(color = "Sun-Abraham (2021)"), linewidth = 0.8) +
  geom_point(data = sa_coef_df, aes(color = "Sun-Abraham (2021)"), size = 2) +
  scale_color_manual(
    values = c("TWFE" = "steelblue", "Sun-Abraham (2021)" = "tomato"),
    name   = "Estimator"
  ) +
  labs(
    title    = "TWFE vs Sun-Abraham: CMHC Effect on Mortality (Ages 20-49)",
    subtitle = sprintf(
      "SA aggregate ATT = %.3f (SE = %.3f). TWFE post mean = %.3f. Difference = %.3f.",
      sa_att, sa_att_se, twfe_post_mean, sa_att - twfe_post_mean
    ),
    x = "Years Relative to CMHC Opening",
    y = "Change in Deaths per 100,000"
  ) +
  theme_paper()

print(p_compare)
save_fig(p_compare, "fig05b_cmhc_es_ad_sa_compare.pdf")
save_csv(sa_coef_df, "cmhc_es_ad_sa_coefs.csv")
save_csv(
  data.frame(
    estimator = c("TWFE_post_mean", "SA_ATT"),
    estimate  = c(twfe_post_mean, sa_att),
    se        = c(NA_real_, sa_att_se)
  ),
  "cmhc_es_ad_sa_summary.csv"
)

