#!/usr/bin/env Rscript
# ============================================================================
# 14_fig04_timing_exogeneity.R — Pre-program AMR vs CMHC timing scatter
# Adapted from: cmhc_event_study.R Step 12b
# ============================================================================
log_section("14: Figure 4 — Timing Exogeneity")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Build cross-section of treated counties
fig4_data <- panel %>%
  filter(!is.na(cmhc_year_exp)) %>%
  filter(year %in% c(1959, 1960)) %>%
  filter((cmhc_year_exp <= 1975)) %>%
  select(fips, year, cmhc_year_exp, amr_ad, copop_ad, popwt_ad,
         `_tot_act_md`, `_60pcturban`, `_60pctnonwhit`, `_pct59inclt3k`) %>%
  pivot_wider(names_from = year, values_from = c(amr_ad, copop_ad),
              names_sep = "_") %>%
  mutate(
    amr_ad_level_1960 = amr_ad_1960,
    amr_ad_change_59_60 = amr_ad_1960 - amr_ad_1959
  ) %>%
  filter(!is.na(amr_ad_level_1960))

cat(sprintf("  Treated counties with valid 1960 AMR: %d\n", nrow(fig4_data)))

# --- Panel A: Level ---
fit_level <- lm(amr_ad_level_1960 ~ cmhc_year_exp, data = fig4_data, weights = popwt_ad)
cat(sprintf("  Panel A slope: %.3f (SE: %.3f, p = %.3f)\n",
            coef(fit_level)[2],
            summary(fit_level)$coefficients[2, 2],
            summary(fit_level)$coefficients[2, 4]))

p_a <- ggplot(fig4_data, aes(x = cmhc_year_exp, y = amr_ad_level_1960,
  weight = popwt_ad)) +
  geom_point(aes(size = popwt_ad), alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_size_continuous(range = c(0.5, 4), guide = "none") +
  labs(
    # title = "A. 1960 AMR (Ages 20-49) vs. CMHC Establishment Year",
    x = "CMHC Establishment Year",
    y = "AMR (20-49), 1960"
  ) +
  theme_paper()

# --- Panel B: Change ---
fig4_change <- fig4_data %>% filter(!is.na(amr_ad_change_59_60))
fit_change <- lm(amr_ad_change_59_60 ~ cmhc_year_exp, data = fig4_change, weights = popwt_ad)
cat(sprintf("  Panel B slope: %.3f (SE: %.3f, p = %.3f)\n",
            coef(fit_change)[2],
            summary(fit_change)$coefficients[2, 2],
            summary(fit_change)$coefficients[2, 4]))

p_b <- ggplot(fig4_change, aes(x = cmhc_year_exp, y = amr_ad_change_59_60, weight = popwt_ad)) +
  geom_point(aes(size = popwt_ad), alpha = 0.4, color = "darkred") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_size_continuous(range = c(0.5, 4), guide = "none") +
  labs(
    # title = "B. Change in AMR (Ages 20-49), 1959-1960, vs. CMHC Year",
    x = "CMHC Establishment Year",
    y = expression(Delta ~ "AMR (20-49), 1959-1960")
  ) +
  theme_paper()

p <- p_a + p_b +
  plot_annotation(
    title = "No Relationship Between CMHC Timing and Pre-Program Mortality",
    subtitle = "Point size proportional to 1960 population. Lines are population-weighted OLS fits.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "gray30")
    )
  )

save_fig(p, "fig04_timing_exogeneity.pdf", width = 9, height = 4)

# Save test statistics
save_csv(data.frame(
  panel = c("A_level", "B_change"),
  slope = c(coef(fit_level)[2], coef(fit_change)[2]),
  se    = c(summary(fit_level)$coefficients[2, 2], summary(fit_change)$coefficients[2, 2]),
  pval  = c(summary(fit_level)$coefficients[2, 4], summary(fit_change)$coefficients[2, 4])
), "fig04_timing_exogeneity_tests.csv")
