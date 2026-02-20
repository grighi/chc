#!/usr/bin/env Rscript
# ============================================================================
# 27_fig_side_by_side.R — CMHC vs CCBHC Event Studies Side by Side
# ============================================================================
log_section("27: Figure — Side-by-Side Comparison")

# Load coefficient CSVs from prior figures
cmhc_file <- file.path(CSV_DIR, "cmhc_es_ad_coefs.csv")
ccbhc_file <- file.path(CSV_DIR, "ccbhc_mort_baseline_coefs.csv")

if (!file.exists(cmhc_file) || !file.exists(ccbhc_file)) {
  cat("  WARNING: Coefficient CSVs not found. Run Figures 5 and 8 first.\n")
} else {
  cmhc_coefs <- read_csv(cmhc_file, show_col_types = FALSE) %>%
    mutate(era = "CMHC (Ages 20-49)")
  ccbhc_coefs <- read_csv(ccbhc_file, show_col_types = FALSE) %>%
    mutate(era = "CCBHC (Ages 25-44)")

  # Determine common y-axis range
  all_coefs <- bind_rows(cmhc_coefs, ccbhc_coefs)
  y_range <- range(c(all_coefs$ci_lower, all_coefs$ci_upper), na.rm = TRUE) * 1.1

  p_a <- ggplot(cmhc_coefs, aes(x = event_time, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
    geom_point(color = "steelblue", size = 2) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    coord_cartesian(ylim = y_range) +
    labs(title = "A. CMHC Era (1963-1981)",
         subtitle = "AMR, Ages 20-49",
         x = "Years Relative to CMHC Opening",
         y = "Change in Deaths per 100,000") +
    theme_paper()

  p_b <- ggplot(ccbhc_coefs, aes(x = event_time, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "darkred", alpha = 0.2) +
    geom_point(color = "darkred", size = 2) +
    geom_line(color = "darkred", linewidth = 0.8) +
    coord_cartesian(ylim = y_range) +
    labs(title = "B. CCBHC Era (2017-Present)",
         subtitle = "Death Rate, Ages 25-44",
         x = "Years Relative to CCBHC Grant",
         y = "Change in Deaths per 100,000") +
    theme_paper()

  p <- p_a | p_b
  p <- p + plot_annotation(
    title = "Community Mental Health: Then and Now",
    subtitle = "Event study coefficients with 95% CI, same y-axis scale.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30")
    )
  )

  save_fig(p, "fig_side_by_side.pdf", width = 14, height = 6)
}
