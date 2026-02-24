#!/usr/bin/env Rscript
# ============================================================================
# 11_fig02_national_mortality.R — National Mortality Trends by Age Group
# ============================================================================
log_section("11: Figure 2 — National Mortality")

nat_file <- file.path(DATA_DIR, "national_mortality.parquet")

if (!file.exists(nat_file)) {
  cat("  WARNING: national_mortality.parquet not found. Skipping Figure 2.\n")
} else {
  nat <- arrow::read_parquet(nat_file) %>% as.data.table()

  if (nrow(nat) == 0) {
    cat("  WARNING: Empty national mortality data. Skipping.\n")
  } else {

    # Plot ages 20-49 all-cause and external causes with dual axes
    nat_plot <- nat[age_group %in% c("20-49", "external") & !is.na(rate)][year < 2025]
    
    # Separate into two series for dual-axis plotting
    nat_all <- nat_plot[age_group == "20-49"][, age_group := "All causes"]
    nat_dod <- nat_plot[age_group == "external"][, age_group := "External causes"]

    p <- ggplot() +
      annotate("rect", xmin = 1963, xmax = 1981, ymin = -Inf, ymax = Inf,
               fill = "#2166ac", alpha = 0.08) +
      annotate("rect", xmin = 2017, xmax = max(nat_plot$year, na.rm = TRUE), ymin = -Inf, ymax = Inf,
           fill = "#b2182b", alpha = 0.08) +
      # All-cause mortality on left axis
      geom_line(data = nat_all, aes(x = year, y = rate, color = "All causes", linetype = "All causes"),
            linewidth = 1.2) +
      # External causes on right axis (scaled relative to DoD values)
      geom_line(data = nat_dod, aes(x = year, y = rate * 8, color = "External causes (DoD)", linetype = "External causes (DoD)"),
        linewidth = 1.2, linetype = "dotted") +
      scale_color_manual(values = c("All causes" = "steelblue", "External causes (DoD)" = "darkred")) +
      scale_linetype_manual(values = c("All causes" = "solid", "External causes (DoD)" = "dashed")) +
      scale_y_continuous(
        name = "All-cause deaths per 100,000",
        sec.axis = sec_axis(~ . / 8, name = "External causes per 100,000")
      ) +
      labs(
        title = "National Mortality Trends: Ages 20-49",
        subtitle = "Shaded regions: CMHC era (blue) and CCBHC era (red)",
        x = "Year",
        color = NULL,
        linetype = NULL
      ) +
      theme_paper() +
      theme(
        axis.title.y.right = element_text(color = "darkred"),
        axis.text.y.right = element_text(color = "darkred"),
        legend.position = "bottom"
      )

    save_fig(p, "fig02_national_mortality.pdf", width = 10, height = 6)
    save_csv(nat_plot, "fig02_national_mortality.csv")
  }
}
