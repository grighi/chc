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

    nat_plot <- nat[age_group %in% c("20-49", "50+") & !is.na(rate)]

    p_a <- ggplot(nat_plot[age_group == "50+"], aes(x = year, y = rate)) +
      annotate("rect", xmin = 1963, xmax = 1981, ymin = -Inf, ymax = Inf,
               fill = "#2166ac", alpha = 0.08) +
      annotate("rect", xmin = 2017, xmax = max(nat_plot$year), ymin = -Inf, ymax = Inf,
               fill = "#b2182b", alpha = 0.08) +
      geom_line(color = "steelblue", linewidth = 1) +
      labs(title = "A. Ages 50+", x = NULL, y = "Deaths per 100,000") +
      theme_paper()

    p_b <- ggplot(nat_plot[age_group == "20-49"], aes(x = year, y = rate)) +
      annotate("rect", xmin = 1963, xmax = 1981, ymin = -Inf, ymax = Inf,
               fill = "#2166ac", alpha = 0.08) +
      annotate("rect", xmin = 2017, xmax = max(nat_plot$year), ymin = -Inf, ymax = Inf,
               fill = "#b2182b", alpha = 0.08) +
      geom_line(color = "darkred", linewidth = 1) +
      labs(title = "B. Ages 20-49", x = "Year", y = "Deaths per 100,000") +
      theme_paper()

    p <- p_a / p_b +
      plot_annotation(
        title = "National Mortality Trends by Age Group",
        subtitle = "Shaded regions: CMHC era (blue) and CCBHC era (red)",
        theme = theme(
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 11, color = "gray30")
        )
      )

    save_fig(p, "fig02_national_mortality.pdf", width = 10, height = 10)
    save_csv(nat_plot, "fig02_national_mortality.csv")
  }
}
