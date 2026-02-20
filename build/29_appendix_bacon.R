#!/usr/bin/env Rscript
# ============================================================================
# 29_appendix_bacon.R — Bacon Decomposition (CMHC)
# ============================================================================
log_section("29: Appendix — Bacon Decomposition")

tryCatch({
  if (!requireNamespace("bacondecomp", quietly = TRUE)) {
    cat("  WARNING: bacondecomp not installed. Skipping.\n")
  } else {
    library(bacondecomp)

    panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

    # Create binary treatment indicator (post-CMHC)
    panel_bacon <- panel %>%
      mutate(treated_post = as.integer(!is.na(cmhc_year_exp) & year >= cmhc_year_exp)) %>%
      filter(!is.na(amr_ad)) %>%
      select(fips, year, amr_ad, treated_post, popwt_ad) %>%
      filter(!is.na(popwt_ad) & popwt_ad > 0)

    cat(sprintf("  Bacon sample: %d rows, %d counties\n",
                nrow(panel_bacon), n_distinct(panel_bacon$fips)))

    # Run Bacon decomposition
    bacon_out <- bacon(amr_ad ~ treated_post,
                        data = as.data.frame(panel_bacon),
                        id_var = "fips",
                        time_var = "year")

    cat("\n  Bacon decomposition summary:\n")
    bacon_summ <- bacon_out %>%
      group_by(type) %>%
      summarize(
        weight = sum(weight),
        avg_estimate = weighted.mean(estimate, weight),
        .groups = "drop"
      )
    print(bacon_summ)

    save_csv(as.data.frame(bacon_summ), "appendix_bacon_summary.csv")

    # Scatter plot
    p <- ggplot(bacon_out, aes(x = weight, y = estimate, color = type)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Bacon Decomposition: CMHC Effect on AMR (Ages 20-49)",
        subtitle = "Each point is a 2x2 DD comparison. x = weight, y = estimate.",
        x = "Weight",
        y = "2x2 DD Estimate",
        color = "Comparison Type"
      ) +
      theme_paper() +
      theme(legend.position = "right")

    save_fig(p, "fig_bacon_decomp.pdf")
  }
}, error = function(e) {
  cat(sprintf("  WARNING: Bacon decomposition failed: %s\n", e$message))
})
