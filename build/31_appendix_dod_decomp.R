#!/usr/bin/env Rscript
# ============================================================================
# 31_appendix_dod_decomp.R — CCBHC Deaths-of-Despair Decomposition
# ============================================================================
log_section("31: Appendix — CCBHC DoD Decomposition")

panel <- arrow::read_parquet(file.path(DATA_DIR, "ccbhc_panel.parquet")) %>% as.data.table()

# Check for cause-specific mortality variables
has_f <- "mort_rate_f" %in% names(panel) && sum(!is.na(panel$mort_rate_f)) > 100
has_ext <- "mort_rate_external" %in% names(panel) && sum(!is.na(panel$mort_rate_external)) > 100

if (!has_f && !has_ext) {
  cat("  WARNING: No cause-specific mortality data available.\n")
  cat("  To generate DoD decomposition, pull cause-specific WONDER data.\n")
} else {
  cause_specs <- list()

  if (has_f) {
    cause_specs[["ICD-F (Mental/Behavioral)"]] <- list(var = "mort_rate_f", color = "purple")
  }
  if (has_ext) {
    cause_specs[["External Causes (U/V)"]] <- list(var = "mort_rate_external", color = "darkorange")
  }

  panels <- list()
  for (nm in names(cause_specs)) {
    spec <- cause_specs[[nm]]
    es <- panel[!is.na(get(spec$var)) & year > 2012]
    es <- es[(event_time_binned >= CCBHC_EVENT_MIN & event_time_binned <= CCBHC_EVENT_MAX) |
              event_time_binned == -999]

    if (nrow(es) < 100) next

    fml <- as.formula(sprintf(
      "%s ~ i(event_time_binned, ref = -1) + total_pop | fips + year", spec$var
    ))
    m <- tryCatch(
      feols(fml, data = es, weights = ~total_pop, cluster = ~fips),
      error = function(e) NULL
    )

    if (!is.null(m)) {
      coefs <- extract_es_coefs(m, max_et = CCBHC_EVENT_MAX)
      save_csv(coefs, sprintf("ccbhc_dod_%s_coefs.csv", gsub("[^a-z]", "", tolower(nm))))

      panels[[nm]] <- ggplot(coefs, aes(x = event_time, y = coefficient)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = spec$color, alpha = 0.2) +
        geom_point(color = spec$color, size = 2) +
        geom_line(color = spec$color, linewidth = 0.8) +
        labs(title = nm, x = "Years Relative to CCBHC Grant",
             y = "Change in Deaths per 100,000") +
        theme_paper()
    }
  }

  if (length(panels) > 0) {
    p <- wrap_plots(panels, ncol = 1) +
      plot_annotation(
        title = "CCBHC: Deaths-of-Despair Decomposition",
        theme = theme(plot.title = element_text(face = "bold", size = 13))
      )
    save_fig(p, "fig_ccbhc_dod_decomp.pdf", height = 5 * length(panels))
  }
}
