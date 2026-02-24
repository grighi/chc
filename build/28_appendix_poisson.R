#!/usr/bin/env Rscript
# ============================================================================
# 28_appendix_poisson.R — CMHC Poisson Specification
# ============================================================================
log_section("28: Appendix — Poisson Specification")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Need deaths count (amr * pop / 100000)
panel <- panel %>%
  mutate(
    deaths_ad = amr_ad * copop_ad / 100000,
    log_pop_ad = log(copop_ad)
  ) %>%
  filter(!is.na(deaths_ad) & deaths_ad >= 0 & copop_ad > 0)

tryCatch({
  m_pois <- fepois(
    deaths_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
    | fips + year^Durb + year^stfips,
    data = panel,
    offset = ~log_pop_ad,
    cluster = ~fips
  )

  coef_pois <- extract_es_coefs(m_pois, max_et = CMHC_PLOT_MAX)

  # Convert to IRR for table
  coef_pois <- coef_pois %>%
    mutate(irr = exp(coefficient),
           irr_lower = exp(ci_lower),
           irr_upper = exp(ci_upper))

  save_csv(coef_pois, "appendix_poisson_coefs.csv")

  p <- plot_event_study(
    coef_pois,
    title = "CMHC Event Study: Poisson Specification (Ages 20-49)",
    subtitle = "Log-linear model with log(population) offset. County-clustered.",
    ylab = "Coefficient (log scale)",
    color = "purple"
  )

  save_fig(p, "fig_appendix_poisson.pdf")

  # Comparison table: linear vs Poisson post-treatment means
  cmhc_linear <- tryCatch(
    read_csv(file.path(CSV_DIR, "cmhc_es_ad_coefs.csv"), show_col_types = FALSE),
    error = function(e) NULL
  )
  if (!is.null(cmhc_linear)) {
    comp <- data.frame(
      spec = c("Linear", "Poisson (IRR)"),
      post_mean = c(
        mean(cmhc_linear$coefficient[cmhc_linear$event_time >= 0]),
        mean(coef_pois$irr[coef_pois$event_time >= 0]) - 1
      )
    )
    save_csv(comp, "appendix_poisson_comparison.csv")
  }

  # Write shell
  tex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{CMHC Event Study: Poisson vs.\\ Linear Specification}",
    "\\label{tab:poisson}",
    "\\small",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & Linear & Poisson (IRR$-$1) \\\\",
    "\\midrule",
    "%%TABLE_BODY%%",
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(tex_lines, file.path(TABLE_DIR, "table_appendix_poisson_shell.tex"))

}, error = function(e) {
  cat(sprintf("  WARNING: Poisson specification failed: %s\n", e$message))
})
