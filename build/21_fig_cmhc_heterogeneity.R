#!/usr/bin/env Rscript
# ============================================================================
# 21_fig_cmhc_heterogeneity.R — CMHC Heterogeneity by Baseline Characteristics
# ============================================================================
log_section("21: Figure — CMHC Heterogeneity")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Compute county-level medians for splitting
xsec <- panel %>%
  filter(year == 1960) %>%
  select(fips, `_pct59inclt3k`, `_60pcturban`, copop)

# Add median splits
xsec <- xsec %>%
  mutate(
    high_poverty = as.integer(`_pct59inclt3k` > median(`_pct59inclt3k`, na.rm = TRUE)),
    high_urban   = as.integer(`_60pcturban` > median(`_60pcturban`, na.rm = TRUE)),
    high_pop     = as.integer(copop > median(copop, na.rm = TRUE))
  )

panel <- panel %>%
  left_join(xsec %>% select(fips, high_poverty, high_urban, high_pop), by = "fips")

# Run event study for each subsample
run_het_es <- function(data, subset_var, subset_val, label) {
  dt <- data %>% filter(!!sym(subset_var) == subset_val)
  tryCatch({
    m <- feols(
      amr_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
        + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`
      | fips + year^Durb + year^stfips,
      data = dt, weights = ~popwt_ad, cluster = ~fips
    )
    coefs <- extract_es_coefs(m, max_et = CMHC_PLOT_MAX)
    coefs$group <- label
    coefs
  }, error = function(e) {
    cat(sprintf("  Error for %s: %s\n", label, e$message))
    NULL
  })
}

splits <- list(
  list(var = "high_poverty", vals = c(0, 1),
       labels = c("Low Poverty", "High Poverty"), title = "A. By Baseline Poverty"),
  list(var = "high_urban", vals = c(0, 1),
       labels = c("Rural", "Urban"), title = "B. By Urbanicity"),
  list(var = "high_pop", vals = c(0, 1),
       labels = c("Small Counties", "Large Counties"), title = "C. By Population Size")
)

panels <- lapply(splits, function(s) {
  c1 <- run_het_es(panel, s$var, s$vals[1], s$labels[1])
  c2 <- run_het_es(panel, s$var, s$vals[2], s$labels[2])
  if (is.null(c1) || is.null(c2)) return(NULL)
  coef_all <- bind_rows(c1, c2)

  ggplot(coef_all, aes(x = event_time, y = coefficient, color = group, fill = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.1, color = NA) +
    geom_point(size = 1.5, position = position_dodge(0.4)) +
    geom_line(linewidth = 0.7, position = position_dodge(0.4)) +
    scale_color_manual(values = c("steelblue", "darkred")) +
    scale_fill_manual(values = c("steelblue", "darkred")) +
    labs(title = s$title, x = NULL, y = NULL, color = NULL, fill = NULL) +
    theme_paper(base_size = 10)
})

panels <- Filter(Negate(is.null), panels)

if (length(panels) >= 2) {
  p <- wrap_plots(panels, ncol = 1) +
    plot_annotation(
      title = "CMHC Effects on Mortality (Ages 20-49): Heterogeneity",
      subtitle = "Split at median of baseline county characteristic.",
      theme = theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray30")
      )
    )

  save_fig(p, "fig_cmhc_heterogeneity.pdf", width = 10, height = 4 * length(panels))
}
