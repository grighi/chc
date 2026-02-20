#!/usr/bin/env Rscript
# ============================================================================
# 24_fig09_ccbhc_robust.R — CCBHC Mortality Robustness
# ============================================================================
log_section("24: Figure 9 — CCBHC Robustness")

panel <- arrow::read_parquet(file.path(DATA_DIR, "ccbhc_panel.parquet")) %>% as.data.table()

es <- panel[!is.na(mort_rate) & year > 2012]
es <- es[(event_time_binned >= CCBHC_EVENT_MIN & event_time_binned <= CCBHC_EVENT_MAX) |
          event_time_binned == -999]

# Load planning grant states
planning_states <- tryCatch(
  read_csv(file.path(DATA_DIR, "planning_grant_states.csv"), show_col_types = FALSE)$stfips,
  error = function(e) NULL
)

# --- Panel A: Exclude Medicaid waiver states ---
es_no_waiver <- es[!stfips %in% WAIVER_ST_FIPS]
cat(sprintf("  Panel A (excl. waiver): %d treated, %d control\n",
            uniqueN(es_no_waiver[treated == 1, fips]),
            uniqueN(es_no_waiver[treated == 0, fips])))

m_a <- feols(mort_rate ~ i(event_time_binned, ref = -1) + total_pop | fips + year,
             data = es_no_waiver, weights = ~total_pop, cluster = ~fips)
coef_a <- extract_es_coefs(m_a, max_et = CCBHC_EVENT_MAX)
coef_a$panel <- "Excl. Medicaid Waiver States"

# --- Panel B: Planning grant states only ---
if (!is.null(planning_states) && length(planning_states) > 0) {
  es_planning <- es[stfips %in% planning_states]
  cat(sprintf("  Panel B (planning): %d treated, %d control\n",
              uniqueN(es_planning[treated == 1, fips]),
              uniqueN(es_planning[treated == 0, fips])))

  m_b <- feols(mort_rate ~ i(event_time_binned, ref = -1) + total_pop | fips + year,
               data = es_planning, weights = ~total_pop, cluster = ~fips)
  coef_b <- extract_es_coefs(m_b, max_et = CCBHC_EVENT_MAX)
  coef_b$panel <- "Planning Grant States Only"
} else {
  coef_b <- NULL
  cat("  Panel B skipped: no planning grant state data.\n")
}

# Plot
p_a <- ggplot(coef_a, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
  geom_point(color = "steelblue", size = 2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(title = "A. Excluding Medicaid Waiver States",
       x = "Years Relative to CCBHC Grant", y = "Change in Deaths per 100,000") +
  theme_paper()

plots <- list(p_a)

if (!is.null(coef_b)) {
  p_b <- ggplot(coef_b, aes(x = event_time, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "darkred", alpha = 0.2) +
    geom_point(color = "darkred", size = 2) +
    geom_line(color = "darkred", linewidth = 0.8) +
    labs(title = "B. Planning Grant States Only",
         x = "Years Relative to CCBHC Grant", y = "Change in Deaths per 100,000") +
    theme_paper()
  plots <- c(plots, list(p_b))
}

p <- wrap_plots(plots, ncol = 1) +
  plot_annotation(
    title = "CCBHC Mortality Robustness",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )

save_fig(p, "fig09_ccbhc_robust.pdf", height = 5 * length(plots))
save_csv(coef_a, "ccbhc_rob_no_waiver_coefs.csv")
if (!is.null(coef_b)) save_csv(coef_b, "ccbhc_rob_planning_coefs.csv")
