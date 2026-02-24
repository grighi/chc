#!/usr/bin/env Rscript
# ============================================================================
# 20_fig_cmhc_race.R — CMHC Event Study by Race
# NOTE: Race-specific AMR for 20-49 is NOT in AER data.
#       Falls back to 50+ race decomposition (amr_w_eld, amr_nw_eld).
# ============================================================================
log_section("20: Figure — CMHC by Race")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Check for race-specific 20-49 variables
has_race_ad <- all(c("amr_w_ad", "amr_nw_ad") %in% names(panel))

if (has_race_ad) {
  cat("  Race-specific AMR (20-49) available. Running main spec.\n")
  outcomes <- list(
    list(var = "amr_w_ad",  label = "White (20-49)",    color = "steelblue"),
    list(var = "amr_nw_ad", label = "Nonwhite (20-49)", color = "darkred")
  )
  wt_var <- "popwt_ad"
} else {
  cat("  Race-specific AMR (20-49) NOT available.\n")
  cat("  Falling back to 50+ race decomposition (amr_w_eld, amr_nw_eld).\n")
  outcomes <- list(
    list(var = "amr_w_eld",  label = "White (50+)",    color = "steelblue"),
    list(var = "amr_nw_eld", label = "Nonwhite (50+)", color = "darkred")
  )
  wt_var <- "popwt_eld"
}

coef_list <- lapply(outcomes, function(o) {
  if (!o$var %in% names(panel)) {
    cat(sprintf("  WARNING: %s not in panel. Skipping.\n", o$var))
    return(NULL)
  }
  fml <- as.formula(sprintf(
    "%s ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips",
    o$var
  ))
  m <- feols(fml, data = panel, weights = as.formula(paste0("~", wt_var)), cluster = ~fips)
  coefs <- extract_es_coefs(m, max_et = CMHC_PLOT_MAX)
  coefs$race <- o$label
  coefs$color <- o$color
  coefs
})

coef_list <- Filter(Negate(is.null), coef_list)

if (length(coef_list) == 2) {
  coef_all <- bind_rows(coef_list)

  p <- ggplot(coef_all, aes(x = event_time, y = coefficient, color = race, fill = race)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.12, color = NA) +
    geom_point(size = 2, position = position_dodge(0.4)) +
    geom_line(linewidth = 0.8, position = position_dodge(0.4)) +
    scale_color_manual(values = setNames(sapply(outcomes, `[[`, "color"),
                                          sapply(outcomes, `[[`, "label"))) +
    scale_fill_manual(values = setNames(sapply(outcomes, `[[`, "color"),
                                         sapply(outcomes, `[[`, "label"))) +
    labs(
      title = "CMHC Event Study by Race",
      subtitle = ifelse(has_race_ad,
        "AMR, Ages 20-49, by race.",
        "AMR, Ages 50+, by race (20-49 race-specific data not available)."),
      x = "Years Relative to CMHC Opening",
      y = "Change in Deaths per 100,000",
      color = NULL, fill = NULL
    ) +
    theme_paper()

  save_fig(p, "fig_cmhc_race.pdf")
  save_csv(coef_all, "fig_cmhc_race_coefs.csv")
} else {
  cat("  Insufficient race data for race comparison figure.\n")
}
