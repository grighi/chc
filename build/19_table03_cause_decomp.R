#!/usr/bin/env Rscript
# ============================================================================
# 19_table03_cause_decomp.R — Cause-Specific Mortality Decomposition, CMHC
# ============================================================================
log_section("19: Table 3 — Cause Decomposition")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Create post-treatment indicator for DD
panel <- panel %>%
  mutate(post = as.integer(event_time_binned >= 0 & event_time_binned != -999))

# Cause-specific outcomes (AER variable names)
cause_vars <- c(
  "amr_ad"   = "All-Cause (20-49)",
  "amr_ad_2" = "Cardiovascular",
  "amr_ad_3" = "Cerebrovascular",
  "amr_ad_4" = "Cancer",
  "amr_ad_5" = "Infectious Disease",
  "amr_ad_6" = "Diabetes",
  "amr_ad_7" = "Accidents"
)

# Run main spec for each cause
cause_results <- lapply(names(cause_vars), function(v) {
  if (!v %in% names(panel)) {
    cat(sprintf("  WARNING: %s not in panel. Skipping.\n", v))
    return(NULL)
  }

  tryCatch({
    fml <- as.formula(sprintf(
      "%s ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit` | fips + year^Durb + year^stfips",
      v
    ))
    m <- feols(fml, data = panel, weights = ~popwt_ad, cluster = ~fips)

    coefs <- extract_es_coefs(m, max_et = CMHC_PLOT_MAX)
    post_coefs <- coefs %>% filter(event_time >= 0)

    # Pre-trend test
    pre_names <- paste0("event_time_binned::", CMHC_EVENT_MIN:-2)
    pre_names <- pre_names[pre_names %in% names(coef(m))]
    pre_p <- NA
    if (length(pre_names) > 1) {
      wt <- wald(m, pre_names)
      pre_p <- wt$p
    }

    data.frame(
      cause         = cause_vars[v],
      variable      = v,
      post_mean     = mean(post_coefs$coefficient),
      post_se       = mean(post_coefs$se),
      pre_trend_p   = pre_p,
      n_obs         = nobs(m),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat(sprintf("  Error for %s: %s\n", v, e$message))
    NULL
  })
})

cause_df <- do.call(rbind, Filter(Negate(is.null), cause_results))

save_csv(cause_df, "table03_cause_decomp_cmhc.csv")

# Compute residual (all-cause minus named causes)
if (all(c("amr_ad", "amr_ad_2", "amr_ad_3", "amr_ad_4", "amr_ad_5", "amr_ad_6", "amr_ad_7") %in% names(panel))) {
  cat("  Computing residual cause category...\n")
  panel <- panel %>%
    mutate(amr_ad_resid = amr_ad - (amr_ad_2 + amr_ad_3 + amr_ad_4 + amr_ad_5 + amr_ad_6 + amr_ad_7))

  m_resid <- feols(
    amr_ad_resid ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
      + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`
    | fips + year^Durb + year^stfips,
    data = panel, weights = ~popwt_ad, cluster = ~fips
  )
  coefs_resid <- extract_es_coefs(m_resid, max_et = CMHC_PLOT_MAX)
  post_resid <- coefs_resid %>% filter(event_time >= 0)

  resid_row <- data.frame(
    cause = "Other (Residual)",
    variable = "amr_ad_resid",
    post_mean = mean(post_resid$coefficient),
    post_se   = mean(post_resid$se),
    pre_trend_p = NA,
    n_obs = nobs(m_resid)
  )
  cause_df <- rbind(cause_df, resid_row)
  save_csv(cause_df, "table03_cause_decomp_cmhc.csv")
}

# Write shell
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Cause-Specific Mortality Decomposition: CMHC Effects (Ages 20-49)}",
  "\\label{tab:cause_decomp}",
  "\\small",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Cause of Death & Post-Treatment Mean & SE & Pre-Trend $p$ \\\\",
  "\\midrule",
  "%%TABLE_BODY%%",
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(tex_lines, file.path(TABLE_DIR, "table03_cause_decomp_cmhc_shell.tex"))
