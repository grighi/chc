#!/usr/bin/env Rscript
# ============================================================================
# 17_table02_dd_robustness.R — DD Estimates, Robustness Specifications
# ============================================================================
log_section("17: Table 2 — DD Robustness")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Create post-treatment indicator
panel <- panel %>%
  mutate(post = as.integer(event_time_binned >= 0 & event_time_binned != -999))

results <- list()

# (1) County FE + Year FE only
m1 <- feols(amr_ad ~ post | fips + year,
            data = panel, weights = ~popwt_ad, cluster = ~fips)
results[["(1) County + Year FE"]] <- m1

# (2) + Urban x Year FE
m2 <- feols(amr_ad ~ post | fips + year^Durb,
            data = panel, weights = ~popwt_ad, cluster = ~fips)
results[["(2) + Urban x Year"]] <- m2

# (3) + State x Year FE (main spec)
m3 <- feols(amr_ad ~ post + D_tot_act_md_t + H_bpc
            + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`
            | fips + year^Durb + year^stfips,
            data = panel, weights = ~popwt_ad, cluster = ~fips)
results[["(3) Main Spec"]] <- m3

# (4) + State linear trends
m4 <- tryCatch({
  feols(amr_ad ~ post + D_tot_act_md_t + H_bpc
        + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`
        | fips + year^Durb + stfips[year],
        data = panel, weights = ~popwt_ad, cluster = ~fips)
}, error = function(e) {
  cat(sprintf("  Note: State trends spec failed: %s\n", e$message))
  NULL
})
if (!is.null(m4)) results[["(4) + State Trends"]] <- m4

# (6) Callaway & Sant'Anna
cs_result <- tryCatch({
  if (requireNamespace("did", quietly = TRUE)) {
    panel_cs <- panel %>%
      filter(cmhc_treated == 1 | event_time_binned == -999) %>%
      mutate(g = ifelse(cmhc_treated == 1, cmhc_year_exp, 0))
    cs <- did::att_gt(
      yname = "amr_ad", tname = "year", idname = "fips", gname = "g",
      data = as.data.frame(panel_cs),
      control_group = "nevertreated"
    )
    agg <- did::aggte(cs, type = "simple")
    data.frame(
      spec = "(6) Callaway-Sant'Anna",
      coef = agg$overall.att,
      se   = agg$overall.se,
      n    = nrow(panel_cs)
    )
  }
}, error = function(e) {
  cat(sprintf("  Note: C&S failed: %s\n", e$message))
  NULL
})

# (7) Sun & Abraham
m7 <- tryCatch({
  panel_sa <- panel %>%
    mutate(cohort = ifelse(cmhc_treated == 1, cmhc_year_exp, 10000))
  feols(amr_ad ~ sunab(cohort, year) + D_tot_act_md_t + H_bpc
        + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`
        | fips + year^Durb + year^stfips,
        data = panel_sa, weights = ~popwt_ad, cluster = ~fips)
}, error = function(e) {
  cat(sprintf("  Note: Sun & Abraham failed: %s\n", e$message))
  NULL
})

# Extract results into CSV
rob_rows <- lapply(names(results), function(nm) {
  m <- results[[nm]]
  data.frame(
    spec = nm,
    coef = coef(m)["post"],
    se   = sqrt(diag(vcov(m)))["post"],
    n    = nobs(m),
    stringsAsFactors = FALSE
  )
})
rob_df <- do.call(rbind, rob_rows)

# Add C&S if available
if (!is.null(cs_result)) rob_df <- rbind(rob_df, cs_result)

# Add S&A aggregated
if (!is.null(m7)) {
  sa_coefs <- coef(m7)
  sa_post <- sa_coefs[grepl("year::", names(sa_coefs))]
  # Average post-treatment
  et_names <- as.integer(gsub(".*::(-?[0-9]+):.*", "\\1", names(sa_post)))
  post_idx <- et_names >= 0
  if (sum(post_idx) > 0) {
    rob_df <- rbind(rob_df, data.frame(
      spec = "(7) Sun-Abraham",
      coef = mean(sa_post[post_idx]),
      se   = NA,
      n    = nobs(m7)
    ))
  }
}

rob_df$pval <- 2 * pnorm(-abs(rob_df$coef / rob_df$se))

save_csv(rob_df, "table02_dd_robustness.csv")

# Also run placebo (50+) for comparison row
m_eld <- feols(amr_eld ~ post + D_tot_act_md_t + H_bpc
               + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`
               | fips + year^Durb + year^stfips,
               data = panel, weights = ~popwt_eld, cluster = ~fips)

rob_eld <- data.frame(
  spec = "Placebo: AMR 50+",
  coef = coef(m_eld)["post"],
  se   = sqrt(diag(vcov(m_eld)))["post"],
  n    = nobs(m_eld),
  pval = 2 * pnorm(-abs(coef(m_eld)["post"] / sqrt(diag(vcov(m_eld)))["post"]))
)

save_csv(rbind(rob_df, rob_eld), "table02_dd_robustness_full.csv")

# Write shell
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Difference-in-Differences Estimates: CMHC Effect on AMR (Ages 20-49)}",
  "\\label{tab:dd_robust}",
  "\\small",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) & (6) & (7) \\\\",
  " & FE Only & +Urban$\\times$Year & Main & +State Trends & C\\&S & S\\&A \\\\",
  "\\midrule",
  "%%TABLE_BODY%%",
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(tex_lines, file.path(TABLE_DIR, "table02_dd_robustness_shell.tex"))
