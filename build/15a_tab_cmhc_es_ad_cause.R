#!/usr/bin/env Rscript
# ============================================================================
# 15a_tab_cmhc_es_ad_cause.R — CMHC Event Study by Cause of Death (AER vars)
#   Runs the main TWFE event-study specification on each cause-specific AMR
#   (amr_ad_2 through amr_ad_7) and prints a summary table with grouped
#   time-bin averages.
# ============================================================================
log_section("15a: Cause-of-Death Event Studies (Ages 20-49, AER variables)")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# ── Cause dictionary ─────────────────────────────────────────────────────────
causes <- c(
  amr_ad   = "All Causes",
  amr_ad_2 = "Cardiovascular",
  amr_ad_3 = "Cerebrovascular",
  amr_ad_4 = "Cancer",
  amr_ad_5 = "Infectious Disease",
  amr_ad_6 = "Diabetes",
  amr_ad_7 = "Accidents"
)

# ── Time bins ─────────────────────────────────────────────────────────────────
time_bins <- list(
  "[-6,-1]"  = -6:-1,
  "[0,3]"    = 0:3,
  "[4,7]"    = 4:7,
  "[8,13]"   = 8:13
)

# ── Run models ────────────────────────────────────────────────────────────────
results <- list()

for (dv in names(causes)) {
  cat(sprintf("  Estimating: %s (%s) ...\n", dv, causes[dv]))

  # Build formula dynamically
  fml <- as.formula(sprintf(
    "%s ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year + year^Durb + year^stfips",
    dv
  ))

  mod <- feols(
    fml,
    data    = panel,
    weights = ~popwt_ad,
    cluster = ~fips
  )

  coef_df <- extract_es_coefs(mod, max_et = CMHC_PLOT_MAX)

  # Compute grouped-bin averages (coefficient & stars)
  bin_vals <- sapply(time_bins, function(et_range) {
    rows <- coef_df[coef_df$event_time %in% et_range, ]
    if (nrow(rows) == 0) return(list(mean = NA, se = NA, p = NA))

    avg   <- mean(rows$coefficient)
    avg_se <- sqrt(mean(rows$se^2))  # approximate pooled SE
    t_stat <- avg / avg_se
    p_val  <- 2 * pnorm(-abs(t_stat))

    list(mean = avg, se = avg_se, p = p_val)
  }, simplify = FALSE)

  results[[dv]] <- bin_vals
}

# ── Build display table ──────────────────────────────────────────────────────
star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}

# Coefficient row + SE row per cause
row_list <- list()
for (dv in names(causes)) {
  coef_row <- c(Cause = causes[dv])
  se_row   <- c(Cause = "")
  for (bn in names(time_bins)) {
    bv <- results[[dv]][[bn]]
    coef_row[bn] <- sprintf("%.3f%s", bv$mean, star(bv$p))
    se_row[bn]   <- sprintf("(%.3f)", bv$se)
  }
  row_list[[length(row_list) + 1]] <- coef_row
  row_list[[length(row_list) + 1]] <- se_row
}

tbl <- do.call(rbind, row_list)

colnames(tbl)[1] <- "Cause"

# ── Print table ───────────────────────────────────────────────────────────────
cat("\n")
cat("  ══════════════════════════════════════════════════════════════════\n")
cat("  CMHC Event Study — Cause-Specific Grouped Coefficients (Ages 20-49)\n")
cat("  ══════════════════════════════════════════════════════════════════\n\n")

# Header
col_widths <- c(20, rep(14, length(time_bins)))
header <- sprintf("%-*s", col_widths[1], "Cause")
for (i in seq_along(names(time_bins))) {
  header <- paste0(header, sprintf("%*s", col_widths[i + 1], names(time_bins)[i]))
}
cat("  ", header, "\n", sep = "")
cat("  ", paste(rep("─", sum(col_widths)), collapse = ""), "\n", sep = "")

for (i in seq_len(nrow(tbl))) {
  line <- sprintf("%-*s", col_widths[1], tbl[i, "Cause"])
  for (j in seq_along(names(time_bins))) {
    line <- paste0(line, sprintf("%*s", col_widths[j + 1], tbl[i, names(time_bins)[j]]))
  }
  cat("  ", line, "\n", sep = "")
}

cat("  ", paste(rep("─", sum(col_widths)), collapse = ""), "\n", sep = "")
cat("  Stars: * p<0.10, ** p<0.05, *** p<0.01\n")
cat("  Cells show mean coefficient across event times in each bin.\n")
cat("  SE computed as sqrt(mean(se_i^2)) within each bin.\n\n")

# ── Save CSV ──────────────────────────────────────────────────────────────────
out_df <- do.call(rbind, lapply(names(causes), function(dv) {
  do.call(rbind, lapply(names(time_bins), function(bn) {
    bv <- results[[dv]][[bn]]
    data.frame(
      outcome  = dv,
      cause    = causes[dv],
      bin      = bn,
      mean_coef = bv$mean,
      se       = bv$se,
      p_value  = bv$p,
      stringsAsFactors = FALSE
    )
  }))
}))

save_csv(out_df, "cmhc_es_ad_cause_bins.csv")
