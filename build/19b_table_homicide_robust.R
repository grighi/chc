#!/usr/bin/env Rscript
# ============================================================================
# 19b_table_homicide_robust.R — Homicide Pre-Trend Robustness Checks
#
#   Investigates the homicide pre-trend and tests whether the all-cause
#   mortality result is robust to:
#     A. Finer pre-trend bins (diagnostic)
#     B. Removing homicide from the outcome
#     C. Dropping high-homicide counties
#     D. Adding county-specific linear trends
#
#   Reads: panel_nber_cause.fst (saved by 19a_table_cause_nber.R)
#   Outputs: table_homicide_robust.tex
#
#   Adapted from: server_cmhc/aer_data/15aa_verify_homicide.R
# ============================================================================
log_section("19b: Homicide Robustness")

# ============================================================================
# 1. LOAD DATA
# ============================================================================
cat("Loading panel from panel_nber_cause.fst ...\n")
panel <- as.data.table(fst::read_fst(file.path(DATA_DIR, "panel_nber_cause.fst")))
cat(sprintf("  Rows: %s, Counties: %d\n",
            format(nrow(panel), big.mark = ","), uniqueN(panel$fips)))

# Construct all-cause ex-homicide
panel[, amr_ad_ex_hom_nber := amr_ad_nber - amr_ad_homicide_nber]

# ============================================================================
# 2. HELPER FUNCTIONS
# ============================================================================

time_bins <- list(
  "[-6,-1]" = -6:-1,
  "[0,3]"   = 0:3,
  "[4,7]"   = 4:7,
  "[8,13]"  = 8:13
)

time_bins_fine <- list(
  "[-6,-4]" = -6:-4,
  "[-3,-1]" = -3:-1,
  "[0,3]"   = 0:3,
  "[4,7]"   = 4:7,
  "[8,13]"  = 8:13
)

hom_extract_es_coefs <- function(mod, max_et = 13) {
  cf <- coef(mod)
  se <- sqrt(diag(vcov(mod)))
  nms <- names(cf)
  idx <- grepl("^event_time_binned::", nms)
  et_vals <- as.integer(sub("^event_time_binned::(-?[0-9]+)$", "\\1", nms[idx]))
  df <- data.frame(event_time = et_vals, coefficient = unname(cf[idx]),
                   se = unname(se[idx]), stringsAsFactors = FALSE)
  df <- rbind(df, data.frame(event_time = -1L, coefficient = 0, se = 0))
  df <- df[order(df$event_time), ]
  df <- df[df$event_time <= max_et, ]
  rownames(df) <- NULL
  df
}

star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

FML_BASELINE <- "%s ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year + year^Durb + year^stfips"
FML_TREND    <- "%s ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + fips[year] + year + year^Durb + year^stfips"

run_es <- function(dv, data, fml_template = FML_BASELINE, tb = time_bins) {
  fml <- as.formula(sprintf(fml_template, dv))
  mod <- feols(fml, data = data, weights = ~popwt_ad, cluster = ~fips)
  coef_df <- hom_extract_es_coefs(mod, max_et = 13)

  bin_vals <- sapply(tb, function(et_range) {
    rows <- coef_df[coef_df$event_time %in% et_range, ]
    if (nrow(rows) == 0) return(list(mean = NA, se = NA, p = NA))
    avg    <- mean(rows$coefficient)
    avg_se <- sqrt(mean(rows$se^2))
    t_stat <- avg / avg_se
    p_val  <- 2 * pnorm(-abs(t_stat))
    list(mean = avg, se = avg_se, p = p_val)
  }, simplify = FALSE)

  bin_vals
}

# ============================================================================
# A. PRE-TREND DIAGNOSTIC (finer bins)
# ============================================================================
cat("\n  Direction A: Pre-Trend Diagnostic (finer pre-period bins)\n")

dvs_a <- c("amr_ad_nber", "amr_ad_homicide_nber", "amr_ad_ex_hom_nber")
labels_a <- c(
  amr_ad_nber          = "All Causes",
  amr_ad_homicide_nber = "Homicide",
  amr_ad_ex_hom_nber   = "All-Cause ex-Homicide"
)

results_a <- list()
for (dv in dvs_a) {
  cat(sprintf("  Estimating: %s ...\n", labels_a[[dv]]))
  results_a[[dv]] <- run_es(dv, panel, tb = time_bins_fine)
}

# ============================================================================
# B. ALL-CAUSE EX-HOMICIDE (standard bins)
# ============================================================================
cat("\n  Direction B: All-Cause Excluding Homicide\n")

results_b <- list()
for (dv in dvs_a) {
  cat(sprintf("  Estimating: %s ...\n", labels_a[[dv]]))
  results_b[[dv]] <- run_es(dv, panel, tb = time_bins)
}

# ============================================================================
# C. DROP HIGH-HOMICIDE COUNTIES
# ============================================================================
cat("\n  Direction C: Drop High-Homicide Counties\n")

hom_mean <- panel[!is.na(amr_ad_homicide_nber),
                  .(mean_hom = weighted.mean(amr_ad_homicide_nber, popwt_ad, na.rm = TRUE)),
                  by = fips]
cutoff <- quantile(hom_mean$mean_hom, 0.75, na.rm = TRUE)
low_hom_fips <- hom_mean[mean_hom <= cutoff]$fips

cat(sprintf("  Mean homicide AMR by county: median=%.1f, 75th pctile=%.1f\n",
            median(hom_mean$mean_hom), cutoff))
cat(sprintf("  Dropping %d counties (top quartile), keeping %d\n",
            nrow(hom_mean) - length(low_hom_fips), length(low_hom_fips)))

panel_lo <- panel[fips %in% low_hom_fips]

dvs_c <- c("amr_ad_nber_full", "amr_ad_nber_lo",
            "amr_ad_hom_full", "amr_ad_hom_lo")
labels_c <- c(
  amr_ad_nber_full = "All-Cause (full)",
  amr_ad_nber_lo   = "All-Cause (low-hom)",
  amr_ad_hom_full  = "Homicide (full)",
  amr_ad_hom_lo    = "Homicide (low-hom)"
)

results_c <- list()
results_c[["amr_ad_nber_full"]] <- run_es("amr_ad_nber", panel)
results_c[["amr_ad_nber_lo"]]   <- run_es("amr_ad_nber", panel_lo)
results_c[["amr_ad_hom_full"]]  <- run_es("amr_ad_homicide_nber", panel)
results_c[["amr_ad_hom_lo"]]    <- run_es("amr_ad_homicide_nber", panel_lo)

# ============================================================================
# D. COUNTY-SPECIFIC LINEAR TRENDS
# ============================================================================
cat("\n  Direction D: County-Specific Linear Trends\n")

dvs_d <- c("amr_ad_nber_base", "amr_ad_nber_trend",
            "amr_ad_hom_base", "amr_ad_hom_trend")
labels_d <- c(
  amr_ad_nber_base  = "All-Cause (baseline)",
  amr_ad_nber_trend = "All-Cause (cty trend)",
  amr_ad_hom_base   = "Homicide (baseline)",
  amr_ad_hom_trend  = "Homicide (cty trend)"
)

results_d <- list()
results_d[["amr_ad_nber_base"]]  <- run_es("amr_ad_nber", panel, FML_BASELINE)
results_d[["amr_ad_nber_trend"]] <- run_es("amr_ad_nber", panel, FML_TREND)
results_d[["amr_ad_hom_base"]]   <- run_es("amr_ad_homicide_nber", panel, FML_BASELINE)
results_d[["amr_ad_hom_trend"]]  <- run_es("amr_ad_homicide_nber", panel, FML_TREND)

# ============================================================================
# 5. GENERATE LATEX TABLE
# ============================================================================
cat("\nGenerating LaTeX table ...\n")

make_panel_rows <- function(results, dv_names, labels, tb) {
  rows <- character(0)
  for (dv in dv_names) {
    if (is.null(results[[dv]])) next
    lbl <- labels[[dv]]

    coef_vals <- character(length(tb))
    se_vals   <- character(length(tb))
    for (j in seq_along(names(tb))) {
      bn <- names(tb)[j]
      bv <- results[[dv]][[bn]]
      coef_vals[j] <- sprintf("%.3f%s", bv$mean, star(bv$p))
      se_vals[j]   <- sprintf("(%.3f)", bv$se)
    }

    rows <- c(rows,
      sprintf("%s & %s \\\\", lbl, paste(coef_vals, collapse = " & ")),
      sprintf(" & %s \\\\", paste(se_vals, collapse = " & "))
    )
  }
  rows
}

note_hom <- paste(
  "Panel~A uses finer pre-treatment bins $[-6,-4]$ and $[-3,-1]$ to diagnose pre-trend timing.",
  "Panel~B compares all-cause AMR with and without homicide deaths.",
  "Panel~C drops counties in the top quartile of mean homicide AMR.",
  "Panel~D adds county-specific linear trends.",
  "All specifications include county, year, urban$\\times$year, and state$\\times$year FE,",
  "controls for physician supply and hospital beds.",
  "Population-weighted (1960 base). Clustered at county level.",
  "* $p<0.10$, ** $p<0.05$, *** $p<0.01$."
)

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Homicide Pre-Trend Robustness (NBER-Derived AMRs, Ages 20--49)}",
  "\\label{tab:homicide_robust}",
  "\\small",
  "",
  "% Panel A: Finer bins",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "\\multicolumn{6}{l}{\\textit{Panel A: Finer Pre-Period Bins}} \\\\",
  "\\midrule",
  " & $[-6,-4]$ & $[-3,-1]$ & $[0,3]$ & $[4,7]$ & $[8,13]$ \\\\",
  "\\midrule",
  make_panel_rows(results_a, dvs_a, labels_a, time_bins_fine),
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\vspace{1em}",
  "",
  "% Panel B: Ex-Homicide",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "\\multicolumn{5}{l}{\\textit{Panel B: All-Cause vs.\\ All-Cause Excluding Homicide}} \\\\",
  "\\midrule",
  " & $[-6,-1]$ & $[0,3]$ & $[4,7]$ & $[8,13]$ \\\\",
  "\\midrule",
  make_panel_rows(results_b, dvs_a, labels_a, time_bins),
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\vspace{1em}",
  "",
  "% Panel C: Drop high-homicide",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "\\multicolumn{5}{l}{\\textit{Panel C: Full Sample vs.\\ Low-Homicide Counties}} \\\\",
  "\\midrule",
  " & $[-6,-1]$ & $[0,3]$ & $[4,7]$ & $[8,13]$ \\\\",
  "\\midrule",
  make_panel_rows(results_c, dvs_c, labels_c, time_bins),
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\vspace{1em}",
  "",
  "% Panel D: County trends",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "\\multicolumn{5}{l}{\\textit{Panel D: Baseline vs.\\ County-Specific Linear Trends}} \\\\",
  "\\midrule",
  " & $[-6,-1]$ & $[0,3]$ & $[4,7]$ & $[8,13]$ \\\\",
  "\\midrule",
  make_panel_rows(results_d, dvs_d, labels_d, time_bins),
  "\\bottomrule",
  "\\end{tabular}",
  "",
  sprintf("\\begin{minipage}{\\textwidth}"),
  sprintf("\\vspace{0.5em}\\footnotesize \\textit{Notes:} %s", note_hom),
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tex, file.path(TABLE_DIR, "table_homicide_robust.tex"))
cat(sprintf("  Saved %s\n", file.path(TABLE_DIR, "table_homicide_robust.tex")))

# Save CSV
hom_csv <- data.table()
all_results <- list(A = results_a, B = results_b, C = results_c, D = results_d)
all_dvs     <- list(A = dvs_a, B = dvs_a, C = dvs_c, D = dvs_d)
all_labels  <- list(A = labels_a, B = labels_a, C = labels_c, D = labels_d)
all_tb      <- list(A = time_bins_fine, B = time_bins, C = time_bins, D = time_bins)

for (pn in names(all_results)) {
  res <- all_results[[pn]]
  for (dv in all_dvs[[pn]]) {
    if (is.null(res[[dv]])) next
    for (bn in names(all_tb[[pn]])) {
      bv <- res[[dv]][[bn]]
      hom_csv <- rbind(hom_csv, data.table(
        panel = pn, outcome = dv, label = all_labels[[pn]][[dv]],
        bin = bn, mean_coef = bv$mean, se = bv$se, p = bv$p
      ))
    }
  }
}
save_csv(hom_csv, "table_homicide_robust.csv")
