#!/usr/bin/env Rscript
# ============================================================================
# 17_table02_dd_robustness.R — Robustness Checks: CMHC–Mortality
# Structure mirrors Bailey & Goodman-Bacon (2015, AER) Table 2
#   4 columns:  (1) C, U-Y
#               (2) C, U-Y, S-Y, R, D·Year
#               (3) C, U-Y, S-Y, R, C·Year
#               (4) C, U-Y, S-Y, R, P-weights
#   2 panels:   A = ages 20-49, B = 50+ (placebo)
#   Rows:       binned DD event-time categories
# ============================================================================
log_section("17: Table 2 — DD Robustness")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# ── 1. Create DD event-time bins (matching BGB did1 coding) ─────────────────
#   2  = pre    (t = -6 to -2)
#  -1  = ref    (t = -1, omitted)
#   4  = post   (t = 0  to  4)
#   5  = post   (t = 5  to  9)
#   6  = post   (t = 10 to 14)
#   0  = never-treated  (omitted)
#  99  = endpoint / outside window (omitted)
panel <- panel %>%
  mutate(
    did_bin = case_when(
      is.na(event_time)                    ~ 0L,
      event_time >= -6  & event_time <= -2 ~ 2L,
      event_time == -1                     ~ -1L,
      event_time >= 0   & event_time <= 4  ~ 4L,
      event_time >= 5   & event_time <= 9  ~ 5L,
      event_time >= 10  & event_time <= 14 ~ 6L,
      TRUE                                 ~ 99L
    )
  )

# ── 2. Identify available covariates ────────────────────────────────────────
# D_ : 1960 characteristics × linear trend
# R_ : annual county-level covariates
# H_ : hospital capacity
# D_vars <- intersect(c("D_tot_act_md_t", "D_pct59inclt3k_t",
#                        "D_60pctnonwhit_t", "D_60pcturban_t",
#                        "D_60pctrurf_t"), names(panel))
D_vars <- intersect(c("D_tot_act_md_t", 
                       "D_60pctrurf_t"), names(panel))
R_vars <- intersect(c("R_tranpcret", "R_tranpcpa1"), names(panel))
H_vars <- intersect(c("H_bpc", "H_hpc"), names(panel))

cov_all <- paste(c(D_vars, R_vars, H_vars), collapse = " + ")
cov_noD <- paste(c(R_vars, H_vars), collapse = " + ")

cat(sprintf("  Covariates found: D=%d  R=%d  H=%d\n",
            length(D_vars), length(R_vars), length(H_vars)))

# ── 3. Run four specifications per panel ────────────────────────────────────
run_panel_specs <- function(yvar, wvar, dfl_wvar, dat) {

  dd <- "i(did_bin, ref = c(-1, 0, 99))"

  f1 <- as.formula(sprintf("%s ~ %s | fips + year^Durb", yvar, dd))
  f2 <- as.formula(sprintf("%s ~ %s + %s | fips + year^Durb + year^stfips",
                           yvar, dd, cov_all))
  f3 <- as.formula(sprintf(
    "%s ~ %s + %s | fips + year^Durb + year^stfips + fips[year]",
    yvar, dd, cov_noD))
  f4 <- f2                        # same formula as (2), different weights

  wt     <- as.formula(paste0("~", wvar))
  models <- vector("list", 4)

  cat("    (1) C, U-Y ... ")
  models[[1]] <- feols(f1, data = dat, weights = wt, cluster = ~fips)
  cat("done\n")

  cat("    (2) + S-Y, R, D·Year ... ")
  models[[2]] <- feols(f2, data = dat, weights = wt, cluster = ~fips)
  cat("done\n")

  cat("    (3) + C·Year (county trends) ... ")
  models[[3]] <- tryCatch(
    feols(f3, data = dat, weights = wt, cluster = ~fips),
    error = function(e) { cat(sprintf("FAILED: %s\n", e$message)); NULL }
  )
  if (!is.null(models[[3]])) cat("done\n")

  cat("    (4) P-weights ... ")
  if (dfl_wvar %in% names(dat) &&
      any(!is.na(dat[[dfl_wvar]]) & dat[[dfl_wvar]] > 0)) {
    models[[4]] <- tryCatch(
      feols(f4, data = dat,
            weights = as.formula(paste0("~", dfl_wvar)),
            cluster = ~fips),
      error = function(e) { cat(sprintf("FAILED: %s\n", e$message)); NULL }
    )
    if (!is.null(models[[4]])) cat("done\n")
  } else {
    cat("weight variable unavailable, skipped\n")
    models[[4]] <- NULL
  }

  models
}

# ── 4. Extract DD coefficients from a list of 4 models ─────────────────────
extract_dd <- function(models) {
  bins      <- c(2, 4, 5, 6)
  bin_names <- paste0("did_bin::", bins)

  out <- data.frame(
    bin   = bins,
    label = c("Years -6 to -2", "Years 0 to 4",
              "Years 5 to 9", "Years 10 to 14"),
    stringsAsFactors = FALSE
  )

  for (s in seq_along(models)) {
    m <- models[[s]]
    if (is.null(m)) {
      out[[paste0("b",  s)]] <- NA_real_
      out[[paste0("se", s)]] <- NA_real_
    } else {
      cf <- coef(m)
      sv <- sqrt(diag(vcov(m)))
      out[[paste0("b",  s)]] <- unname(cf[bin_names])
      out[[paste0("se", s)]] <- unname(sv[bin_names])
    }
  }

  r2_vec <- sapply(models, function(m)
    if (is.null(m)) NA_real_ else r2(m, type = "wr2"))

  list(coefs = out, r2 = r2_vec)
}

# ── 5. Panel A: Age-adjusted mortality, ages 20-49 ─────────────────────────
cat("\n  ── Panel A: AMR  ──\n")
mods_a <- run_panel_specs("amr", "popwt_ad", "dflpopwgt1_ad", panel)
res_a  <- extract_dd(mods_a)

mean_a <- panel %>%
  filter(event_time == -1) %>%
  summarise(m = weighted.mean(amr, popwt_ad, na.rm = TRUE)) %>%
  pull(m)
cat(sprintf("  Mean at t*=-1: %.1f\n", mean_a))

# ── 6. Panel B: Age-adjusted mortality, 50+ (placebo) ─────────────────────
cat("\n  ── Panel B: AMR 20-49 ──\n")

# Adjust hospital variables to per-elderly-capita (following BGB)
panel_b <- panel %>%
  mutate(H_bpc = H_bpc * (copop / copop_eld),
         H_hpc = H_hpc * (copop / copop_eld))

mods_b <- run_panel_specs("amr_ad", "popwt_eld", "dflpopwgt1_eld", panel_b)
res_b  <- extract_dd(mods_b)

mean_b <- panel %>%
  filter(event_time == -1) %>%
  summarise(m = weighted.mean(amr_ad, popwt_eld, na.rm = TRUE)) %>%
  pull(m)
cat(sprintf("  Mean at t*=-1: %.1f\n", mean_b))

# ── 7. Save CSV ────────────────────────────────────────────────────────────
csv_out <- bind_rows(
  res_a$coefs %>% mutate(panel = "A: AMR 20-49"),
  res_b$coefs %>% mutate(panel = "B: AMR 50+")
)
save_csv(csv_out, "table02_dd_robustness.csv")

# ── 8. Build LaTeX table ───────────────────────────────────────────────────
fc <- function(x) ifelse(is.na(x), "", sprintf("%.1f", x))
fs <- function(x) ifelse(is.na(x), "", sprintf("[%.1f]", abs(x)))
fr <- function(x) ifelse(is.na(x), "", sprintf("%.2f", x))

panel_block <- function(res, title, mu) {
  rc <- res$coefs
  lines <- c(
    sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", title),
    sprintf("Mean at $t^* = -1$ & \\multicolumn{4}{c}{%s} \\\\",
            format(round(mu, 1), big.mark = ","))
  )
  for (i in seq_len(nrow(rc))) {
    b_cells  <- paste(fc(rc$b1[i]),  fc(rc$b2[i]),
                      fc(rc$b3[i]),  fc(rc$b4[i]),  sep = " & ")
    se_cells <- paste(fs(rc$se1[i]), fs(rc$se2[i]),
                      fs(rc$se3[i]), fs(rc$se4[i]), sep = " & ")
    lines <- c(lines,
      sprintf("Years $%s$ & %s \\\\",
              sub("Years ", "", rc$label[i]), b_cells),
      sprintf(" & %s \\\\", se_cells))
  }
  r2_cells <- paste(fr(res$r2), collapse = " & ")
  lines <- c(lines, sprintf("$R^2$ & %s \\\\", r2_cells))
  lines
}

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Robustness Checks on the Relationship between",
  "Community Mental Health Centers and All-Cause Mortality Rates}",
  "\\label{tab:dd_robust}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  "\\midrule",
  panel_block(res_a,
    "Panel A. Age-adjusted mortality", mean_a),
  "\\midrule",
  panel_block(res_b,
    "Panel B. Age-adjusted mortality, ages 20--49", mean_b),
  "\\midrule",
  "Covariates & C, U--Y & C, U--Y, S--Y, & C, U--Y, S--Y, & C, U--Y, S--Y, \\\\",
  " & & R, D$\\cdot$Year & R, C$\\cdot$Year & R, P-weights \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{\\textwidth}",
  "\\vspace{6pt}",
  "\\footnotesize",
  paste0(
    # "\\textit{Notes:} Models presented are weighted least-squares estimates ",
    # "using event-year categories. C: county fixed effects; U--Y: urban by ",
    # "year fixed effects; S--Y: state-by-year fixed effects; R: annual, ",
    # "county-level covariates; D$\\cdot$Year: 1960 characteristics interacted ",
    # "with linear time trends; C$\\cdot$Year: county-specific linear time ",
    # "trends; P-weights: uses an estimate of the propensity of receiving a ",
    # "CHC to reweight untreated counties. See text for more details. ",
    # "Weights are the relevant county populations in 1960. Standard errors ",
    # "clustered at the county level in brackets."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tex, file.path(TABLE_DIR, "table02_dd_robustness.tex"))
cat(sprintf("\n  Saved: %s\n", file.path(TABLE_DIR, "table02_dd_robustness.tex")))
