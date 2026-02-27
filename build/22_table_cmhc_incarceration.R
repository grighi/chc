#!/usr/bin/env Rscript
# ============================================================================
# 22_table_cmhc_incarceration.R — Incarceration Long-Difference Regressions
# Adapted from: cmhc_event_study.R Step 13
# ============================================================================
log_section("22: Table — CMHC Incarceration Long-Difference")

panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet"))

# Load full CMHC openings (all cohorts, not just <=1975)
cmhc_all <- read_csv(file.path(CMHC_DIR, "cmhc_openings.csv"), show_col_types = FALSE)

# Load incarceration data directly
inc <- fst::read_fst(file.path(ROOT, "incarceration_trends_county.fst"), as.data.table = TRUE)
inc[, fips := as.integer(fips)]
inc_annual <- inc[, .(jail_rate = mean(total_jail_pop_rate, na.rm = TRUE)),
                   by = .(fips, year)]
inc_annual[is.nan(jail_rate), jail_rate := NA_real_]

# 1970 baseline
data_1970 <- panel %>%
  filter(year == 1970) %>%
  select(fips, stfips, cofips, Durb,
         `_60pcturban`, `_60pctrurf`, `_60pct04years`, `_60pctmt64years`,
         `_60pctnonwhit`, `_60pctmt12schl`, `_60pctlt4schl`,
         `_pct59inclt3k`, `_pct59incmt10k`, `_tot_act_md`,
         copop, popwt)

# Helper: run long-difference
run_long_diff <- function(end_year) {
  end_data <- inc_annual[year == end_year, .(fips, jail_rate_end = jail_rate)]

  ld <- data_1970 %>%
    inner_join(inc_annual[year == 1970, .(fips, jail_rate_1970 = jail_rate)] %>% as_tibble(),
               by = "fips") %>%
    inner_join(as_tibble(end_data), by = "fips") %>%
    left_join(cmhc_all, by = "fips") %>%
    mutate(
      delta_log_jail = log(jail_rate_end) - log(jail_rate_1970),
      cmhc = as.integer(!is.na(cmhc_year_exp) & cmhc_year_exp <= end_year),
      log_pop = log(popwt)
    ) %>%
    filter(!is.na(delta_log_jail) & is.finite(delta_log_jail))

  # Small counties
  ld_small <- ld %>%
    filter(log_pop < log(30000)) %>%
    group_by(stfips) %>% filter(n() > 1) %>% ungroup()

  if (nrow(ld_small) < 10 || sum(ld_small$cmhc) < 3) {
    return(data.frame(end_year = end_year, coef = NA, se = NA, pval = NA,
                      n = nrow(ld_small), n_treated = sum(ld_small$cmhc)))
  }

  m <- lm(delta_log_jail ~ cmhc + `_60pcturban` + `_60pctrurf` +
            `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` +
            `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` +
            `_pct59incmt10k` + `_tot_act_md` + factor(stfips),
          weights = popwt, data = ld_small)

  rse <- sqrt(diag(vcovHC(m, type = "HC1")))
  p <- 2 * pt(-abs(coef(m)["cmhc"] / rse["cmhc"]), df = m$df.residual)

  data.frame(
    end_year  = end_year,
    coef      = coef(m)["cmhc"],
    se        = rse["cmhc"],
    pval      = p,
    n         = nobs(m),
    n_treated = sum(ld_small$cmhc)
  )
}

end_years <- c(1978, 1983, 1988, 1993, 2000)
ld_results <- do.call(rbind, lapply(end_years, run_long_diff))

cat("\n  Long-difference results (small counties, controls + state FE):\n")
print(ld_results)

save_csv(ld_results, "table_cmhc_incarceration_ld.csv")

# ── Formatted terminal output ──────────────────────────────────────────────
star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("")
}

cat("\n══════════════════════════════════════════════════════════════════════\n")
cat("  Long-Difference: CMHC Effect on Δlog(Jail Rate)\n")
cat("  ══════════════════════════════════════════════════════════════════════\n\n")

# Panel A
cat("  Panel A: Any CMHC (ever-treated vs never)\n")
cat("  ────────────────────────────────────────────────────────────────────────────────────────── \n")
cat("                           1970-78       1970-83       1970-88       1970-93       1970-00\n")
  cat(" \n")
cat("  ────────────────────────────────────────────────────────────────────────────────────────── \n")
cat("  Stars: * p<0.10, ** p<0.05, *** p<0.01. HC1 robust SEs.\n")
cat("  Sample: small counties (pop < 30k), state FE + 1960 census controls.\n")

# ── Panel B: By CMHC Cohort (Earlier vs Later Treated) ─────────────────────
# Identify main observed cohorts (years of first CMHC opening)
cohort_years <- sort(unique(na.omit(cmhc_all$cmhc_year_exp)))
# Optionally, restrict to cohorts with enough counties
cohort_counts <- table(cmhc_all$cmhc_year_exp)
main_cohorts <- as.integer(names(cohort_counts[cohort_counts >= 10]))
main_cohorts <- sort(main_cohorts)

run_long_diff_cohort <- function(end_year) {
  end_data <- inc_annual[year == end_year, .(fips, jail_rate_end = jail_rate)]

  ld <- data_1970 %>%
    inner_join(inc_annual[year == 1970, .(fips, jail_rate_1970 = jail_rate)] %>% as_tibble(),
               by = "fips") %>%
    inner_join(as_tibble(end_data), by = "fips") %>%
    left_join(cmhc_all, by = "fips") %>%
    mutate(
      delta_log_jail = log(jail_rate_end) - log(jail_rate_1970),
      any_cmhc = as.integer(!is.na(cmhc_year_exp) & cmhc_year_exp <= end_year),
      log_pop = log(popwt)
    ) %>%
    filter(!is.na(delta_log_jail) & is.finite(delta_log_jail))

  # Small counties
  ld_small <- ld %>%
    filter(log_pop < log(30000)) %>%
    group_by(stfips) %>% filter(n() > 1) %>% ungroup()

  # Build cohort dummies
  for (cy in main_cohorts) {
    ld_small[[paste0("cmhc_", cy)]] <- as.integer(!is.na(ld_small$cmhc_year_exp) & ld_small$cmhc_year_exp == cy & cy <= end_year)
  }

  # Only keep if enough treated
  if (nrow(ld_small) < 10 || sum(ld_small$any_cmhc) < 3) {
    return(data.frame(end_year = end_year, var = NA, coef = NA, se = NA, pval = NA, n = nrow(ld_small), n_treated = sum(ld_small$any_cmhc)))
  }

  # Regression: any_cmhc + cohort dummies (omit one for collinearity)
  cohort_vars <- paste0("cmhc_", main_cohorts)
  # Omit the last cohort as reference
  cohort_vars <- cohort_vars[-length(cohort_vars)]
  fml <- as.formula(paste(
    "delta_log_jail ~ any_cmhc +",
    paste(cohort_vars, collapse = " + "),
    "+ `_60pcturban` + `_60pctrurf` + `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` + `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` + `_pct59incmt10k` + `_tot_act_md` + factor(stfips)"
  ))

  m <- lm(fml, weights = popwt, data = ld_small)
  rse <- sqrt(diag(vcovHC(m, type = "HC1")))
  coefs <- coef(m)

  # Collect results for any_cmhc and each cohort dummy
  out <- data.frame(
    end_year = end_year,
    var = c("any_cmhc", cohort_vars),
    coef = NA, se = NA, pval = NA, n = nobs(m), n_treated = sum(ld_small$any_cmhc)
  )
  for (i in seq_along(out$var)) {
    v <- out$var[i]
    if (v %in% names(coefs)) {
      out$coef[i] <- coefs[v]
      out$se[i] <- rse[v]
      out$pval[i] <- 2 * pt(-abs(coefs[v]/rse[v]), df = m$df.residual)
    }
  }
  out
}

ld_results_cohort <- do.call(rbind, lapply(end_years, run_long_diff_cohort))

cat("\n  Panel B: Long-diff by CMHC cohort (small counties, controls + state FE):\n")
print(ld_results_cohort)

save_csv(ld_results_cohort, "table_cmhc_incarceration_ld_panelB.csv")

# Write shell
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Long-Difference: CMHC Effect on $\\Delta\\log$(Jail Rate)}",
  "\\label{tab:incarceration_ld}",
  "\\small",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & 1970-78 & 1970-83 & 1970-88 & 1970-93 & 1970-00 \\\\",
  "\\midrule",
  "%%TABLE_BODY%%",
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(tex_lines, file.path(TABLE_DIR, "table_cmhc_incarceration_ld_shell.tex"))

# Optionally, you can update the LaTeX shell to add a Panel B placeholder:
tex_lines_panelB <- c(
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel B: By CMHC Cohort}} \\",
  "%%TABLE_BODY_PANELB%%"
)
writeLines(tex_lines_panelB, file.path(TABLE_DIR, "table_cmhc_incarceration_ld_panelB_shell.tex"))
