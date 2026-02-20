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
