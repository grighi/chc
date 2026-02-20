#!/usr/bin/env Rscript
# ============================================================================
# 26_table_ccbhc_heterogeneity.R — CCBHC Heterogeneity by Opioid Overdose
# ============================================================================
log_section("26: Table — CCBHC Heterogeneity")

panel <- arrow::read_parquet(file.path(DATA_DIR, "ccbhc_panel.parquet")) %>% as.data.table()

es <- panel[!is.na(mort_rate) & year > 2012]
es <- es[(event_time_binned >= CCBHC_EVENT_MIN & event_time_binned <= CCBHC_EVENT_MAX) |
          event_time_binned == -999]

# Pre-treatment mortality rate (2015-2017 average) as proxy for overdose severity
pre_mort <- es[year %in% 2015:2017 & !is.na(mort_rate),
               .(pre_mort = mean(mort_rate, na.rm = TRUE)), by = fips]
pre_mort[, high_mort := as.integer(pre_mort > median(pre_mort, na.rm = TRUE))]

es <- merge(es, pre_mort[, .(fips, high_mort)], by = "fips", all.x = TRUE)

results <- list()

for (hm in c(0L, 1L)) {
  label <- ifelse(hm == 1, "High Pre-Treatment Mortality", "Low Pre-Treatment Mortality")
  dt <- es[high_mort == hm]

  m <- tryCatch(
    feols(mort_rate ~ i(event_time_binned, ref = -1) + total_pop | fips + year,
          data = dt, weights = ~total_pop, cluster = ~fips),
    error = function(e) NULL
  )

  if (!is.null(m)) {
    coefs <- extract_es_coefs(m, max_et = CCBHC_EVENT_MAX)
    post <- coefs %>% filter(event_time >= 0)
    results[[label]] <- data.frame(
      group     = label,
      post_mean = mean(post$coefficient),
      post_se   = mean(post$se),
      n_treated = uniqueN(dt[treated == 1, fips]),
      n_control = uniqueN(dt[treated == 0, fips])
    )
  }
}

het_df <- do.call(rbind, results)
save_csv(het_df, "table_ccbhc_heterogeneity.csv")

# Write shell
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{CCBHC Mortality Effects: Heterogeneity by Pre-Treatment Mortality}",
  "\\label{tab:ccbhc_het}",
  "\\small",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "Group & Post-Treatment Mean & SE & N Treated & N Control \\\\",
  "\\midrule",
  "%%TABLE_BODY%%",
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(tex_lines, file.path(TABLE_DIR, "table_ccbhc_heterogeneity_shell.tex"))
