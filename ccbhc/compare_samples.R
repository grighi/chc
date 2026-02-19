#!/usr/bin/env Rscript
# compare_samples.R
#
# Compare the incarceration and mortality event study samples to understand
# differences in sample composition, missing data patterns, and county characteristics.
#
# Prerequisites: Run ccbhc_grants_did.R twice (once for each ANALYSIS_TYPE)
#                and save es_inc and es_mort objects
#
# Usage:
#   1. Run mortality analysis and save: saveRDS(es, "es_mort.rds")
#   2. Run incarceration analysis and save: saveRDS(es, "es_inc.rds")
#   3. Run this script: Rscript compare_samples.R

library(data.table)

cat("\n=============================================================================================\n")
cat("CCBHC Sample Comparison: Mortality vs Incarceration\n")
cat("=============================================================================================\n\n")

# ========== 1. Load both event study datasets ================================

if (!file.exists("es_mort.rds") || !file.exists("es_inc.rds")) {
  cat("ERROR: Missing required files. Please run:\n")
  cat("  1. Set ANALYSIS_TYPE='mortality' in ccbhc_grants_did.R and run, then add: saveRDS(es, 'es_mort.rds')\n")
  cat("  2. Set ANALYSIS_TYPE='incarceration' in ccbhc_grants_did.R and run, then add: saveRDS(es, 'es_inc.rds')\n")
  stop("Required RDS files not found")
}

es_mort <- readRDS("es_mort.rds")
es_inc <- readRDS("es_inc.rds")

cat(sprintf("Mortality sample: %d county-years, %d unique counties\n",
            nrow(es_mort), uniqueN(es_mort$fips)))
cat(sprintf("Incarceration sample: %d county-years, %d unique counties\n\n",
            nrow(es_inc), uniqueN(es_inc$fips)))

# ========== 2. Compare treated counties at t=0 ===============================

mort_t0 <- es_mort[event_time == 0, .(fips, total_pop_mort = total_pop, 
                                       outcome_mort = outcome, treat_year)]
inc_t0 <- es_inc[event_time == 0, .(fips, total_pop_inc = total_pop,
                                     outcome_inc = outcome, treat_year)]

cat("========== Counties at t=0 (treatment year) ==========\n\n")
cat(sprintf("Mortality sample at t=0: %d counties\n", nrow(mort_t0)))
cat(sprintf("Incarceration sample at t=0: %d counties\n", nrow(inc_t0)))

# Counties in mortality but not incarceration
mort_only <- mort_t0[!fips %in% inc_t0$fips]
cat(sprintf("\nCounties in MORTALITY but NOT in INCARCERATION at t=0: %d\n", nrow(mort_only)))
if (nrow(mort_only) > 0) {
  cat("\nSummary statistics for mortality-only counties:\n")
  print(summary(mort_only[, .(total_pop_mort, outcome_mort, treat_year)]))
  
  cat("\nTop 10 mortality-only counties by population:\n")
  print(mort_only[order(-total_pop_mort)][1:min(10, .N), 
                  .(fips, treat_year, total_pop_mort, outcome_mort)])
}

# Counties in incarceration but not mortality
inc_only <- inc_t0[!fips %in% mort_t0$fips]
cat(sprintf("\n\nCounties in INCARCERATION but NOT in MORTALITY at t=0: %d\n", nrow(inc_only)))
if (nrow(inc_only) > 0) {
  cat("\nSummary statistics for incarceration-only counties:\n")
  print(summary(inc_only[, .(total_pop_inc, outcome_inc, treat_year)]))
  
  cat("\nTop 10 incarceration-only counties by population:\n")
  print(inc_only[order(-total_pop_inc)][1:min(10, .N),
                 .(fips, treat_year, total_pop_inc, outcome_inc)])
}

# Counties in both
both_t0 <- merge(mort_t0, inc_t0, by = "fips")
cat(sprintf("\n\nCounties in BOTH samples at t=0: %d\n", nrow(both_t0)))

# ========== 3. Sample composition by event time ==============================

cat("\n\n========== Sample composition by event time ==========\n\n")

# Get treated counties only
mort_treated <- es_mort[treated == 1]
inc_treated <- es_inc[treated == 1]

# Count counties by event time
mort_by_et <- mort_treated[, .(n_counties_mort = uniqueN(fips)), by = event_time_binned]
inc_by_et <- inc_treated[, .(n_counties_inc = uniqueN(fips)), by = event_time_binned]

comparison <- merge(mort_by_et, inc_by_et, by = "event_time_binned", all = TRUE)
comparison[is.na(n_counties_mort), n_counties_mort := 0]
comparison[is.na(n_counties_inc), n_counties_inc := 0]
comparison[, difference := n_counties_inc - n_counties_mort]
comparison[, pct_diff := round(100 * difference / pmax(n_counties_mort, n_counties_inc, 1), 1)]
setorder(comparison, event_time_binned)

cat("County counts by event time (treated counties only):\n")
print(comparison[event_time_binned != -999])

# ========== 4. Population quantiles by event time ============================

cat("\n\n========== Population distribution by event time ==========\n\n")

quantile_table <- function(dt, label) {
  cat(sprintf("\n%s sample - Population quantiles:\n", label))
  
  # Get treated counties only, exclude binned never-treated
  dt_treated <- dt[treated == 1 & event_time_binned != -999]
  
  # Calculate quantiles for each event time
  quant_list <- lapply(-5:5, function(et) {
    sub <- dt_treated[event_time_binned == et]
    if (nrow(sub) > 0) {
      q <- quantile(sub$total_pop, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      data.table(
        event_time = et,
        min = q[1],
        p25 = q[2],
        median = q[3],
        p75 = q[4],
        max = q[5],
        mean = mean(sub$total_pop, na.rm = TRUE),
        n_counties = uniqueN(sub$fips)
      )
    } else {
      data.table(event_time = et, min = NA, p25 = NA, median = NA, 
                 p75 = NA, max = NA, mean = NA, n_counties = 0)
    }
  })
  
  quant_dt <- rbindlist(quant_list)
  
  # Transpose to have quantiles on rows, event time on cols
  cat("\n(Rows = quantiles, Columns = event time)\n\n")
  
  # Format as wide table
  min_row <- dcast(quant_dt, . ~ event_time, value.var = "min")[, -1]
  p25_row <- dcast(quant_dt, . ~ event_time, value.var = "p25")[, -1]
  med_row <- dcast(quant_dt, . ~ event_time, value.var = "median")[, -1]
  p75_row <- dcast(quant_dt, . ~ event_time, value.var = "p75")[, -1]
  max_row <- dcast(quant_dt, . ~ event_time, value.var = "max")[, -1]
  mean_row <- dcast(quant_dt, . ~ event_time, value.var = "mean")[, -1]
  n_row <- dcast(quant_dt, . ~ event_time, value.var = "n_counties")[, -1]
  
  result <- data.table(
    Quantile = c("Min", "P25", "Median", "P75", "Max", "Mean", "N_Counties")
  )
  for (col_name in names(min_row)) {
    result[[paste0("t", col_name)]] <- c(
      sprintf("%.0f", min_row[[col_name]]),
      sprintf("%.0f", p25_row[[col_name]]),
      sprintf("%.0f", med_row[[col_name]]),
      sprintf("%.0f", p75_row[[col_name]]),
      sprintf("%.0f", max_row[[col_name]]),
      sprintf("%.0f", mean_row[[col_name]]),
      sprintf("%d", n_row[[col_name]])
    )
  }
  
  print(result)
  
  return(quant_dt)
}

mort_quant <- quantile_table(es_mort, "MORTALITY")
inc_quant <- quantile_table(es_inc, "INCARCERATION")

# ========== 5. Check which specific counties drop out ========================

cat("\n\n========== Counties dropping out between event times ==========\n\n")

check_dropouts <- function(dt, label, from_et, to_et) {
  counties_from <- dt[treated == 1 & event_time_binned == from_et, unique(fips)]
  counties_to <- dt[treated == 1 & event_time_binned == to_et, unique(fips)]
  
  dropped <- setdiff(counties_from, counties_to)
  
  if (length(dropped) > 0) {
    cat(sprintf("\n%s: Counties present at t=%d but missing at t=%d (%d counties):\n",
                label, from_et, to_et, length(dropped)))
    
    # Get characteristics of dropped counties at from_et
    dropped_info <- dt[fips %in% dropped & event_time_binned == from_et,
                       .(fips, total_pop, outcome, treat_year)]
    
    if (nrow(dropped_info) > 0) {
      setorder(dropped_info, -total_pop)
      print(head(dropped_info, 10))
      
      cat(sprintf("\nPopulation summary of dropped counties:\n"))
      cat(sprintf("  Mean: %.0f, Median: %.0f, Min: %.0f, Max: %.0f\n",
                  mean(dropped_info$total_pop, na.rm = TRUE),
                  median(dropped_info$total_pop, na.rm = TRUE),
                  min(dropped_info$total_pop, na.rm = TRUE),
                  max(dropped_info$total_pop, na.rm = TRUE)))
    }
  } else {
    cat(sprintf("\n%s: No counties dropped between t=%d and t=%d\n", 
                label, from_et, to_et))
  }
  
  return(dropped)
}

# Check critical transitions
cat("\n=== MORTALITY ===")
mort_drop_0_1 <- check_dropouts(es_mort, "MORTALITY", 0, 1)
mort_drop_2_3 <- check_dropouts(es_mort, "MORTALITY", 2, 3)

cat("\n\n=== INCARCERATION ===")
inc_drop_0_1 <- check_dropouts(es_inc, "INCARCERATION", 0, 1)
inc_drop_2_3 <- check_dropouts(es_inc, "INCARCERATION", 2, 3)

# ========== 6. Export comparison summary =====================================

summary_stats <- data.table(
  metric = c("Total county-years", "Unique counties", "Treated counties", 
             "Counties at t=0", "Counties at t=3", "Counties at t=5",
             "Mean population (treated, t=0)", "Median population (treated, t=0)"),
  mortality = c(
    nrow(es_mort),
    uniqueN(es_mort$fips),
    uniqueN(es_mort[treated == 1, fips]),
    uniqueN(es_mort[treated == 1 & event_time == 0, fips]),
    uniqueN(es_mort[treated == 1 & event_time == 3, fips]),
    uniqueN(es_mort[treated == 1 & event_time == 5, fips]),
    round(mean(es_mort[treated == 1 & event_time == 0, total_pop], na.rm = TRUE), 0),
    round(median(es_mort[treated == 1 & event_time == 0, total_pop], na.rm = TRUE), 0)
  ),
  incarceration = c(
    nrow(es_inc),
    uniqueN(es_inc$fips),
    uniqueN(es_inc[treated == 1, fips]),
    uniqueN(es_inc[treated == 1 & event_time == 0, fips]),
    uniqueN(es_inc[treated == 1 & event_time == 3, fips]),
    uniqueN(es_inc[treated == 1 & event_time == 5, fips]),
    round(mean(es_inc[treated == 1 & event_time == 0, total_pop], na.rm = TRUE), 0),
    round(median(es_inc[treated == 1 & event_time == 0, total_pop], na.rm = TRUE), 0)
  )
)

cat("\n\n========== Summary comparison table ==========\n\n")
print(summary_stats)

fwrite(summary_stats, "sample_comparison_summary.csv")
fwrite(comparison, "sample_comparison_by_event_time.csv")
fwrite(mort_quant, "mortality_population_quantiles.csv")
fwrite(inc_quant, "incarceration_population_quantiles.csv")

cat("\n==========================================================\n")
cat("Sample comparison complete. Files saved:\n")
cat("  - sample_comparison_summary.csv\n")
cat("  - sample_comparison_by_event_time.csv\n")
cat("  - mortality_population_quantiles.csv\n")
cat("  - incarceration_population_quantiles.csv\n")
cat("==========================================================\n")
