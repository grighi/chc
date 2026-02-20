#!/usr/bin/env Rscript
# ccbhc_grants_did.R
#
# Diff-in-diff event study of CCBHC grant awards on county-level outcomes.
#
# ANALYSIS_TYPE controls the outcome:
#   "incarceration" -> total jail pop rate (per 100k)
#   "mortality"     -> death rate per 100k (ages 25-44, MCOD)
#
# Usage:
#   Rscript ccbhc_grants_did.R

library(data.table)
library(fixest)
library(ggplot2)

# ===== PARAMETERS ============================================================

ANALYSIS_TYPE <- "incarceration"

weighting      <- TRUE
matching       <- TRUE
balanced_panel <- FALSE

# if (ANALYSIS_TYPE == "incarceration") {
# } else if (ANALYSIS_TYPE == "mortality") {
#   weighting      <- TRUE
#   matching       <- FALSE
#   balanced_panel <- FALSE
# } else {
#   stop("ANALYSIS_TYPE must be 'incarceration' or 'mortality'")
# }

cat(rep(".\n", 20))
cat("\n=============================================================================================\n")
cat(sprintf("CCBHC grants DiD analysis: %s\n", ANALYSIS_TYPE))
cat("=============================================================================================\n")

# Medicaid demonstration waiver state FIPS (MN,MO,NV,NJ,NY,OK,OR,PA)
WAIVER_ST_FIPS <- c("27","29","32","34","36","40","41","42")

# ========== 1. Load CCBHC grants and extract treatment timing ================

# First file (93.829)
grants1 <- fread("Assistance_PrimeAwardSummaries_2026-02-09_H21M27S10_1_93.829.csv")

# Identify original planning grant states BEFORE filtering (for robustness)
planning_grant_st_fips <- grants1[
  period_of_performance_start_date == "2015-10-23",
  unique(sprintf("%02d", prime_award_summary_recipient_state_fips_code))
]
cat("Planning grant states (FIPS):", paste(planning_grant_st_fips, collapse = ", "), "\n")

# Drop planning grants (period of performance start year < 2017)
grants1 <- grants1[as.integer(substr(period_of_performance_start_date, 1, 4)) >= 2017]

# Second file (93.696)
grants2 <- fread("Assistance_PrimeAwardSummaries_2026-02-11_H16M34S02_1_93.696.csv")

# Combine both grant files
grants <- rbindlist(list(grants1, grants2), fill = TRUE)

cat(sprintf("Loaded %d grant records (after dropping pre-2017 from 93.829)\n", nrow(grants)))

# Extract treatment year and county FIPS
grants[, treat_year := as.integer(substr(period_of_performance_start_date, 1, 4))]
grants[, fips := sprintf("%05d", prime_award_summary_recipient_county_fips_code)]

# Keep first grant per county (earliest treatment year)
treat <- grants[!is.na(treat_year) & !grepl("NA", fips),
                .(treat_year = min(treat_year)),
                by = fips]

cat(sprintf("Unique treated counties: %d\n", nrow(treat)))
cat("Treatment year distribution:\n")
print(table(treat$treat_year))

treat <- treat[treat_year %in% 2018:2024]

# ========== 2. Load outcome data =============================================

if (ANALYSIS_TYPE == "incarceration") {

    inc <- fst::read_fst('../incarceration_trends_county.fst', as.data.table = TRUE)

  # Average across quarters within county-year
  panel <- inc[, .(total_pop = mean(total_pop, na.rm = TRUE),
                  # outcome   = log(mean(jail_pop_per_capita, na.rm = TRUE))),
                  outcome   = log(mean(total_jail_pop_rate, na.rm = TRUE))),
                  # outcome   =     mean(total_jail_pop_rate, na.rm = TRUE)),    # pretrends!
                  # outcome   =  mean(jail_sentenced_rate, na.rm = TRUE)),
               by = .(fips, year)]
  panel <- panel[!is.na(outcome)]
  panel[, fips := sprintf("%05d", as.integer(fips))]

  outcome_label <- "Change in Jail Pop Rate (per 100k)"

  cat(sprintf("Incarceration panel: %d county-year rows, %d counties, years %d-%d\n",
              nrow(panel), uniqueN(panel$fips),
              min(panel$year), max(panel$year)))

} else if (ANALYSIS_TYPE == "mortality") {

  # Load three death files (ages 25-44, MCOD)
  d1 <- fread("Deaths_2010-2015_25-44_MCOD.csv",
              na.strings = c("", "Suppressed", "Unreliable", "Not Applicable"))
  d2 <- fread("Deaths_2016-2020_25-44_MCOD.csv",
              na.strings = c("", "Suppressed", "Unreliable", "Not Applicable"))
  d3 <- fread("Deaths_2018-2023_25-44_MCOD.csv",
              na.strings = c("", "Suppressed", "Unreliable", "Not Applicable"))
  d4 <- fread("Provisional Mortality Statistics, 2018 through Last Week.csv", 
              na.strings = c("", "Suppressed", "Unreliable", "Not Applicable"),
              fill=T)

  # Standardize column names
  setnames(d1, c("County Code", "Year Code"), c("fips", "year"))
  setnames(d2, c("County Code", "Year Code"), c("fips", "year"))
  setnames(d3, c("County Code", "Year Code"), c("fips", "year"))
  setnames(d4, c("Residence County Code", "Year Code"), c("fips", "year"))

  d1[, year := as.integer(year)]
  d2[, year := as.integer(year)]
  d3[, year := as.integer(trimws(as.character(year)))]
  d4[, year := as.integer(trimws(as.character(year)))]

  # Drop 2018-2020 from d3 (overlap with d2)
  # d3 <- d3[year %in% 2021:2023]
  d4 <- d4[year %in% 2021:2025]

  # Keep relevant columns and combine
  d1_sub <- d1[!is.na(fips), .(fips, year,
               Deaths = as.numeric(Deaths), Population = as.numeric(Population))]
  d2_sub <- d2[!is.na(fips), .(fips, year,
               Deaths = as.numeric(Deaths), Population = as.numeric(Population))]
  d3_sub <- d3[!is.na(fips), .(fips, year,
               Deaths = as.numeric(Deaths))]
  d3_sub[, Population := NA_real_]
  d4_sub <- d4[!is.na(fips), .(fips, year,
               Deaths = as.numeric(Deaths))]
  d4_sub[, Population := NA_real_]

  mort <- rbindlist(list(d1_sub, d2_sub, d4_sub))

  # --- Impute population for 2021-2025 using linear trend from 2010-2020 ---
  pop_data <- mort[!is.na(Population), .(fips, year, Population)]
  pop_models <- pop_data[, {
    if (.N >= 2) {
      m <- lm(Population ~ year)
      .(intercept = coef(m)[1], slope = coef(m)[2])
    } else {
      .(intercept = NA_real_, slope = NA_real_)
    }
  }, by = fips]

  impute_grid <- CJ(fips = pop_models[!is.na(slope), fips], year = 2021:2025)
  impute_grid <- merge(impute_grid, pop_models, by = "fips")
  impute_grid[, Population_imp := intercept + slope * year]
  impute_grid[Population_imp < 0, Population_imp := NA_real_]

  mort <- merge(mort, impute_grid[, .(fips, year, Population_imp)],
                by = c("fips", "year"), all.x = TRUE)
  mort[is.na(Population) & !is.na(Population_imp), Population := Population_imp]
  mort[, Population_imp := NULL]

  cat(sprintf("Population imputed for %d county-years (2021-2023)\n",
              mort[year >= 2021 & !is.na(Population), .N]))

  # Calculate death rate per 100k
  mort[, outcome := Deaths / Population * 100000]

  panel <- mort[!is.na(outcome) & Population > 0,
                .(fips, year, total_pop = Population, outcome)]

  outcome_label <- "Change in Death Rate per 100k (ages 25-44)"

  cat(sprintf("Mortality panel: %d county-year rows, %d counties, years %d-%d\n",
              nrow(panel), uniqueN(panel$fips),
              min(panel$year), max(panel$year)))

  panel[, fips := sprintf(fips, fmt = "%05d")]

} else if (ANALYSIS_TYPE == "mortality_f") {

  # Load mortality data for substance use deaths (ICD-F codes)
  # Historical data 2012-2017
  d_hist <- fread(
    # "Mortality_ICD_F.csv",
    # "Mortality_Covid.csv",
    # "Mortality_non_F_or_Covid.csv",
    "Mortality_U_V_codes.csv",
                  na.strings = c("", "Suppressed", "Unreliable", "Not Applicable"),
                  fill = TRUE)
  
  # Provisional data 2018-2025
  d_prov <- fread(
    # "Provisional_Mortality_ICD_F.csv",
    # "Mortality_Covid_2018-25.csv",
    # "Mortality_non_F_or_Covid_2018-25.csv",
    "Mortality_U_V_codes_2018-25.csv",
                  na.strings = c("", "Suppressed", "Unreliable", "Not Applicable"),
                  fill = TRUE)

  # Standardize column names
  setnames(d_hist, c("County Code", "Year Code"), c("fips", "year"), skip_absent = TRUE)
  setnames(d_prov, c("Residence County Code", "Year Code"), c("fips", "year"), skip_absent = TRUE)

  d_hist[, year := as.integer(year)]
  d_prov[, year := as.integer(trimws(as.character(year)))]

  # Filter to desired years
  d_hist <- d_hist[year >= 2012 & year <= 2017]
  d_prov <- d_prov[year >= 2018 & year <= 2025]

  # Keep relevant columns and combine
  d_hist_sub <- d_hist[!is.na(fips), .(fips, year,
                Deaths = as.numeric(Deaths), Population = as.numeric(Population))]
  d_prov_sub <- d_prov[!is.na(fips), .(fips, year,
                Deaths = as.numeric(Deaths))]
  d_prov_sub[, Population := NA_real_]

  mort <- rbindlist(list(d_hist_sub, d_prov_sub))

  # --- Impute population for 2018-2025 using linear trend from 2012-2017 ---
  pop_data <- mort[!is.na(Population), .(fips, year, Population)]
  pop_models <- pop_data[, {
    if (.N >= 2) {
      m <- lm(Population ~ year)
      .(intercept = coef(m)[1], slope = coef(m)[2])
    } else {
      .(intercept = NA_real_, slope = NA_real_)
    }
  }, by = fips]

  impute_grid <- CJ(fips = pop_models[!is.na(slope), fips], year = 2018:2025)
  impute_grid <- merge(impute_grid, pop_models, by = "fips")
  impute_grid[, Population_imp := intercept + slope * year]
  impute_grid[Population_imp < 0, Population_imp := NA_real_]

  mort <- merge(mort, impute_grid[, .(fips, year, Population_imp)],
                by = c("fips", "year"), all.x = TRUE)
  mort[is.na(Population) & !is.na(Population_imp), Population := Population_imp]
  mort[, Population_imp := NULL]

  cat(sprintf("Population imputed for %d county-years (2018-2025)\n",
              mort[year >= 2018 & !is.na(Population), .N]))

  # Calculate death rate per 100k (substance use deaths)
  mort[, outcome := Deaths / Population * 100000]

  panel <- mort[!is.na(outcome) & Population > 0,
                .(fips, year, total_pop = Population, outcome)]

  outcome_label <- "Change in Substance Use Death Rate per 100k (ICD-F)"

  cat(sprintf("Substance use mortality panel: %d county-year rows, %d counties, years %d-%d\n",
              nrow(panel), uniqueN(panel$fips),
              min(panel$year), max(panel$year)))

  panel[, fips := sprintf(fips, fmt = "%05d")]

} else {
  stop("ANALYSIS_TYPE must be 'incarceration', 'mortality', or 'mortality_f'")
}

# ========== 3. Merge and build event-study dataset ===========================


es <- merge(panel, treat, by = "fips", all.x = TRUE)
es[, treated := as.integer(!is.na(treat_year))]
es[, event_time := year - treat_year]
es[, stfips := substr(fips, 1, 2)]

cat(sprintf("Merged: %d county-years, %d treated counties, %d control counties\n",
            nrow(es), uniqueN(es[treated == 1, fips]),
            uniqueN(es[treated == 0, fips])))

# ========== 4. Bin event time ================================================

min_et <- -5L
max_et <-  5L

matching_dataset <- es[year == 2015]

es <- es[(event_time >= min_et & event_time <= max_et) | is.na(event_time)]
es <- es[year > 2012]

es[, event_time_binned := fcase(
  is.na(event_time), -999L,
  event_time <= min_et, min_et,
  event_time >= max_et, max_et,
  default = as.integer(event_time)
)]

cat("\nEvent time distribution:\n")
print(table(es$event_time_binned, useNA = "ifany"))
year_table <- table(es$event_time_binned, useNA = "ifany")

# ========== 5. Matching ======================================================

tmp <- readRDS('directories/facilities_geocoded.rds')[year == 2017, .(county_fips, facility_name, services)] |>
  unique()
wide <- tmp[, .(service = unlist(tstrsplit(services, "\\s+"))), by = .I][, val := 1L] |>
  dcast(I ~ service, value.var = "val")
tmp <- cbind(tmp, wide)
services_by_county <- tmp[, .(omh = sum(OMH), act = sum(ACT), cmhc = sum(CMHC),
        therapy = sum(CBT + DBT + IPT + GT), case_mgmt = sum(CM+ICM), crisis = sum(CIT + CFT + ES),
        residential = sum(OSF + SNR + TCC)), .(fips = county_fips)]

matching_dataset <- merge(matching_dataset, services_by_county, by = 'fips', all.x = T)
fill_cols <- c("omh", "act", "cmhc", "therapy", "case_mgmt", "crisis", "residential")
matching_dataset[, (fill_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = fill_cols]

m <- MatchIt::matchit(
  treated ~ act + cmhc + omh + therapy + case_mgmt + crisis + residential,
  data = matching_dataset,
  method = "nearest",
  distance = "logit",
  ratio = 2
  ### note: I can increase this ratio (more control counties) but that introduces pre-trends
)

if(matching == T) {
  matched_counties <- c(MatchIt::match.data(m)$fips, es[treated == 1, unique(fips)])
} else {
  matched_counties <- es[, unique(fips)]
}

# ========== 6. Helper: run model and extract coefficients ====================

run_event_study <- function(dt, label) {
  cat(sprintf("\n##########################################################\n"))
  cat(sprintf("EVENT STUDY: %s\n", label))
  cat(sprintf("##########################################################\n\n"))

  if (balanced_panel) {
    balanced_panel <- es[event_time_binned == 4 | event_time_binned == -999, unique(fips)]
    county_sample <- balanced_panel
  } else {
    county_sample <- es[, unique(fips)]
  }
  
  mod <- feols(
    outcome ~ i(event_time_binned, ref = -1) + total_pop |
      fips + year,
    data = dt[fips %in% county_sample],
    weights = if (weighting) ~total_pop else NULL,
    cluster = ~fips
  )

  print(summary(mod))

  # Extract coefficients
  cn  <- names(coef(mod))
  idx <- grepl("^event_time_binned::(-?[0-9]+)$", cn)
  coef_df <- data.table(
    event_time  = as.integer(gsub(".*::(-?[0-9]+)$", "\\1", cn[idx])),
    coefficient = coef(mod)[idx],
    se          = sqrt(diag(vcov(mod)))[idx]
  )
  coef_df <- coef_df[event_time != -999L & event_time < ifelse(ANALYSIS_TYPE == "mortality", 6, 5)]
  coef_df[, `:=`(ci_lower = coefficient - 1.96 * se,
                 ci_upper = coefficient + 1.96 * se)]

  # Add reference period
  coef_df <- rbindlist(list(
    coef_df,
    data.table(event_time = -1L, coefficient = 0, se = 0,
               ci_lower = 0, ci_upper = 0)
  ))
  setorder(coef_df, event_time)

  cat("\nEvent study coefficients:\n")
  print(coef_df[, .(event_time,
                    coef   = sprintf("%.3f", coefficient),
                    se     = sprintf("%.3f", se),
                    ci_95  = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
                    signif = fifelse(abs(coefficient / se) > 2.576, "***",
                             fifelse(abs(coefficient / se) > 1.96,  "**",
                             fifelse(abs(coefficient / se) > 1.645, "*", ""))))])

    # Print table of units by treatment and event time
  cat("\nNumber of counties by treatment status and event time:\n")
  units_table <- dt[, .(n_counties = uniqueN(fips)), by = .(treated, event_time_binned)]
  units_wide <- dcast(units_table, treated ~ event_time_binned, value.var = "n_counties", fill = 0)
  setorder(units_wide, -treated)
  print(units_wide)
  cat("\n")


  # Joint tests
  pre_names <- paste0("event_time_binned::", min_et:-2)
  pre_names <- pre_names[pre_names %in% names(coef(mod))]
  if (length(pre_names) > 1) {
    wt <- wald(mod, pre_names)
    cat(sprintf("\nJoint pre-trend test (t<=-2): F=%.2f, p=%.4f\n", wt$stat, wt$p))
  }

  post_names <- paste0("event_time_binned::", 0:max_et)
  post_names <- post_names[post_names %in% names(coef(mod))]
  if (length(post_names) > 1) {
    wt <- wald(mod, post_names)
    cat(sprintf("Joint post-treatment test (t>=0): F=%.2f, p=%.4f\n", wt$stat, wt$p))
  }

  return(coef_df)
}

plot_event_study <- function(coef_df, title, filename) {
  p <- ggplot(coef_df, aes(x = event_time, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = "steelblue", alpha = 0.2) +
    geom_point(color = "steelblue", size = 2) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    labs(
      title = title,
      subtitle = "Event study coefficients with 95% CI, ref = t-1, clustered by county",
      x = "Years Relative to First CCBHC Grant",
      y = outcome_label
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())

  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  cat(sprintf("Plot saved to %s\n", filename))
}

# ========== 7. Baseline model (matched) ======================================

es_matched <- es[fips %in% matched_counties]
cat(sprintf('\nMatched counties: %d\n', length(matched_counties)))

# Heterogeneity: small vs large counties (cutoff 30k)
county_pop <- es_matched[, .(avg_pop = mean(total_pop, na.rm = TRUE)), by = fips]
county_pop[, size_group := fifelse(avg_pop < 30000, "Small (<30k)", "Large (>=30k)")]
es_matched <- merge(es_matched, county_pop[, .(fips, size_group)], by = "fips", all.x = TRUE)

coef_baseline <- run_event_study(
  es_matched,
  sprintf("CCBHC Grants -> %s (Baseline, Matched)", ANALYSIS_TYPE)
)

fwrite(coef_baseline, sprintf("ccbhc_%s_baseline_coefs.csv", ANALYSIS_TYPE))
plot_event_study(
  coef_baseline,
  sprintf("Effect of CCBHC Grant on %s",
          ifelse(ANALYSIS_TYPE == "mortality", "Death Rate (25-44)", "Jail Incarceration Rate")),
  sprintf("ccbhc_%s_event_study.png", ANALYSIS_TYPE)
)

# Heterogeneity: small vs large counties (unweighted)
coef_small <- run_event_study(
  es_matched[size_group == "Small (<30k)"],
  sprintf("CCBHC Grants -> %s (Small Counties <30k, Unweighted)", ANALYSIS_TYPE)
)

coef_large <- run_event_study(
  es_matched[size_group == "Large (>=30k)"],
  sprintf("CCBHC Grants -> %s (Large Counties >=30k, Unweighted)", ANALYSIS_TYPE)
)

fwrite(coef_small, sprintf("ccbhc_%s_heterogeneity_small_coefs.csv", ANALYSIS_TYPE))
fwrite(coef_large, sprintf("ccbhc_%s_heterogeneity_large_coefs.csv", ANALYSIS_TYPE))

# ========== 8. Robustness: Drop Medicaid waiver states =======================

es_no_waiver <- es_matched[!stfips %in% WAIVER_ST_FIPS]

cat(sprintf('\nRobustness — dropping waiver states: %d county-years, %d treated, %d control\n',
            nrow(es_no_waiver),
            uniqueN(es_no_waiver[treated == 1, fips]),
            uniqueN(es_no_waiver[treated == 0, fips])))

coef_no_waiver <- run_event_study(
  es_no_waiver,
  sprintf("Robustness: Drop Medicaid Waiver States (%s)", ANALYSIS_TYPE)
)

fwrite(coef_no_waiver, sprintf("ccbhc_%s_rob_no_waiver_coefs.csv", ANALYSIS_TYPE))
plot_event_study(
  coef_no_waiver,
  sprintf("CCBHC Effect excl. Medicaid Waiver States (%s)",
          ifelse(ANALYSIS_TYPE == "mortality", "Death Rate", "Jail Rate")),
  sprintf("ccbhc_%s_rob_no_waiver.png", ANALYSIS_TYPE)
)

# ========== 9. Robustness: Planning grant states only ========================

es_planning <- es_matched[stfips %in% planning_grant_st_fips]

cat(sprintf('\nRobustness — planning grant states only: %d county-years, %d treated, %d control\n',
            nrow(es_planning),
            uniqueN(es_planning[treated == 1, fips]),
            uniqueN(es_planning[treated == 0, fips])))

coef_planning <- run_event_study(
  es_planning,
  sprintf("Robustness: Planning Grant States Only (%s)", ANALYSIS_TYPE)
)

fwrite(coef_planning, sprintf("ccbhc_%s_rob_planning_states_coefs.csv", ANALYSIS_TYPE))
plot_event_study(
  coef_planning,
  sprintf("CCBHC Effect — Planning Grant States Only (%s)",
          ifelse(ANALYSIS_TYPE == "mortality", "Death Rate", "Jail Rate")),
  sprintf("ccbhc_%s_rob_planning_states.png", ANALYSIS_TYPE)
)



if (ANALYSIS_TYPE == "mortality") {
  saveRDS(es, "es_mort.rds")
} else if (ANALYSIS_TYPE == "incarceration") {
  saveRDS(es, "es_inc.rds")
}
# ========== Done =============================================================

cat("\n==========================================================\n")
cat(sprintf("CCBHC grants DiD (%s) complete.\n", ANALYSIS_TYPE))
cat("==========================================================\n")


# ========== Reprint Baseline with event time labels =================================


  cat("\nBaseline event study coefficients:\n")
  print(coef_baseline[, .(event_time,
                    coef   = sprintf("%.3f", coefficient),
                    se     = sprintf("%.3f", se),
                    ci_95  = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
                    signif = fifelse(abs(coefficient / se) > 2.576, "***",
                             fifelse(abs(coefficient / se) > 1.96,  "**",
                             fifelse(abs(coefficient / se) > 1.645, "*", ""))))])

cat("\nBaseline treatment year distribution:\n")
print(unique(es_matched[, .(fips, treat_year)])[, table(treat_year, useNA = "ifany")])
