#!/usr/bin/env Rscript
# facility_incarceration_analysis.R
#
# Reads the geocoded facilities dataset, creates county-year counts of facility
# types (all_omh, cmhc, act), merges with incarceration trends data, and runs
# regressions of incarceration rate changes on facility count changes.
#
# Usage:
#   Rscript facility_incarceration_analysis.R

library(data.table)
library(fixest)

# ---------- 1. Load facilities and create county-year facility counts ---------

dt <- as.data.table(readRDS("facilities_geocoded.rds"))

cat(sprintf("Loaded %d facilities across %d counties, years: %s\n",
            nrow(dt), uniqueN(dt$county_fips, na.rm = TRUE),
            paste(sort(unique(dt$year)), collapse = ", ")))

# Flag facility types based on service codes
dt[, all_omh := as.integer(grepl("\\bCMHC\\b|\\bOMH\\b|\\bMSNH\\b", services))]
dt[, cmhc   := as.integer(grepl("\\bCMHC\\b", services))]
dt[, act    := as.integer(grepl("\\bACT\\b",  services))]

cat(sprintf("Facility type flags — all_omh: %d, cmhc: %d, act: %d\n",
            sum(dt$all_omh, na.rm = TRUE),
            sum(dt$cmhc,   na.rm = TRUE),
            sum(dt$act,    na.rm = TRUE)))

# Collapse to county-year counts
fac <- dt[!is.na(county_fips),
          .(all_omh = sum(all_omh),
            cmhc    = sum(cmhc),
            act     = sum(act)),
          by = .(county_fips, year)]

cat(sprintf("Facility counts: %d county-year rows\n", nrow(fac)))

# ---------- 2. Load incarceration data and average across quarters ------------

inc <- fread("../incarceration_trends_county.csv",
             select = c("fips", "year", "quarter",
                        "total_pop", "total_incarceration_rate"))
inc[, fips := sprintf("%05d", fips)]

cat(sprintf("Loaded %d rows of incarceration data\n", nrow(inc)))

# Average across quarters within each county-year
inc_yr <- inc[, .(total_pop              = mean(total_pop, na.rm = TRUE),
                  total_incarceration_rate = mean(total_incarceration_rate, na.rm = TRUE)),
              by = .(fips, year)]

cat(sprintf("Incarceration data collapsed to %d county-year rows\n", nrow(inc_yr)))

# ---------- 3. Merge ----------------------------------------------------------

merged <- merge(inc_yr, fac,
                by.x = c("fips", "year"),
                by.y = c("county_fips", "year"),
                all.x = TRUE)

# Counties with no facility get count = 0
merged[is.na(all_omh), all_omh := 0L]
merged[is.na(cmhc),    cmhc    := 0L]
merged[is.na(act),     act     := 0L]

cat(sprintf("Merged dataset: %d county-year rows, %d unique counties\n",
            nrow(merged), uniqueN(merged$fips)))

# Drop rows missing the outcome
merged <- merged[!is.na(total_incarceration_rate)]

cat(sprintf("After dropping missing incarceration rate: %d rows\n", nrow(merged)))

# ---------- 4. Regressions: change in incarceration rate ~ change in counts ---
#   County and year fixed effects capture levels; coefficients reflect
#   within-county changes in facility counts on within-county changes
#   in incarceration rate.

cat("\n========== Regression Results ==========\n\n")

reg_omh  <- feols(total_incarceration_rate ~ all_omh | fips + year, data = merged)
reg_cmhc <- feols(total_incarceration_rate ~ cmhc   | fips + year, data = merged)
reg_act  <- feols(total_incarceration_rate ~ act    | fips + year, data = merged)

cat("--- (1) All OMH facilities (CMHC | OMH | MSNH) ---\n")
print(summary(reg_omh))

cat("\n--- (2) CMHC facilities ---\n")
print(summary(reg_cmhc))

cat("\n--- (3) ACT facilities ---\n")
print(summary(reg_act))

# ---------- 5. Export results -------------------------------------------------

coefs <- data.table(
  model       = c("all_omh", "cmhc", "act"),
  coefficient = c(coef(reg_omh),  coef(reg_cmhc),  coef(reg_act)),
  std_error   = c(se(reg_omh),    se(reg_cmhc),    se(reg_act)),
  t_value     = c(tstat(reg_omh), tstat(reg_cmhc), tstat(reg_act)),
  p_value     = c(pvalue(reg_omh), pvalue(reg_cmhc), pvalue(reg_act)),
  n_obs       = c(reg_omh$nobs,   reg_cmhc$nobs,   reg_act$nobs)
)

fwrite(coefs, "facility_incarceration_results.csv")
cat("\nResults saved to facility_incarceration_results.csv\n")
