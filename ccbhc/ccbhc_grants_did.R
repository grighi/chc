#!/usr/bin/env Rscript
# ccbhc_grants_did.R
#
# Diff-in-diff event study of CCBHC grant awards on county jail
# incarceration rates.
#
# Treatment: year(period_of_performance_start_date) x county FIPS from
# the USASpending assistance awards file.  A county is "treated" in the
# first year it receives a CCBHC grant.
#
# Dependent variable: total_jail_pop_rate (per 100k)
# Specification: two-way FE (county + year), population control,
# clustered SEs by county.
#
# Usage:
#   Rscript ccbhc_grants_did.R

library(data.table)
library(fixest)
library(ggplot2)

# ========== 1. Load CCBHC grants and extract treatment timing ================

grants <- fread("Assistance_PrimeAwardSummaries_2026-02-09_H21M27S10_1_93.829.csv")

cat(sprintf("Loaded %d grant records\n", nrow(grants)))

# Extract treatment year and county FIPS
grants[, treat_year := as.integer(substr(period_of_performance_start_date, 1, 4))]
grants[, fips := sprintf("%05d", prime_award_summary_recipient_county_fips_code)]

# Keep first grant per county (earliest treatment year)
treat <- grants[!is.na(treat_year) & fips != "   NA",
                .(treat_year = min(treat_year)),
                by = fips]

cat(sprintf("Unique treated counties: %d\n", nrow(treat)))
cat("Treatment year distribution:\n")
print(table(treat$treat_year))

# ========== 2. Load incarceration data =======================================

# inc <- fread("../incarceration_trends_county.csv",
#              select = c("fips", "year", "quarter",
#                         "total_pop", "total_jail_pop_rate"))
inc <- fst::write_fst(inc, '../incarceration_trends_county.fst') %>%
  data.table() %>% 
  subset( select = c(fips, year, quarter, total_pop, total_jail_pop_rate))

# Average across quarters within county-year
inc_yr <- inc[, .(total_pop          = mean(total_pop, na.rm = TRUE),
                  total_jail_pop_rate = mean(total_jail_pop_rate, na.rm = TRUE)),
              by = .(fips, year)]

# Drop missing outcome
inc_yr <- inc_yr[!is.na(total_jail_pop_rate)]

cat(sprintf("Incarceration panel: %d county-year rows, %d counties, years %d-%d\n",
            nrow(inc_yr), uniqueN(inc_yr$fips),
            min(inc_yr$year), max(inc_yr$year)))

# ========== 3. Merge and build event-study dataset ===========================

es <- merge(inc_yr, treat, by = "fips", all.x = TRUE)
es[, treated := as.integer(!is.na(treat_year))]
es[, event_time := year - treat_year]

cat(sprintf("Merged: %d county-years, %d treated counties, %d control counties\n",
            nrow(es), uniqueN(es[treated == 1, fips]),
            uniqueN(es[treated == 0, fips])))

# ========== 4. Bin event time ================================================

min_et <- -5L
max_et <-  5L

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

# ========== 5. Baseline event study ==========================================

cat("\n##########################################################\n")
cat("EVENT STUDY: CCBHC Grants -> total_jail_pop_rate\n")
cat("##########################################################\n\n")

# amr_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
# data = data,
# weights = ~popwt_ad,
# cluster = ~fips

es[, stfips := sprintf(as.numeric(fips) %/% 1000, fmt = '%02s')]
es[, Durb := total_pop > 30000]


model <- feols(
  total_jail_pop_rate ~ i(event_time_binned, ref = -1) + total_pop |
    fips[year] + year,
  data = es,
  cluster = ~fips,
  weight = ~total_pop
)


tmp <- readRDS('facilities_geocoded.rds')[year == 2017, .(county_fips, facility_name, services)] %>%
  unique()
wide <- tmp[, .(service = unlist(tstrsplit(services, "\\s+"))), by = .I][, val := 1L] %>%
  dcast(
  I ~ service,
  value.var = "val",
)
tmp <- cbind(tmp, wide)
services_by_county <- tmp[, .(omh = sum(OMH), act = sum(ACT), cmhc = sum(CMHC),
        therapy = sum(CBT + DBT + IPT + GT), case_mgmt = sum(CM+ICM), crisis = sum(CIT + CFT + ES),
        residential = sum(OSF + SNR + TCC)), .(fips = county_fips)]



m <- MatchIt::matchit(
  treated ~ act + cmhc + omh + therapy + case_mgmt + crisis + residential,
  data = merge(es[year == 2015], services_by_county, by = 'fips'),
  method = "nearest",
  distance = "logit",
  ratio = 1
  ### note: I can increase this ratio (more control counties) but that introduces pre-trends
)

matched_counties <- c(MatchIt::match.data(m)$fips, es[treated == 1, unique(fips)])

model <- feols(
  total_jail_pop_rate ~ i(event_time_binned, ref = -1) + total_pop |
    fips + year,
  data = es[fips %in% matched_counties],
  cluster = ~fips,
  weight = ~total_pop
)



print(summary(model))

# ========== 6. Extract coefficients ==========================================

cn  <- names(coef(model))
idx <- grepl("^event_time_binned::(-?[0-9]+)$", cn)
coef_df <- data.table(
  event_time  = as.integer(gsub(".*::(-?[0-9]+)$", "\\1", cn[idx])),
  coefficient = coef(model)[idx],
  se          = sqrt(diag(vcov(model)))[idx]
)
coef_df <- coef_df[event_time != -999L]
coef_df[, `:=`(ci_lower = coefficient - 1.96 * se,
               ci_upper = coefficient + 1.96 * se)]

# Add reference period
coef_df <- rbindlist(list(
  coef_df,
  data.table(event_time = -1L, coefficient = 0, se = 0,
             ci_lower = 0, ci_upper = 0)
))
setorder(coef_df, event_time)

# Print
cat("\nEvent study coefficients:\n")
print(coef_df[, .(event_time,
                  coef   = sprintf("%.3f", coefficient),
                  se     = sprintf("%.3f", se),
                  ci_95  = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
                  signif = fifelse(abs(coefficient / se) > 2.576, "***",
                           fifelse(abs(coefficient / se) > 1.96,  "**",
                           fifelse(abs(coefficient / se) > 1.645, "*", ""))))])

cat(paste0('\n---- Matched Counties count: ', length(matched_counties)), '\n\n')

# Joint tests
pre_names <- paste0("event_time_binned::", min_et:-2)
pre_names <- pre_names[pre_names %in% names(coef(model))]
if (length(pre_names) > 1) {
  wt <- wald(model, pre_names)
  cat(sprintf("\nJoint pre-trend test (t<=-2): F=%.2f, p=%.4f\n", wt$stat, wt$p))
}

post_names <- paste0("event_time_binned::", 0:max_et)
post_names <- post_names[post_names %in% names(coef(model))]
if (length(post_names) > 1) {
  wt <- wald(model, post_names)
  cat(sprintf("Joint post-treatment test (t>=0): F=%.2f, p=%.4f\n", wt$stat, wt$p))
}

# ========== 7. Save coefficients =============================================

fwrite(coef_df, "ccbhc_grants_event_study_coefficients.csv")
cat("\nCoefficients saved to ccbhc_grants_event_study_coefficients.csv\n")

# ========== 8. Plot ==========================================================

p <- ggplot(coef_df, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              fill = "steelblue", alpha = 0.2) +
  geom_point(color = "steelblue", size = 2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Effect of CCBHC Grant on Jail Incarceration Rate",
    subtitle = "Event study coefficients with 95% CI, ref = t-1, clustered by county",
    x = "Years Relative to First CCBHC Grant",
    y = "Change in Jail Pop Rate (per 100k)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("ccbhc_grants_event_study.png", p, width = 10, height = 6, dpi = 300)
cat("Plot saved to ccbhc_grants_event_study.png\n")

cat("\n==========================================================\n")
cat("CCBHC grants DiD complete.\n")
cat("==========================================================\n")
