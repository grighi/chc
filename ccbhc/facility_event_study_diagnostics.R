#!/usr/bin/env Rscript
# facility_event_study_diagnostics.R
#
# Diagnostics and robustness checks for the CMHC DiD event study.
#
# Test 1: Baseline imbalance (mean pre-period incarceration rate)
# Test 2: Pre-trend slopes (county-level slope comparison + density)
# Spec A: County-specific linear trends
# Spec B: Placebo — shift treatment year back one year
# Spec C: Large counties only (pop >= 50k)
#
# Usage:
#   Rscript facility_event_study_diagnostics.R

library(data.table)
library(fixest)
library(ggplot2)

# ========== 1. Data prep (same as facility_event_study.R) ====================

dt <- as.data.table(readRDS("facilities_geocoded.rds"))
dt[, cmhc := as.integer(grepl("\\bCMHC\\b", services))]

fac <- dt[!is.na(county_fips),
          .(cmhc = sum(cmhc)),
          by = .(county_fips, year)]

inc <- fread("../incarceration_trends_county.csv",
             select = c("fips", "year", "quarter",
                        "total_pop", "total_incarceration_rate")
            )[year > 2010]
inc[, fips := sprintf("%05d", fips)]

inc_yr <- inc[, .(total_pop               = mean(total_pop, na.rm = TRUE),
                  total_incarceration_rate = mean(total_incarceration_rate, na.rm = TRUE)),
              by = .(fips, year)]

merged <- merge(inc_yr, fac,
                by.x = c("fips", "year"),
                by.y = c("county_fips", "year"),
                all.x = TRUE)
set(merged, which(is.na(merged$cmhc)), "cmhc", 0L)
merged <- merged[!is.na(total_incarceration_rate)]

setorder(merged, fips, year)
merged[, d_cmhc := cmhc - shift(cmhc), by = fips]

# Treatment timing (same rule as main script: year != 2017)
treat <- merged[!is.na(d_cmhc) & d_cmhc >= 1 & year != 2017,
                .(treat_year = min(year)), by = fips]

cat(sprintf("Panel: %d county-years, %d counties, %d treated\n",
            nrow(merged), uniqueN(merged$fips), nrow(treat)))
cat("Treatment year distribution:\n")
print(table(treat$treat_year))

# ========== 2. Build event-study dataset =====================================

es <- merge(merged[, .(fips, year, total_incarceration_rate, total_pop)],
            treat, by = "fips", all.x = TRUE)
es[, event_time := year - treat_year]
es[, treated := as.integer(!is.na(treat_year))]

min_et <- -5L; max_et <- 3L

es <- es[(event_time >= min_et & event_time <= max_et) | is.na(event_time)]
es[, event_time_binned := fcase(
  is.na(event_time), -999L,
  event_time <= min_et, min_et,
  event_time >= max_et, max_et,
  default = as.integer(event_time)
)]

# ========== TEST 1: Baseline imbalance =======================================

cat("\n##########################################################\n")
cat("TEST 1: Baseline Imbalance (pre-period mean incarceration rate)\n")
cat("##########################################################\n\n")

pre <- es[is.na(event_time) | event_time <= -2]
pre_means <- pre[, .(mean_rate = mean(total_incarceration_rate, na.rm = TRUE),
                     sd_rate   = sd(total_incarceration_rate, na.rm = TRUE),
                     n         = .N),
                 by = treated]

cat("Group means (t <= -2 for treated; all years for never-treated):\n")
print(pre_means)

rate_treat   <- pre[treated == 1, total_incarceration_rate]
rate_control <- pre[treated == 0, total_incarceration_rate]

tt <- t.test(rate_treat, rate_control)
cat(sprintf("\nDifference in means: %.2f  (treated %.2f vs control %.2f)\n",
            tt$estimate[1] - tt$estimate[2], tt$estimate[1], tt$estimate[2]))
cat(sprintf("t = %.3f, p = %.4f\n", tt$statistic, tt$p.value))
if (tt$p.value < 0.05) {
  cat("=> Significant baseline imbalance detected.\n")
} else {
  cat("=> No significant baseline imbalance at 5%% level.\n")
}

# ========== TEST 2: Pre-trend slopes =========================================

cat("\n##########################################################\n")
cat("TEST 2: Pre-trend slopes (county-level slope comparison)\n")
cat("##########################################################\n\n")

# For treated counties: use only pre-treatment years (event_time <= -1)
# For never-treated counties: use all years
pre_slope_data <- es[(treated == 0) | (treated == 1 & event_time <= -1)]

slopes <- pre_slope_data[, {
  if (.N >= 2) {
    m <- lm(total_incarceration_rate ~ year)
    .(slope = coef(m)[2], n_years = .N)
  } else {
    .(slope = NA_real_, n_years = .N)
  }
}, by = .(fips, treated)]
slopes <- slopes[!is.na(slope)]

slope_summary <- slopes[, .(mean_slope = mean(slope),
                            sd_slope   = sd(slope),
                            n          = .N),
                        by = treated]

cat("Mean pre-period slope of incarceration rate on year:\n")
print(slope_summary)

st <- t.test(slopes[treated == 1, slope], slopes[treated == 0, slope])
cat(sprintf("\nDifference in mean slopes: %.3f  (treated %.3f vs control %.3f)\n",
            st$estimate[1] - st$estimate[2], st$estimate[1], st$estimate[2]))
cat(sprintf("t = %.3f, p = %.4f\n", st$statistic, st$p.value))
if (st$p.value < 0.05) {
  cat("=> Significant difference in pre-trends detected.\n")
} else {
  cat("=> No significant difference in pre-trends at 5%% level.\n")
}

# Density plot
p_dens <- ggplot(slopes, aes(x = slope, fill = factor(treated))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("0" = "gray60", "1" = "darkgreen"),
                    labels = c("Never-treated", "Treated")) +
  geom_vline(xintercept = slope_summary[treated == 0, mean_slope],
             linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = slope_summary[treated == 1, mean_slope],
             linetype = "dashed", color = "darkgreen") +
  labs(title = "Pre-period Slopes: Treated vs Never-Treated Counties",
       subtitle = "County-level slope of incarceration rate on year",
       x = "Slope (change in rate per year)", y = "Density", fill = "Group") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave("diagnostics_pretrend_density_cmhc.png", p_dens, width = 9, height = 5, dpi = 300)
cat("Density plot saved to diagnostics_pretrend_density_cmhc.png\n")


# ========== Helper: extract coefficients & plot ==============================

extract_and_plot <- function(model, spec_label, color = "darkgreen") {
  cn  <- names(coef(model))
  idx <- grepl("^event_time_binned::(-?[0-9]+)$", cn)
  cdf <- data.table(
    event_time  = as.integer(gsub(".*::(-?[0-9]+)$", "\\1", cn[idx])),
    coefficient = coef(model)[idx],
    se          = sqrt(diag(vcov(model)))[idx]
  )
  cdf <- cdf[event_time != -999L]
  cdf[, `:=`(ci_lower = coefficient - 1.96 * se,
             ci_upper = coefficient + 1.96 * se)]
  cdf <- rbindlist(list(cdf,
    data.table(event_time = -1L, coefficient = 0, se = 0,
               ci_lower = 0, ci_upper = 0)))
  setorder(cdf, event_time)

  # Print table
  cat(sprintf("\nCoefficients (%s):\n", spec_label))
  print(cdf[, .(event_time,
                coef   = sprintf("%.3f", coefficient),
                se     = sprintf("%.3f", se),
                ci_95  = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
                signif = fifelse(abs(coefficient / se) > 2.576, "***",
                         fifelse(abs(coefficient / se) > 1.96,  "**",
                         fifelse(abs(coefficient / se) > 1.645, "*", ""))))])

  # Pre-trend joint test (t <= -2)
  pre_names <- paste0("event_time_binned::", min_et:-2)
  pre_names <- pre_names[pre_names %in% names(coef(model))]
  if (length(pre_names) > 1) {
    wt <- wald(model, pre_names)
    cat(sprintf("Joint pre-trend test (t<=-2): F=%.2f, p=%.4f\n", wt$stat, wt$p))
  }

  # Post-treatment joint test (t >= 0)
  post_names <- paste0("event_time_binned::", 0:max_et)
  post_names <- post_names[post_names %in% names(coef(model))]
  if (length(post_names) > 1) {
    wt <- wald(model, post_names)
    cat(sprintf("Joint post-treatment test (t>=0): F=%.2f, p=%.4f\n", wt$stat, wt$p))
  }

  # Plot
  p <- ggplot(cdf, aes(x = event_time, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = color, alpha = 0.2) +
    geom_point(color = color, size = 2) +
    geom_line(color = color, linewidth = 0.8) +
    labs(
      title = paste0("CMHC Event Study — ", spec_label),
      subtitle = "95% CI, clustered by county, ref = t-1",
      x = "Years Relative to CMHC Opening",
      y = "Change in Incarceration Rate (per 100k)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())

  fname <- paste0("diagnostics_es_", gsub("[^a-z0-9]", "_", tolower(spec_label)), ".png")
  ggsave(fname, p, width = 10, height = 6, dpi = 300)
  cat(sprintf("Plot saved to %s\n", fname))

  # Save CSV
  csv_name <- paste0("diagnostics_coefs_", gsub("[^a-z0-9]", "_", tolower(spec_label)), ".csv")
  fwrite(cdf, csv_name)

  invisible(list(model = model, coef_df = cdf))
}


# ========== Baseline event study (for comparison) ============================

cat("\n##########################################################\n")
cat("BASELINE: Two-way FE event study (county + year)\n")
cat("##########################################################\n")

model_base <- feols(
  total_incarceration_rate ~ i(event_time_binned, ref = -1) + total_pop |
    fips + year,
  data = es, cluster = ~fips
)
res_base <- extract_and_plot(model_base, "Baseline", "darkgreen")


# ========== SPEC A: County-specific linear trends ============================

cat("\n##########################################################\n")
cat("SPEC A: County-specific linear trends\n")
cat("##########################################################\n")

model_a <- feols(
  total_incarceration_rate ~ i(event_time_binned, ref = -1) + total_pop |
    fips[year] + year,
  data = es, cluster = ~fips
)
res_a <- extract_and_plot(model_a, "Spec A: County Trends", "steelblue")

cat("\nSummary: County-specific trends absorb linear pre-trends.\n")
cat("Compare pre-period coefficients to baseline to see if they shrink toward zero.\n")
cat("If post-treatment effects attenuate substantially, the baseline effect may\n")
cat("partly reflect differential trends rather than treatment.\n")


# ========== SPEC B: Placebo — shift treatment back 1 year ====================

cat("\n\n##########################################################\n")
cat("SPEC B: Placebo — treatment year shifted back 1 year\n")
cat("##########################################################\n")

es_b <- copy(es)
es_b[treated == 1, event_time := year - (treat_year - 1L)]
es_b[, event_time_binned := fcase(
  is.na(event_time) | treated == 0, -999L,
  event_time <= min_et, min_et,
  event_time >= max_et, max_et,
  default = as.integer(event_time)
)]

model_b <- feols(
  total_incarceration_rate ~ i(event_time_binned, ref = -1) + total_pop |
    fips + year,
  data = es_b, cluster = ~fips
)
res_b <- extract_and_plot(model_b, "Spec B: Placebo Timing", "darkorange")

cat("\nSummary: If treatment is real, shifting timing back 1 year should show\n")
cat("no effect at the placebo t=0 (which is actually t=-1 in true time).\n")
cat("A significant placebo t=0 suggests confounding or anticipation effects.\n")


# ========== SPEC C: Large counties only (pop >= 50k) =========================

cat("\n\n##########################################################\n")
cat("SPEC C: Large counties only (total_pop >= 50,000)\n")
cat("##########################################################\n")

es_c <- es[total_pop >= 50000]
cat(sprintf("Large-county sample: %d county-years, %d counties (%d treated)\n",
            nrow(es_c), uniqueN(es_c$fips),
            uniqueN(es_c[treated == 1, fips])))

model_c <- feols(
  total_incarceration_rate ~ i(event_time_binned, ref = -1) + total_pop |
    fips + year,
  data = es_c, cluster = ~fips
)
res_c <- extract_and_plot(model_c, "Spec C: Pop >= 50k", "darkred")

cat("\nSummary: Restricting to larger counties reduces noise from small-county\n")
cat("volatility. If effects strengthen, small counties may be adding noise.\n")
cat("If effects disappear, the baseline result may be driven by small counties.\n")


# ========== Summary table ====================================================

cat("\n\n##########################################################\n")
cat("COMPARISON SUMMARY\n")
cat("##########################################################\n\n")

summarize_spec <- function(res, label) {
  cdf <- res$coef_df
  pre  <- cdf[event_time <= -2 & event_time != -999]
  post <- cdf[event_time >= 0]
  data.table(
    spec           = label,
    n_obs          = res$model$nobs,
    mean_pre_coef  = mean(pre$coefficient),
    max_abs_pre    = max(abs(pre$coefficient)),
    mean_post_coef = mean(post$coefficient),
    post_t0_coef   = cdf[event_time == 0, coefficient],
    post_t0_se     = cdf[event_time == 0, se]
  )
}

comp <- rbindlist(list(
  summarize_spec(res_base, "Baseline"),
  summarize_spec(res_a,    "A: County trends"),
  summarize_spec(res_b,    "B: Placebo timing"),
  summarize_spec(res_c,    "C: Pop >= 50k")
))

print(comp, digits = 3)
fwrite(comp, "diagnostics_comparison_cmhc.csv")
cat("\nComparison saved to diagnostics_comparison_cmhc.csv\n")

cat("\n==========================================================\n")
cat("Diagnostics complete.\n")
cat("==========================================================\n")
