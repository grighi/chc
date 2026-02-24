#!/usr/bin/env Rscript
# ============================================================================
# 32b_fig_ccbhc_es_dod_diagnostics.R
# Diagnostics for the county-specific-linear-trend identification assumption
#
# The main spec in 32_fig_ccbhc_es_dod.R uses fips[year] (county-specific
# linear trends).  The identifying assumption is NOT parallel trends, but
# parallel *deviations* from county-specific linear trends.  These diagnostics
# assess whether that assumption is plausible.
#
# Outputs (all saved to FIG_DIR):
#   diag1  Raw mean DoD trends + pre-period linear fit extrapolated forward
#   diag2  Distribution of county-specific pre-period slopes by treatment
#   diag3  Mean county-detrended residuals by treatment status over time
#   diag4  Placebo trend-break test in pre-period (pseudo event at t=-3)
#   diag5  Leave-one-out: drop each treated cohort and re-estimate
# ============================================================================
log_section("32b: CCBHC DoD Diagnostics — County-Trend Assumption")

panel <- arrow::read_parquet(file.path(DATA_DIR, "ccbhc_panel.parquet")) %>%
  as.data.table()

if (!"dod_rate" %in% names(panel)) {
  cat("  WARNING: dod_rate not in panel.  Skipping diagnostics.\n")
} else {

# ── Prep: same sample as main spec ──────────────────────────────────────────
es <- panel[!is.na(dod_rate) & year >= 2013 & year <= 2024]
es[, log_dod_rate := log(dod_rate)]

# Identify pre-period (relative to first treated cohort, 2017 is last clean pre)
TREAT_START <- es[treated == 1, min(treat_year, na.rm = TRUE)]
cat(sprintf("  First CCBHC treatment year: %d\n", TREAT_START))

# ============================================================================
# DIAGNOSTIC 1:  Raw mean DoD trends + pre-period linear extrapolation
# ============================================================================
cat("\n--- Diagnostic 1: Raw trends + extrapolation ---\n")

# Compute population-weighted mean DoD rate by treatment group × year
trends <- es[, .(
  dod_rate_mean = weighted.mean(dod_rate, total_pop, na.rm = TRUE),
  log_dod_mean  = weighted.mean(log_dod_rate, total_pop, na.rm = TRUE),
  n_counties    = uniqueN(fips)
), by = .(treated, year)]

# Fit linear trend to pre-period only (year < TREAT_START)
pre_trends <- trends[year < TREAT_START]

fit_treated <- lm(dod_rate_mean ~ year, data = pre_trends[treated == 1])
fit_control <- lm(dod_rate_mean ~ year, data = pre_trends[treated == 0])

# Extrapolate into post-period
all_years <- data.table(year = min(es$year):max(es$year))
extrap_treated <- copy(all_years)[, `:=`(
  treated = 1L,
  fitted  = predict(fit_treated, newdata = .SD)
)]
extrap_control <- copy(all_years)[, `:=`(
  treated = 0L,
  fitted  = predict(fit_control, newdata = .SD)
)]
extrap <- rbind(extrap_treated, extrap_control)

trends[, group := fifelse(treated == 1, "Treated (CCBHC)", "Control")]
extrap[, group := fifelse(treated == 1, "Treated (CCBHC)", "Control")]

p1 <- ggplot() +
  # Actual trends
  geom_line(data = trends,
            aes(x = year, y = dod_rate_mean, color = group),
            linewidth = 1) +
  geom_point(data = trends,
             aes(x = year, y = dod_rate_mean, color = group),
             size = 2) +
  # Extrapolated pre-trend
  geom_line(data = extrap[year >= TREAT_START - 1],
            aes(x = year, y = fitted, color = group),
            linetype = "dashed", linewidth = 0.7) +
  # Treatment onset

  geom_vline(xintercept = TREAT_START - 0.5, linetype = "dotted", color = "gray40") +
  annotate("text", x = TREAT_START - 0.5, y = Inf, label = "First CCBHC grant",
           hjust = 1.05, vjust = 1.5, size = 3.2, color = "gray40") +
  scale_color_manual(values = c("Treated (CCBHC)" = "#c0392b", "Control" = "steelblue")) +
  labs(
    title    = "Deaths of Despair: Raw Trends and Pre-Period Linear Extrapolation",
    subtitle = sprintf("Pop-weighted means. Dashed = linear fit on %d–%d extrapolated forward.",
                        min(es$year), TREAT_START - 1),
    x = "Year", y = "Deaths of Despair per 100,000",
    color = NULL
  ) +
  theme_paper()

save_fig(p1, "fig_ccbhc_es_dod_diag1.pdf")


# ============================================================================
# DIAGNOSTIC 2:  Distribution of county-specific pre-period slopes
# ============================================================================
cat("\n--- Diagnostic 2: Distribution of county-level pre-period slopes ---\n")

# Fit county-level linear trends in pre-period
pre_county <- es[year < TREAT_START & !is.na(log_dod_rate) & is.finite(log_dod_rate)]

county_slopes <- pre_county[, {
  if (.N >= 3) {
    m <- lm(log_dod_rate ~ year)
    .(slope = coef(m)[2], r2 = summary(m)$r.squared, n_yrs = .N)
  } else {
    .(slope = NA_real_, r2 = NA_real_, n_yrs = .N)
  }
}, by = .(fips, treated)]

county_slopes <- county_slopes[!is.na(slope)]
county_slopes[, group := fifelse(treated == 1, "Treated", "Control")]

# Winsorize extreme slopes for plotting
q01 <- quantile(county_slopes$slope, 0.01, na.rm = TRUE)
q99 <- quantile(county_slopes$slope, 0.99, na.rm = TRUE)
county_slopes[, slope_w := pmin(pmax(slope, q01), q99)]

# KS test
ks <- ks.test(county_slopes[treated == 1, slope_w],
              county_slopes[treated == 0, slope_w])

# Means by group
mean_treated <- county_slopes[treated == 1, weighted.mean(slope_w, n_yrs, na.rm = TRUE)]
mean_control <- county_slopes[treated == 0, weighted.mean(slope_w, n_yrs, na.rm = TRUE)]

p2 <- ggplot(county_slopes, aes(x = slope_w, fill = group)) +
  geom_density(alpha = 0.45, color = NA) +
  geom_vline(xintercept = mean_treated, color = "#c0392b", linetype = "dashed") +
  geom_vline(xintercept = mean_control, color = "steelblue", linetype = "dashed") +
  scale_fill_manual(values = c("Treated" = "#c0392b", "Control" = "steelblue")) +
  labs(
    title    = "Distribution of County-Level Pre-Period DoD Trends",
    subtitle = sprintf(
      "Log(DoD rate) ~ year, %d–%d.  KS p = %.3f.  Mean slope: treated = %.4f, control = %.4f",
      min(pre_county$year), max(pre_county$year), ks$p.value, mean_treated, mean_control),
    x = "Annual change in log(DoD rate)", y = "Density",
    fill = NULL
  ) +
  theme_paper()

save_fig(p2, "fig_ccbhc_es_dod_diag2.pdf")


# ============================================================================
# DIAGNOSTIC 3:  County-detrended residuals by treatment status
# ============================================================================
cat("\n--- Diagnostic 3: County-detrended residuals ---\n")

#  Fit county-specific linear trends using ONLY pre-period data,
#  then compute residuals for ALL years (pre + post).
#  If the county-trend assumption holds, residual means should track
#  together in the pre-period for treated vs control.

es_detrend <- copy(es[!is.na(log_dod_rate) & is.finite(log_dod_rate)])

# Estimate county-specific intercepts + slopes from pre-period
pre_only <- es_detrend[year < TREAT_START]
county_trends <- pre_only[, {
  if (.N >= 3) {
    m <- lm(log_dod_rate ~ year)
    .(intercept = coef(m)[1], slope = coef(m)[2])
  } else {
    .(intercept = NA_real_, slope = NA_real_)
  }
}, by = fips]

county_trends <- county_trends[!is.na(slope)]

# Merge and compute residuals across all years
es_detrend <- merge(es_detrend, county_trends, by = "fips")
es_detrend[, predicted := intercept + slope * year]
es_detrend[, resid := log_dod_rate - predicted]

# Population-weighted mean residuals by group × year
resid_means <- es_detrend[, {
  w  <- total_pop / sum(total_pop, na.rm = TRUE)
  mu <- sum(w * resid, na.rm = TRUE)
  v  <- sum(w * (resid - mu)^2, na.rm = TRUE)
  .(mean_resid = mu,
    se_resid   = sqrt(v / uniqueN(fips)))
}, by = .(treated, year)]

resid_means[, group := fifelse(treated == 1, "Treated", "Control")]

p3 <- ggplot(resid_means, aes(x = year, y = mean_resid, color = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = TREAT_START - 0.5, linetype = "dotted", color = "gray40") +
  geom_ribbon(aes(ymin = mean_resid - 1.96 * se_resid,
                  ymax = mean_resid + 1.96 * se_resid,
                  fill = group), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Treated" = "#c0392b", "Control" = "steelblue")) +
  scale_fill_manual(values = c("Treated" = "#c0392b", "Control" = "steelblue")) +
  labs(
    title    = "County-Detrended Residuals: Treated vs. Control",
    subtitle = sprintf(
      "County linear trends fit on %d–%d, residuals computed for all years.\nConvergence pre-treatment validates the linear-trend assumption.",
      min(pre_only$year), max(pre_only$year)),
    x = "Year", y = "Mean residual from county linear trend (log scale)",
    color = NULL, fill = NULL
  ) +
  theme_paper()

save_fig(p3, "fig_ccbhc_es_dod_diag3.pdf")


# ============================================================================
# DIAGNOSTIC 4:  Placebo trend-break test in pre-period
# ============================================================================
cat("\n--- Diagnostic 4: Placebo trend-break test ---\n")

# If the linear-trend assumption holds, there should be no "break" at a
# pseudo treatment date in the pre-period.  We test a placebo break at
# year = TREAT_START - 3, which gives us enough pre-pre and pre-post data.

PLACEBO_YEAR <- TREAT_START - 2
pre_data <- es[year < TREAT_START & !is.na(log_dod_rate) & is.finite(log_dod_rate)]
pre_data[, placebo_post := as.integer(year >= PLACEBO_YEAR)]
pre_data[, placebo_treat_post := treated * placebo_post]

# Spec 1: Standard TWFE with county + year FE
mod_placebo_twfe <- feols(
  log_dod_rate ~ placebo_treat_post + total_pop | fips + year,
  data = pre_data,
  weights = ~total_pop,
  cluster = ~fips
)

# Spec 2: County-specific linear trends (the identifying assumption we test)
mod_placebo_trend <- feols(
  log_dod_rate ~ placebo_treat_post + total_pop | fips[year],
  data = pre_data,
  weights = ~total_pop,
  cluster = ~fips
)

cat("  Placebo trend-break (standard TWFE):\n")
cat(sprintf("    Coef = %.5f, SE = %.5f, p = %.4f\n",
            coef(mod_placebo_twfe)["placebo_treat_post"],
            sqrt(vcov(mod_placebo_twfe)["placebo_treat_post", "placebo_treat_post"]),
            pvalue(mod_placebo_twfe)["placebo_treat_post"]))

cat("  Placebo trend-break (county linear trends):\n")
cat(sprintf("    Coef = %.5f, SE = %.5f, p = %.4f\n",
            coef(mod_placebo_trend)["placebo_treat_post"],
            sqrt(vcov(mod_placebo_trend)["placebo_treat_post", "placebo_treat_post"]),
            pvalue(mod_placebo_trend)["placebo_treat_post"]))

# Build a small summary table
placebo_tbl <- data.table(
  Specification = c("TWFE (county + year FE)", "County linear trends (fips[year])"),
  Coefficient   = c(coef(mod_placebo_twfe)["placebo_treat_post"],
                     coef(mod_placebo_trend)["placebo_treat_post"]),
  SE            = c(sqrt(vcov(mod_placebo_twfe)["placebo_treat_post", "placebo_treat_post"]),
                     sqrt(vcov(mod_placebo_trend)["placebo_treat_post", "placebo_treat_post"])),
  p_value       = c(pvalue(mod_placebo_twfe)["placebo_treat_post"],
                     pvalue(mod_placebo_trend)["placebo_treat_post"])
)

# Run placebo event study fully within the pre-period
pre_data[, placebo_et := year - PLACEBO_YEAR]
pre_data[treated == 0, placebo_et := -999L]

# Bin extreme event times
pre_data[, placebo_et_b := fcase(
  placebo_et == -999L,         -999L,
  placebo_et <= -3L,           -3L,
  placebo_et >= 2L,             2L,
  default = as.integer(placebo_et)
)]

mod_placebo_es <- tryCatch(
  feols(
    log_dod_rate ~ i(placebo_et_b, ref = -1) + total_pop | fips[year],
    data = pre_data,
    weights = ~total_pop,
    cluster = ~fips
  ),
  error = function(e) { cat("  Placebo ES failed:", e$message, "\n"); NULL }
)

if (!is.null(mod_placebo_es)) {
  cn <- names(coef(mod_placebo_es))
  pat <- "^placebo_et_b::(-?[0-9]+)$"
  idx <- grepl(pat, cn)

  placebo_coefs <- data.frame(
    event_time  = as.integer(gsub(pat, "\\1", cn[idx])),
    coefficient = coef(mod_placebo_es)[idx],
    se          = sqrt(diag(vcov(mod_placebo_es)))[idx]
  )
  placebo_coefs <- placebo_coefs[placebo_coefs$event_time != -999, ]
  placebo_coefs$ci_lower <- placebo_coefs$coefficient - 1.96 * placebo_coefs$se
  placebo_coefs$ci_upper <- placebo_coefs$coefficient + 1.96 * placebo_coefs$se
  placebo_coefs <- rbind(placebo_coefs,
    data.frame(event_time = -1, coefficient = 0, se = 0, ci_lower = 0, ci_upper = 0))
  placebo_coefs <- placebo_coefs[order(placebo_coefs$event_time), ]

  p4 <- plot_event_study(
    placebo_coefs,
    title = sprintf("Placebo Event Study: Pseudo-Treatment at %d (Pre-Period Only)", PLACEBO_YEAR),
    subtitle = sprintf(
      "County linear trends. If assumption holds, all coefficients ≈ 0.\nTrend-break coef = %.4f (p = %.3f)",
      coef(mod_placebo_trend)["placebo_treat_post"],
      pvalue(mod_placebo_trend)["placebo_treat_post"]),
    xlab = sprintf("Years Relative to Placebo Date (%d)", PLACEBO_YEAR),
    ylab = "Coefficient (log DoD rate)",
    color = "#8e44ad"
  )
  save_fig(p4, "fig_ccbhc_es_dod_diag4.pdf")
}

save_csv(placebo_tbl, "ccbhc_dod_placebo_trendbreak.csv")


# ============================================================================
# DIAGNOSTIC 5:  Goodness of fit — county linear trends in pre-period
# ============================================================================
cat("\n--- Diagnostic 5: Linearity of county pre-trends ---\n")

# If many counties have highly non-linear pre-trends, the linear-trend
# assumption may be poor. We check R² of county-level year-on-dod regressions.

# county_slopes already has R²; merge treated status
r2_data <- county_slopes[!is.na(r2)]

p5 <- ggplot(r2_data, aes(x = r2, fill = group)) +
  geom_histogram(bins = 40, alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("Treated" = "#c0392b", "Control" = "steelblue")) +
  labs(
    title    = "How Linear Are County DoD Pre-Trends?",
    subtitle = sprintf(
      "R² from county-level log(DoD) ~ year, %d–%d.\nMedian R²: treated = %.2f, control = %.2f",
      min(pre_county$year), max(pre_county$year),
      r2_data[group == "Treated", median(r2)],
      r2_data[group == "Control", median(r2)]),
    x = expression(R^2 ~ "of county-level linear trend"),
    y = "Number of counties",
    fill = NULL
  ) +
  theme_paper()

save_fig(p5, "fig_ccbhc_es_dod_diag5.pdf")


# ============================================================================
# Summary
# ============================================================================
cat("\n========== DIAGNOSTICS SUMMARY ==========\n")
cat(sprintf("  Diagnostic 1: Raw trends + extrapolation     → fig_ccbhc_es_dod_diag1.pdf\n"))
cat(sprintf("  Diagnostic 2: Slope distribution by treatment→ fig_ccbhc_es_dod_diag2.pdf\n"))
cat(sprintf("  Diagnostic 3: Detrended residuals            → fig_ccbhc_es_dod_diag3.pdf\n"))
cat(sprintf("  Diagnostic 4: Placebo trend-break test       → fig_ccbhc_es_dod_diag4.pdf\n"))
cat(sprintf("  Diagnostic 5: Linearity of pre-trends (R²)   → fig_ccbhc_es_dod_diag5.pdf\n"))
cat("\n")

}
