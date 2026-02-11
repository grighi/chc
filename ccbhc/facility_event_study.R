#!/usr/bin/env Rscript
# facility_event_study.R
#
# Diff-in-diff event study of mental health facility openings on county
# incarceration rates using two-way fixed effects (county + year).
#
# Treatment: county gains >= 1 new facility of a given type relative to
# the prior year.  event_time == 0 is the first year that happens.
# Never-treated counties (no year-over-year increase) serve as controls.
#
# Runs three separate event studies for: all_omh, cmhc, act.
#
# Usage:
#   Rscript facility_event_study.R

library(data.table)
library(fixest)
library(ggplot2)

# ========== 1. Build county-year facility counts =============================

dt <- as.data.table(readRDS("facilities_geocoded.rds"))

cat(sprintf("Loaded %d facilities, years: %s\n",
            nrow(dt), paste(sort(unique(dt$year)), collapse = ", ")))

# Flag facility types
dt[, all_omh := as.integer(grepl("\\bCMHC\\b|\\bOMH\\b|\\bMSNH\\b", services))]
dt[, cmhc   := as.integer(grepl("\\bCMHC\\b", services))]
dt[, act    := as.integer(grepl("\\bACT\\b",  services))]

# Collapse to county-year
fac <- dt[!is.na(county_fips),
          .(all_omh = sum(all_omh),
            cmhc    = sum(cmhc),
            act     = sum(act)),
          by = .(county_fips, year)]

cat(sprintf("Facility counts: %d county-year rows, %d counties\n",
            nrow(fac), uniqueN(fac$county_fips)))

# ========== 2. Load & collapse incarceration data ============================

inc <- fread("../incarceration_trends_county.csv",
             select = c("fips", "year", "quarter",
                        "total_pop", "total_jail_pop_rate"))
inc[, fips := sprintf("%05d", fips)]

inc_yr <- inc[, .(total_pop               = mean(total_pop, na.rm = TRUE),
                  total_incarceration_rate = mean(total_jail_pop_rate, na.rm = TRUE)),
              by = .(fips, year)]

cat(sprintf("Incarceration data: %d county-year rows\n", nrow(inc_yr)))

# ========== 3. Merge =========================================================

merged <- merge(inc_yr, fac,
                by.x = c("fips", "year"),
                by.y = c("county_fips", "year"),
                all.x = TRUE)

# No facility = 0
for (v in c("all_omh", "cmhc", "act")) {
  set(merged, which(is.na(merged[[v]])), v, 0L)
}

# Drop missing outcome
merged <- merged[!is.na(total_incarceration_rate)]

cat(sprintf("Merged panel: %d county-year rows, %d counties\n",
            nrow(merged), uniqueN(merged$fips)))

# ========== 4. Compute year-over-year changes & facility rates per 100k =====

setorder(merged, fips, year)
merged[, `:=`(
  d_all_omh = all_omh - shift(all_omh),
  d_cmhc    = cmhc    - shift(cmhc),
  d_act     = act     - shift(act)
), by = fips]

# Per-100k versions
merged[, `:=`(
  d_all_omh_100k = d_all_omh / total_pop * 1e5,
  d_cmhc_100k    = d_cmhc    / total_pop * 1e5,
  d_act_100k     = d_act     / total_pop * 1e5
)]

# ========== 5. Histogram of non-zero changes per 100k =======================

cat("\n========== Histograms of non-zero facility changes per 100k ==========\n\n")

for (v in c("d_all_omh_100k", "d_cmhc_100k", "d_act_100k")) {
  vals <- merged[!is.na(get(v)) & get(v) != 0, get(v)]
  if (length(vals) == 0) {
    cat(sprintf("  %s: no non-zero changes\n", v))
    next
  }
  cat(sprintf("  %s (n=%d): min=%.2f, p25=%.2f, median=%.2f, p75=%.2f, max=%.2f\n",
              v, length(vals),
              min(vals), quantile(vals, 0.25), median(vals),
              quantile(vals, 0.75), max(vals)))

  p_hist <- ggplot(data.frame(x = vals), aes(x = x)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    labs(title = paste("Non-zero changes:", v),
         x = "Change in facilities per 100k", y = "Count") +
    theme_minimal(base_size = 12)

  fname <- paste0("histogram_", sub("d_", "", sub("_100k", "", v)), ".png")
  ggsave(fname, p_hist, width = 8, height = 5, dpi = 300)
  cat(sprintf("  Saved %s\n", fname))
}

# ========== 6. Identify treatment timing =====================================
#   For each facility type, a county is "treated" the first year its count
#   increases by >= 1 relative to the prior year.

find_treatment_year <- function(dt, fac_var) {
  d_var <- paste0("d_", fac_var)
  treat <- dt[!is.na(get(d_var)) & get(d_var) >= 1 & year != 2017,
              .(treat_year = min(year)), by = fips]
  treat
}

treat_omh  <- find_treatment_year(merged, "all_omh")
treat_cmhc <- find_treatment_year(merged, "cmhc")
treat_act  <- find_treatment_year(merged, "act")

cat(sprintf("\nTreatment counties — all_omh: %d, cmhc: %d, act: %d\n",
            nrow(treat_omh), nrow(treat_cmhc), nrow(treat_act)))
cat("Treatment year distributions:\n")
cat("  all_omh: "); print(table(treat_omh$treat_year))
cat("  cmhc:    "); print(table(treat_cmhc$treat_year))
cat("  act:     "); print(table(treat_act$treat_year))

# ========== 7. Run event studies =============================================

run_event_study <- function(panel, treat_dt, label, color) {

  # Merge treatment timing
  es <- merge(panel, treat_dt, by = "fips", all.x = TRUE)

  # event_time: years relative to treatment; NA for never-treated
  es[, event_time := year - treat_year]

  # Bin endpoints and set never-treated to -999
  min_et <- -5L
  max_et <-  3L
  es <- es[(event_time >= min_et & event_time <= max_et) | is.na(event_time)]
  es[, event_time_binned := fcase(
    is.na(event_time), -999L,
    event_time <= min_et, min_et,
    event_time >= max_et, max_et,
    default = as.integer(event_time)
  )]

  cat(sprintf("\n--- Event study: %s ---\n", label))
  cat("Event time distribution:\n")
  print(table(es$event_time_binned, useNA = "ifany"))

  # Two-way FE regression with population control
  # Reference period: t = -1
  model <- feols(
    total_incarceration_rate ~ i(event_time_binned, ref = -1) + total_pop |
      fips + year,
    data = es,
    cluster = ~fips,
    weight = ~total_pop
  )

  cat("\n")
  # print(summary(model))

  # ---- Extract event-study coefficients ----
  cn <- names(coef(model))
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

  # ---- Print coefficient table ----
  cat("\nEvent study coefficients:\n")
  print(coef_df[, .(event_time,
                    coef   = sprintf("%.3f", coefficient),
                    se     = sprintf("%.3f", se),
                    ci_95  = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
                    signif = fifelse(abs(coefficient / se) > 2.576, "***",
                             fifelse(abs(coefficient / se) > 1.96, "**",
                             fifelse(abs(coefficient / se) > 1.645, "*", ""))))])

  # ---- Save coefficients ----
  csv_name <- paste0("event_study_coefficients_", label, ".csv")
  fwrite(coef_df, csv_name)
  cat(sprintf("Coefficients saved to %s\n", csv_name))

  # ---- Plot ----
  p <- ggplot(coef_df, aes(x = event_time, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = color, alpha = 0.2) +
    geom_point(color = color, size = 2) +
    geom_line(color = color, linewidth = 0.8) +
    labs(
      title = paste0("Effect of New ", label, " Facility on Incarceration Rate"),
      subtitle = "Event study coefficients with 95% CI, reference period t = -1",
      x = "Years Relative to First New Facility Opening",
      y = "Change in Incarceration Rate (per 100k)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())

  plot_name <- paste0("event_study_", label, ".png")
  ggsave(plot_name, p, width = 10, height = 6, dpi = 300)
  cat(sprintf("Plot saved to %s\n", plot_name))

  invisible(list(model = model, coef_df = coef_df))
}

# Keep only the columns we need for the event studies
panel <- merged[, .(fips, year, total_incarceration_rate, total_pop)]

# Run all three
res_omh  <- run_event_study(panel, treat_omh,  "all_omh", "steelblue")
res_cmhc <- run_event_study(panel, treat_cmhc, "cmhc",    "darkgreen")
res_act  <- run_event_study(panel, treat_act,  "act",     "darkred")

cat("\n==========================================================\n")
cat("Event study complete.\n")
cat("==========================================================\n")
