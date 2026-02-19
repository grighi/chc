#!/usr/bin/env Rscript
# ============================================================================
# CMHC Placebo Robustness — Median-Split Design
# ============================================================================
# Placebo-treated: above-median priority rank, no CMHC in area
# Controls:        below-median priority rank, no CMHC in area
# Both groups drawn from the same universe of priority-ranked counties
# in catchment areas that never received a CMHC.
#
# Tests:
# (A) Pre/post DiD coefficient comparison (treatment vs. placebo)
# (B) Randomization inference: 1000 random pseudo-treatments
# (C) Event-study comparison plot: treatment vs. placebo coefficients
# ============================================================================

library(haven)
library(dplyr)
library(readr)
library(fixest)
library(tidyr)
library(fst)
library(ggplot2)

set.seed(42)

cat("============================================================\n")
cat("CMHC PLACEBO ROBUSTNESS — Median-Split Design\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# 1. Load and prepare data
# ------------------------------------------------------------------

cmhc_openings <- read_csv("cmhc_data/cmhc_openings.csv", show_col_types = FALSE)
data <- read_fst("aer_data/aer_data.fst") %>% as_tibble()

data <- data %>%
  left_join(cmhc_openings, by = "fips") %>%
  filter(!(stfips == 36 & cofips == 61),
         !(stfips == 6  & cofips == 37),
         !(stfips == 17 & cofips == 31)) %>%
  filter(year <= 1988) %>%
  mutate(cmhc_year_exp = ifelse(!is.na(cmhc_year_exp) & cmhc_year_exp <= 1975,
                                 cmhc_year_exp, NA))

# Urban categories
data <- data %>%
  group_by(fips) %>%
  mutate(urb60 = sum(`_60pcturban` * (year == 1960), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Durb = cut(urb60, breaks = c(-Inf, 1, 25, 50, 75, Inf),
                    labels = c("0", "1-25", "25-50", "50-75", "75+"),
                    include.lowest = TRUE))

# Population weights and quintile
data <- data %>%
  group_by(fips) %>%
  mutate(popwt    = sum(copop    * (year == 1960), na.rm = TRUE),
         popwt_ad = sum(copop_ad * (year == 1960), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pop_quintile = ntile(popwt, 5))

# Pre-treatment AMR quintile
pre_amr_for_quintile <- data %>%
  filter(year >= 1959 & year <= 1964) %>%
  group_by(fips) %>%
  summarize(pre_amr_mean = mean(amr, na.rm = TRUE),
            pre_amr_ad_mean = mean(amr_ad, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(amr_quintile    = ntile(pre_amr_mean, 5),
         amr_ad_quintile = ntile(pre_amr_ad_mean, 5))

data <- data %>% left_join(pre_amr_for_quintile, by = "fips")

# Priority data
priority <- read_csv("cmhc_data/priority_ranks_for_eventstudy.csv", show_col_types = FALSE)

data <- data %>%
  left_join(priority %>% select(fips, priority_pctile, priority_tercile,
                                 high_priority, area_has_cmhc, high_priority_no_cmhc),
            by = "fips")

median_cmhc_year <- median(cmhc_openings$cmhc_year_exp, na.rm = TRUE)

# ------------------------------------------------------------------
# 2. Build samples
# ------------------------------------------------------------------

# --- MAIN TREATMENT SAMPLE ---
data_main <- data %>%
  mutate(
    post_cmhc = case_when(
      is.na(cmhc_year_exp) ~ 0L,
      year >= cmhc_year_exp ~ 1L,
      TRUE ~ 0L
    )
  )

# --- PLACEBO SAMPLE: Median split among ranked counties in untreated areas ---
# Universe: no CMHC in their area, have priority data
data_placebo <- data %>%
  filter(is.na(cmhc_year_exp)) %>%                       # no direct CMHC
  filter(is.na(area_has_cmhc) | area_has_cmhc == 0) %>%  # no area CMHC
  filter(!is.na(priority_pctile)) %>%                     # has priority data
  mutate(
    above_median = as.integer(priority_pctile >= 0.5),
    placebo_year = ifelse(above_median == 1, median_cmhc_year, NA_real_),
    post_placebo = case_when(
      is.na(placebo_year) ~ 0L,
      year >= placebo_year ~ 1L,
      TRUE ~ 0L
    )
  )

n_plac_treated <- n_distinct(data_placebo$fips[data_placebo$above_median == 1])
n_plac_control <- n_distinct(data_placebo$fips[data_placebo$above_median == 0])
n_main_treated <- n_distinct(data_main$fips[!is.na(data_main$cmhc_year_exp)])

cat("Main treatment sample:", n_main_treated, "treated counties\n")
cat("Placebo sample (median split):\n")
cat("  Above-median priority (placebo-treated):", n_plac_treated, "counties\n")
cat("  Below-median priority (controls):", n_plac_control, "counties\n")
cat("  All in areas that never received a CMHC.\n\n")

# ------------------------------------------------------------------
# Helper: run pre/post regression
# ------------------------------------------------------------------

run_prepost_raw <- function(df, treat_var, depvar = "amr", weight_var = "popwt") {
  amr_q <- if (depvar == "amr_ad") "amr_ad_quintile" else "amr_quintile"
  fml <- as.formula(paste0(
    depvar, " ~ ", treat_var,
    " + D_tot_act_md_t + H_bpc",
    " + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`",
    " | fips + year^Durb + year^stfips + year^pop_quintile + year^", amr_q
  ))
  m <- feols(fml, data = df, weights = as.formula(paste0("~", weight_var)),
             cluster = ~fips)
  idx <- grep(treat_var, names(coef(m)), fixed = TRUE)
  list(coef = coef(m)[idx],
       se   = sqrt(diag(vcov(m)))[idx],
       nobs = m$nobs,
       model = m)
}

# ------------------------------------------------------------------
# Helper: run event-study regression and extract coefficients
# ------------------------------------------------------------------

run_eventstudy <- function(df, et_var, depvar = "amr", weight_var = "popwt") {
  amr_q <- if (depvar == "amr_ad") "amr_ad_quintile" else "amr_quintile"
  fml <- as.formula(paste0(
    depvar, " ~ i(", et_var, ", ref = -1)",
    " + D_tot_act_md_t + H_bpc",
    " + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`",
    " | fips + year^Durb + year^stfips + year^pop_quintile + year^", amr_q
  ))
  m <- feols(fml, data = df, weights = as.formula(paste0("~", weight_var)),
             cluster = ~fips)
  
  # Extract event-time coefficients
  pat <- paste0("^", et_var, "::(-?[0-9]+)$")
  cnames <- names(coef(m))
  idx <- grepl(pat, cnames)
  
  coef_df <- data.frame(
    event_time  = as.numeric(gsub(pat, "\\1", cnames[idx])),
    coefficient = coef(m)[idx],
    se          = sqrt(diag(vcov(m)))[idx]
  ) %>%
    filter(event_time != -999 & event_time < 15) %>%
    mutate(ci_lower = coefficient - 1.96 * se,
           ci_upper = coefficient + 1.96 * se)
  
  # Add reference period
  coef_df <- bind_rows(coef_df,
                       data.frame(event_time = -1, coefficient = 0, se = 0,
                                  ci_lower = 0, ci_upper = 0)) %>%
    arrange(event_time)
  
  list(coef_df = coef_df, model = m)
}


# ==================================================================
# (A) PRE/POST DiD: Treatment vs. Placebo
# ==================================================================

cat("##########################################################\n")
cat("(A) PRE/POST DiD: Treatment vs. Median-Split Placebo\n")
cat("##########################################################\n\n")

# Treatment
treat_amr    <- run_prepost_raw(data_main, "post_cmhc", "amr", "popwt")
treat_amr_ad <- run_prepost_raw(data_main, "post_cmhc", "amr_ad", "popwt_ad")

# Placebo
plac_amr    <- run_prepost_raw(data_placebo, "post_placebo", "amr", "popwt")
plac_amr_ad <- run_prepost_raw(data_placebo, "post_placebo", "amr_ad", "popwt_ad")

for (dv in c("amr", "amr_ad")) {
  tr <- if (dv == "amr") treat_amr else treat_amr_ad
  pl <- if (dv == "amr") plac_amr else plac_amr_ad
  label <- if (dv == "amr") "AMR All Ages" else "AMR Adults 20-49"
  
  ratio <- pl$coef / tr$coef
  diff_val <- pl$coef - tr$coef
  se_diff  <- sqrt(tr$se^2 + pl$se^2)
  p_diff   <- 2 * pnorm(-abs(diff_val / se_diff))
  
  cat("-----------------------------------------------------------\n")
  cat(label, ":\n")
  cat(sprintf("  Treatment effect:  %7.3f (SE %.3f, p = %.4f)\n",
              tr$coef, tr$se, 2*pnorm(-abs(tr$coef/tr$se))))
  cat(sprintf("  Placebo effect:    %7.3f (SE %.3f, p = %.4f)\n",
              pl$coef, pl$se, 2*pnorm(-abs(pl$coef/pl$se))))
  cat(sprintf("  Ratio (placebo/treatment): %.2f (%.0f%%)\n", ratio, abs(ratio)*100))
  cat(sprintf("  Difference: %.3f (SE %.3f, p = %.4f)\n", diff_val, se_diff, p_diff))
  cat(sprintf("  Reject placebo = treatment? %s\n\n",
              ifelse(p_diff < 0.05, "YES (p < 0.05)", "NO")))
}


# ==================================================================
# (B) RANDOMIZATION INFERENCE
# ==================================================================

cat("\n##########################################################\n")
cat("(B) RANDOMIZATION INFERENCE: 1000 random pseudo-treatments\n")
cat("##########################################################\n\n")

# RI pool: all ranked counties in untreated areas (both above & below median)
ri_fips <- unique(data_placebo$fips)
n_iters <- 1000

cat("Pool:", length(ri_fips), "ranked counties in untreated areas\n")
cat("Drawing", n_plac_treated, "pseudo-treated per iteration\n")
cat("Running", n_iters, "iterations...\n")

run_ri_iter <- function(df, pseudo_fips, depvar = "amr", weight_var = "popwt") {
  amr_q <- if (depvar == "amr_ad") "amr_ad_quintile" else "amr_quintile"
  df <- df %>%
    mutate(pseudo_post = as.integer(fips %in% pseudo_fips & year >= median_cmhc_year))
  fml <- as.formula(paste0(
    depvar, " ~ pseudo_post",
    " + D_tot_act_md_t + H_bpc",
    " + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`",
    " | fips + year^Durb + year^stfips + year^pop_quintile + year^", amr_q
  ))
  tryCatch({
    m <- suppressMessages(feols(fml, data = df,
                                weights = as.formula(paste0("~", weight_var)),
                                cluster = ~fips, notes = FALSE))
    idx <- grep("pseudo_post", names(coef(m)), fixed = TRUE)
    coef(m)[idx]
  }, error = function(e) NA_real_)
}

# AMR all ages
cat("\nRI for AMR all ages...")
ri_amr <- numeric(n_iters)
for (i in seq_len(n_iters)) {
  ri_amr[i] <- run_ri_iter(data_placebo, sample(ri_fips, n_plac_treated), "amr", "popwt")
  if (i %% 100 == 0) cat(" ", i)
}
cat(" done.\n")

# AMR adults
cat("RI for AMR adults 20-49...")
ri_amr_ad <- numeric(n_iters)
for (i in seq_len(n_iters)) {
  ri_amr_ad[i] <- run_ri_iter(data_placebo, sample(ri_fips, n_plac_treated), "amr_ad", "popwt_ad")
  if (i %% 100 == 0) cat(" ", i)
}
cat(" done.\n")

# RI results
for (dv in c("amr", "amr_ad")) {
  ri_coefs <- if (dv == "amr") ri_amr else ri_amr_ad
  actual   <- if (dv == "amr") plac_amr$coef else plac_amr_ad$coef
  label    <- if (dv == "amr") "AMR All Ages" else "AMR Adults 20-49"
  
  ri_p    <- mean(abs(ri_coefs) >= abs(actual), na.rm = TRUE)
  pctile  <- mean(ri_coefs <= actual, na.rm = TRUE) * 100
  
  cat(sprintf("\n--- %s ---\n", label))
  cat(sprintf("  Actual placebo: %.3f\n", actual))
  cat(sprintf("  RI null: mean = %.3f, sd = %.3f, [5th, 95th] = [%.3f, %.3f]\n",
              mean(ri_coefs, na.rm = TRUE), sd(ri_coefs, na.rm = TRUE),
              quantile(ri_coefs, 0.05, na.rm = TRUE), quantile(ri_coefs, 0.95, na.rm = TRUE)))
  cat(sprintf("  Percentile: %.1f%% | RI p-value: %.4f\n", pctile, ri_p))
  cat(sprintf("  Interpretation: %s the null distribution.\n",
              ifelse(ri_p > 0.10, "WELL WITHIN", ifelse(ri_p > 0.05, "MARGINAL", "OUTSIDE"))))
}

# RI plots
for (dv in c("amr", "amr_ad")) {
  ri_coefs <- if (dv == "amr") ri_amr else ri_amr_ad
  actual   <- if (dv == "amr") plac_amr$coef else plac_amr_ad$coef
  tr_coef  <- if (dv == "amr") treat_amr$coef else treat_amr_ad$coef
  ri_p     <- mean(abs(ri_coefs) >= abs(actual), na.rm = TRUE)
  pctile   <- mean(ri_coefs <= actual, na.rm = TRUE) * 100
  label    <- if (dv == "amr") "AMR All Ages" else "AMR Adults 20-49"
  fname    <- paste0("cmhc_ri_", dv, ".png")
  
  p <- ggplot(data.frame(coef = ri_coefs), aes(x = coef)) +
    geom_histogram(bins = 50, fill = "grey70", color = "grey40", alpha = 0.8) +
    geom_vline(xintercept = actual, color = "red", linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = tr_coef, color = "blue", linewidth = 1, linetype = "dashed") +
    annotate("text", x = actual, y = Inf,
             label = paste0("Actual placebo: ", round(actual, 1)),
             vjust = 2, hjust = -0.1, color = "red", size = 3.5) +
    annotate("text", x = tr_coef, y = Inf,
             label = paste0("Treatment: ", round(tr_coef, 1)),
             vjust = 3.5, hjust = -0.1, color = "blue", size = 3.5) +
    labs(title = paste("Randomization Inference:", label),
         subtitle = paste0("1000 random pseudo-treatments among ranked counties in untreated areas\n",
                           "RI p-value = ", round(ri_p, 3), " | Percentile: ", round(pctile, 1), "%"),
         x = "Pseudo-treatment coefficient", y = "Count") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  ggsave(fname, p, width = 8, height = 5, dpi = 200)
  cat(sprintf("\nPlot saved: %s\n", fname))
}


# ==================================================================
# (C) EVENT-STUDY COMPARISON PLOT
# ==================================================================

cat("\n\n##########################################################\n")
cat("(C) EVENT-STUDY: Treatment vs. Placebo coefficients\n")
cat("##########################################################\n\n")

# --- Main treatment event study ---
data_main_es <- data_main %>%
  mutate(
    event_time = year - cmhc_year_exp,
    et_binned = case_when(
      is.na(event_time) ~ -999L,
      event_time <= -6  ~ -6L,
      event_time >= 20  ~ 20L,
      TRUE ~ as.integer(event_time)
    )
  )

# --- Placebo event study ---
data_placebo_es <- data_placebo %>%
  mutate(
    placebo_event_time = year - placebo_year,
    pet_binned = case_when(
      is.na(placebo_event_time) ~ -999L,
      placebo_event_time <= -6  ~ -6L,
      placebo_event_time >= 20  ~ 20L,
      TRUE ~ as.integer(placebo_event_time)
    )
  )

for (dv in c("amr", "amr_ad")) {
  wt    <- if (dv == "amr") "popwt" else "popwt_ad"
  label <- if (dv == "amr") "AMR All Ages" else "AMR Adults 20\u201349"
  fname <- paste0("cmhc_placebo_vs_actual_", dv, ".png")
  
  cat(sprintf("Running event study for %s...\n", label))
  
  # Treatment event study
  es_treat <- run_eventstudy(data_main_es, "et_binned", dv, wt)
  
  # Placebo event study
  es_plac <- run_eventstudy(data_placebo_es, "pet_binned", dv, wt)
  
  # Combine for plotting
  coef_compare <- bind_rows(
    es_treat$coef_df %>% mutate(model = "Actual CMHC Treatment"),
    es_plac$coef_df  %>% mutate(model = "Placebo (Above-Median Priority, No CMHC in Area)")
  )
  
  # Pre/post F-tests for placebo
  pre_names  <- paste0("pet_binned::", -6:-2)
  pre_names  <- pre_names[pre_names %in% names(coef(es_plac$model))]
  post_names <- paste0("^pet_binned::", 0:14, "$")
  
  pre_test  <- tryCatch(wald(es_plac$model, pre_names), error = function(e) NULL)
  post_test <- tryCatch(wald(es_plac$model, post_names), error = function(e) NULL)
  
  pre_p  <- if (!is.null(pre_test))  round(pre_test$p, 3)  else NA
  post_p <- if (!is.null(post_test)) round(post_test$p, 3) else NA
  
  cat(sprintf("  Placebo pre-trend F-test p = %s\n", pre_p))
  cat(sprintf("  Placebo post-treatment F-test p = %s\n", post_p))
  
  # Plot
  p <- ggplot(coef_compare, aes(x = event_time, y = coefficient,
                                 color = model, fill = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.12, color = NA) +
    geom_point(size = 2, position = position_dodge(width = 0.4)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.4)) +
    scale_color_manual(values = c("Actual CMHC Treatment" = "steelblue",
                                   "Placebo (Above-Median Priority, No CMHC in Area)" = "firebrick")) +
    scale_fill_manual(values = c("Actual CMHC Treatment" = "steelblue",
                                  "Placebo (Above-Median Priority, No CMHC in Area)" = "firebrick")) +
    labs(
      title = paste("Placebo Test:", label),
      subtitle = paste0(
        "Blue: actual CMHC counties vs. never-treated. ",
        "Red: above-median priority vs. below-median priority (no CMHC in area).\n",
        "Placebo pre-trend p = ", pre_p, ", post-treatment p = ", post_p
      ),
      x = "Years Relative to (Pseudo-)Treatment",
      y = "Change in Deaths per 100,000",
      color = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  ggsave(fname, p, width = 11, height = 6.5, dpi = 300)
  cat(sprintf("  Plot saved: %s\n\n", fname))
}

# ------------------------------------------------------------------
# Summary Table
# ------------------------------------------------------------------

cat("\n==========================================================\n")
cat("SUMMARY TABLE\n")
cat("==========================================================\n\n")

ratio_amr    <- plac_amr$coef / treat_amr$coef
ratio_amr_ad <- plac_amr_ad$coef / treat_amr_ad$coef
diff_amr     <- plac_amr$coef - treat_amr$coef
diff_amr_ad  <- plac_amr_ad$coef - treat_amr_ad$coef
se_d_amr     <- sqrt(treat_amr$se^2 + plac_amr$se^2)
se_d_amr_ad  <- sqrt(treat_amr_ad$se^2 + plac_amr_ad$se^2)
p_d_amr      <- 2*pnorm(-abs(diff_amr/se_d_amr))
p_d_amr_ad   <- 2*pnorm(-abs(diff_amr_ad/se_d_amr_ad))
ri_p_amr     <- mean(abs(ri_amr) >= abs(plac_amr$coef), na.rm = TRUE)
ri_p_amr_ad  <- mean(abs(ri_amr_ad) >= abs(plac_amr_ad$coef), na.rm = TRUE)
pct_amr      <- mean(ri_amr <= plac_amr$coef, na.rm = TRUE) * 100
pct_amr_ad   <- mean(ri_amr_ad <= plac_amr_ad$coef, na.rm = TRUE) * 100

summary_df <- data.frame(
  Test = c(
    "Treatment effect (AMR)",
    "Treatment effect (AMR 20-49)",
    "Placebo, median-split (AMR)",
    "Placebo, median-split (AMR 20-49)",
    "Placebo / Treatment ratio (AMR)",
    "Placebo / Treatment ratio (AMR 20-49)",
    "Diff: placebo - treatment (AMR)",
    "Diff: placebo - treatment (AMR 20-49)",
    "RI p-value (AMR)",
    "RI p-value (AMR 20-49)",
    "RI percentile (AMR)",
    "RI percentile (AMR 20-49)",
    "N placebo-treated (above-median)",
    "N placebo-control (below-median)"
  ),
  Value = c(
    sprintf("%.3f (%.3f)", treat_amr$coef, treat_amr$se),
    sprintf("%.3f (%.3f)", treat_amr_ad$coef, treat_amr_ad$se),
    sprintf("%.3f (%.3f)", plac_amr$coef, plac_amr$se),
    sprintf("%.3f (%.3f)", plac_amr_ad$coef, plac_amr_ad$se),
    sprintf("%.2f (%.0f%%)", ratio_amr, abs(ratio_amr)*100),
    sprintf("%.2f (%.0f%%)", ratio_amr_ad, abs(ratio_amr_ad)*100),
    sprintf("%.3f (p = %.4f)", diff_amr, p_d_amr),
    sprintf("%.3f (p = %.4f)", diff_amr_ad, p_d_amr_ad),
    sprintf("%.4f", ri_p_amr),
    sprintf("%.4f", ri_p_amr_ad),
    sprintf("%.1f%%", pct_amr),
    sprintf("%.1f%%", pct_amr_ad),
    as.character(n_plac_treated),
    as.character(n_plac_control)
  ),
  stringsAsFactors = FALSE
)

print(as.data.frame(summary_df), right = FALSE, row.names = FALSE)

write_csv(summary_df, "cmhc_placebo_robustness_summary.csv")
cat("\nSaved: cmhc_placebo_robustness_summary.csv\n")

cat("\n============================================================\n")
cat("DONE\n")
cat("============================================================\n")
