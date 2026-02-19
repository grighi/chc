#!/usr/bin/env Rscript
# ============================================================================
# CMHC Placebo Test — Pre/Post DiD with Heterogeneity Analysis
# ============================================================================
# This script runs the placebo test for high-priority counties whose CMHC
# catchment area never received a CMHC. Instead of event-study coefficients,
# we estimate a single pre/post coefficient. We then split the placebo group
# along ~20 dimensions and report heterogeneity in a summary table.
# ============================================================================

library(haven)
library(dplyr)
library(readr)
library(fixest)
library(tidyr)
library(fst)

cat("============================================================\n")
cat("CMHC PLACEBO TEST — Pre/Post with Heterogeneity\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------
# 1. Load and prepare data (mirrors cmhc_event_study.R Steps 1–6)
# ------------------------------------------------------------------

cmhc_openings <- read_csv("cmhc_data/cmhc_openings.csv", show_col_types = FALSE)
data <- read_fst("aer_data/aer_data.fst") %>% as_tibble()

data <- data %>%
  left_join(cmhc_openings, by = "fips") %>%
  filter(!(stfips == 36 & cofips == 61),   # NYC
         !(stfips == 6  & cofips == 37),   # LA
         !(stfips == 17 & cofips == 31)) %>% # Chicago
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

# Population weights from 1960
data <- data %>%
  group_by(fips) %>%
  mutate(popwt    = sum(copop    * (year == 1960), na.rm = TRUE),
         popwt_ad = sum(copop_ad * (year == 1960), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pop_quintile = ntile(popwt, 5))

# Pre-treatment AMR quintile (for year x baseline-mortality FE)
pre_amr_for_quintile <- data %>%
  filter(year >= 1959 & year <= 1964) %>%
  group_by(fips) %>%
  summarize(pre_amr_mean = mean(amr, na.rm = TRUE),
            pre_amr_ad_mean = mean(amr_ad, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(amr_quintile    = ntile(pre_amr_mean, 5),
         amr_ad_quintile = ntile(pre_amr_ad_mean, 5))

data <- data %>%
  left_join(pre_amr_for_quintile, by = "fips")

# ------------------------------------------------------------------
# 2. Load pscore data for extra covariates
# ------------------------------------------------------------------

pscore_data <- haven::read_dta("aer_data/aer_pscore_data.dta") %>%
  mutate(fips = stfips * 1000 + cofips) %>%
  select(fips,
         ps_popden      = `_popden`,
         ps_pctpopgro   = `_pctpopgro`,
         ps_pctnw       = `_pctnw`,
         ps_pct65       = `_pct65`,
         ps_pcturb      = `_pcturb`,
         ps_pctrur      = `_pctrur`,
         ps_pctinclt3k  = `_pctinclt3k`,
         ps_pctincgt10k = `_pctincgt10k`,
         ps_pctschlt4   = `_pctschlt4`,
         ps_pctschlgt12 = `_pctschlgt12`,
         ps_lfp         = `_lfp`,
         ps_pctlfue     = `_pctlfue`,
         ps_pctlfmale   = `_pctlfmale`,
         ps_pctrent     = `_pctrent`,
         ps_pctplumb    = `_pctplumb`,
         ps_pcttv       = `_pcttv`,
         ps_pcttel      = `_pcttel`,
         ps_pctcar      = `_pctcar`,
         ps_md_per1000  = `_md_per1000`,
         ps_govtexp     = `_govtexp_per1000`,
         ps_region      = region)

data <- data %>% left_join(pscore_data, by = "fips")

# ------------------------------------------------------------------
# 3. Load priority data and build placebo sample
# ------------------------------------------------------------------

priority <- read_csv("cmhc_data/priority_ranks_for_eventstudy.csv", show_col_types = FALSE)

data <- data %>%
  left_join(priority %>% select(fips, priority_pctile, priority_tercile,
                                 high_priority, area_has_cmhc, high_priority_no_cmhc),
            by = "fips")

median_cmhc_year <- median(cmhc_openings$cmhc_year_exp, na.rm = TRUE)

# === MAIN TREATMENT SAMPLE (for the "actual" pre/post spec) ===
# Treated = counties with CMHC; Control = never-treated counties
data_main <- data %>%
  mutate(
    post_cmhc = case_when(
      is.na(cmhc_year_exp) ~ 0L,
      year >= cmhc_year_exp ~ 1L,
      TRUE ~ 0L
    )
  )

# === PLACEBO SAMPLE ===
# Drop CMHC recipients + counties in treated areas
data_placebo <- data %>%
  filter(is.na(cmhc_year_exp)) %>%
  filter(is.na(area_has_cmhc) | area_has_cmhc == 0) %>%
  mutate(
    placebo_year = ifelse(high_priority_no_cmhc == 1, median_cmhc_year, NA_real_),
    post_placebo = case_when(
      is.na(placebo_year) ~ 0L,
      year >= placebo_year ~ 1L,
      TRUE ~ 0L
    )
  )

n_main_treated   <- n_distinct(data_main$fips[!is.na(data_main$cmhc_year_exp)])
n_main_control   <- n_distinct(data_main$fips[is.na(data_main$cmhc_year_exp)])
n_placebo_treated <- n_distinct(data_placebo$fips[data_placebo$high_priority_no_cmhc == 1 & !is.na(data_placebo$high_priority_no_cmhc)])
n_placebo_control <- n_distinct(data_placebo$fips[is.na(data_placebo$placebo_year)])

cat("Main sample: ", n_main_treated, " treated,", n_main_control, "control counties\n")
cat("Placebo sample:", n_placebo_treated, "placebo-treated,", n_placebo_control, "control counties\n\n")

# ------------------------------------------------------------------
# 4. Helper: Run pre/post spec and extract results
# ------------------------------------------------------------------

run_prepost <- function(df, treat_var, depvar = "amr", weight_var = "popwt",
                        spec_label = "", extra_info = "") {
                            
  # Choose baseline-mortality quintile matching the dependent variable
  amr_q <- if (depvar == "amr_ad") "amr_ad_quintile" else "amr_quintile"

  fml <- as.formula(paste0(
    depvar, " ~ ", treat_var,
    " + D_tot_act_md_t + H_bpc",
    " + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`",
    " | fips + year^Durb + year^stfips + year^pop_quintile + year^", amr_q
  ))
  
  tryCatch({
    m <- feols(fml, data = df, weights = as.formula(paste0("~", weight_var)),
               cluster = ~fips)
    
    idx <- grep(treat_var, names(coef(m)), fixed = TRUE)
    if (length(idx) == 0) return(NULL)
    
    coef_val <- coef(m)[idx]
    se_val   <- sqrt(diag(vcov(m)))[idx]
    t_val    <- coef_val / se_val
    p_val    <- 2 * pt(abs(t_val), df = m$nobs - length(coef(m)), lower.tail = FALSE)
    n_obs    <- m$nobs
    n_fips   <- length(unique(m$fixef_id$fips))
    
    stars <- case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01  ~ "**",
      p_val < 0.05  ~ "*",
      p_val < 0.1   ~ ".",
      TRUE          ~ ""
    )
    
    data.frame(
      specification = spec_label,
      details       = extra_info,
      depvar        = depvar,
      coef          = round(coef_val, 3),
      se            = round(se_val, 3),
      p_value       = round(p_val, 4),
      signif        = stars,
      n_counties    = n_fips,
      n_obs         = n_obs,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      specification = spec_label,
      details       = extra_info,
      depvar        = depvar,
      coef          = NA_real_,
      se            = NA_real_,
      p_value       = NA_real_,
      signif        = "ERR",
      n_counties    = NA_integer_,
      n_obs         = NA_integer_,
      stringsAsFactors = FALSE
    )
  })
}

# ------------------------------------------------------------------
# 5. Compute county-level baseline covariates for splits
# ------------------------------------------------------------------

# Get cross-sectional 1960 values (take first non-NA per county)
baseline <- data %>%
  filter(year == 1960) %>%
  select(fips, stfips,
         copop_1960   = copop,
         copop_ad_1960 = copop_ad,
         copop_eld_1960 = copop_eld,
         pct_urban    = `_60pcturban`,
         pct_rural    = `_60pctrurf`,
         pct_nonwhite = `_60pctnonwhit`,
         pct_young    = `_60pct04years`,
         pct_elderly  = `_60pctmt64years`,
         med_fam_inc  = `_59medfaminc`,
         pct_inc_lt3k = `_pct59inclt3k`,
         pct_inc_gt10k = `_pct59incmt10k`,
         med_schooling = `_60medschlmt24`,
         pct_lt4_schl  = `_60pctlt4schl`,
         pct_gt12_schl = `_60pctmt12schl`,
         tot_act_md    = `_tot_act_md`,
         H_bpc_1960    = H_bpc) %>%
  distinct(fips, .keep_all = TRUE)

# Merge pscore covariates (already one-row-per-county)
baseline <- baseline %>%
  left_join(
    pscore_data %>% select(fips, ps_popden, ps_pctpopgro, ps_lfp, ps_pctlfue,
                           ps_pctrent, ps_pctplumb, ps_pcttv, ps_pcttel,
                           ps_pctcar, ps_md_per1000, ps_govtexp, ps_region),
    by = "fips"
  )

# South indicator (Census region 3 = South — AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN, TX, VA, WV)
baseline <- baseline %>%
  mutate(south = as.integer(ps_region == 3))

# CHC indicator
chc_info <- data %>%
  filter(year == 1960) %>%
  select(fips, ever_chc = chc_year_exp) %>%
  mutate(ever_chc = as.integer(!is.na(ever_chc))) %>%
  distinct(fips, .keep_all = TRUE)

baseline <- baseline %>% left_join(chc_info, by = "fips")

# Pre-treatment AMR (average 1959–1964)
pre_amr <- data %>%
  filter(year >= 1959 & year <= 1964) %>%
  group_by(fips) %>%
  summarize(pre_amr = mean(amr, na.rm = TRUE),
            pre_amr_ad = mean(amr_ad, na.rm = TRUE),
            .groups = "drop")

baseline <- baseline %>% left_join(pre_amr, by = "fips")

# ------------------------------------------------------------------
# 6. Define heterogeneity splits
# ------------------------------------------------------------------

# Helper: compute median split within the placebo sample
compute_split <- function(df_baseline, var_name, label_high, label_low) {
  vals <- df_baseline[[var_name]]
  med  <- median(vals, na.rm = TRUE)
  above <- df_baseline$fips[!is.na(vals) & vals >= med]
  below <- df_baseline$fips[!is.na(vals) & vals <  med]
  list(above = above, below = below, label_high = label_high, label_low = label_low,
       median_val = round(med, 2))
}

# Get baseline for placebo-treated counties only
placebo_fips <- unique(data_placebo$fips[data_placebo$high_priority_no_cmhc == 1 & !is.na(data_placebo$high_priority_no_cmhc)])
placebo_baseline <- baseline %>% filter(fips %in% placebo_fips)

cat("Defining heterogeneity splits across", nrow(placebo_baseline), "placebo-treated counties\n\n")

# Define all splits
splits <- list(
  list(var = "copop_1960",    hi = "Large population",       lo = "Small population"),
  list(var = "ps_popden",     hi = "High population density", lo = "Low population density"),
  list(var = "pct_urban",     hi = "More urban",             lo = "More rural"),
  list(var = "pct_nonwhite",  hi = "High % nonwhite",        lo = "Low % nonwhite"),
  list(var = "pct_elderly",   hi = "High % elderly (65+)",   lo = "Low % elderly (65+)"),
  list(var = "pct_young",     hi = "High % young (0-4)",     lo = "Low % young (0-4)"),
  list(var = "med_fam_inc",   hi = "High median income",     lo = "Low median income"),
  list(var = "pct_inc_lt3k",  hi = "High poverty rate",      lo = "Low poverty rate"),
  list(var = "med_schooling", hi = "High education",         lo = "Low education"),
  list(var = "pct_lt4_schl",  hi = "High % very low educ.",  lo = "Low % very low educ."),
  list(var = "tot_act_md",    hi = "More physicians",        lo = "Fewer physicians"),
  list(var = "ps_md_per1000", hi = "High MDs per 1000",      lo = "Low MDs per 1000"),
  list(var = "H_bpc_1960",    hi = "High hosp. beds/capita", lo = "Low hosp. beds/capita"),
  list(var = "ps_pctlfue",    hi = "High unemployment",      lo = "Low unemployment"),
  list(var = "ps_lfp",        hi = "High labor force partic.", lo = "Low labor force partic."),
  list(var = "ps_pctpopgro",  hi = "Growing population",     lo = "Declining/stable pop."),
  list(var = "ps_pctplumb",   hi = "Better housing quality", lo = "Worse housing quality"),
  list(var = "ps_pctrent",    hi = "High % renters",         lo = "Low % renters"),
  list(var = "ps_govtexp",    hi = "High gov't expenditure", lo = "Low gov't expenditure"),
  list(var = "pre_amr",       hi = "High baseline mortality", lo = "Low baseline mortality")
)

# ------------------------------------------------------------------
# 7. Run all specifications and collect results
# ------------------------------------------------------------------

results <- list()
row_num <- 0

# --- Row 1: Main spec (actual treatment) — AMR all ages ---
cat("Running main treatment spec...\n")
row_num <- row_num + 1
results[[row_num]] <- run_prepost(
  data_main, "post_cmhc", "amr", "popwt",
  "Main treatment",
  paste0("CMHC counties vs. never-treated | AMR all ages | N_treated=", n_main_treated)
)

# --- Row 2: Main spec — AMR adults ---
row_num <- row_num + 1
results[[row_num]] <- run_prepost(
  data_main, "post_cmhc", "amr_ad", "popwt_ad",
  "Main treatment",
  paste0("CMHC counties vs. never-treated | AMR ages 20-49 | N_treated=", n_main_treated)
)

# --- Row 3: Placebo full sample — AMR all ages ---
cat("Running placebo spec...\n")
row_num <- row_num + 1
results[[row_num]] <- run_prepost(
  data_placebo, "post_placebo", "amr", "popwt",
  "Placebo (full)",
  paste0("High-priority no-CMHC-area vs. never-treated | AMR all ages | N_placebo=", n_placebo_treated)
)

# --- Row 4: Placebo full sample — AMR adults ---
row_num <- row_num + 1
results[[row_num]] <- run_prepost(
  data_placebo, "post_placebo", "amr_ad", "popwt_ad",
  "Placebo (full)",
  paste0("High-priority no-CMHC-area vs. never-treated | AMR ages 20-49 | N_placebo=", n_placebo_treated)
)

# --- Heterogeneity: split placebo-treated on each dimension ---
cat("Running heterogeneity splits...\n")

for (s in splits) {
  sp <- compute_split(placebo_baseline, s$var, s$hi, s$lo)
  
  n_hi <- length(sp$above)
  n_lo <- length(sp$below)
  
  # High subgroup: keep placebo-treated who are above median + all controls
  data_hi <- data_placebo %>%
    filter(is.na(placebo_year) |  # controls
           fips %in% sp$above)     # high-subgroup treated
  
  data_lo <- data_placebo %>%
    filter(is.na(placebo_year) |  # controls
           fips %in% sp$below)     # low-subgroup treated
  
  # AMR all ages — high
  row_num <- row_num + 1
  results[[row_num]] <- run_prepost(
    data_hi, "post_placebo", "amr", "popwt",
    paste0("Placebo: ", s$hi),
    paste0(s$var, " >= median (", sp$median_val, ") | AMR all ages | N_placebo=", n_hi)
  )
  
  # AMR all ages — low
  row_num <- row_num + 1
  results[[row_num]] <- run_prepost(
    data_lo, "post_placebo", "amr", "popwt",
    paste0("Placebo: ", s$lo),
    paste0(s$var, " < median (", sp$median_val, ") | AMR all ages | N_placebo=", n_lo)
  )
}

# --- Also run: South vs non-South (categorical, not median split) ---
south_fips    <- placebo_baseline$fips[placebo_baseline$south == 1 & !is.na(placebo_baseline$south)]
nonsouth_fips <- placebo_baseline$fips[placebo_baseline$south == 0 | is.na(placebo_baseline$south)]

data_south    <- data_placebo %>% filter(is.na(placebo_year) | fips %in% south_fips)
data_nonsouth <- data_placebo %>% filter(is.na(placebo_year) | fips %in% nonsouth_fips)

row_num <- row_num + 1
results[[row_num]] <- run_prepost(
  data_south, "post_placebo", "amr", "popwt",
  "Placebo: South",
  paste0("Census South region | AMR all ages | N_placebo=", length(south_fips))
)

row_num <- row_num + 1
results[[row_num]] <- run_prepost(
  data_nonsouth, "post_placebo", "amr", "popwt",
  "Placebo: Non-South",
  paste0("Census non-South | AMR all ages | N_placebo=", length(nonsouth_fips))
)

# --- Ever had a CHC ---
chc_fips    <- placebo_baseline$fips[placebo_baseline$ever_chc == 1 & !is.na(placebo_baseline$ever_chc)]
no_chc_fips <- placebo_baseline$fips[placebo_baseline$ever_chc == 0 | is.na(placebo_baseline$ever_chc)]

data_chc    <- data_placebo %>% filter(is.na(placebo_year) | fips %in% chc_fips)
data_nochc  <- data_placebo %>% filter(is.na(placebo_year) | fips %in% no_chc_fips)

row_num <- row_num + 1
results[[row_num]] <- run_prepost(
  data_chc, "post_placebo", "amr", "popwt",
  "Placebo: Has CHC",
  paste0("County has a Community Health Center | AMR all ages | N_placebo=", length(chc_fips))
)

row_num <- row_num + 1
results[[row_num]] <- run_prepost(
  data_nochc, "post_placebo", "amr", "popwt",
  "Placebo: No CHC",
  paste0("County has no CHC | AMR all ages | N_placebo=", length(no_chc_fips))
)

# ------------------------------------------------------------------
# 8. Combine and display results table
# ------------------------------------------------------------------

results_df <- bind_rows(results)

cat("\n\n")
cat("================================================================================================================================\n")
cat("PLACEBO HETEROGENEITY TABLE\n")
cat("================================================================================================================================\n")
cat("Dep var: Age-adjusted mortality rate (AMR). Pre/post DiD coefficient.\n")
cat("Controls: MDs x trend, pop x trend, beds/capita, % urban, % poverty, % nonwhite.\n")
cat("FE: county, year x urban-quartile, year x state.\n")
cat("Clustering: county. Weights: 1960 population.\n")
cat("Pseudo-treatment year:", median_cmhc_year, "\n")
cat("================================================================================================================================\n\n")

# Print formatted table
options(width = 160)

# Format for printing
results_print <- results_df %>%
  mutate(
    Coef    = ifelse(is.na(coef), "  —", sprintf("%8.3f%s", coef, signif)),
    SE      = ifelse(is.na(se),   "  —", sprintf("(%6.3f)", se)),
    `p-val` = ifelse(is.na(p_value), "  —", sprintf("%.4f", p_value)),
    Counties = as.character(ifelse(is.na(n_counties), "—", n_counties)),
    Obs      = as.character(ifelse(is.na(n_obs), "—", n_obs))
  ) %>%
  select(Specification = specification, Details = details,
         Coef, SE, `p-val`, Counties, Obs)

print(as.data.frame(results_print), right = FALSE, row.names = FALSE)

# ------------------------------------------------------------------
# 9. Save to CSV
# ------------------------------------------------------------------

write_csv(results_df, "cmhc_placebo_heterogeneity_table.csv")
cat("\n\nTable saved to: cmhc_placebo_heterogeneity_table.csv\n")

cat("\n============================================================\n")
cat("DONE\n")
cat("============================================================\n")
