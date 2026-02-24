#!/usr/bin/env Rscript
# ============================================================================
# Linear TWFE version of the Avery replication
#
# Same dataset and treatment definition as cmhc_replication.r, but uses
# the OLS/linear TWFE specification from cmhc_event_study.R:
#   - feols instead of fepois
#   - State-by-year FE instead of state linear trends
#   - 1960 baseline population weights
#   - 1-year event-time bins
#   - Level controls (D_tot_act_md_t, H_bpc, _60pcturban, etc.)
#
# Goal: Verify negative post-period results for adults < 50
# ============================================================================

library(dplyr)
library(fixest)
library(ggplot2)

cat("============================================================\n")
cat("LINEAR TWFE: CMHC Effect on Mortality\n")
cat("(Main analysis specification applied to replication dataset)\n")
cat("============================================================\n\n")

# ==========================================
# STEP 1: Load and merge data
# ==========================================

cmhc_openings <- readr::read_csv('cmhc_data/cmhc_openings.csv', show_col_types = FALSE)
cmhcs_by_year <- readr::read_csv('cmhc_data/cmhcs_by_year.csv', show_col_types = FALSE)

# Drop 1977-only catchment areas (same correction as replication)
appearances <- cmhcs_by_year %>%
  filter(cmhc_count > 0) %>%
  group_by(fips) %>%
  summarize(n_dir_years = n(), .groups = "drop")

only_1977 <- cmhc_openings %>%
  left_join(appearances, by = "fips") %>%
  filter(cmhc_year_exp == 1977 & (is.na(n_dir_years) | n_dir_years == 1))

cmhc_openings <- cmhc_openings %>%
  filter(!(fips %in% only_1977$fips))

cat("Treated counties (corrected):", nrow(cmhc_openings), "\n")
print(table(cmhc_openings$cmhc_year_exp))

data <- fst::read_fst('aer_data/aer_data.fst') %>% as_tibble()
data <- data %>% left_join(cmhc_openings, by = "fips")

# ==========================================
# STEP 2: Sample restrictions
# ==========================================

# Drop NY/LA/Chicago
data <- data %>%
  filter(!(stfips == 36 & cofips == 61),
         !(stfips == 6 & cofips == 37),
         !(stfips == 17 & cofips == 31))

# Keep years up to 1988 (start from 1959 like the main analysis)
data <- data %>% filter(year <= 1988)

cat("\nSample:", nrow(data), "obs,", n_distinct(data$fips), "counties,",
    min(data$year), "-", max(data$year), "\n")

# ==========================================
# STEP 3: Treatment and event-time variables
# ==========================================

# Following main analysis: restrict to 1971-1975 cohorts only
data <- data %>%
  mutate(
    cmhc_year_use = ifelse(!is.na(cmhc_year_exp) & cmhc_year_exp <= 1975,
                           cmhc_year_exp, NA_real_),
    event_time = year - cmhc_year_use,
    event_time_binned = case_when(
      is.na(event_time) ~ -999L,
      event_time <= -6  ~ -6L,
      event_time >= 20  ~ 20L,
      TRUE ~ as.integer(event_time)
    ),
    cmhc_post = as.integer(!is.na(cmhc_year_use) & year >= cmhc_year_use),
    ever_cmhc = as.integer(!is.na(cmhc_year_use))
  )

n_treated <- n_distinct(data$fips[data$ever_cmhc == 1])
n_control <- n_distinct(data$fips[data$ever_cmhc == 0])
cat("Treated (1971-1975 only):", n_treated,
    " | Control:", n_control, "\n")

# ==========================================
# STEP 4: Urban categories
# ==========================================

data <- data %>%
  mutate(Durb = cut(`_60pcturban`,
                    breaks = c(-Inf, 1, 25, 50, 75, Inf),
                    labels = c("0", "1-25", "25-50", "50-75", "75+"),
                    include.lowest = TRUE))

# ==========================================
# STEP 5: 1960 baseline population weights
# ==========================================

data <- data %>%
  group_by(fips) %>%
  mutate(
    popwt    = sum(copop    * (year == 1960), na.rm = TRUE),
    popwt_ad = sum(copop_ad * (year == 1960), na.rm = TRUE),
    popwt_eld = sum(copop_eld * (year == 1960), na.rm = TRUE),
    popwt_ch = sum(copop_ch * (year == 1960), na.rm = TRUE)
  ) %>%
  ungroup()

# ============================================================================
# STATIC DiD ESTIMATES (linear TWFE)
# ============================================================================

cat("\n============================================================\n")
cat("STATIC DiD: Linear TWFE (feols)\n")
cat("  FE: county + state-by-year + urban-by-year\n")
cat("  Controls: D_tot_act_md_t, H_bpc, _60pcturban, _pct59inclt3k, _60pctnonwhit\n")
cat("  Weights: 1960 population\n")
cat("  Treated: 1971-1975 cohorts only\n")
cat("============================================================\n\n")

fe_str <- "fips + year^Durb + year^stfips"
ctl_str <- "D_tot_act_md_t + H_bpc + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`"

run_static_lin <- function(dv, d, wt, label) {
  fml <- as.formula(paste(dv, "~ cmhc_post +", ctl_str, "|", fe_str))
  mod <- tryCatch(
    feols(fml, data = d, weights = as.formula(paste0("~", wt)), cluster = ~fips),
    error = function(e) { cat(sprintf("  %-35s  FAILED: %s\n", label, e$message)); return(NULL) }
  )
  if (is.null(mod)) return(invisible(NULL))
  b  <- coef(mod)["cmhc_post"]
  se <- sqrt(vcov(mod)["cmhc_post","cmhc_post"])
  stars <- ifelse(abs(b/se) > 2.576, "***",
           ifelse(abs(b/se) > 1.96, "**",
           ifelse(abs(b/se) > 1.645, "*", "")))
  cat(sprintf("  %-35s  %9.3f (%7.3f) %s\n", label, b, se, stars))
  invisible(mod)
}

cat("All-cause AMR:\n")
mod_all  <- run_static_lin("amr",     data, "popwt",     "AMR (all ages)")
mod_ad   <- run_static_lin("amr_ad",  data, "popwt_ad",  "AMR Adults (20-49)")
mod_eld  <- run_static_lin("amr_eld", data, "popwt_eld", "AMR Elderly (50+)")
mod_ch   <- run_static_lin("amr_ch",  data, "popwt_ch",  "AMR Children (1-19)")

cat("\nRace-specific (elderly only):\n")
nw_data <- data %>% filter(!is.na(amr_nw_eld) & copop_nw_eld > 0 & amr_nw_eld > 0)
w_data  <- data %>% filter(!is.na(amr_w_eld) & copop_w_eld > 0 & amr_w_eld > 0)

# Use 1960 weights for race groups too
nw_data <- nw_data %>%
  group_by(fips) %>%
  mutate(popwt_nw_eld = sum(copop_nw_eld * (year == 1960), na.rm = TRUE)) %>%
  ungroup() %>% filter(popwt_nw_eld > 0)

w_data <- w_data %>%
  group_by(fips) %>%
  mutate(popwt_w_eld = sum(copop_w_eld * (year == 1960), na.rm = TRUE)) %>%
  ungroup() %>% filter(popwt_w_eld > 0)

mod_nw <- run_static_lin("amr_nw_eld", nw_data, "popwt_nw_eld", "Nonwhite Elderly AMR (50+)")
mod_w  <- run_static_lin("amr_w_eld",  w_data,  "popwt_w_eld",  "White Elderly AMR (50+)")

cat("\nCause-specific (Adults 20-49):\n")
cause_labs <- c("2"="CVD", "3"="Cerebrovascular", "4"="Cancer",
                "5"="Infectious", "6"="Diabetes", "7"="Accidents")
for (c_i in c(2,3,4,5,6,7)) {
  vn <- paste0("amr_ad_", c_i)
  d_sub <- data %>% filter(.data[[vn]] > 0)
  run_static_lin(vn, d_sub, "popwt_ad", paste0("  ", cause_labs[as.character(c_i)]))
}

# ============================================================================
# EVENT STUDY: 1-year bins, linear TWFE
# ============================================================================

cat("\n============================================================\n")
cat("EVENT STUDY: 1-year bins, linear TWFE\n")
cat("============================================================\n")

run_es_lin <- function(dv, d, wt, label, trim_at = 14) {
  fml <- as.formula(paste(dv, "~ i(event_time_binned, ref = -1) +",
                          ctl_str, "|", fe_str))
  mod <- tryCatch(
    feols(fml, data = d, weights = as.formula(paste0("~", wt)), cluster = ~fips),
    error = function(e) { cat(sprintf("  %s: FAILED\n", label)); return(NULL) }
  )
  if (is.null(mod)) return(NULL)

  # Extract event-time coefficients
  cn <- names(coef(mod))
  idx <- grepl("^event_time_binned::(-?[0-9]+)$", cn)
  es <- data.frame(
    event_time = as.numeric(gsub("^event_time_binned::(-?[0-9]+)$", "\\1", cn[idx])),
    coefficient = coef(mod)[idx],
    se = sqrt(diag(vcov(mod))[idx])
  ) %>%
    filter(event_time != -999 & event_time <= trim_at)

  es$ci_lower <- es$coefficient - 1.96 * es$se
  es$ci_upper <- es$coefficient + 1.96 * es$se
  es <- rbind(es, data.frame(event_time = -1, coefficient = 0, se = 0,
                              ci_lower = 0, ci_upper = 0))
  es <- es %>% arrange(event_time)
  es$panel <- label
  rownames(es) <- NULL

  cat(sprintf("\n%s:\n", label))
  cat(sprintf("  %-6s %9s %8s\n", "t", "Coef", "SE"))
  for (i in 1:nrow(es)) {
    stars <- ifelse(abs(es$coefficient[i]/es$se[i]) > 2.576, "***",
             ifelse(abs(es$coefficient[i]/es$se[i]) > 1.96, "**",
             ifelse(abs(es$coefficient[i]/es$se[i]) > 1.645, "*", "")))
    cat(sprintf("  %+3d    %9.3f (%7.3f) %s\n",
                es$event_time[i], es$coefficient[i], es$se[i], stars))
  }

  invisible(es)
}

es_all <- run_es_lin("amr",     data, "popwt",     "AMR (all ages)")
es_ad  <- run_es_lin("amr_ad",  data, "popwt_ad",  "AMR Adults (20-49)")
es_eld <- run_es_lin("amr_eld", data, "popwt_eld", "AMR Elderly (50+)")
es_ch  <- run_es_lin("amr_ch",  data, "popwt_ch",  "AMR Children (1-19)")

es_nw <- run_es_lin("amr_nw_eld", nw_data, "popwt_nw_eld", "Nonwhite Elderly (50+)")
es_w  <- run_es_lin("amr_w_eld",  w_data,  "popwt_w_eld",  "White Elderly (50+)")

# ============================================================================
# VERIFICATION: Negative post-period for adults < 50
# ============================================================================

cat("\n============================================================\n")
cat("VERIFICATION: Post-period effects for Adults (20-49)\n")
cat("============================================================\n\n")

if (!is.null(es_ad)) {
  post <- es_ad %>% filter(event_time >= 0)
  pre  <- es_ad %>% filter(event_time < -1)

  cat("Post-treatment coefficients (t >= 0):\n")
  cat(sprintf("  Mean:   %9.3f\n", mean(post$coefficient)))
  cat(sprintf("  Min:    %9.3f (t = %d)\n", min(post$coefficient),
              post$event_time[which.min(post$coefficient)]))
  cat(sprintf("  Max:    %9.3f (t = %d)\n", max(post$coefficient),
              post$event_time[which.max(post$coefficient)]))
  cat(sprintf("  # negative: %d of %d\n", sum(post$coefficient < 0), nrow(post)))
  cat(sprintf("  # significant (p<0.10): %d\n",
              sum(abs(post$coefficient / post$se) > 1.645)))

  cat("\nPre-treatment coefficients (t < -1):\n")
  cat(sprintf("  Mean:   %9.3f\n", mean(pre$coefficient)))
  cat(sprintf("  Min:    %9.3f\n", min(pre$coefficient)))
  cat(sprintf("  Max:    %9.3f\n", max(pre$coefficient)))
}

# ============================================================================
# PLOTS
# ============================================================================

# --- Adults vs Elderly event study (like cmhc_age_heterogeneity.png) ---
if (!is.null(es_ad) && !is.null(es_eld)) {
  es_age <- bind_rows(
    es_ad %>% mutate(panel = "Adults (20-49)"),
    es_eld %>% mutate(panel = "Elderly (50+)")
  )

  p_age <- ggplot(es_age, aes(x = event_time, y = coefficient,
                               color = panel, fill = panel)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    geom_point(size = 1.5) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(values = c("Adults (20-49)" = "darkred",
                                   "Elderly (50+)" = "steelblue")) +
    scale_fill_manual(values = c("Adults (20-49)" = "darkred",
                                  "Elderly (50+)" = "steelblue")) +
    labs(
      title = "CMHC Effects on Mortality by Age Group (Linear TWFE)",
      subtitle = "1-year bins, state-by-year FE, 1960 pop weights, 1971-1975 cohorts",
      x = "Years Relative to CMHC Opening",
      y = "Change in Deaths per 100,000",
      color = "Age Group", fill = "Age Group"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  ggsave("cmhc_replication_linear_age_het.png", p_age,
         width = 10, height = 6, dpi = 300)
  cat("\nPlot saved: cmhc_replication_linear_age_het.png\n")
}

# --- Nonwhite vs White elderly ---
if (!is.null(es_nw) && !is.null(es_w)) {
  es_race <- bind_rows(
    es_nw %>% mutate(panel = "Nonwhite Elderly (50+)"),
    es_w  %>% mutate(panel = "White Elderly (50+)")
  )

  p_race <- ggplot(es_race, aes(x = event_time, y = coefficient,
                                 color = panel, fill = panel)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    geom_point(size = 1.5) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(values = c("Nonwhite Elderly (50+)" = "darkred",
                                   "White Elderly (50+)" = "steelblue")) +
    scale_fill_manual(values = c("Nonwhite Elderly (50+)" = "darkred",
                                  "White Elderly (50+)" = "steelblue")) +
    labs(
      title = "CMHC Effects on Mortality by Race (Linear TWFE, Elderly 50+)",
      subtitle = "1-year bins, state-by-year FE, 1960 pop weights, 1971-1975 cohorts",
      x = "Years Relative to CMHC Opening",
      y = "Change in Deaths per 100,000",
      color = "Group", fill = "Group"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  ggsave("cmhc_replication_linear_race.png", p_race,
         width = 10, height = 6, dpi = 300)
  cat("Plot saved: cmhc_replication_linear_race.png\n")
}

# --- 3-panel event study (AMR all, Adults, Elderly) ---
es_3panel <- bind_rows(es_all, es_ad, es_eld) %>%
  mutate(panel = factor(panel, levels = c("AMR Adults (20-49)",
                                           "AMR Elderly (50+)",
                                           "AMR (all ages)")))

p_3panel <- ggplot(es_3panel, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
  geom_point(color = "navy", size = 1.5) +
  geom_line(color = "navy", linewidth = 0.6) +
  facet_wrap(~panel, scales = "free_y", ncol = 1) +
  labs(
    title = "CMHC Event Study: Linear TWFE",
    subtitle = "1-year bins, state-by-year FE, 1960 pop weights, 1971-1975 cohorts, ref = t-1",
    x = "Years Relative to CMHC Opening",
    y = "Change in AMR (Deaths per 100,000)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("cmhc_replication_linear_3panel.png", p_3panel,
       width = 9, height = 10, dpi = 300)
cat("Plot saved: cmhc_replication_linear_3panel.png\n")

# Save coefficients
write.csv(bind_rows(es_all, es_ad, es_eld, es_nw, es_w),
          "cmhc_replication_linear_coefs.csv", row.names = FALSE)
cat("Coefficients saved: cmhc_replication_linear_coefs.csv\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n============================================================\n")
cat("FINAL SUMMARY: Linear TWFE\n")
cat("============================================================\n\n")

cat("Specification: feols, county + state-by-year + urban-by-year FE\n")
cat("  Controls: D_tot_act_md_t, H_bpc, _60pcturban, _pct59inclt3k, _60pctnonwhit\n")
cat("  Weights: 1960 population\n")
cat("  Treatment: 1971-1975 CMHC cohorts only\n")
cat("  Sample: 1959-1988\n\n")

cat("Static DiD estimates (cmhc_post coefficient):\n")
for (m in list(list("AMR (all ages)", mod_all),
               list("AMR Adults (20-49)", mod_ad),
               list("AMR Elderly (50+)", mod_eld),
               list("AMR Children (1-19)", mod_ch),
               list("NW Elderly AMR", mod_nw),
               list("White Elderly AMR", mod_w))) {
  if (is.null(m[[2]])) next
  b  <- coef(m[[2]])["cmhc_post"]
  se <- sqrt(vcov(m[[2]])["cmhc_post","cmhc_post"])
  stars <- ifelse(abs(b/se)>2.576,"***", ifelse(abs(b/se)>1.96,"**",
           ifelse(abs(b/se)>1.645,"*","")))
  cat(sprintf("  %-30s  %9.3f (%7.3f) %s\n", m[[1]], b, se, stars))
}

cat("\n============================================================\n")
cat("DONE\n")
cat("============================================================\n")
