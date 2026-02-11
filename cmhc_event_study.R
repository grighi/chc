#!/usr/bin/env Rscript
# CMHC Event Study: Replicating Bailey & Goodman-Bacon (2015) Figure 5 style
# Difference-in-Differences Event Study for Community Mental Health Centers
# Dependent variable: Age-Adjusted Mortality Rate (AMR)

library(haven)
library(dplyr)
library(readxl)
library(fixest)  # For fast fixed effects estimation
library(tidyr)

# ==========================================
# STEP 1: Load CMHC data from Excel files
# ==========================================

cmhc_openings <- readr::read_csv('cmhc_data/cmhc_openings.csv', show_col_types = FALSE)

cat("Found", nrow(cmhc_openings), "counties with CMHC openings\n")
cat("Year distribution:\n")
print(table(cmhc_openings$cmhc_year_exp))
cat("\n")

# ==========================================
# STEP 2: Load base data and merge
# ==========================================

cat("Loading base data...\n")

# read_dta("aer_data/aer_data.dta") |> unique() |> fst::write_fst('aer_data/aer_data.fst') 
data <- fst::read_fst('aer_data/aer_data.fst') %>% as_tibble()


# Merge CMHC openings
data <- data %>%
  left_join(cmhc_openings, by = "fips")

# Drop NY/LA/Chicago (as in original)
data <- data %>%
  filter(!(stfips == 36 & cofips == 61)) %>%  # NYC
  filter(!(stfips == 6 & cofips == 37)) %>%   # LA
  filter(!(stfips == 17 & cofips == 31))      # Chicago

# Keep data up to 1988 (as in original)
data <- data %>% filter(year <= 1988)

# ==========================================
# STEP 3: Create event-time variable
# ==========================================

# Filter to CMHCs that opened before 1976 (or never opened)
# If opened after 1976, treat as never-treated (set to NA)
data <- data %>%
  mutate(cmhc_year_exp = ifelse(!is.na(cmhc_year_exp) & cmhc_year_exp <= 1975, 
                                 cmhc_year_exp, NA))

# Create event time: year relative to CMHC opening
data <- data %>%
  mutate(event_time = year - cmhc_year_exp)

# Check event time distribution
cat("\nEvent time distribution (treated counties only):\n")
print(table(data$event_time, useNA = "ifany"))

# ==========================================
# STEP 4: Create urban categories for urban-by-year FE
# ==========================================

# Create urban quartiles (following original code)
data <- data %>%
  group_by(fips) %>%
  mutate(urb60 = sum(`_60pcturban` * (year == 1960), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Durb = cut(urb60, breaks = c(-Inf, 1, 25, 50, 75, Inf), 
                    labels = c("0", "1-25", "25-50", "50-75", "75+"),
                    include.lowest = TRUE))

# ==========================================
# STEP 5: Prepare event-time indicators
# ==========================================

# Bin event time at endpoints: -6 or less, -5 to +20, 21+
# Omit -1 as reference period
data <- data %>%
  mutate(
    event_time_binned = case_when(
      is.na(event_time) ~ NA_real_,          # Never treated
      event_time <= -6 ~ -6,                  # Bin at -6
      event_time >= 20 ~ 20,                  # Bin at +20
      TRUE ~ event_time
    )
  )

# Create factor, omitting -1 as reference
data <- data %>%
  mutate(event_time_factor = factor(event_time_binned))

# ==========================================
# STEP 6: Population weights from 1960
# ==========================================

# Get 1960 population weight
data <- data %>%
  group_by(fips) %>%
  mutate(popwt = sum(copop * (year == 1960), na.rm = TRUE)) %>%
  ungroup()


# ==========================================
# STEP 7: Run Event Study Regression
# ==========================================

cat("\n==========================================================\n")
cat("CMHC EVENT STUDY: Effect on Age-Adjusted Mortality Rate\n")
cat("==========================================================\n")
cat("Specification: County FE + Year FE + Urban-by-Year FE + State-by-Year FE\n")
cat("Reference period: t = -1 (year before CMHC opening)\n")
cat("CMHCs that opened 1971-1975 only\n\n")

# Add event time = -999 for never-treated
data <- data %>%
  mutate(event_time_binned = if_else(is.na(event_time_binned), -999L, event_time_binned))

# Check if fixest is available, otherwise use lfe or base R
if (!requireNamespace("fixest", quietly = TRUE)) {
  stop("Please install the 'fixest' package: install.packages('fixest')")
}

# Run the event study with fixest
# sunab() handles the event study with never-treated as control
# Alternatively, use i() for manual event-time dummies

# Method 1: Using i() with event_time_factor
# County FE, Year FE, Urban-by-Year FE, State-by-Year FE
model_es <- feols(
  amr ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
  data = data,
  weights = ~popwt,
  cluster = ~fips
)

# Print summary
cat("-----------------------------------------------------------\n")
cat("Event Study Results: AMR (All Ages)\n")
cat("-----------------------------------------------------------\n\n")
print(summary(model_es))

# ==========================================
# STEP 8: Extract and format coefficients
# ==========================================

# Extract coefficients
coef_names_es <- names(coef(model_es))
event_idx_es <- grepl("^event_time_binned::(-?[0-9]+)$", coef_names_es)
coef_df <- data.frame(
  event_time = as.numeric(gsub("^event_time_binned::(-?[0-9]+)$", "\\1", coef_names_es[event_idx_es])),
  coefficient = coef(model_es)[event_idx_es],
  se = sqrt(diag(vcov(model_es)))[event_idx_es]
) %>% 
    filter(event_time != -999)

coef_df$ci_lower <- coef_df$coefficient - 1.96 * coef_df$se
coef_df$ci_upper <- coef_df$coefficient + 1.96 * coef_df$se

# Add reference period (t = -1)
coef_df <- rbind(coef_df, data.frame(event_time = -1, coefficient = 0, se = 0, ci_lower = 0, ci_upper = 0))
coef_df <- coef_df %>% arrange(event_time)

# Print table
cat("\n==========================================================\n")
cat("EVENT STUDY COEFFICIENTS TABLE\n")
cat("==========================================================\n")
cat("Dependent Variable: AMR (Age-Adjusted Mortality Rate)\n")
cat("Treatment: CMHC opening (1971-1975 cohorts)\n")
cat("Reference period: t = -1\n\n")

print_table <- coef_df %>%
  mutate(
    Coefficient = sprintf("%.3f", coefficient),
    Std_Error = sprintf("%.3f", se),
    CI_95 = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
    Signif = case_when(
      abs(coefficient / se) > 2.576 ~ "***",
      abs(coefficient / se) > 1.96 ~ "**",
      abs(coefficient / se) > 1.645 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Event_Time = event_time, Coefficient, Std_Error, CI_95, Signif)

print(print_table, row.names = FALSE)

# ==========================================
# STEP 9: Save results
# ==========================================

# Save coefficients to CSV
write.csv(coef_df, "cmhc_event_study_coefficients.csv", row.names = FALSE)
cat("\n\nCoefficients saved to: cmhc_event_study_coefficients.csv\n")

# ==========================================
# STEP 10: Pre/Post tests
# ==========================================

cat("\n-----------------------------------------------------------\n")
cat("Joint Significance Tests\n")
cat("-----------------------------------------------------------\n")

# Pre-trend test (t = -6 to -2)
pre_periods <- paste0("event_time_binned::", -6:-2)
pre_periods <- pre_periods[pre_periods %in% names(coef(model_es))]
if (length(pre_periods) > 0) {
  pre_test <- wald(model_es, pre_periods)
  cat("\nPre-trend test (t = -6 to -2):\n")
  cat("  F-statistic:", pre_test$stat, "\n")
  cat("  p-value:", pre_test$p, "\n")
}

# Post-treatment test (t = 0 to +20)
post_periods <- paste0("^event_time_binned::", 0:17, "$")
# post_periods <- post_periods[post_periods %in% names(coef(model_es))]
if (length(post_periods) > 0) {
  post_test <- wald(model_es, post_periods)
  cat("\nPost-treatment test (t = 0 to +20):\n")
  cat("  F-statistic:", post_test$stat, "\n")
  cat("  p-value:", post_test$p, "\n")
}

# ==========================================
# STEP 11: Heterogeneity by Age Groups
# ==========================================

cat("\n\n==========================================================\n")
cat("EVENT STUDY: AMR for Elderly (50+)\n")
cat("==========================================================\n\n")

# Get 1960 population weight for elderly
data <- data %>%
  group_by(fips) %>%
  mutate(popwt_eld = sum(copop_eld * (year == 1960), na.rm = TRUE)) %>%
  ungroup()

# model_es_eld <- feols(
#   jail_rate ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
#   data = data %>% filter(jail_bal_sample == 1),
#   weights = ~popwt_eld,
#   cluster = ~fips
# )

model_es_eld <- feols(
  amr_eld ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
  data = data,
  weights = ~popwt_eld,
  cluster = ~fips
)

print(summary(model_es_eld))

# Extract coefficients for elderly
coef_names_eld <- names(coef(model_es_eld))
event_idx_eld <- grepl("^event_time_binned::(-?[0-9]+)$", coef_names_eld)
coef_df_eld <- data.frame(
  event_time = as.numeric(gsub("^event_time_binned::(-?[0-9]+)$", "\\1", coef_names_eld[event_idx_eld])),
  coefficient = coef(model_es_eld)[event_idx_eld],
  se = sqrt(diag(vcov(model_es_eld)))[event_idx_eld]
) %>% 
  filter(event_time != -999)

coef_df_eld$ci_lower <- coef_df_eld$coefficient - 1.96 * coef_df_eld$se
coef_df_eld$ci_upper <- coef_df_eld$coefficient + 1.96 * coef_df_eld$se
coef_df_eld <- rbind(coef_df_eld, data.frame(event_time = -1, coefficient = 0, se = 0, ci_lower = 0, ci_upper = 0))
coef_df_eld <- coef_df_eld %>% arrange(event_time)

cat("\n-----------------------------------------------------------\n")
cat("EVENT STUDY COEFFICIENTS TABLE: AMR Age 50+\n")
cat("-----------------------------------------------------------\n\n")

print_table_eld <- coef_df_eld %>%
  mutate(
    Coefficient = sprintf("%.3f", coefficient),
    Std_Error = sprintf("%.3f", se),
    CI_95 = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
    Signif = case_when(
      abs(coefficient / se) > 2.576 ~ "***",
      abs(coefficient / se) > 1.96 ~ "**",
      abs(coefficient / se) > 1.645 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Event_Time = event_time, Coefficient, Std_Error, CI_95, Signif)

print(print_table_eld, row.names = FALSE)

write.csv(coef_df_eld, "cmhc_event_study_coefficients_eld.csv", row.names = FALSE)
cat("\n\nElderly coefficients saved to: cmhc_event_study_coefficients_eld.csv\n")


cat("\n\n==========================================================\n")
cat("EVENT STUDY: AMR for Adults (20-49)\n")
cat("==========================================================\n\n")

# Add event time = -999 for never-treated
data <- data %>%
  mutate(event_time_binned = if_else(is.na(event_time_binned), -999L, event_time_binned))

# Get 1960 population weight for elderly
data <- data %>%
  group_by(fips) %>%
  mutate(popwt_ad = sum(copop_ad * (year == 1960), na.rm = TRUE)) %>%
  ungroup()

# model_es_ad <- feols(
#   jail_rate ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
#   data = data %>% filter(jail_bal_sample == 1),
#   weights = ~popwt_ad,
#   cluster = ~fips
# )

model_es_ad <- feols(
  amr_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
  data = data,
  weights = ~popwt_ad,
  cluster = ~fips
)

print(summary(model_es_ad))

# Extract coefficients for adults
coef_names_ad <- names(coef(model_es_ad))
event_idx_ad <- grepl("^event_time_binned::(-?[0-9]+)$", coef_names_ad)
coef_df_ad <- data.frame(
  event_time = as.numeric(gsub("^event_time_binned::(-?[0-9]+)$", "\\1", coef_names_ad[event_idx_ad])),
  coefficient = coef(model_es_ad)[event_idx_ad],
  se = sqrt(diag(vcov(model_es_ad)))[event_idx_ad]
) %>% 
  filter(event_time != -999)

coef_df_ad$ci_lower <- coef_df_ad$coefficient - 1.96 * coef_df_ad$se
coef_df_ad$ci_upper <- coef_df_ad$coefficient + 1.96 * coef_df_ad$se
coef_df_ad <- rbind(coef_df_ad, data.frame(event_time = -1, coefficient = 0, se = 0, ci_lower = 0, ci_upper = 0))
coef_df_ad <- coef_df_ad %>% arrange(event_time)

cat("\n-----------------------------------------------------------\n")
cat("EVENT STUDY COEFFICIENTS TABLE: AMR Age 20-49\n")
cat("-----------------------------------------------------------\n\n")

print_table_ad <- coef_df_ad %>%
  mutate(
    Coefficient = sprintf("%.3f", coefficient),
    Std_Error = sprintf("%.3f", se),
    CI_95 = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
    Signif = case_when(
      abs(coefficient / se) > 2.576 ~ "***",
      abs(coefficient / se) > 1.96 ~ "**",
      abs(coefficient / se) > 1.645 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Event_Time = event_time, Coefficient, Std_Error, CI_95, Signif)

print(print_table_ad, row.names = FALSE)

write.csv(coef_df_ad, "cmhc_event_study_coefficients_ad.csv", row.names = FALSE)
cat("\n\nElderly coefficients saved to: cmhc_event_study_coefficients_ad.csv\n")



# ==========================================
# STEP 12: Plot Event Study Coefficients
# ==========================================

library(ggplot2)

# Plot for AMR (all ages)
p_amr <- ggplot(coef_df, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
  geom_point(color = "steelblue", size = 2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Effect of CMHC Opening on Age-Adjusted Mortality Rate",
    subtitle = "Event study coefficients with 95% CI, reference period t = -1",
    x = "Years Relative to CMHC Opening",
    y = "Change in Deaths per 100,000"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("cmhc_event_study_amr.png", p_amr, width = 10, height = 6, dpi = 300)
cat("\nPlot saved to: cmhc_event_study_amr.png\n")

# Plot for AMR Age 50+ (elderly)
p_eld <- ggplot(coef_df_eld, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "darkred", alpha = 0.2) +
  geom_point(color = "darkred", size = 2) +
  geom_line(color = "darkred", linewidth = 0.8) +
  labs(
    title = "Effect of CMHC Opening on Mortality Rate (Age 50+)",
    subtitle = "Event study coefficients with 95% CI, reference period t = -1",
    x = "Years Relative to CMHC Opening",
    y = "Change in Deaths per 100,000"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("cmhc_event_study_amr_eld.png", p_eld, width = 10, height = 6, dpi = 300)
cat("Plot saved to: cmhc_event_study_amr_eld.png\n")

# Plot for AMR Age 20-49 (adults)
p_ad <- ggplot(coef_df_ad, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "darkred", alpha = 0.2) +
  geom_point(color = "darkred", size = 2) +
  geom_line(color = "darkred", linewidth = 0.8) +
  labs(
    title = "Effect of CMHC Opening on Mortality Rate (Age 20-49)",
    subtitle = "Event study coefficients with 95% CI, reference period t = -1",
    x = "Years Relative to CMHC Opening",
    y = "Change in Deaths per 100,000"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("cmhc_event_study_amr_ad.png", p_ad, width = 10, height = 6, dpi = 300)
cat("Plot saved to: cmhc_event_study_amr_ad.png\n")

# Display plots
# print(p_amr)
# print(p_eld)
# print(p_ad)



# ==========================================
# STEP 12b: Figure 4 — Pre-program AMR vs CMHC establishment timing
# Scatter plots: 1960 AMR levels and 1959–1960 changes (ages 20–49)
# against CMHC establishment year. Tests that timing is as-good-as-random.
# ==========================================

cat("\n==========================================================\n")
cat("FIGURE 4: Pre-Program Mortality vs CMHC Establishment Timing\n")
cat("==========================================================\n\n")

# Build cross-section of treated counties with pre-program AMR
fig4_data <- data %>%
  filter(!is.na(cmhc_year_exp)) %>%
  filter(year %in% c(1959, 1960)) %>%
  select(fips, year, cmhc_year_exp, amr_ad, copop_ad, popwt_ad,
         `_60pcturban`, `_60pctnonwhit`, `_pct59inclt3k`) %>%
  pivot_wider(names_from = year, values_from = c(amr_ad, copop_ad),
              names_sep = "_") %>%
  mutate(
    amr_ad_level_1960 = amr_ad_1960,
    amr_ad_change_59_60 = amr_ad_1960 - amr_ad_1959
  ) %>%
  filter(!is.na(amr_ad_level_1960))

cat("Counties with valid 1960 AMR (20-49):", nrow(fig4_data), "\n")
cat("Counties with valid 1959-1960 change:", sum(!is.na(fig4_data$amr_ad_change_59_60)), "\n")
cat("CMHC year range:", min(fig4_data$cmhc_year_exp), "-", max(fig4_data$cmhc_year_exp), "\n\n")

# --- Panel A: 1960 AMR level vs CMHC year ---
# OLS fit for reference line
fit_level <- lm(amr_ad_level_1960 ~ cmhc_year_exp, data = fig4_data, weights = popwt_ad)
cat("Panel A — 1960 AMR level regressed on CMHC year:\n")
cat(sprintf("  Slope: %.3f (SE: %.3f, p = %.3f)\n",
            coef(fit_level)[2],
            summary(fit_level)$coefficients[2, 2],
            summary(fit_level)$coefficients[2, 4]))

p_fig4a <- ggplot(fig4_data, aes(x = cmhc_year_exp, y = amr_ad_level_1960)) +
  geom_point(aes(size = popwt_ad), alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_size_continuous(range = c(0.5, 4), guide = "none") +
  labs(
    title = "A. 1960 AMR (Ages 20\u201349) vs. CMHC Establishment Year",
    x = "CMHC Establishment Year",
    y = "Age-Adjusted Mortality Rate (20\u201349), 1960"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

# --- Panel B: 1959–1960 AMR change vs CMHC year ---
fig4_data_change <- fig4_data %>% filter(!is.na(amr_ad_change_59_60))

fit_change <- lm(amr_ad_change_59_60 ~ cmhc_year_exp, data = fig4_data_change, weights = popwt_ad)
cat("\nPanel B — 1959-1960 AMR change regressed on CMHC year:\n")
cat(sprintf("  Slope: %.3f (SE: %.3f, p = %.3f)\n",
            coef(fit_change)[2],
            summary(fit_change)$coefficients[2, 2],
            summary(fit_change)$coefficients[2, 4]))

p_fig4b <- ggplot(fig4_data_change, aes(x = cmhc_year_exp, y = amr_ad_change_59_60)) +
  geom_point(aes(size = popwt_ad), alpha = 0.4, color = "darkred") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_size_continuous(range = c(0.5, 4), guide = "none") +
  labs(
    title = "B. Change in AMR (Ages 20\u201349), 1959\u20131960, vs. CMHC Establishment Year",
    x = "CMHC Establishment Year",
    y = "\u0394 AMR (20\u201349), 1959\u20131960"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

# --- Combined figure ---
library(patchwork)

p_fig4 <- p_fig4a / p_fig4b +
  plot_annotation(
    title = "Fig 4. No Relationship Between CMHC Establishment Timing and Pre-Program Mortality",
    subtitle = "Scatter plots of pre-program AMR (ages 20\u201349) against CMHC establishment year.\nPoint size proportional to 1960 population. Lines are population-weighted OLS fits.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "gray30")
    )
  )

ggsave("cmhc_fig4_timing_exogeneity.png", p_fig4, width = 9, height = 10, dpi = 300)
cat("\nFigure 4 saved to: cmhc_fig4_timing_exogeneity.png\n")

# --- Robustness: conditional on controls (residualized) ---
cat("\n--- Robustness: Residualized scatter (conditional on controls) ---\n")

fig4_resid <- fig4_data %>%
  filter(!is.na(`_60pcturban`) & !is.na(`_60pctnonwhit`) & !is.na(`_pct59inclt3k`))

if (nrow(fig4_resid) > 20) {
  # Residualize AMR level on controls
  resid_y <- lm(amr_ad_level_1960 ~ `_60pcturban` + `_60pctnonwhit` + `_pct59inclt3k`,
                 data = fig4_resid, weights = popwt_ad)
  resid_x <- lm(cmhc_year_exp ~ `_60pcturban` + `_60pctnonwhit` + `_pct59inclt3k`,
                 data = fig4_resid, weights = popwt_ad)
  fig4_resid$amr_resid <- residuals(resid_y)
  fig4_resid$year_resid <- residuals(resid_x)

  fit_resid <- lm(amr_resid ~ year_resid, data = fig4_resid, weights = popwt_ad)
  cat(sprintf("  Residualized slope: %.3f (SE: %.3f, p = %.3f)\n",
              coef(fit_resid)[2],
              summary(fit_resid)$coefficients[2, 2],
              summary(fit_resid)$coefficients[2, 4]))

  p_fig4c <- ggplot(fig4_resid, aes(x = year_resid, y = amr_resid)) +
    geom_point(aes(size = popwt_ad), alpha = 0.4, color = "forestgreen") +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_size_continuous(range = c(0.5, 4), guide = "none") +
    labs(
      title = "Residualized: 1960 AMR vs. CMHC Year (conditional on % urban, % nonwhite, % poverty)",
      x = "CMHC Year (residualized)",
      y = "AMR 20\u201349, 1960 (residualized)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank()
    )

  ggsave("cmhc_fig4_timing_exogeneity_residualized.png", p_fig4c, width = 9, height = 6, dpi = 300)
  cat("Residualized scatter saved to: cmhc_fig4_timing_exogeneity_residualized.png\n")
}

cat("\n==========================================================\n")
cat("End of CMHC Event Study\n")
cat("==========================================================\n")


# ==========================================
# STEP 13: Crime Long-Difference Analysis (1970-1978)
# ==========================================


cat("Loading incarceration data...\n")

# Read incarceration trends data

# incarceration <- read.csv("incarceration_trends_county.csv") %>%
#   select(year, fips, total_jail_pop, black_jail_pop, 
#     total_prison_pop, black_prison_pop,
#     total_pop, total_pop_15to64, black_pop_15to64) %>%
#   mutate(across(everything(), ~replace_na(., 0))) %>%
#   mutate(total_inc_pop = total_jail_pop + total_prison_pop,
#          black_inc_pop = black_jail_pop + black_prison_pop,
#          inc_rate = total_inc_pop / total_pop_15to64,
#          black_inc_rate = black_inc_pop / black_pop_15to64) %>%
#   unique() %>%
#   group_by(fips, year) %>%
#   slice_max(order_by = inc_rate, n = 1) %>%
#   ungroup()
# fst::write_fst(incarceration, 'incarceration_trends_county.fst')

incarceration <- fst::read_fst('incarceration_trends_county.fst')

# Create jail rates
incarceration <- incarceration %>%
  mutate(
    jail_rate = total_jail_pop / total_pop_15to64,
    bl_jail_rate = black_jail_pop / black_pop_15to64
  ) %>%
  select(fips, year, jail_rate, bl_jail_rate, total_jail_pop, black_jail_pop,
         total_pop_15to64, black_pop_15to64, inc_rate, black_inc_rate)

# Create balanced sample indicator: 1 if county has jail_rate for all years in data
years_in_data <- 1965:1980
n_years_needed <- length(years_in_data)

jail_bal_flag <- incarceration %>%
  filter(year %in% years_in_data, !is.na(jail_rate)) %>%
  group_by(fips) %>%
  summarize(n_years_obs = n()) %>%
  mutate(jail_bal_sample = as.integer(n_years_obs >= 7)) %>%
  select(fips, jail_bal_sample) 

incarceration <- incarceration %>%
  left_join(jail_bal_flag, by = "fips") %>%
  mutate(jail_bal_sample = replace_na(jail_bal_sample, 0L))

cat("  Balanced jail sample counties:", sum(jail_bal_flag$jail_bal_sample == 1), "of", n_distinct(jail_bal_flag$fips), "\n")

# Merge with main data
data <- data %>%
  left_join(incarceration, by = c("fips", "year"))

cat("Incarceration data merged.\n")
cat("  Jail rate available for", sum(!is.na(data$jail_rate)), "observations\n")
cat("  Black jail rate available for", sum(!is.na(data$bl_jail_rate)), "observations\n")
cat("  Observations in balanced jail sample:", sum(data$jail_bal_sample == 1, na.rm = TRUE), "\n\n")


cat("\n\n==========================================================\n")
cat("LONG-DIFFERENCE ANALYSIS: Jail Incarceration Rate (1970-1978)\n")
cat("==========================================================\n\n")

library(lmtest)
library(sandwich)

# Reload CMHC openings to get full treatment timing (not just <=1975)
cmhc_openings_full <- cmhc_all %>%
  group_by(fips) %>%
  summarize(cmhc_year_full = min(cmhc_year, na.rm = TRUE)) %>%
  ungroup()

# Create long-difference dataset
# Get 1970 data
data_1970 <- data %>%
  filter(year == 1970) %>%
  select(fips, stfips, cofips, Durb,
         jail_rate_1970 = inc_rate,
         total_jail_pop_1970 = total_jail_pop,
         total_pop_15to64_1970 = total_pop_15to64,
         # Baseline controls
         `_60pcturban`, `_60pctrurf`, `_60pct04years`, `_60pctmt64years`,
         `_60pctnonwhit`, `_60pctmt12schl`, `_60pctlt4schl`,
         `_pct59inclt3k`, `_pct59incmt10k`, `_tot_act_md`,
         copop, popwt)

# Get 1978 data
data_1978 <- data %>%
  filter(year == 1978) %>%
  select(fips,
         jail_rate_1978 = inc_rate,
         total_jail_pop_1978 = total_jail_pop,
         total_pop_15to64_1978 = total_pop_15to64)

# Merge 1970 and 1978
ld_data <- data_1970 %>%
  inner_join(data_1978, by = "fips") %>%
  left_join(cmhc_openings_full, by = "fips")

# Compute log change in jail rate
ld_data <- ld_data %>%
  mutate(
    log_jail_rate_1970 = log(jail_rate_1970),
    log_jail_rate_1978 = log(jail_rate_1978),
    delta_log_jail_rate = log_jail_rate_1978 - log_jail_rate_1970,
    # Treatment: CMHC operating by 1978
    cmhc = as.integer(!is.na(cmhc_year_full) & cmhc_year_full <= 1978)
  )

# Filter to counties with valid outcome
ld_data_valid <- ld_data %>%
  filter(!is.na(delta_log_jail_rate) & is.finite(delta_log_jail_rate))

cat("Long-difference sample:\n")
cat("  Counties with valid Δlog(jail_rate):", nrow(ld_data_valid), "\n")
cat("  Treated (CMHC by 1978):", sum(ld_data_valid$cmhc == 1), "\n")
cat("  Control:", sum(ld_data_valid$cmhc == 0), "\n\n")

# ==========================================
# Summary Statistics by Treatment Status
# ==========================================

cat("-----------------------------------------------------------\n")
cat("Summary Statistics: Treated vs Control Counties\n")
cat("-----------------------------------------------------------\n\n")

summary_stats <- ld_data_valid %>%
  group_by(cmhc) %>%
  summarize(
    n_counties = n(),
    mean_delta_log_jail = mean(delta_log_jail_rate, na.rm = TRUE),
    sd_delta_log_jail = sd(delta_log_jail_rate, na.rm = TRUE),
    mean_jail_rate_1970 = mean(jail_rate_1970, na.rm = TRUE),
    mean_jail_rate_1978 = mean(jail_rate_1978, na.rm = TRUE),
    mean_pct_urban = mean(`_60pcturban`, na.rm = TRUE),
    mean_pct_nonwhite = mean(`_60pctnonwhit`, na.rm = TRUE),
    mean_pct_poverty = mean(`_pct59inclt3k`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(cmhc == 1, "Treated (CMHC by 1978)", "Control"))

cat("                              Control     Treated\n")
cat("-----------------------------------------------------------\n")
cat(sprintf("N counties                    %7d     %7d\n", 
            summary_stats$n_counties[summary_stats$cmhc == 0],
            summary_stats$n_counties[summary_stats$cmhc == 1]))
cat(sprintf("Mean Δlog(jail_rate)          %7.3f     %7.3f\n",
            summary_stats$mean_delta_log_jail[summary_stats$cmhc == 0],
            summary_stats$mean_delta_log_jail[summary_stats$cmhc == 1]))
cat(sprintf("SD Δlog(jail_rate)            %7.3f     %7.3f\n",
            summary_stats$sd_delta_log_jail[summary_stats$cmhc == 0],
            summary_stats$sd_delta_log_jail[summary_stats$cmhc == 1]))
cat(sprintf("Mean jail rate 1970           %7.4f     %7.4f\n",
            summary_stats$mean_jail_rate_1970[summary_stats$cmhc == 0],
            summary_stats$mean_jail_rate_1970[summary_stats$cmhc == 1]))
cat(sprintf("Mean jail rate 1978           %7.4f     %7.4f\n",
            summary_stats$mean_jail_rate_1978[summary_stats$cmhc == 0],
            summary_stats$mean_jail_rate_1978[summary_stats$cmhc == 1]))
cat(sprintf("Mean %% urban (1960)           %7.1f     %7.1f\n",
            summary_stats$mean_pct_urban[summary_stats$cmhc == 0],
            summary_stats$mean_pct_urban[summary_stats$cmhc == 1]))
cat(sprintf("Mean %% nonwhite (1960)        %7.1f     %7.1f\n",
            summary_stats$mean_pct_nonwhite[summary_stats$cmhc == 0],
            summary_stats$mean_pct_nonwhite[summary_stats$cmhc == 1]))
cat(sprintf("Mean %% poverty (1959)         %7.1f     %7.1f\n",
            summary_stats$mean_pct_poverty[summary_stats$cmhc == 0],
            summary_stats$mean_pct_poverty[summary_stats$cmhc == 1]))
cat("\n")

# ==========================================
# OLS Regression with Robust SEs
# ==========================================

ld_data_valid <- ld_data_valid %>%
  mutate(log_total_pop_15to64_1970 = log(total_pop_15to64_1970))

ld_data_valid_small <- ld_data_valid %>%
  filter(log_total_pop_15to64_1970 < log(30000)) %>%
  group_by(stfips) %>%
  filter(n() > 1) %>%
  ungroup()

ld_data_valid_large <- ld_data_valid %>%
  filter(log_total_pop_15to64_1970 > log(30000)) %>%
  group_by(stfips) %>%
  filter(n() > 1) %>%
  ungroup()


# Model 1: No controls
model_ld1 <- lm(delta_log_jail_rate ~ cmhc, 
                # weights = popwt,
                data = ld_data_valid_small)
robust_se1 <- sqrt(diag(vcovHC(model_ld1, type = "HC1")))

# Model 2: With baseline controls
model_ld2 <- lm(delta_log_jail_rate ~ cmhc + `_60pcturban` + `_60pctrurf` + 
                  `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` + 
                  `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` + 
                  `_pct59incmt10k` + `_tot_act_md`,
                # weights = popwt,
                data = ld_data_valid_small)
robust_se2 <- sqrt(diag(vcovHC(model_ld2, type = "HC1")))

# Model 3: With controls + state FE
## consider adding: cmhc*log(total_pop_15to64_1970)
model_ld3 <- lm(delta_log_jail_rate ~ cmhc + `_60pcturban` + `_60pctrurf` + 
                  `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` + 
                  `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` + 
                  `_pct59incmt10k` + `_tot_act_md` + factor(stfips),
                weights = popwt,
                data = ld_data_valid_small)
robust_se3 <- sqrt(diag(vcovHC(model_ld3, type = "HC1")))

model_ld3b <- lm(delta_log_jail_rate ~ cmhc + `_60pcturban` + `_60pctrurf` + 
                  `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` + 
                  `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` + 
                  `_pct59incmt10k` + `_tot_act_md` + factor(stfips),
                weights = popwt,
                data = ld_data_valid_large)
robust_se3b <- sqrt(diag(vcovHC(model_ld3b, type = "HC1")))


# Model 4: Interaction with log total population 15-64 in 1970
model_4 <- feols(
  delta_log_jail_rate ~ cmhc * log_total_pop_15to64_1970 + `_60pcturban` + `_60pctrurf` + 
                  `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` + 
                  `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` + 
                  `_pct59incmt10k` + `_tot_act_md` | stfips,
  data = ld_data_valid,
  weights = ~popwt,
  cluster = ~fips
)
marginaleffects::plot_slopes(model_4, variables = "cmhc", condition = "log_total_pop_15to64_1970")

# Full coefficient table for Model 3
cat("\n-----------------------------------------------------------\n")
cat("Full Coefficient Table (Model 3a) except stfips FEs\n")
cat("-----------------------------------------------------------\n\n")

coef_table_ld <- data.frame(
  Variable = names(coef(model_ld3)),
  Coefficient = coef(model_ld3),
  Robust_SE = robust_se3,
  t_value = coef(model_ld3) / robust_se3,
  p_value = 2 * pt(-abs(coef(model_ld3) / robust_se3), df = model_ld3$df.residual)
) %>% filter(!grepl("stfips", rownames(.)))

coef_table_ld$Signif <- case_when(
  coef_table_ld$p_value < 0.01 ~ "***",
  coef_table_ld$p_value < 0.05 ~ "**",
  coef_table_ld$p_value < 0.10 ~ "*",
  TRUE ~ ""
)
rownames(coef_table_ld) <- NULL

# Rename cmhc for clarity
coef_table_ld$Variable[coef_table_ld$Variable == "cmhc"] <- "CMHC exposure (1970-78)"

print(coef_table_ld, digits = 4)


cat("\n\n-----------------------------------------------------------\n")
cat("OLS Regression: Δlog(jail_rate) on CMHC exposure\n")
cat("-----------------------------------------------------------\n\n")
cat("Dependent Variable: Δlog(jail_rate) = log(jail_rate_1978) - log(jail_rate_1970)\n\n")

# Extract model_4 statistics
coef_m4 <- coef(model_4)["cmhc"]
se_m4 <- sqrt(diag(vcov(model_4)))["cmhc"]
t_m4 <- coef_m4 / se_m4
p_m4 <- 2 * pt(-abs(t_m4), df = nrow(ld_data_valid) - length(coef(model_4)))

cat("                                    (1)         (2)         (3a)        (3b)        (4)\n")
cat("                                No Controls  Controls   Small+StateFE Large+StateFE Interaction\n")
cat("-----------------------------------------------------------------------------------------------\n")
cat(sprintf("CMHC exposure (1970-78)         %8.4f    %8.4f    %8.4f    %8.4f    %8.4f\n",
            coef(model_ld1)["cmhc"], coef(model_ld2)["cmhc"], coef(model_ld3)["cmhc"], 
            coef(model_ld3b)["cmhc"], coef_m4))
cat(sprintf("  Robust SE                     (%6.4f)    (%6.4f)    (%6.4f)    (%6.4f)    (%6.4f)\n",
            robust_se1["cmhc"], robust_se2["cmhc"], robust_se3["cmhc"], 
            robust_se3b["cmhc"], se_m4))
cat(sprintf("  t-statistic                   %8.3f    %8.3f    %8.3f    %8.3f    %8.3f\n",
            coef(model_ld1)["cmhc"]/robust_se1["cmhc"],
            coef(model_ld2)["cmhc"]/robust_se2["cmhc"],
            coef(model_ld3)["cmhc"]/robust_se3["cmhc"],
            coef(model_ld3b)["cmhc"]/robust_se3b["cmhc"],
            t_m4))

# p-values
p1 <- 2 * pt(-abs(coef(model_ld1)["cmhc"]/robust_se1["cmhc"]), df = model_ld1$df.residual)
p2 <- 2 * pt(-abs(coef(model_ld2)["cmhc"]/robust_se2["cmhc"]), df = model_ld2$df.residual)
p3 <- 2 * pt(-abs(coef(model_ld3)["cmhc"]/robust_se3["cmhc"]), df = model_ld3$df.residual)
p3b <- 2 * pt(-abs(coef(model_ld3b)["cmhc"]/robust_se3b["cmhc"]), df = model_ld3b$df.residual)

cat(sprintf("  p-value                       %8.4f    %8.4f    %8.4f    %8.4f    %8.4f\n", 
            p1, p2, p3, p3b, p_m4))
cat("-----------------------------------------------------------------------------------------------\n")
cat(sprintf("Baseline controls                   No          Yes         Yes         Yes         Yes\n"))
cat(sprintf("State fixed effects                 No          No          Yes         Yes         Yes\n"))
cat(sprintf("CMHC x log(pop) interaction         No          No          No          No          Yes\n"))
cat(sprintf("Sample                              All         All         Small       Large       All\n"))
cat(sprintf("N counties                      %8d    %8d    %8d    %8d    %8d\n",
            nobs(model_ld1), nobs(model_ld2), nobs(model_ld3), 
            nobs(model_ld3b), nobs(model_4)))
cat(sprintf("R-squared                       %8.4f    %8.4f    %8.4f    %8.4f    %8.4f\n",
            summary(model_ld1)$r.squared, summary(model_ld2)$r.squared, 
            summary(model_ld3)$r.squared, summary(model_ld3b)$r.squared,
            r2(model_4, "r2")))
cat("-----------------------------------------------------------------------------------------------\n")
cat("Robust/clustered standard errors in parentheses\n")
cat("Small: log(pop_15to64_1970) < log(30000); Large: log(pop_15to64_1970) > log(30000)\n")
cat("Model (4): CMHC effect varies with county population (interaction term)\n")
cat("* p<0.10, ** p<0.05, *** p<0.01\n")


# Save results
write.csv(coef_table_ld, "cmhc_long_difference_results.csv", row.names = FALSE)
cat("\nLong-difference results saved to: cmhc_long_difference_results.csv\n")
