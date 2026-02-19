#!/usr/bin/env Rscript
# CMHC Event Study: Replicating Bailey & Goodman-Bacon (2015) Figure 5 style
# Difference-in-Differences Event Study for Community Mental Health Centers
# Dependent variable: Age-Adjusted Mortality Rate (AMR)

library(haven)
library(dplyr)
library(readxl)
library(fixest)  # For fast fixed effects estimation
library(tidyr)

baseline_use_controls <- TRUE

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
amr ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
  + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit` | fips + year^Durb + year^stfips,
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
    filter(event_time != -999 & event_time < 15)

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
  amr_eld ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc 
  + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit` | fips + year^Durb + year^stfips,
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
  filter(event_time != -999 & event_time < 15)

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
  amr_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc 
  + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit` | fips + year^Durb + year^stfips,
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
  filter(event_time != -999 & event_time < 15)

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

# Age heterogeneity plot: Adults vs Elderly side-by-side
coef_df_ad$age_group <- "Adults (20-49)"
coef_df_eld$age_group <- "Elderly (50+)"
coef_df_combined <- rbind(coef_df_ad, coef_df_eld)

p_age_het <- ggplot(coef_df_combined, aes(x = event_time, y = coefficient, 
                                           color = age_group, fill = age_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, 
              color = NA) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Adults (20-49)" = "darkred", "Elderly (50+)" = "steelblue")) +
  scale_fill_manual(values = c("Adults (20-49)" = "darkred", "Elderly (50+)" = "steelblue")) +
  labs(
    title = "CMHC Effects on Mortality by Age Group",
    subtitle = "Event study coefficients with 95% CI, reference period t = -1",
    x = "Years Relative to CMHC Opening",
    y = "Change in Deaths per 100,000",
    color = "Age Group",
    fill = "Age Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave("cmhc_age_heterogeneity.png", p_age_het, width = 10, height = 6, dpi = 300)
cat("Plot saved to: cmhc_age_heterogeneity.png\n")

# Display plots
# print(p_amr)
# print(p_eld)
# print(p_ad)
# print(p_age_het)



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
  select(fips, year, cmhc_year_exp, amr_ad, copop_ad, popwt_ad, `_tot_act_md`,
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
  resid_y <- lm(amr_ad_level_1960 ~ `_60pcturban` + `_tot_act_md`,
    # amr_ad_level_1960 ~ `_60pcturban` + `_60pctnonwhit` + `_pct59inclt3k`,
                 data = fig4_resid, weights = popwt_ad)
  resid_x <- lm(cmhc_year_exp ~ `_60pcturban` + `_tot_act_md`,
    # amr_ad_level_1960 ~ `_60pcturban` + `_60pctnonwhit` + `_pct59inclt3k`,
                 data = fig4_resid, weights = popwt_ad)
  fig4_resid$amr_resid <- residuals(resid_y)
  fig4_resid$year_resid <- residuals(resid_x) + resid_x$coef['(Intercept)']

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

# ==========================================
# STEP 12c: Robustness — CMHC effects are not driven by CHCs
# ==========================================
# Community Health Centers (CHCs) were rolling out in the same era.
# We show the CMHC effect is not confounded by CHC openings using:
#   (i)   Horse race: CMHC + CHC event-time indicators
#   (ii)  Control for CHC per-capita funding (pcrfund_chc)
#   (iii) Restrict sample to never-CHC counties

cat("\n\n##########################################################\n")
cat("ROBUSTNESS: CMHC Effects Are Not Driven by CHC Openings\n")
cat("##########################################################\n\n")

# --- Prepare CHC event-time variable ---
# chc_year_exp is already in the data from the original AER dataset
# Create CHC event time, analogous to CMHC event time

data <- data %>%
  mutate(
    chc_event_time = year - chc_year_exp,
    chc_event_time_binned = case_when(
      is.na(chc_event_time) ~ -999L,
      chc_event_time <= -6  ~ -6L,
      chc_event_time >= 20  ~ 20L,
      TRUE ~ as.integer(chc_event_time)
    )
  )

# Flag counties that EVER received a CHC grant
data <- data %>%
  group_by(fips) %>%
  mutate(ever_chc = as.integer(any(!is.na(chc_year_exp)))) %>%
  ungroup()

cat("CHC exposure summary:\n")
cat("  Counties with CHC grant:", n_distinct(data$fips[data$ever_chc == 1]), "\n")
cat("  Counties without CHC grant:", n_distinct(data$fips[data$ever_chc == 0]), "\n")
cat("  Counties with CMHC (treated):", n_distinct(data$fips[!is.na(data$cmhc_year_exp)]), "\n")
cat("  CMHC-treated counties that also got CHC:",
    n_distinct(data$fips[!is.na(data$cmhc_year_exp) & data$ever_chc == 1]), "\n\n")


# ==================================================================
# (i) Horse Race: CMHC + CHC event-time indicators (AMR 20-49)
# ==================================================================

cat("-----------------------------------------------------------\n")
cat("(i) Horse Race: CMHC + CHC Event-Time Indicators\n")
cat("    Dependent Variable: AMR (Ages 20-49)\n")
cat("-----------------------------------------------------------\n\n")

model_horse_ad <- feols(
  amr_ad ~ i(event_time_binned, ref = -1) + i(chc_event_time_binned, ref = -1) +
    D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
  data = data,
  weights = ~popwt_ad,
  cluster = ~fips
)

cat("CMHC coefficients (with CHC event-time controls):\n")
print(summary(model_horse_ad))

# Extract CMHC coefficients from horse race
coef_names_hr <- names(coef(model_horse_ad))
cmhc_idx_hr <- grepl("^event_time_binned::(-?[0-9]+)$", coef_names_hr)
coef_df_horse <- data.frame(
  event_time = as.numeric(gsub("^event_time_binned::(-?[0-9]+)$", "\\1", coef_names_hr[cmhc_idx_hr])),
  coefficient = coef(model_horse_ad)[cmhc_idx_hr],
  se = sqrt(diag(vcov(model_horse_ad)))[cmhc_idx_hr]
) %>%
  filter(event_time != -999 & event_time < 15)

coef_df_horse$ci_lower <- coef_df_horse$coefficient - 1.96 * coef_df_horse$se
coef_df_horse$ci_upper <- coef_df_horse$coefficient + 1.96 * coef_df_horse$se
coef_df_horse <- rbind(coef_df_horse, data.frame(event_time = -1, coefficient = 0, se = 0, ci_lower = 0, ci_upper = 0))
coef_df_horse <- coef_df_horse %>% arrange(event_time)

# Plot horse race
p_horse <- ggplot(coef_df_horse, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
  geom_point(color = "steelblue", size = 2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "CMHC Event Study: Horse Race with CHC Event-Time Controls",
    subtitle = "AMR (Ages 20\u201349). CMHC coefficients shown; CHC event-time indicators included as controls.",
    x = "Years Relative to CMHC Opening",
    y = "Change in Deaths per 100,000"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave("cmhc_robustness_horse_race_ad.png", p_horse, width = 10, height = 6, dpi = 300)
cat("\nHorse race plot saved to: cmhc_robustness_horse_race_ad.png\n")

# Also run for all-ages AMR
model_horse_all <- feols(
  amr ~ i(event_time_binned, ref = -1) + i(chc_event_time_binned, ref = -1) +
    D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
  data = data,
  weights = ~popwt,
  cluster = ~fips
)

cat("\n\nHorse race — AMR (All Ages):\n")
print(summary(model_horse_all))


# ==================================================================
# (ii) Control for CHC Per-Capita Funding
# ==================================================================

cat("\n-----------------------------------------------------------\n")
cat("(ii) Controlling for CHC Per-Capita Funding (pcrfund_chc)\n")
cat("     Dependent Variable: AMR (Ages 20-49)\n")
cat("-----------------------------------------------------------\n\n")

# Replace NA funding with 0 (no CHC = no funding)
data <- data %>%
  mutate(pcrfund_chc = replace_na(pcrfund_chc, 0))

model_chcfund_ad <- feols(
  amr_ad ~ i(event_time_binned, ref = -1) + pcrfund_chc +
    D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
  data = data,
  weights = ~popwt_ad,
  cluster = ~fips
)

cat("CMHC event study controlling for CHC funding:\n")
print(summary(model_chcfund_ad))

# Extract CMHC coefficients
coef_names_cf <- names(coef(model_chcfund_ad))
cmhc_idx_cf <- grepl("^event_time_binned::(-?[0-9]+)$", coef_names_cf)
coef_df_chcfund <- data.frame(
  event_time = as.numeric(gsub("^event_time_binned::(-?[0-9]+)$", "\\1", coef_names_cf[cmhc_idx_cf])),
  coefficient = coef(model_chcfund_ad)[cmhc_idx_cf],
  se = sqrt(diag(vcov(model_chcfund_ad)))[cmhc_idx_cf]
) %>%
  filter(event_time != -999 & event_time < 15)

coef_df_chcfund$ci_lower <- coef_df_chcfund$coefficient - 1.96 * coef_df_chcfund$se
coef_df_chcfund$ci_upper <- coef_df_chcfund$coefficient + 1.96 * coef_df_chcfund$se
coef_df_chcfund <- rbind(coef_df_chcfund, data.frame(event_time = -1, coefficient = 0, se = 0, ci_lower = 0, ci_upper = 0))
coef_df_chcfund <- coef_df_chcfund %>% arrange(event_time)

# Comparison plot: baseline vs CHC-fund-controlled
coef_compare <- bind_rows(
  coef_df_ad %>% mutate(model = "Baseline"),
  coef_df_chcfund %>% mutate(model = "+ CHC Funding Control")
)

p_chcfund <- ggplot(coef_compare, aes(x = event_time, y = coefficient, color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.1, color = NA) +
  geom_point(size = 2, position = position_dodge(width = 0.4)) +
  geom_line(linewidth = 0.8, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Baseline" = "steelblue", "+ CHC Funding Control" = "darkorange")) +
  scale_fill_manual(values = c("Baseline" = "steelblue", "+ CHC Funding Control" = "darkorange")) +
  labs(
    title = "CMHC Event Study: Baseline vs. Controlling for CHC Funding",
    subtitle = "AMR (Ages 20\u201349). Adding per-capita CHC funding as a time-varying control.",
    x = "Years Relative to CMHC Opening",
    y = "Change in Deaths per 100,000",
    color = "Specification", fill = "Specification"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave("cmhc_robustness_chc_funding_control.png", p_chcfund, width = 10, height = 6, dpi = 300)
cat("\nCHC funding control comparison plot saved to: cmhc_robustness_chc_funding_control.png\n")


# ==================================================================
# (iii) Restrict to Never-CHC Counties
# ==================================================================

cat("\n-----------------------------------------------------------\n")
cat("(iii) Restrict Sample to Never-CHC Counties\n")
cat("      Dependent Variable: AMR (Ages 20-49)\n")
cat("-----------------------------------------------------------\n\n")

data_no_chc <- data %>% filter(ever_chc == 0)

cat("Never-CHC sample:\n")
cat("  Total counties:", n_distinct(data_no_chc$fips), "\n")
cat("  CMHC-treated counties:", n_distinct(data_no_chc$fips[!is.na(data_no_chc$cmhc_year_exp)]), "\n")
cat("  Observations:", nrow(data_no_chc), "\n\n")

# Check there are enough treated counties
n_treated_no_chc <- n_distinct(data_no_chc$fips[!is.na(data_no_chc$cmhc_year_exp)])

if (n_treated_no_chc >= 10) {
  model_nochc_ad <- feols(
    amr_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year^Durb + year^stfips,
    data = data_no_chc,
    weights = ~popwt_ad,
    cluster = ~fips
  )

  cat("CMHC event study — never-CHC counties only:\n")
  print(summary(model_nochc_ad))

  # Extract coefficients
  coef_names_nc <- names(coef(model_nochc_ad))
  cmhc_idx_nc <- grepl("^event_time_binned::(-?[0-9]+)$", coef_names_nc)
  coef_df_nochc <- data.frame(
    event_time = as.numeric(gsub("^event_time_binned::(-?[0-9]+)$", "\\1", coef_names_nc[cmhc_idx_nc])),
    coefficient = coef(model_nochc_ad)[cmhc_idx_nc],
    se = sqrt(diag(vcov(model_nochc_ad)))[cmhc_idx_nc]
  ) %>%
    filter(event_time != -999 & event_time < 15)

  coef_df_nochc$ci_lower <- coef_df_nochc$coefficient - 1.96 * coef_df_nochc$se
  coef_df_nochc$ci_upper <- coef_df_nochc$coefficient + 1.96 * coef_df_nochc$se
  coef_df_nochc <- rbind(coef_df_nochc, data.frame(event_time = -1, coefficient = 0, se = 0, ci_lower = 0, ci_upper = 0))
  coef_df_nochc <- coef_df_nochc %>% arrange(event_time)

  # Comparison plot: full sample vs never-CHC
  coef_compare_nc <- bind_rows(
    coef_df_ad %>% mutate(model = "Full Sample"),
    coef_df_nochc %>% mutate(model = "Never-CHC Counties Only")
  )

  p_nochc <- ggplot(coef_compare_nc, aes(x = event_time, y = coefficient, color = model, fill = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.1, color = NA) +
    geom_point(size = 2, position = position_dodge(width = 0.4)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.4)) +
    scale_color_manual(values = c("Full Sample" = "steelblue", "Never-CHC Counties Only" = "darkgreen")) +
    scale_fill_manual(values = c("Full Sample" = "steelblue", "Never-CHC Counties Only" = "darkgreen")) +
    labs(
      title = "CMHC Event Study: Full Sample vs. Never-CHC Counties",
      subtitle = "AMR (Ages 20\u201349). Excludes all counties that ever received a CHC grant.",
      x = "Years Relative to CMHC Opening",
      y = "Change in Deaths per 100,000",
      color = "Sample", fill = "Sample"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  ggsave("cmhc_robustness_never_chc.png", p_nochc, width = 10, height = 6, dpi = 300)
  cat("\nNever-CHC comparison plot saved to: cmhc_robustness_never_chc.png\n")

  write.csv(coef_df_nochc, "cmhc_event_study_coefficients_ad_nochc.csv", row.names = FALSE)
} else {
  cat("WARNING: Only", n_treated_no_chc, "CMHC-treated counties without CHC. Too few for reliable estimation.\n")
}

cat("\n==========================================================\n")
cat("End of CHC Robustness Checks (Event Study)\n")
cat("==========================================================\n")


# ==========================================
# STEP 12d: PLACEBO TEST — High-Priority Counties That Never Got a CMHC
# ==========================================
# If CMHC construction is driving the mortality decline, then counties
# that were rated as high-priority but never received a CMHC should NOT
# show the same decline. We assign them a pseudo-treatment year (median
# CMHC opening = 1973) and run the same event-study specification.

cat("\n\n##########################################################\n")
cat("PLACEBO TEST: High-Priority Counties That Never Got a CMHC\n")
cat("##########################################################\n\n")

# Load priority ranks
priority <- readr::read_csv("cmhc_data/priority_ranks_for_eventstudy.csv", show_col_types = FALSE)
cat("Loaded priority data:", nrow(priority), "counties\n")
cat("  High priority (top tercile):", sum(priority$high_priority == 1, na.rm = TRUE), "\n")
cat("  High priority, no CMHC:", sum(priority$high_priority_no_cmhc == 1, na.rm = TRUE), "\n\n")

# Median CMHC opening year among treated counties (for pseudo-treatment)
median_cmhc_year <- median(cmhc_openings$cmhc_year_exp, na.rm = TRUE)
cat("Median CMHC opening year (pseudo-treatment year):", median_cmhc_year, "\n\n")

# Merge priority into the panel
data_placebo <- data %>%
  left_join(priority %>% select(fips, priority_pctile, priority_tercile,
                                 high_priority, area_has_cmhc, high_priority_no_cmhc),
            by = "fips")

# Create placebo treatment:
#   - "Treated" = counties in high-priority AREAS where NO county in the area
#     got a CMHC (pseudo year = median)
#   - "Control" = counties with no CMHC (neither direct nor via area)
# Exclude all CMHC recipients AND counties in areas that got a CMHC
# (since those counties were effectively treated through their area)

data_placebo <- data_placebo %>%
  filter(is.na(cmhc_year_exp)) %>%                      # Drop direct CMHC recipients
  filter(is.na(area_has_cmhc) | area_has_cmhc == 0) %>%  # Also drop area-treated counties
  mutate(
    placebo_year = ifelse(high_priority_no_cmhc == 1, median_cmhc_year, NA_real_),
    placebo_event_time = year - placebo_year,
    placebo_et_binned = case_when(
      is.na(placebo_event_time) ~ -999L,
      placebo_event_time <= -6  ~ -6L,
      placebo_event_time >= 20  ~ 20L,
      TRUE ~ as.integer(placebo_event_time)
    )
  )

cat("Placebo sample (area-based definition):\n")
cat("  Total counties:", n_distinct(data_placebo$fips), "\n")
cat("  Placebo-treated (high priority area, no CMHC in area):",
    n_distinct(data_placebo$fips[data_placebo$high_priority_no_cmhc == 1]), "\n")
cat("  Never-treated controls:",
    n_distinct(data_placebo$fips[is.na(data_placebo$placebo_year)]), "\n")
cat("  NOTE: Excludes all counties whose area received a CMHC (even if the\n")
cat("         county itself did not directly host one).\n\n")

# --- Run placebo event study: AMR (all ages) ---
cat("-----------------------------------------------------------\n")
cat("Placebo Event Study: AMR (All Ages)\n")
cat("-----------------------------------------------------------\n\n")

model_placebo <- feols(
  amr ~ i(placebo_et_binned, ref = -1) + D_tot_act_md_t + H_bpc
    + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit` | fips + year^Durb + year^stfips,
  data = data_placebo,
  weights = ~popwt,
  cluster = ~fips
)

print(summary(model_placebo))

# Extract placebo coefficients
coef_names_pl <- names(coef(model_placebo))
pl_idx <- grepl("^placebo_et_binned::(-?[0-9]+)$", coef_names_pl)
coef_df_placebo <- data.frame(
  event_time = as.numeric(gsub("^placebo_et_binned::(-?[0-9]+)$", "\\1", coef_names_pl[pl_idx])),
  coefficient = coef(model_placebo)[pl_idx],
  se = sqrt(diag(vcov(model_placebo)))[pl_idx]
) %>%
  filter(event_time != -999 & event_time < 15)

coef_df_placebo$ci_lower <- coef_df_placebo$coefficient - 1.96 * coef_df_placebo$se
coef_df_placebo$ci_upper <- coef_df_placebo$coefficient + 1.96 * coef_df_placebo$se
coef_df_placebo <- rbind(coef_df_placebo, data.frame(event_time = -1, coefficient = 0, se = 0,
                                                      ci_lower = 0, ci_upper = 0))
coef_df_placebo <- coef_df_placebo %>% arrange(event_time)

# Pre/post tests for placebo
cat("\n-----------------------------------------------------------\n")
cat("Placebo Joint Significance Tests\n")
cat("-----------------------------------------------------------\n")

pre_pl <- paste0("placebo_et_binned::", -6:-2)
pre_pl <- pre_pl[pre_pl %in% names(coef(model_placebo))]
if (length(pre_pl) > 0) {
  pre_test_pl <- wald(model_placebo, pre_pl)
  cat("\nPre-trend test (t = -6 to -2):\n")
  cat("  F-statistic:", pre_test_pl$stat, "\n")
  cat("  p-value:", pre_test_pl$p, "\n")
}

post_pl <- paste0("^placebo_et_binned::", 0:14, "$")
post_test_pl <- wald(model_placebo, post_pl)
cat("\nPost-pseudo-treatment test (t = 0 to +14):\n")
cat("  F-statistic:", post_test_pl$stat, "\n")
cat("  p-value:", post_test_pl$p, "\n")

# --- Run placebo event study: AMR (ages 20-49) ---
cat("\n-----------------------------------------------------------\n")
cat("Placebo Event Study: AMR (Ages 20-49)\n")
cat("-----------------------------------------------------------\n\n")

model_placebo_ad <- feols(
  amr_ad ~ i(placebo_et_binned, ref = -1) + D_tot_act_md_t + H_bpc
    + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit` | fips + year^Durb + year^stfips,
  data = data_placebo,
  weights = ~popwt_ad,
  cluster = ~fips
)

print(summary(model_placebo_ad))

# Extract placebo adult coefficients
coef_names_pla <- names(coef(model_placebo_ad))
pla_idx <- grepl("^placebo_et_binned::(-?[0-9]+)$", coef_names_pla)
coef_df_placebo_ad <- data.frame(
  event_time = as.numeric(gsub("^placebo_et_binned::(-?[0-9]+)$", "\\1", coef_names_pla[pla_idx])),
  coefficient = coef(model_placebo_ad)[pla_idx],
  se = sqrt(diag(vcov(model_placebo_ad)))[pla_idx]
) %>%
  filter(event_time != -999 & event_time < 15)

coef_df_placebo_ad$ci_lower <- coef_df_placebo_ad$coefficient - 1.96 * coef_df_placebo_ad$se
coef_df_placebo_ad$ci_upper <- coef_df_placebo_ad$coefficient + 1.96 * coef_df_placebo_ad$se
coef_df_placebo_ad <- rbind(coef_df_placebo_ad, data.frame(event_time = -1, coefficient = 0, se = 0,
                                                            ci_lower = 0, ci_upper = 0))
coef_df_placebo_ad <- coef_df_placebo_ad %>% arrange(event_time)

# Save placebo coefficients
write.csv(coef_df_placebo, "cmhc_event_study_placebo_amr.csv", row.names = FALSE)
write.csv(coef_df_placebo_ad, "cmhc_event_study_placebo_amr_ad.csv", row.names = FALSE)
cat("\nPlacebo coefficients saved.\n")

# --- Plot: Placebo vs Actual Treatment Effect ---
cat("\n-----------------------------------------------------------\n")
cat("Generating Placebo Comparison Plots\n")
cat("-----------------------------------------------------------\n")

library(ggplot2)

# All-ages comparison
coef_compare_placebo <- bind_rows(
  coef_df %>% mutate(model = "Actual CMHC Treatment"),
  coef_df_placebo %>% mutate(model = "Placebo (High Priority, No CMHC)")
)

p_placebo_all <- ggplot(coef_compare_placebo, aes(x = event_time, y = coefficient,
                                                    color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.12, color = NA) +
  geom_point(size = 2, position = position_dodge(width = 0.4)) +
  geom_line(linewidth = 0.8, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Actual CMHC Treatment" = "steelblue",
                                 "Placebo (High Priority, No CMHC)" = "firebrick")) +
  scale_fill_manual(values = c("Actual CMHC Treatment" = "steelblue",
                                "Placebo (High Priority, No CMHC)" = "firebrick")) +
  labs(
    title = "Placebo Test: Actual CMHC Effect vs. High-Priority Counties That Never Got a CMHC",
    subtitle = "AMR (All Ages). Placebo counties assigned pseudo-treatment at median CMHC year (1973).",
    x = "Years Relative to (Pseudo-)Treatment",
    y = "Change in Deaths per 100,000",
    color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave("cmhc_placebo_vs_actual_amr.png", p_placebo_all, width = 11, height = 6.5, dpi = 300)
cat("Plot saved to: cmhc_placebo_vs_actual_amr.png\n")

# Adult (20-49) comparison
coef_compare_placebo_ad <- bind_rows(
  coef_df_ad %>% mutate(model = "Actual CMHC Treatment"),
  coef_df_placebo_ad %>% mutate(model = "Placebo (High Priority, No CMHC)")
)

p_placebo_ad <- ggplot(coef_compare_placebo_ad, aes(x = event_time, y = coefficient,
                                                      color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.12, color = NA) +
  geom_point(size = 2, position = position_dodge(width = 0.4)) +
  geom_line(linewidth = 0.8, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c("Actual CMHC Treatment" = "steelblue",
                                 "Placebo (High Priority, No CMHC)" = "firebrick")) +
  scale_fill_manual(values = c("Actual CMHC Treatment" = "steelblue",
                                "Placebo (High Priority, No CMHC)" = "firebrick")) +
  labs(
    title = "Placebo Test: Actual CMHC Effect vs. High-Priority Counties (Ages 20\u201349)",
    subtitle = "AMR (Ages 20-49). Placebo counties assigned pseudo-treatment at median CMHC year.",
    x = "Years Relative to (Pseudo-)Treatment",
    y = "Change in Deaths per 100,000",
    color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave("cmhc_placebo_vs_actual_amr_ad.png", p_placebo_ad, width = 11, height = 6.5, dpi = 300)
cat("Plot saved to: cmhc_placebo_vs_actual_amr_ad.png\n")

# Standalone placebo plot (AMR 20-49)
p_placebo_standalone <- ggplot(coef_df_placebo_ad, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "firebrick", alpha = 0.2) +
  geom_point(color = "firebrick", size = 2) +
  geom_line(color = "firebrick", linewidth = 0.8) +
  labs(
    title = "Placebo Test: High-Priority Counties That Never Received a CMHC",
    subtitle = paste0("AMR (Ages 20\u201349). Pseudo-treatment at ", median_cmhc_year,
                      ". N = ", n_distinct(data_placebo$fips[data_placebo$high_priority_no_cmhc == 1]),
                      " placebo-treated counties."),
    x = "Years Relative to Pseudo-Treatment",
    y = "Change in Deaths per 100,000"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("cmhc_placebo_standalone_ad.png", p_placebo_standalone, width = 10, height = 6, dpi = 300)
cat("Plot saved to: cmhc_placebo_standalone_ad.png\n")

cat("\n==========================================================\n")
cat("PLACEBO TEST COMPLETE\n")
cat("==========================================================\n")
cat("If CMHC construction is causal, the placebo (red) should show\n")
cat("no post-treatment decline, while actual treatment (blue) does.\n")
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

# The .fst has pre-computed rates (quarterly): total_jail_pop_rate, black_incarceration_rate, etc.
# Aggregate to annual means
incarceration <- incarceration %>%
  group_by(fips, year) %>%
  summarize(
    jail_rate = mean(total_jail_pop_rate, na.rm = TRUE),
    bl_jail_rate = mean(black_incarceration_rate, na.rm = TRUE),
    jail_pop_per_capita = mean(jail_pop_per_capita, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(jail_rate, bl_jail_rate, jail_pop_per_capita),
                ~ifelse(is.nan(.), NA, .)))

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

# Get 1970 baseline data (used for all long-difference windows)
data_1970 <- data %>%
  filter(year == 1970) %>%
  select(fips, stfips, cofips, Durb,
         jail_rate_1970 = jail_rate,
         # Baseline controls
         `_60pcturban`, `_60pctrurf`, `_60pct04years`, `_60pctmt64years`,
         `_60pctnonwhit`, `_60pctmt12schl`, `_60pctlt4schl`,
         `_pct59inclt3k`, `_pct59incmt10k`, `_tot_act_md`,
         copop, popwt)

# ==========================================
# Helper function: run long-difference for a given end year
# ==========================================

run_long_difference <- function(end_year, data_1970, incarceration, cmhc_openings_full) {
  
  cat(sprintf("\n###########################################################\n"))
  cat(sprintf("  LONG DIFFERENCE: 1970 – %d\n", end_year))
  cat(sprintf("###########################################################\n\n"))
  
  # Get end-year data from the incarceration panel directly
  data_end <- incarceration %>%
    filter(year == end_year) %>%
    select(fips, jail_rate_end = jail_rate)
  
  # Merge 1970 and end year
  ld_data <- data_1970 %>%
    inner_join(data_end, by = "fips") %>%
    left_join(cmhc_openings_full, by = "fips")
  
  # Compute log change in jail rate
  ld_data <- ld_data %>%
    mutate(
      log_jail_rate_1970 = log(jail_rate_1970),
      log_jail_rate_end  = log(jail_rate_end),
      delta_log_jail_rate = log_jail_rate_end - log_jail_rate_1970,
      # Treatment: CMHC operating by end year
      cmhc = as.integer(!is.na(cmhc_year_full) & cmhc_year_full <= end_year),
      log_pop_1960 = log(popwt)
    )
  
  # Filter to valid outcomes
  ld_data_valid <- ld_data %>%
    filter(!is.na(delta_log_jail_rate) & is.finite(delta_log_jail_rate))
  
  cat(sprintf("Long-difference sample (1970-%d):\n", end_year))
  cat("  Counties with valid Δlog(jail_rate):", nrow(ld_data_valid), "\n")
  cat(sprintf("  Treated (CMHC by %d): %d\n", end_year, sum(ld_data_valid$cmhc == 1)))
  cat("  Control:", sum(ld_data_valid$cmhc == 0), "\n\n")
  
  # Split by population
  ld_data_valid_small <- ld_data_valid %>%
    filter(log_pop_1960 < log(30000)) %>%
    group_by(stfips) %>% filter(n() > 1) %>% ungroup()
  
  ld_data_valid_large <- ld_data_valid %>%
    filter(log_pop_1960 > log(30000)) %>%
    group_by(stfips) %>% filter(n() > 1) %>% ungroup()
  
  # Model 1: No controls (small counties)
  model_ld1 <- lm(delta_log_jail_rate ~ cmhc, data = ld_data_valid_small)
  robust_se1 <- sqrt(diag(vcovHC(model_ld1, type = "HC1")))
  
  # Model 2: With baseline controls (small counties)
  model_ld2 <- lm(delta_log_jail_rate ~ cmhc + `_60pcturban` + `_60pctrurf` + 
                    `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` + 
                    `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` + 
                    `_pct59incmt10k` + `_tot_act_md`,
                  data = ld_data_valid_small)
  robust_se2 <- sqrt(diag(vcovHC(model_ld2, type = "HC1")))
  
  # Model 3a: Controls + state FE, small counties, pop-weighted
  model_ld3 <- lm(delta_log_jail_rate ~ cmhc + `_60pcturban` + `_60pctrurf` + 
                    `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` + 
                    `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` + 
                    `_pct59incmt10k` + `_tot_act_md` + factor(stfips),
                  weights = popwt, data = ld_data_valid_small)
  robust_se3 <- sqrt(diag(vcovHC(model_ld3, type = "HC1")))
  
  # Model 3b: Controls + state FE, large counties, pop-weighted
  model_ld3b <- lm(delta_log_jail_rate ~ cmhc + `_60pcturban` + `_60pctrurf` + 
                     `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` + 
                     `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` + 
                     `_pct59incmt10k` + `_tot_act_md` + factor(stfips),
                   weights = popwt, data = ld_data_valid_large)
  robust_se3b <- sqrt(diag(vcovHC(model_ld3b, type = "HC1")))
  
  # Model 4: Interaction with log(pop)
  model_4 <- feols(
    delta_log_jail_rate ~ cmhc * log_pop_1960 + `_60pcturban` + `_60pctrurf` + 
                    `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` + 
                    `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` + 
                    `_pct59incmt10k` + `_tot_act_md` | stfips,
    data = ld_data_valid, weights = ~popwt, cluster = ~fips
  )
  
  # Extract model_4 statistics
  coef_m4 <- coef(model_4)["cmhc"]
  se_m4 <- sqrt(diag(vcov(model_4)))["cmhc"]
  t_m4 <- coef_m4 / se_m4
  p_m4 <- 2 * pt(-abs(t_m4), df = nrow(ld_data_valid) - length(coef(model_4)))
  
  # p-values
  p1 <- 2 * pt(-abs(coef(model_ld1)["cmhc"]/robust_se1["cmhc"]), df = model_ld1$df.residual)
  p2 <- 2 * pt(-abs(coef(model_ld2)["cmhc"]/robust_se2["cmhc"]), df = model_ld2$df.residual)
  p3 <- 2 * pt(-abs(coef(model_ld3)["cmhc"]/robust_se3["cmhc"]), df = model_ld3$df.residual)
  p3b <- 2 * pt(-abs(coef(model_ld3b)["cmhc"]/robust_se3b["cmhc"]), df = model_ld3b$df.residual)
  
  # Print table
  label <- sprintf("CMHC exposure (1970-%02d)", end_year %% 100)
  dep_label <- sprintf("Dependent Variable: Δlog(jail_rate) = log(jail_rate_%d) - log(jail_rate_1970)", end_year)
  
  cat("-----------------------------------------------------------\n")
  cat("OLS Regression: Δlog(jail_rate) on CMHC exposure\n")
  cat("-----------------------------------------------------------\n\n")
  cat(dep_label, "\n\n")
  
  cat("                                    (1)         (2)         (3a)        (3b)        (4)\n")
  cat("                                No Controls  Controls   Small+StateFE Large+StateFE Interaction\n")
  cat("-----------------------------------------------------------------------------------------------\n")
  cat(sprintf("%-32s%8.4f    %8.4f    %8.4f    %8.4f    %8.4f\n",
              label,
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
  cat(sprintf("  p-value                       %8.4f    %8.4f    %8.4f    %8.4f    %8.4f\n", 
              p1, p2, p3, p3b, p_m4))
  cat("-----------------------------------------------------------------------------------------------\n")
  cat(sprintf("N counties                      %8d    %8d    %8d    %8d    %8d\n",
              nobs(model_ld1), nobs(model_ld2), nobs(model_ld3), 
              nobs(model_ld3b), nobs(model_4)))
  cat(sprintf("R-squared                       %8.4f    %8.4f    %8.4f    %8.4f    %8.4f\n",
              summary(model_ld1)$r.squared, summary(model_ld2)$r.squared, 
              summary(model_ld3)$r.squared, summary(model_ld3b)$r.squared,
              r2(model_4, "r2")))
  cat("-----------------------------------------------------------------------------------------------\n\n")
  
  # Return key results for summary table
  data.frame(
    end_year    = end_year,
    window      = sprintf("1970-%d", end_year),
    coef_noctr  = coef(model_ld1)["cmhc"],
    se_noctr    = robust_se1["cmhc"],
    p_noctr     = p1,
    coef_ctr    = coef(model_ld2)["cmhc"],
    se_ctr      = robust_se2["cmhc"],
    p_ctr       = p2,
    coef_small  = coef(model_ld3)["cmhc"],
    se_small    = robust_se3["cmhc"],
    p_small     = p3,
    coef_large  = coef(model_ld3b)["cmhc"],
    se_large    = robust_se3b["cmhc"],
    p_large     = p3b,
    coef_inter  = coef_m4,
    se_inter    = se_m4,
    p_inter     = p_m4,
    n_small     = nobs(model_ld3),
    n_large     = nobs(model_ld3b),
    n_all       = nobs(model_4),
    n_treated   = sum(ld_data_valid$cmhc == 1),
    n_control   = sum(ld_data_valid$cmhc == 0),
    row.names   = NULL
  )
}

# ==========================================
# Run all long-difference windows
# ==========================================

end_years <- c(1978, 1983, 1988, 1993, 2000)

ld_results_list <- lapply(end_years, function(ey) {
  run_long_difference(ey, data_1970, incarceration, cmhc_openings_full)
})

ld_results_all <- do.call(rbind, ld_results_list)

# ==========================================
# Summary comparison across windows
# ==========================================

cat("\n\n===========================================================\n")
cat("SUMMARY: CMHC Effect on Δlog(jail_rate) Across Time Windows\n")
cat("===========================================================\n\n")
cat("Preferred specification: Model 3a (controls + state FE, small counties, pop-weighted)\n\n")

cat(sprintf("%-12s  %8s  %8s  %8s  %8s  %8s\n",
            "Window", "Coef", "SE", "t", "p", "N"))
cat("-------------------------------------------------------------------\n")
for (i in 1:nrow(ld_results_all)) {
  r <- ld_results_all[i, ]
  t_val <- r$coef_small / r$se_small
  cat(sprintf("%-12s  %8.4f  %8.4f  %8.3f  %8.4f  %8d\n",
              r$window, r$coef_small, r$se_small, t_val, r$p_small, r$n_small))
}
cat("-------------------------------------------------------------------\n")
cat("Robust (HC1) standard errors. Pop-weighted. Small counties only.\n\n")

# Also show large-county results
cat(sprintf("%-12s  %8s  %8s  %8s  %8s  %8s\n",
            "Window", "Coef", "SE", "t", "p", "N"))
cat("-------------------------------------------------------------------\n")
for (i in 1:nrow(ld_results_all)) {
  r <- ld_results_all[i, ]
  t_val <- r$coef_large / r$se_large
  cat(sprintf("%-12s  %8.4f  %8.4f  %8.3f  %8.4f  %8d\n",
              r$window, r$coef_large, r$se_large, t_val, r$p_large, r$n_large))
}
cat("-------------------------------------------------------------------\n")
cat("Large counties. Robust (HC1) standard errors. Pop-weighted.\n")

# Save combined results
write.csv(ld_results_all, "cmhc_long_difference_results.csv", row.names = FALSE)
cat("\nLong-difference results saved to: cmhc_long_difference_results.csv\n")


# ==========================================
# STEP 14: Long-Difference CHC Robustness
# ==========================================
# Add CHC controls to the incarceration long-difference regressions
# to show the CMHC effect on jail rates is not driven by CHCs.

cat("\n\n##########################################################\n")
cat("ROBUSTNESS: Long-Difference with CHC Controls\n")
cat("##########################################################\n\n")

# Rebuild 1978 long-difference data for Step 14 (since Step 13 now uses a function)
data_1978_chc <- incarceration %>%
  filter(year == 1978) %>%
  select(fips, jail_rate_1978 = jail_rate)

ld_data <- data_1970 %>%
  inner_join(data_1978_chc, by = "fips") %>%
  left_join(cmhc_openings_full, by = "fips") %>%
  mutate(
    log_jail_rate_1970 = log(jail_rate_1970),
    log_jail_rate_1978 = log(jail_rate_1978),
    delta_log_jail_rate = log_jail_rate_1978 - log_jail_rate_1970,
    cmhc = as.integer(!is.na(cmhc_year_full) & cmhc_year_full <= 1978),
    log_pop_1960 = log(popwt)
  )

ld_data_valid <- ld_data %>%
  filter(!is.na(delta_log_jail_rate) & is.finite(delta_log_jail_rate))

# Baseline model 3a for comparison
ld_data_valid_small <- ld_data_valid %>%
  filter(log_pop_1960 < log(30000)) %>%
  group_by(stfips) %>% filter(n() > 1) %>% ungroup()

model_ld3 <- lm(delta_log_jail_rate ~ cmhc + `_60pcturban` + `_60pctrurf` +
                  `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` +
                  `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` +
                  `_pct59incmt10k` + `_tot_act_md` + factor(stfips),
                weights = popwt, data = ld_data_valid_small)
robust_se3 <- sqrt(diag(vcovHC(model_ld3, type = "HC1")))

# Add CHC indicator to long-difference data
# chc_year_exp is in the original data; get cross-sectional version
chc_xsec <- data %>%
  filter(year == 1970) %>%
  select(fips, chc_year_exp, pcrfund_chc_1970 = pcrfund_chc, grant_chc_1970 = grant_chc) %>%
  mutate(
    chc_by_1978 = as.integer(!is.na(chc_year_exp) & chc_year_exp <= 1978),
    ever_chc = as.integer(!is.na(chc_year_exp))
  )

# Also get 1978 CHC funding for change
chc_1978 <- data %>%
  filter(year == 1978) %>%
  select(fips, pcrfund_chc_1978 = pcrfund_chc, grant_chc_1978 = grant_chc)

ld_data_valid <- ld_data_valid %>%
  left_join(chc_xsec, by = "fips") %>%
  left_join(chc_1978, by = "fips") %>%
  mutate(
    pcrfund_chc_1970 = replace_na(pcrfund_chc_1970, 0),
    pcrfund_chc_1978 = replace_na(pcrfund_chc_1978, 0),
    grant_chc_1970 = replace_na(grant_chc_1970, 0),
    grant_chc_1978 = replace_na(grant_chc_1978, 0),
    chc_by_1978 = replace_na(chc_by_1978, 0L),
    ever_chc = replace_na(ever_chc, 0L),
    delta_pcrfund_chc = pcrfund_chc_1978 - pcrfund_chc_1970
  )

cat("CHC overlap in long-difference sample:\n")
cat("  Counties with CHC by 1978:", sum(ld_data_valid$chc_by_1978 == 1), "\n")
cat("  Counties with both CMHC and CHC by 1978:",
    sum(ld_data_valid$cmhc == 1 & ld_data_valid$chc_by_1978 == 1), "\n\n")

# Re-create subsamples with CHC variables
ld_data_valid_small <- ld_data_valid %>%
  filter(log_pop_1960 < log(30000)) %>%
  group_by(stfips) %>%
  filter(n() > 1) %>%
  ungroup()

# --- Model 5: Baseline Model 3a + CHC dummy ---
model_ld5 <- lm(delta_log_jail_rate ~ cmhc + chc_by_1978 + `_60pcturban` + `_60pctrurf` +
                  `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` +
                  `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` +
                  `_pct59incmt10k` + `_tot_act_md` + factor(stfips),
                weights = popwt,
                data = ld_data_valid_small)
robust_se5 <- sqrt(diag(vcovHC(model_ld5, type = "HC1")))

# --- Model 6: Baseline Model 3a + CHC per-capita funding ---
model_ld6 <- lm(delta_log_jail_rate ~ cmhc + pcrfund_chc_1978 + `_60pcturban` + `_60pctrurf` +
                  `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` +
                  `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` +
                  `_pct59incmt10k` + `_tot_act_md` + factor(stfips),
                weights = popwt,
                data = ld_data_valid_small)
robust_se6 <- sqrt(diag(vcovHC(model_ld6, type = "HC1")))

# --- Model 7: Restrict to never-CHC counties ---
ld_no_chc <- ld_data_valid_small %>% filter(ever_chc == 0)

# Need states with >1 obs
ld_no_chc <- ld_no_chc %>%
  group_by(stfips) %>%
  filter(n() > 1) %>%
  ungroup()

cat("Never-CHC long-difference sample:", nrow(ld_no_chc), "counties\n")
cat("  CMHC-treated:", sum(ld_no_chc$cmhc == 1), "\n\n")

model_ld7 <- NULL
robust_se7 <- NULL
if (sum(ld_no_chc$cmhc == 1) >= 5) {
  model_ld7 <- lm(delta_log_jail_rate ~ cmhc + `_60pcturban` + `_60pctrurf` +
                    `_60pct04years` + `_60pctmt64years` + `_60pctnonwhit` +
                    `_60pctmt12schl` + `_60pctlt4schl` + `_pct59inclt3k` +
                    `_pct59incmt10k` + `_tot_act_md` + factor(stfips),
                  weights = popwt,
                  data = ld_no_chc)
  robust_se7 <- sqrt(diag(vcovHC(model_ld7, type = "HC1")))
}

# --- Print comparison table ---
cat("-----------------------------------------------------------\n")
cat("Long-Difference with CHC Controls\n")
cat("Dependent Variable: Δlog(jail_rate) 1970-1978\n")
cat("-----------------------------------------------------------\n\n")

cat("                                    (3a)        (5)         (6)         (7)\n")
cat("                                  Baseline   +CHC Dummy  +CHC Funds  No-CHC Only\n")
cat("-----------------------------------------------------------------------------------------------\n")

# CMHC row
cat(sprintf("CMHC exposure (1970-78)         %8.4f    %8.4f    %8.4f",
            coef(model_ld3)["cmhc"], coef(model_ld5)["cmhc"], coef(model_ld6)["cmhc"]))
if (!is.null(model_ld7)) cat(sprintf("    %8.4f", coef(model_ld7)["cmhc"]))
cat("\n")

cat(sprintf("  Robust SE                     (%6.4f)    (%6.4f)    (%6.4f)",
            robust_se3["cmhc"], robust_se5["cmhc"], robust_se6["cmhc"]))
if (!is.null(robust_se7)) cat(sprintf("    (%6.4f)", robust_se7["cmhc"]))
cat("\n")

# CHC rows
cat(sprintf("CHC by 1978 (dummy)                         %8.4f\n", coef(model_ld5)["chc_by_1978"]))
cat(sprintf("  Robust SE                                 (%6.4f)\n", robust_se5["chc_by_1978"]))
cat(sprintf("CHC PC Funding 1978                                     %8.4f\n", coef(model_ld6)["pcrfund_chc_1978"]))
cat(sprintf("  Robust SE                                             (%6.4f)\n", robust_se6["pcrfund_chc_1978"]))

cat("-----------------------------------------------------------------------------------------------\n")
cat(sprintf("Baseline controls                   Yes         Yes         Yes         Yes\n"))
cat(sprintf("State fixed effects                 Yes         Yes         Yes         Yes\n"))
cat(sprintf("CHC control                         No          Dummy       Funding     Excluded\n"))
cat(sprintf("N counties                      %8d    %8d    %8d",
            nobs(model_ld3), nobs(model_ld5), nobs(model_ld6)))
if (!is.null(model_ld7)) cat(sprintf("    %8d", nobs(model_ld7)))
cat("\n")
cat(sprintf("R-squared                       %8.4f    %8.4f    %8.4f",
            summary(model_ld3)$r.squared, summary(model_ld5)$r.squared, summary(model_ld6)$r.squared))
if (!is.null(model_ld7)) cat(sprintf("    %8.4f", summary(model_ld7)$r.squared))
cat("\n")
cat("-----------------------------------------------------------------------------------------------\n")
cat("Robust standard errors in parentheses. Small counties only (pop 15-64 < 30k).\n")
cat("(3a) = baseline from main table. (5) adds CHC-by-1978 dummy.\n")
cat("(6) adds 1978 per-capita CHC funding. (7) drops all ever-CHC counties.\n")
cat("* p<0.10, ** p<0.05, *** p<0.01\n")
