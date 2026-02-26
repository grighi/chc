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
# vars <- sapply(haven::read_dta("aer_data/aer_data.dta", n_max = 0), attr, "label") |> unlist
# fwrite(data.frame(name = names(vars), value = vars), 'replication/vars.csv')
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

# Replicate Bailey & Goodman-Bacon (2015) Figure 5 specification
# Stata: xtreg amr _Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* [aw=popwt], cluster(fips) fe
#
# Components:
#   θ_c       = county FE                     → fips FE
#   γ_t       = year FE                       → year FE
#   Durb×year = urban-category-by-year FE     → Durb^year FE
#   λ_s * t   = state-specific linear trends  → stfips^year FE (state-by-year)
#   D_*       = 1960 county characteristics × year trends
#   R_*       = REIS transfer variables (time-varying controls)
#   H_*       = AHA hospital variables (time-varying controls)
#   _Texp*    = event-time indicators (ref = -1)
#   [aw=popwt] = analytic weights (1960 population)

cat("\n==========================================================\n")
cat("REPLICATING BAILEY & GOODMAN-BACON (2015) FIGURE 5\n")
cat("==========================================================\n\n")

# ---- Panel A: Early CHCs, All Counties, 1959-1988 ----
cat("Panel A: Early CHCs, All Counties (1959-1988)\n")

mod_early_all <- feols(
  amr ~ i(event_time_binned, ref = -1) +
    D_pct59inclt3k_t + D_60pctnonwhit_t + D_60pctrurf_t + D_60pcturban_t + D_tot_act_md_t +
    R_tranpcret + R_tranpcpa1 +
    H_bpc + H_hpc |
    fips + year + Durb^year + stfips^year,
  data = data %>% filter(year <= 1988),
  weights = ~popwt,
  cluster = ~fips
)

print(summary(mod_early_all))

# Extract coefficients
extract_coefs <- function(mod, label) {
  cn  <- names(coef(mod))
  idx <- grepl("^event_time_binned::(-?[0-9]+)$", cn)
  coef_df <- data.frame(
    event_time  = as.integer(gsub(".*::(-?[0-9]+)$", "\\1", cn[idx])),
    coefficient = coef(mod)[idx],
    se          = sqrt(diag(vcov(mod)))[idx]
  )
  coef_df$ci_lower <- coef_df$coefficient - 1.96 * coef_df$se
  coef_df$ci_upper <- coef_df$coefficient + 1.96 * coef_df$se
  
  # Add reference period
  coef_df <- rbind(coef_df,
    data.frame(event_time = -1, coefficient = 0, se = 0, ci_lower = 0, ci_upper = 0))
  coef_df <- coef_df[order(coef_df$event_time), ]
  coef_df$spec <- label
  rownames(coef_df) <- NULL
  
  cat(sprintf("\n%s coefficients:\n", label))
  print(coef_df %>% mutate(
    coef = sprintf("%.3f", coefficient),
    se_  = sprintf("%.3f", se),
    ci   = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
    sig  = case_when(
      abs(coefficient / se) > 2.576 ~ "***",
      abs(coefficient / se) > 1.96  ~ "**",
      abs(coefficient / se) > 1.645 ~ "*",
      TRUE ~ ""
    )
  ) %>% select(event_time, coef, se_, ci, sig))
  
  # Joint tests
  pre_names <- paste0("event_time_binned::", -6:-2)
  pre_names <- pre_names[pre_names %in% names(coef(mod))]
  if (length(pre_names) > 1) {
    wt <- wald(mod, pre_names)
    cat(sprintf("Joint pre-trend test (t<=-2): F=%.2f, p=%.4f\n", wt$stat, wt$p))
  }
  
  post_names <- paste0("event_time_binned::", 0:20)
  post_names <- post_names[post_names %in% names(coef(mod))]
  if (length(post_names) > 1) {
    wt <- wald(mod, post_names)
    cat(sprintf("Joint post-treatment test (t>=0): F=%.2f, p=%.4f\n", wt$stat, wt$p))
  }
  
  return(coef_df)
}

coef_early_all <- extract_coefs(mod_early_all, "Early CHCs, All Counties (1959-1988)")

# ---- Panel B: All CHCs, All Counties, 1959-1988 ----
# For "All CHCs" the original code uses exp2 instead of exp1
# In our data, event_time uses cmhc_year_exp which should include all CHCs
# To replicate "early only" vs "all", we'd need the chc_year_exp variable too.
# For now, our cmhc_year_exp already filters to <= 1975, so this IS the early CHC spec.

# ==========================================
# STEP 8: Plot replication figure
# ==========================================

library(ggplot2)

p_rep <- ggplot(coef_early_all, aes(x = event_time, y = coefficient)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.3) +
  geom_vline(xintercept = -1, linetype = "solid", color = "black", linewidth = 0.3) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "navy", alpha = 0.15) +
  geom_point(color = "maroon", size = 2, shape = 16) +
  geom_line(color = "maroon", linewidth = 0.8) +
  labs(
    title = "Effect of CMHC Opening on Age-Adjusted Mortality Rate",
    subtitle = "Replication of Bailey & Goodman-Bacon (2015) Figure 5 specification",
    x = "Years Since CHC Establishment",
    y = "Deaths per 100,000 Residents"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("cmhc_replication_figure5.png", p_rep, width = 10, height = 6, dpi = 300)
cat("\nPlot saved to: cmhc_replication_figure5.png\n")

write.csv(coef_early_all, "cmhc_replication_figure5_coefs.csv", row.names = FALSE)
cat("Coefficients saved to: cmhc_replication_figure5_coefs.csv\n")
