#!/usr/bin/env Rscript
# Replicate Table D.1 using CMHC (Community Mental Health Centers) instead of CHC
# This script reads CMHC shapefiles and creates a CMHC opening year variable

library(haven)
library(dplyr)
library(readxl)
library(lmtest)
library(sandwich)
library(car)

# ==========================================
# STEP 1: Load CMHC data from Excel files
# ==========================================

cat("Loading CMHC data from Excel files...\n")

# Initialize data frame to store CMHC opening years by county
cmhc_years <- c(1971, 1973, 1975, 1977, 1979, 1981)
cmhc_data_list <- list()

for (year in cmhc_years) {
  # Try multiple possible file naming patterns
  possible_files <- c(
    paste0("cmhc_data/CMHC", year, " - Copy.xls"),
    # paste0("cmhc_data/CMHC", year, " - Copy.xlsx"),
    paste0("cmhc_data/CMHC", year, ".xls"),
    paste0("cmhc_data/CMHC", year, ".xlsx"),
    paste0("cmhc_data/cmhc", year, ".xls"),
    paste0("cmhc_data/cmhc_", year, ".xls")
  )
  
  file_found <- FALSE
  
  for (xls_file in possible_files) {
    if (file.exists(xls_file)) {
      cat("  Reading", xls_file, "...\n")
      
      # Read Excel file
      tryCatch({
        cmhc_df <- read_excel(xls_file)
        
        # The file has columns: GEOID, count, CMHC[year]
        # Extract GEOID (this is county FIPS code) and add the year
        cmhc_df <- data.frame(
          fips = as.numeric(cmhc_df$GEOID),
          cmhc_year = year
        )
        
        # Remove any NA rows
        cmhc_df <- cmhc_df %>% filter(!is.na(fips))
        
        cmhc_data_list[[as.character(year)]] <- cmhc_df
        file_found <- TRUE
        break
      }, error = function(e) {
        warning(paste("Error reading", xls_file, ":", e$message))
      })
    }
  }
  
  if (!file_found) {
    warning(paste("No CMHC file found for year", year))
  }
}

# Combine all CMHC data
cmhc_all <- bind_rows(cmhc_data_list)

# For counties with multiple CMHCs or appearing in multiple years, 
# take the earliest opening year
cmhc_openings <- cmhc_all %>%
  group_by(fips) %>%
  summarize(cmhc_year_exp = min(cmhc_year, na.rm = TRUE)) %>%
  ungroup()

cat("Found", nrow(cmhc_openings), "counties with CMHC openings\n")
cat("Year distribution:\n")
print(table(cmhc_openings$cmhc_year_exp))
cat("\n")

# ==========================================
# STEP 2: Load base data and merge
# ==========================================

cat("Loading base data...\n")

# Read the data
# Note: Update this path to point to your aer_data.dta file
data <- read_dta("aer_data/aer_data.dta")

# Filter data: keep only early years (analogous to chc_year_exp <= 1974)
# Since CMHC data goes to 1981, we'll use cmhc_year_exp <= 1981
data <- data %>% 
  select(-chc_year_exp) %>%  # Remove original CHC variable
  left_join(cmhc_openings, by = "fips")

# Replace missing CMHC years with a large number (never opened)
data <- data %>%
  mutate(cmhc_year_exp = ifelse(is.na(cmhc_year_exp), 9999, cmhc_year_exp))

# Filter to keep counties that opened CMHC by 1981 (or never opened)
data <- data %>% filter(cmhc_year_exp <= 1981 | cmhc_year_exp == 9999)

# Drop NY/LA/Chicago
data <- data %>%
  filter(!(stfips == 36 & cofips == 61)) %>%  # NYC
  filter(!(stfips == 6 & cofips == 37)) %>%   # LA
  filter(!(stfips == 17 & cofips == 31))      # Chicago

# Create amr65: AMR in 1965 for each county
data <- data %>%
  group_by(fips) %>%
  mutate(amr65 = sum(amr * (year == 1965), na.rm = TRUE)) %>%
  ungroup()

# Create damr: change in AMR from 1965
data <- data %>%
  mutate(damr = amr65 - amr)

# Scale total active MDs to thousands
data <- data %>%
  mutate(`_tot_act_md` = `_tot_act_md` / 1000)


# ==========================================
# MODEL 3: EVER TREATED VS NEVER TREATED (BINARY)
# ==========================================

cat("\n\n-----------------------------------------------------------\n")
cat("Model 3: Ever vs Never Treated (Binary Dependent Variable)\n")
cat("-----------------------------------------------------------\n")

# # Create binary treatment variable: 1 if ever received CHC, 0 otherwise
# data <- data %>%
#   mutate(ever_treated = ifelse(is.na(chc_year_exp), 0, ifelse(chc_year_exp <= 1974, 1, 0)))

# Create binary treatment variable: 1 if ever received CMHC, 0 otherwise
data <- data %>%
  mutate(ever_treated = ifelse(cmhc_year_exp < 9999, 1, 0))

# Keep only year 1960
data <- data %>% filter(year == 1960)


cat("\nTreatment distribution:\n")
cat("Ever treated (received CMHC):", sum(data$ever_treated == 1), "\n")
cat("Never treated:", sum(data$ever_treated == 0), "\n\n")

# Weighted LPM
model3_weighted <- lm(ever_treated ~ `_60pcturban` + `_60pctrurf` + `_60pct04years` + 
               `_60pctmt64years` + `_60pctnonwhit` + `_60pctmt12schl` + 
               `_60pctlt4schl` + `_pct59inclt3k` + `_pct59incmt10k` + 
               `_tot_act_md` + amr + damr,
             data = data,
             weights = copop)

robust_se3_w <- sqrt(diag(vcovHC(model3_weighted, type = "HC1")))

cat("Model 3a: Weighted LPM\n")
cat("Dependent variable: ever_treated (1 if CMHC received, 0 otherwise)\n")
cat("Number of observations:", nobs(model3_weighted), "\n")
cat("R-squared:", summary(model3_weighted)$r.squared, "\n\n")

cat("Coefficients:\n")
coef_table3_w <- data.frame(
  Variable = names(coef(model3_weighted)),
  Coefficient = round(coef(model3_weighted), 3),
  Robust_SE = robust_se3_w,
  t_value = coef(model3_weighted) / robust_se3_w,
  p_value = round(2 * pt(-abs(coef(model3_weighted) / robust_se3_w), df = model3_weighted$df.residual), 4)
)
rownames(coef_table3_w) <- NULL
print(coef_table3_w, digits = 4)

# F-tests for weighted model
cat("\n\nF-Test 1: All non-urban variables\n")
f_test1_3w <- linearHypothesis(model3_weighted, test1_vars, vcov = vcovHC(model3_weighted, type = "HC1"))
cat("F-statistic:", f_test1_3w$F[2], "\n")
cat("p-value:", f_test1_3w$`Pr(>F)`[2], "\n")

cat("\nF-Test 2: All non-urban, non-MD variables\n")
f_test2_3w <- linearHypothesis(model3_weighted, test2_vars, vcov = vcovHC(model3_weighted, type = "HC1"))
cat("F-statistic:", f_test2_3w$F[2], "\n")
cat("p-value:", f_test2_3w$`Pr(>F)`[2], "\n")

# Unweighted LPM
cat("\n\nModel 3b: Unweighted LPM\n")
model3_unweighted <- lm(ever_treated ~ `_60pcturban` + `_60pctrurf` + `_60pct04years` + 
               `_60pctmt64years` + `_60pctnonwhit` + `_60pctmt12schl` + 
               `_60pctlt4schl` + `_pct59inclt3k` + `_pct59incmt10k` + 
               `_tot_act_md` + amr + damr,
             data = data)

robust_se3_uw <- sqrt(diag(vcovHC(model3_unweighted, type = "HC1")))

cat("Dependent variable: ever_treated (1 if CMHC received, 0 otherwise)\n")
cat("Number of observations:", nobs(model3_unweighted), "\n")
cat("R-squared:", summary(model3_unweighted)$r.squared, "\n\n")

cat("Coefficients:\n")
coef_table3_uw <- data.frame(
  Variable = names(coef(model3_unweighted)),
  Coefficient = round(coef(model3_unweighted), 3),
  Robust_SE = robust_se3_uw,
  t_value = coef(model3_unweighted) / robust_se3_uw,
  p_value = round(2 * pt(-abs(coef(model3_unweighted) / robust_se3_uw), df = model3_unweighted$df.residual), 4)
)
rownames(coef_table3_uw) <- NULL
print(coef_table3_uw, digits = 4)

# F-tests for unweighted model
cat("\n\nF-Test 1: All non-urban variables\n")
f_test1_3uw <- linearHypothesis(model3_unweighted, test1_vars, vcov = vcovHC(model3_unweighted, type = "HC1"))
cat("F-statistic:", f_test1_3uw$F[2], "\n")
cat("p-value:", f_test1_3uw$`Pr(>F)`[2], "\n")

cat("\nF-Test 2: All non-urban, non-MD variables\n")
f_test2_3uw <- linearHypothesis(model3_unweighted, test2_vars, vcov = vcovHC(model3_unweighted, type = "HC1"))
cat("F-statistic:", f_test2_3uw$F[2], "\n")
cat("p-value:", f_test2_3uw$`Pr(>F)`[2], "\n")


cat("\n==========================================================\n")
cat("TABLE D.1 (CMHC VERSION): Predicting the timing of CMHC grant\n")
cat("==========================================================\n\n")
cat("Note: Using CMHC opening year instead of CHC opening year\n")
cat("Counties with CMHCs:", sum(data$cmhc_year_exp < 9999), "\n")
cat("Counties without CMHCs:", sum(data$cmhc_year_exp == 9999), "\n\n")

# ==========================================
# MODEL 1: WEIGHTED LPM
# ==========================================

cat("-----------------------------------------------------------\n")
cat("Model 1: Weighted LPM (weighted by county population)\n")
cat("-----------------------------------------------------------\n")


# Keep only year 1960
data <- data %>% filter(year == 1960)

# Filter data: chc_year_exp <= 1974
data <- data %>% filter(cmhc_year_exp <= 1976)

# Regression with county population weights
model1 <- lm(cmhc_year_exp ~ `_60pcturban` + `_60pctrurf` + `_60pct04years` + 
               `_60pctmt64years` + `_60pctnonwhit` + `_60pctmt12schl` + 
               `_60pctlt4schl` + `_pct59inclt3k` + `_pct59incmt10k` + 
               `_tot_act_md` + amr + damr,
             data = data,
             weights = copop)

# Get robust standard errors
robust_se1 <- sqrt(diag(vcovHC(model1, type = "HC1")))

# Print results
cat("\nDependent variable: cmhc_year_exp (year CMHC opened)\n")
cat("Number of observations:", nobs(model1), "\n")
cat("R-squared:", summary(model1)$r.squared, "\n\n")

cat("Coefficients:\n")
coef_table1 <- data.frame(
  Variable = names(coef(model1)),
  Coefficient = round(coef(model1), 3),
  Robust_SE = robust_se1,
  t_value = coef(model1) / robust_se1,
  p_value = round(pt(-abs(coef(model1) / robust_se1), df = model1$df.residual), 4)
)
rownames(coef_table1) <- NULL
print(coef_table1, digits = 4)

# F-test 1: all non-urban variables
cat("\n\nF-Test 1: All non-urban variables\n")
test1_vars <- c("`_60pctrurf`", "`_60pct04years`", "`_60pctmt64years`", "`_60pctnonwhit`",
        "`_60pctmt12schl`", "`_60pctlt4schl`", "`_pct59inclt3k`", "`_pct59incmt10k`",
        "`_tot_act_md`", "amr", "damr")
f_test1 <- linearHypothesis(model1, test1_vars, vcov = vcovHC(model1, type = "HC1"))
cat("F-statistic:", f_test1$F[2], "\n")
cat("p-value:", f_test1$`Pr(>F)`[2], "\n")

# F-test 2: all non-urban, non-MD variables
cat("\nF-Test 2: All non-urban, non-MD variables\n")
test2_vars <- c("`_60pctrurf`", "`_60pct04years`", "`_60pctmt64years`", "`_60pctnonwhit`",
        "`_60pctmt12schl`", "`_60pctlt4schl`", "`_pct59inclt3k`", "`_pct59incmt10k`",
        "amr", "damr")
f_test2 <- linearHypothesis(model1, test2_vars, vcov = vcovHC(model1, type = "HC1"))
cat("F-statistic:", f_test2$F[2], "\n")
cat("p-value:", f_test2$`Pr(>F)`[2], "\n")

# ==========================================
# MODEL 2: UNWEIGHTED LPM
# ==========================================

cat("\n\n-----------------------------------------------------------\n")
cat("Model 2: Unweighted LPM\n")
cat("-----------------------------------------------------------\n")

model2 <- lm(cmhc_year_exp ~ `_60pcturban` + `_60pctrurf` + `_60pct04years` + 
               `_60pctmt64years` + `_60pctnonwhit` + `_60pctmt12schl` + 
               `_60pctlt4schl` + `_pct59inclt3k` + `_pct59incmt10k` + 
               `_tot_act_md` + amr + damr,
             data = data)

robust_se2 <- sqrt(diag(vcovHC(model2, type = "HC1")))

cat("\nDependent variable: cmhc_year_exp (year CMHC opened)\n")
cat("Number of observations:", nobs(model2), "\n")
cat("R-squared:", summary(model2)$r.squared, "\n\n")

cat("Coefficients:\n")
coef_table2 <- data.frame(
  Variable = names(coef(model2)),
  Coefficient = round(coef(model2), 3),
  Robust_SE = robust_se2,
  t_value = coef(model2) / robust_se2,
  p_value = round(pt(-abs(coef(model2) / robust_se2), df = model2$df.residual), 4)
)
rownames(coef_table2) <- NULL
print(coef_table2, digits = 4)

# F-test 1: all non-urban variables
cat("\n\nF-Test 1: All non-urban variables\n")
f_test1_uw <- linearHypothesis(model2, test1_vars, vcov = vcovHC(model2, type = "HC1"))
cat("F-statistic:", f_test1_uw$F[2], "\n")
cat("p-value:", f_test1_uw$`Pr(>F)`[2], "\n")

# F-test 2: all non-urban, non-MD variables
cat("\nF-Test 2: All non-urban, non-MD variables\n")
f_test2_uw <- linearHypothesis(model2, test2_vars, vcov = vcovHC(model2, type = "HC1"))
cat("F-statistic:", f_test2_uw$F[2], "\n")
cat("p-value:", f_test2_uw$`Pr(>F)`[2], "\n")

cat("\n==========================================================\n")
cat("End of Table D.1 (CMHC version) replication\n")
cat("==========================================================\n")

# ==========================================
# OPTIONAL: Export results for further use
# ==========================================

# Save the CMHC openings data
write.csv(cmhc_openings, "cmhc_openings_by_county.csv", row.names = FALSE)
cat("\nCMHC openings by county saved to: cmhc_openings_by_county.csv\n")

