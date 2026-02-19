#!/usr/bin/env Rscript
# Replicate Table D.1: Predicting the timing of CHC grant
# Translation of tableD1.do from Stata to R

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(lmtest)
  library(sandwich)
  library(car)
})

# Read the data
# Note: Update this path to point to your aer_data.dta file
data <- read_dta("aer_data/aer_data.dta")

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

# MODEL 3: EVER TREATED VS NEVER TREATED (BINARY)
cat("\n\n-----------------------------------------------------------\n")
cat("Model 3: Ever vs Never Treated (Binary Dependent Variable)\n")
cat("-----------------------------------------------------------\n")

# Create binary treatment variable: 1 if ever received CHC, 0 otherwise
data <- data %>%
  mutate(ever_treated = ifelse(is.na(chc_year_exp), 0, ifelse(chc_year_exp <= 1974, 1, 0)))

# Keep only year 1960
data <- data %>% filter(year == 1960)

cat("\nTreatment distribution:\n")
cat("Ever treated (CHC by 1974):", sum(data$ever_treated == 1, na.rm = TRUE), "\n")
cat("Never treated:", sum(data$ever_treated == 0, na.rm = TRUE), "\n\n")

# Weighted LPM
model3_weighted <- lm(ever_treated ~ `_60pcturban` + `_60pctrurf` + `_60pct04years` + 
               `_60pctmt64years` + `_60pctnonwhit` + `_60pctmt12schl` + 
               `_60pctlt4schl` + `_pct59inclt3k` + `_pct59incmt10k` + 
               `_tot_act_md` + amr + damr,
             data = data,
             weights = copop)

robust_se3_w <- sqrt(diag(vcovHC(model3_weighted, type = "HC1")))

cat("Model 3a: Weighted LPM\n")
cat("Dependent variable: ever_treated (1 if CHC by 1974, 0 otherwise)\n")
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

cat("Dependent variable: ever_treated (1 if CHC by 1974, 0 otherwise)\n")
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
cat("TABLE D.1: Predicting the timing of CHC grant\n")
cat("==========================================================\n\n")

# Keep only year 1960
data <- data %>% filter(year == 1960)

# Filter data: chc_year_exp <= 1974
data <- data %>% filter(chc_year_exp <= 1974)

# MODEL 1: WEIGHTED LPM
cat("-----------------------------------------------------------\n")
cat("Model 1: Weighted LPM (weighted by county population)\n")
cat("-----------------------------------------------------------\n")

# Regression with county population weights
model1 <- lm(chc_year_exp ~ `_60pcturban` + `_60pctrurf` + `_60pct04years` + 
               `_60pctmt64years` + `_60pctnonwhit` + `_60pctmt12schl` + 
               `_60pctlt4schl` + `_pct59inclt3k` + `_pct59incmt10k` + 
               `_tot_act_md` + amr + damr,
             data = data,
             weights = copop)

# Get robust standard errors
robust_se1 <- sqrt(diag(vcovHC(model1, type = "HC1")))

# Print results
cat("\nDependent variable: chc_year_exp (year CHC opened)\n")
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
# Wald test with robust variance

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

# MODEL 2: UNWEIGHTED LPM
cat("\n\n-----------------------------------------------------------\n")
cat("Model 2: Unweighted LPM\n")
cat("-----------------------------------------------------------\n")

model2 <- lm(chc_year_exp ~ `_60pcturban` + `_60pctrurf` + `_60pct04years` + 
               `_60pctmt64years` + `_60pctnonwhit` + `_60pctmt12schl` + 
               `_60pctlt4schl` + `_pct59inclt3k` + `_pct59incmt10k` + 
               `_tot_act_md` + amr + damr,
             data = data)

robust_se2 <- sqrt(diag(vcovHC(model2, type = "HC1")))

cat("\nDependent variable: chc_year_exp (year CHC opened)\n")
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
cat("End of Table D.1 replication\n")
cat("==========================================================\n")