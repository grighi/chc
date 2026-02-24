#!/usr/bin/env Rscript
# ============================================================================
# MASTER BUILD SCRIPT
# "Community Mental Health Centers: Then and Now"
# Gio Righi, UCLA
#
# Usage:  Rscript build/build.R
#         (or: cd build && Rscript build.R)
#
# Set FORCE_REBUILD=TRUE environment variable to regenerate all data.
# ============================================================================

cat("================================================================\n")
cat("BUILD: Community Mental Health Centers -- Then and Now\n")
cat(sprintf("Started: %s\n", Sys.time()))
cat("================================================================\n\n")

build_start <- Sys.time()

# ── Setup ──
source("build/00_setup.R")

# ── Data Preparation ──
cat("\n========== DATA PREPARATION ==========\n\n")
source("build/01_prep_cmhc_openings.R")
source("build/02_prep_aer_panel.R")
source("build/03_prep_incarceration.R")
source("build/04_prep_ccbhc_grants.R")
source("build/05_prep_ccbhc_mortality.R")
source("build/06_prep_ccbhc_panel.R")
source("build/07_prep_national_mortality.R")

# ── Figures and Tables: CMHC Era ──
cat("\n========== CMHC-ERA OUTPUTS ==========\n\n")
source("build/10_fig01_federal_budgets.R")
source("build/11_fig02_national_mortality.R")
source("build/12_fig03_cmhc_rollout_map.R")
source("build/13_table01_balance.R")
source("build/14_fig04_timing_exogeneity.R")
source("build/15_fig05_cmhc_es_ad.R")
source("build/16_fig06_cmhc_es_eld.R")
source("build/17_table02_dd_robustness.R")
source("build/18_fig07_cmhc_chc_robust.R")
source("build/19_table03_cause_decomp.R")
source("build/20_fig_cmhc_race.R")
source("build/21_fig_cmhc_heterogeneity.R")
source("build/22_table_cmhc_incarceration.R")

# ── Figures and Tables: CCBHC Era ──
cat("\n========== CCBHC-ERA OUTPUTS ==========\n\n")
source("build/23_fig08_ccbhc_es_mort.R")
source("build/24_fig09_ccbhc_robust.R")
source("build/25_fig10_ccbhc_es_inc.R")
source("build/26_table_ccbhc_heterogeneity.R")

# ── Cross-Era and Appendix ──
cat("\n========== CROSS-ERA AND APPENDIX ==========\n\n")
source("build/27_fig_side_by_side.R")
source("build/28_appendix_poisson.R")
source("build/29_appendix_bacon.R")
source("build/30_appendix_ccbhc_placebo.R")
source("build/31_appendix_dod_decomp.R")
source("build/32_fig_ccbhc_es_dod.R")

# ── Generate LaTeX tables from CSV outputs ──
cat("\n========== GENERATING LATEX TABLES ==========\n\n")
py_script <- file.path(BUILD_DIR, "fill_tables.py")
if (file.exists(py_script)) {
  system2("python3", py_script, stdout = "", stderr = "")
  cat("  LaTeX tables generated via fill_tables.py\n")
} else {
  cat("  fill_tables.py not found — skipping LaTeX table generation.\n")
}

# ── Done ──
build_end <- Sys.time()
cat("\n================================================================\n")
cat(sprintf("BUILD COMPLETE: %s\n", Sys.time()))
cat(sprintf("Total time: %.1f minutes\n", difftime(build_end, build_start, units = "mins")))
cat("================================================================\n")

# List outputs
cat("\nGenerated figures:\n")
figs <- list.files(FIG_DIR, full.names = FALSE)
cat(paste("  ", figs, collapse = "\n"), "\n")

cat("\nGenerated tables:\n")
tabs <- list.files(TABLE_DIR, full.names = FALSE)
cat(paste("  ", tabs, collapse = "\n"), "\n")

cat("\nGenerated data:\n")
csvs <- list.files(CSV_DIR, full.names = FALSE)
cat(paste("  ", csvs, collapse = "\n"), "\n")
