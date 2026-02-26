#!/usr/bin/env Rscript
# ============================================================================
# 00_setup.R — Package loading, paths, global parameters, helper functions
# "Community Mental Health Centers: Then and Now"
# ============================================================================

set.seed(42)

# ── Package loading ─────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  # Core
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(readr)

  # Data I/O
  library(fst)
  library(haven)
  library(arrow)

  # Estimation
  library(fixest)
  library(lmtest)
  library(sandwich)
  library(MatchIt)

  # Plotting
  library(ggplot2)
  library(patchwork)

  # Tables
  library(modelsummary)
})

# ── Paths (relative to project root) ────────────────────────────────────────
# Detect project root: if running from project root, "build" is a subdirectory;
# if running from build/, parent is the root.
ROOT <- tryCatch({
  # When sourced, use the file path
  script_dir <- dirname(sys.frame(1)$ofile)
  normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
}, error = function(e) {
  # Fallback: check if we're in root or in build/
  if (dir.exists("build") && dir.exists("aer_data")) {
    normalizePath(".")
  } else if (dir.exists("../aer_data")) {
    normalizePath("..")
  } else {
    normalizePath(".")
  }
})
BUILD_DIR   <- file.path(ROOT, "build")
DATA_DIR    <- file.path(BUILD_DIR, "data")
FIG_DIR     <- file.path(BUILD_DIR, "output", "figures")
TABLE_DIR   <- file.path(BUILD_DIR, "output", "tables")
CSV_DIR     <- file.path(BUILD_DIR, "output", "data")
LOG_DIR     <- file.path(BUILD_DIR, "logs")

# Raw data paths
AER_DIR     <- file.path(ROOT, "aer_data")
CMHC_DIR    <- file.path(ROOT, "cmhc_data")
CCBHC_DIR   <- file.path(ROOT, "ccbhc")
FED_DIR     <- file.path(ROOT, "federal_budgets")
NBER_DIR    <- file.path(ROOT, "raw", "nber_mortality")
WONDER_DIR  <- file.path(ROOT, "raw", "wonder_mortality")

# Ensure output directories exist
for (d in c(DATA_DIR, FIG_DIR, TABLE_DIR, CSV_DIR, LOG_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ── CMHC-era parameters ────────────────────────────────────────────────────
CMHC_MAX_COHORT   <- 1981L
CMHC_PANEL_END    <- 1988L
CMHC_EVENT_MIN    <- -6L
CMHC_EVENT_MAX    <- 13L
CMHC_REF_PERIOD   <- -1L
CMHC_PLOT_MAX     <- 13L

# ── CCBHC-era parameters ───────────────────────────────────────────────────
CCBHC_EVENT_MIN   <- -5L
CCBHC_EVENT_MAX   <- 5L
CCBHC_REF_PERIOD  <- -1L
CCBHC_PANEL_START <- 2010L
CCBHC_TREAT_YEARS <- 2018L:2024L

# ── Excluded counties ──────────────────────────────────────────────────────
DROP_FIPS <- c(36061L, 6037L, 17031L)  # NYC, LA, Chicago

# ── Medicaid waiver state FIPS ─────────────────────────────────────────────
WAIVER_ST_FIPS <- c("27","29","32","34","36","40","41","42")

# ── Common regression specification ───────────────────────────────────────
CMHC_CONTROLS <- "D_tot_act_md_t + H_bpc + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`"
CMHC_FE       <- "fips + year^Durb + year^stfips"

# ── Plotting defaults ─────────────────────────────────────────────────────
PLOT_WIDTH     <- 10
PLOT_HEIGHT    <- 6
PLOT_DPI       <- 300
OUTPUT_FORMAT  <- "pdf"

FORCE_REBUILD  <- as.logical(Sys.getenv("FORCE_REBUILD", "FALSE"))

# ── Publication-quality ggplot theme ──────────────────────────────────────
theme_paper <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle    = element_text(size = base_size - 1, color = "gray30", hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      strip.text       = element_text(face = "bold")
    )
}

# ── Helper: extract event-study coefficients ──────────────────────────────
extract_es_coefs <- function(model, prefix = "event_time_binned",
                              ref = -1L, max_et = 14L, drop_binned = -999L) {
  cn  <- names(coef(model))
  pat <- sprintf("^%s::(-?[0-9]+)$", prefix)
  idx <- grepl(pat, cn)

  coef_df <- data.frame(
    event_time  = as.integer(gsub(pat, "\\1", cn[idx])),
    coefficient = coef(model)[idx],
    se          = sqrt(diag(vcov(model)))[idx],
    row.names   = NULL
  )

  # Drop never-treated bin and far-right bins
  coef_df <- coef_df[coef_df$event_time != drop_binned & coef_df$event_time <= max_et, ]
  coef_df$ci_lower <- coef_df$coefficient - 1.96 * coef_df$se
  coef_df$ci_upper <- coef_df$coefficient + 1.96 * coef_df$se

  # Add reference period
  coef_df <- rbind(
    coef_df,
    data.frame(event_time = ref, coefficient = 0, se = 0, ci_lower = 0, ci_upper = 0)
  )
  coef_df <- coef_df[order(coef_df$event_time), ]
  rownames(coef_df) <- NULL
  coef_df
}

# ── Helper: plot event study ──────────────────────────────────────────────
plot_event_study <- function(coef_df, title, subtitle = NULL,
                              xlab = "Years Relative to Treatment",
                              ylab = "Change in Deaths per 100,000",
                              color = "steelblue", ref_line = -0.5) {
  p <- ggplot(coef_df, aes(x = event_time, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = ref_line, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = color, alpha = 0.2) +
    geom_point(color = color, size = 2) +
    geom_line(color = color, linewidth = 0.8) +
    labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    theme_paper()
  p
}

# ── Helper: run pre/post joint tests ─────────────────────────────────────
run_joint_tests <- function(model, prefix = "event_time_binned",
                             pre_range = NULL, post_range = NULL) {
  results <- list()

  if (!is.null(pre_range)) {
    pre_names <- paste0(prefix, "::", pre_range)
    pre_names <- pre_names[pre_names %in% names(coef(model))]
    if (length(pre_names) > 1) {
      wt <- wald(model, pre_names)
      results$pre_F <- wt$stat
      results$pre_p <- wt$p
      cat(sprintf("  Pre-trend test: F = %.2f, p = %.4f\n", wt$stat, wt$p))
    }
  }

  if (!is.null(post_range)) {
    post_names <- paste0(prefix, "::", post_range)
    post_names <- post_names[post_names %in% names(coef(model))]
    if (length(post_names) > 1) {
      wt <- wald(model, post_names)
      results$post_F <- wt$stat
      results$post_p <- wt$p
      cat(sprintf("  Post-treatment test: F = %.2f, p = %.4f\n", wt$stat, wt$p))
    }
  }

  invisible(results)
}

# ── Helper: save figure in correct format ─────────────────────────────────
save_fig <- function(plot, filename, width = PLOT_WIDTH, height = PLOT_HEIGHT) {
  ext <- paste0(".", OUTPUT_FORMAT)
  if (!grepl(paste0("\\", ext, "$"), filename)) {
    filename <- sub("\\.[^.]+$", ext, filename)
  }
  path <- file.path(FIG_DIR, filename)
  ggsave(path, plot, width = width, height = height, dpi = PLOT_DPI, device = OUTPUT_FORMAT)
  cat(sprintf("  Saved: %s\n", path))
  invisible(path)
}

# ── Helper: save CSV output ──────────────────────────────────────────────
save_csv <- function(df, filename) {
  path <- file.path(CSV_DIR, filename)
  write_csv(df, path)
  cat(sprintf("  Saved: %s\n", path))
  invisible(path)
}

# ── Logging ──────────────────────────────────────────────────────────────
log_section <- function(title) {
  cat(sprintf("\n========== %s ==========\n", toupper(title)))
  cat(sprintf("  Time: %s\n\n", Sys.time()))
}

cat("00_setup.R loaded successfully.\n")
cat(sprintf("  Project root: %s\n", ROOT))
cat(sprintf("  Output format: %s\n", OUTPUT_FORMAT))
