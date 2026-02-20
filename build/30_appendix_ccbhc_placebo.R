#!/usr/bin/env Rscript
# ============================================================================
# 30_appendix_ccbhc_placebo.R — CCBHC Placebo: Ages 50+
# ============================================================================
log_section("30: Appendix — CCBHC 50+ Placebo")

# Check for WONDER 50+ county-level data
wonder_50_files <- list.files(file.path(WONDER_DIR, "data"),
                               pattern = "50|45_64|older|adults_all",
                               full.names = TRUE)

if (length(wonder_50_files) == 0) {
  cat("  WARNING: No 50+ WONDER data found. Skipping CCBHC placebo.\n")
  cat("  To generate this figure, pull county-level WONDER data for ages 50+\n")
  cat("  using wonder_mortality/pull_wonder_unified.py.\n")
} else {
  cat(sprintf("  Found %d potential 50+ WONDER files. Attempting to load...\n",
              length(wonder_50_files)))

  tryCatch({
    # Try to load and aggregate
    wonder_50 <- lapply(wonder_50_files, function(f) {
      tryCatch(arrow::read_parquet(f), error = function(e) NULL)
    })
    wonder_50 <- Filter(Negate(is.null), wonder_50)

    if (length(wonder_50) > 0) {
      cat("  Loaded WONDER 50+ data. Processing...\n")
      # Would need to construct panel and run same CCBHC event study
      # Placeholder for when data is available
    }
  }, error = function(e) {
    cat(sprintf("  Could not load 50+ data: %s\n", e$message))
  })
}
