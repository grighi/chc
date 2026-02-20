#!/usr/bin/env Rscript
# ============================================================================
# 12_fig03_cmhc_rollout_map.R — CMHC Rollout Map (choropleth)
# ============================================================================
log_section("12: Figure 3 — CMHC Rollout Map")

tryCatch({
  if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("tigris", quietly = TRUE)) {
    cat("  WARNING: sf or tigris not installed. Skipping map.\n")
  } else {
    library(sf)
    library(tigris)
    options(tigris_use_cache = TRUE)

    cmhc_openings <- read_csv(file.path(DATA_DIR, "cmhc_openings.csv"), show_col_types = FALSE)
    cmhc_openings$fips <- sprintf("%05d", cmhc_openings$fips)

    counties <- tigris::counties(cb = TRUE, year = 2020, resolution = "20m") %>%
      filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

    counties <- counties %>%
      left_join(cmhc_openings %>% mutate(fips = as.character(fips)),
                by = c("GEOID" = "fips"))

    counties <- counties %>%
      mutate(cohort = case_when(
        cmhc_year_exp == 1971 ~ "1971 (N=208)",
        cmhc_year_exp == 1973 ~ "1973 (N=58)",
        cmhc_year_exp == 1975 ~ "1975 (N=77)",
        cmhc_year_exp == 1977 ~ "1977 (N=715)",
        cmhc_year_exp == 1981 ~ "1981 (N=119)",
        TRUE                  ~ "Never"
      ))
    counties$cohort <- factor(counties$cohort,
      levels = c("1971 (N=208)", "1973 (N=58)", "1975 (N=77)",
                 "1977 (N=715)", "1981 (N=119)", "Never"))

    cohort_colors <- c(
      "1971 (N=208)" = "#08519c",
      "1973 (N=58)"  = "#3182bd",
      "1975 (N=77)"  = "#6baed6",
      "1977 (N=715)" = "#bdd7e7",
      "1981 (N=119)" = "#eff3ff",
      "Never"        = "gray85"
    )

    p <- ggplot(counties) +
      geom_sf(aes(fill = cohort), color = "gray50", linewidth = 0.05) +
      scale_fill_manual(values = cohort_colors, name = "CMHC Opening Year") +
      labs(title = "Geographic Distribution of CMHC Openings") +
      theme_void(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        legend.position = "bottom"
      ) +
      guides(fill = guide_legend(nrow = 1))

    save_fig(p, "fig03_cmhc_rollout_map.pdf", width = 12, height = 8)
  }
}, error = function(e) {
  cat(sprintf("  WARNING: Map generation failed: %s\n", e$message))
})
