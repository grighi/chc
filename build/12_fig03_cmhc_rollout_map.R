#!/usr/bin/env Rscript
# ============================================================================
# 12_fig03_cmhc_rollout_map.R — CMHC Rollout Map (choropleth)
# ============================================================================
log_section("12: Figure 3 — CMHC Rollout Map")

tryCatch({
  if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("tigris", quietly = TRUE) || !requireNamespace("patchwork", quietly = TRUE)) {
    cat("  WARNING: sf, tigris, or patchwork not installed. Skipping map.\n")
  } else {
    library(sf)
    library(tigris)
    library(patchwork)
    options(tigris_use_cache = TRUE)

    cmhc_openings <- read_csv(file.path(DATA_DIR, "cmhc_openings.csv"), show_col_types = FALSE)
    cmhc_openings$fips <- sprintf("%05d", cmhc_openings$fips)

    # Main 48 states
    counties <- tigris::counties(cb = TRUE, year = 2020, resolution = "20m") %>%
      filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

    counties <- counties %>%
      left_join(cmhc_openings %>% mutate(fips = as.character(fips)),
                by = c("GEOID" = "fips"))

    # Alaska
    ak_counties <- tigris::counties(cb = TRUE, year = 2020, resolution = "20m", state = "02") %>%
      left_join(cmhc_openings %>% mutate(fips = as.character(fips)),
                by = c("GEOID" = "fips"))

    # Hawaii
    hi_counties <- tigris::counties(cb = TRUE, year = 2020, resolution = "20m", state = "15") %>%
      left_join(cmhc_openings %>% mutate(fips = as.character(fips)),
                by = c("GEOID" = "fips"))

    # Combine all for cohort factor
    all_counties <- bind_rows(counties, ak_counties, hi_counties)

    all_counties <- all_counties %>%
      mutate(cohort = case_when(
        cmhc_year_exp == 1971 ~ "1971 (N=208)",
        cmhc_year_exp == 1973 ~ "1973 (N=58)",
        cmhc_year_exp == 1975 ~ "1975 (N=77)",
        cmhc_year_exp == 1977 ~ "1977 (N=715)",
        cmhc_year_exp == 1981 ~ "1981 (N=119)",
        TRUE                  ~ "Never"
      ))
    all_counties$cohort <- factor(all_counties$cohort,
      levels = c("1971 (N=208)", "1973 (N=58)", "1975 (N=77)",
                 "1977 (N=715)", "1981 (N=119)", "Never"))

    # Update individual dataframes with cohort
    counties <- all_counties %>% filter(STATEFP %in% setdiff(unique(counties$STATEFP), c("02", "15")))
    ak_counties <- all_counties %>% filter(STATEFP == "02")
    hi_counties <- all_counties %>% filter(STATEFP == "15")

    cohort_colors <- c(
      "1971 (N=208)" = "#08519c",
      "1973 (N=58)"  = "#3182bd",
      "1975 (N=77)"  = "#6baed6",
      "1977 (N=715)" = "#bdd7e7",
      "1981 (N=119)" = "#eff3ff"
    )

    # State outlines
    states_48 <- tigris::states(cb = TRUE, year = 2020, resolution = "20m") %>%
      filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))
    ak_state <- tigris::states(cb = TRUE, year = 2020, resolution = "20m") %>%
      filter(STATEFP == "02")
    hi_state <- tigris::states(cb = TRUE, year = 2020, resolution = "20m") %>%
      filter(STATEFP == "15")

    # Split into Never vs cohort layers
    counties_never <- counties %>% filter(cohort == "Never")
    counties_cmhc  <- counties %>% filter(cohort != "Never")
    ak_never <- ak_counties %>% filter(cohort == "Never")
    ak_cmhc  <- ak_counties %>% filter(cohort != "Never")
    hi_never <- hi_counties %>% filter(cohort == "Never")
    hi_cmhc  <- hi_counties %>% filter(cohort != "Never")

    # Main map (48 states)
    p_main <- ggplot() +
      geom_sf(data = counties_never, fill = "gray85", color = NA) +
      geom_sf(data = counties_cmhc,  aes(fill = cohort), color = "gray50", linewidth = 0.05) +
      geom_sf(data = states_48, fill = NA, color = "gray30", linewidth = 0.3) +
      scale_fill_manual(values = cohort_colors, name = "CMHC Opening Year") +
      labs(title = "Geographic Distribution of CMHC Openings") +
      coord_sf(crs = sf::st_crs("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96")) +
      theme_void(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        legend.position = "bottom"
      ) +
      guides(fill = guide_legend(nrow = 1))

    # Alaska inset
    p_ak <- ggplot() +
      geom_sf(data = ak_never, fill = "gray85", color = NA) +
      geom_sf(data = ak_cmhc,  aes(fill = cohort), color = "gray50", linewidth = 0.1) +
      geom_sf(data = ak_state, fill = NA, color = "gray30", linewidth = 0.3) +
      scale_fill_manual(values = cohort_colors, guide = "none") +
      theme_void(base_size = 8)

    library(grid)
    library(cowplot)
    g_ak <- ggplotGrob(p_ak)
    g_ak_rot <- grobTree(
      g_ak,
      vp = viewport(angle = -30)
    )

    # Hawaii inset
    p_hi <- ggplot() +
      geom_sf(data = hi_never, fill = "gray85", color = NA) +
      geom_sf(data = hi_cmhc,  aes(fill = cohort), color = "gray50", linewidth = 0.1) +
      geom_sf(data = hi_state, fill = NA, color = "gray30", linewidth = 0.3) +
      scale_fill_manual(values = cohort_colors, guide = "none") +
      theme_void(base_size = 8)

    # Combine with insets in bottom left
    p <- p_main
      # + inset_element(g_ak_rot, left = 0, bottom = -1.15, right = 1.7, top = 1.5)
      # + inset_element(p_hi, left = 0.30, bottom = 0.02, right = 0.45, top = 0.15)

    save_fig(p, "fig03_cmhc_rollout_map.pdf", width = 12, height = 8)
  }
}, error = function(e) {
  cat(sprintf("  WARNING: Map generation failed: %s\n", e$message))
})
