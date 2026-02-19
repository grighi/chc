#!/usr/bin/env Rscript
# ============================================================================
# CMHC County Classification Breakdown
# ============================================================================

library(dplyr)
library(readr)
library(fst)

# Load data
cmhc_openings <- read_csv("cmhc_data/cmhc_openings.csv", show_col_types = FALSE) %>%
  filter(cmhc_year_exp <= 1975)  # Only consider openings before 1976 for classification
data <- read_fst("aer_data/aer_data.fst") %>% as_tibble()
priority <- read_csv("cmhc_data/priority_ranks_for_eventstudy.csv", show_col_types = FALSE)

# Get unique counties
counties <- data %>%
  select(fips, stfips) %>%
  distinct()

# Merge CMHC status
counties <- counties %>%
  left_join(cmhc_openings, by = "fips")

# Merge priority data
counties <- counties %>%
  left_join(priority %>% select(fips, priority_pctile, priority_tercile,
                                 area, area_has_cmhc, high_priority_no_cmhc),
            by = "fips")

# Classification
counties <- counties %>%
  mutate(
    category = case_when(
      !is.na(cmhc_year_exp) ~ "1. Directly treated (got CMHC)",
      is.na(cmhc_year_exp) & !is.na(area_has_cmhc) & area_has_cmhc == 1 ~ "2. Area-treated (area got CMHC, county didn't)",
      is.na(cmhc_year_exp) & (is.na(area_has_cmhc) | area_has_cmhc == 0) & !is.na(priority_pctile) ~ "3. Ranked, never treated (no CMHC in area)",
      is.na(cmhc_year_exp) & (is.na(area_has_cmhc) | area_has_cmhc == 0) & is.na(priority_pctile) ~ "4. No ranking, never treated",
      TRUE ~ "5. Other"
    )
  )

cat("============================================================\n")
cat("COUNTY CLASSIFICATION BREAKDOWN\n")
cat("============================================================\n\n")

# Overall counts
cat("Total unique counties in data:", nrow(counties), "\n\n")

breakdown <- counties %>%
  group_by(category) %>%
  summarize(
    n = n(),
    pct = round(100 * n() / nrow(counties), 1),
    .groups = "drop"
  )

for (i in 1:nrow(breakdown)) {
  cat(sprintf("  %-50s %5d  (%4.1f%%)\n",
              breakdown$category[i], breakdown$n[i], breakdown$pct[i]))
}
cat(sprintf("  %-50s %5d  (100.0%%)\n", "TOTAL", sum(breakdown$n)))

# Further breakdown of category 3: ranked never-treated
cat("\n\n--- Within 'Ranked, never treated' ---\n\n")

ranked_never <- counties %>%
  filter(grepl("^3\\.", category))

if (nrow(ranked_never) > 0) {
  # Median split
  med_pctile <- median(ranked_never$priority_pctile, na.rm = TRUE)
  ranked_never <- ranked_never %>%
    mutate(priority_half = ifelse(priority_pctile >= med_pctile,
                                   "Above-median priority", "Below-median priority"))
  
  sub_breakdown <- ranked_never %>%
    group_by(priority_half) %>%
    summarize(n = n(), .groups = "drop")
  
  for (i in 1:nrow(sub_breakdown)) {
    cat(sprintf("  %-40s %5d\n", sub_breakdown$priority_half[i], sub_breakdown$n[i]))
  }
  cat(sprintf("  %-40s %5d\n", "Total ranked never-treated", sum(sub_breakdown$n)))
}

# What about counties with NO priority data at all?
cat("\n\n--- Priority data coverage ---\n\n")
has_priority <- counties %>%
  summarize(
    with_priority = sum(!is.na(priority_pctile)),
    without_priority = sum(is.na(priority_pctile)),
    pct_with = round(100 * sum(!is.na(priority_pctile)) / n(), 1)
  )
cat(sprintf("  Counties with priority ranking: %d (%.1f%%)\n",
            has_priority$with_priority, has_priority$pct_with))
cat(sprintf("  Counties without ranking:       %d (%.1f%%)\n",
            has_priority$without_priority, 100 - has_priority$pct_with))

# Breakdown by state: how many states have priority data?
cat("\n\n--- State-level coverage ---\n\n")
state_coverage <- counties %>%
  group_by(stfips) %>%
  summarize(
    n_counties = n(),
    n_with_priority = sum(!is.na(priority_pctile)),
    pct_covered = round(100 * n_with_priority / n_counties, 1),
    .groups = "drop"
  ) %>%
  mutate(coverage_cat = case_when(
    pct_covered == 0 ~ "No coverage",
    pct_covered < 50 ~ "Partial (<50%)",
    pct_covered < 100 ~ "Partial (50-99%)",
    pct_covered == 100 ~ "Full coverage"
  ))

state_summary <- state_coverage %>%
  group_by(coverage_cat) %>%
  summarize(n_states = n(), n_counties = sum(n_counties), .groups = "drop")

for (i in 1:nrow(state_summary)) {
  cat(sprintf("  %-30s %3d states  (%4d counties)\n",
              state_summary$coverage_cat[i],
              state_summary$n_states[i],
              state_summary$n_counties[i]))
}

# Cross-tab: treatment status x priority ranking availability
cat("\n\n--- Treatment status × Priority data availability ---\n\n")
cross <- counties %>%
  mutate(
    treated = ifelse(!is.na(cmhc_year_exp), "CMHC", "No CMHC"),
    ranked  = ifelse(!is.na(priority_pctile), "Has rank", "No rank")
  ) %>%
  group_by(treated, ranked) %>%
  summarize(n = n(), .groups = "drop")

for (i in 1:nrow(cross)) {
  cat(sprintf("  %-12s × %-12s : %5d\n",
              cross$treated[i], cross$ranked[i], cross$n[i]))
}

cat("\n============================================================\n")
cat("KEY IMPLICATION FOR PLACEBO DESIGN:\n")
cat("============================================================\n\n")

n_direct <- sum(grepl("^1\\.", counties$category))
n_area   <- sum(grepl("^2\\.", counties$category))
n_ranked <- sum(grepl("^3\\.", counties$category))
n_unranked <- sum(grepl("^4\\.", counties$category))

cat(sprintf("In the median-split placebo test:\n"))
cat(sprintf("  Placebo 'treated':  ~%d above-median priority, no CMHC in area\n",
            round(n_ranked / 2)))
cat(sprintf("  Placebo 'control':  ~%d below-median priority, no CMHC in area\n",
            round(n_ranked / 2)))
cat(sprintf("  Excluded:           %d directly treated + %d area-treated + %d unranked\n",
            n_direct, n_area, n_unranked))
cat(sprintf("\nIn the main treatment regression:\n"))
cat(sprintf("  Treated:   %d directly-treated counties\n", n_direct))
cat(sprintf("  Control:   %d never-treated counties (all categories)\n",
            n_ranked + n_unranked))
cat(sprintf("  Excluded:  %d area-treated counties\n", n_area))



cat("\n============================================================\n")
cat("CMHC COUNT DIAGNOSTICS\n")
cat("============================================================\n\n")

# How many unique counties in cmhc_openings?
cat(sprintf("Unique counties in cmhc_openings.csv: %d\n", nrow(cmhc_openings)))

# How many total CMHCs (from cmhcs_by_year)?
cmhcs_by_year <- read_csv("cmhc_data/cmhcs_by_year.csv", show_col_types = FALSE)
cat(sprintf("Total CMHCs (sum of cmhc_count): %d\n", sum(cmhcs_by_year$cmhc_count)))
cat(sprintf("Unique counties in cmhcs_by_year: %d\n", n_distinct(cmhcs_by_year$fips)))

# Max cumulative CMHCs per county
max_by_county <- cmhcs_by_year %>%
  group_by(fips) %>%
  summarize(total = sum(cmhc_count), .groups = "drop")
cat(sprintf("Counties with 1 CMHC: %d\n", sum(max_by_county$total == 1)))
cat(sprintf("Counties with 2+ CMHCs: %d\n", sum(max_by_county$total >= 2)))
cat(sprintf("Max CMHCs in one county: %d\n", max(max_by_county$total)))

# Year distribution
cat("\nCMHC openings by year:\n")
year_dist <- cmhcs_by_year %>%
  group_by(year) %>%
  summarize(n_cmhcs = sum(cmhc_count), n_counties = n_distinct(fips), .groups = "drop")
for (i in 1:nrow(year_dist)) {
  cat(sprintf("  %d: %3d CMHCs across %3d counties\n",
              year_dist$year[i], year_dist$n_cmhcs[i], year_dist$n_counties[i]))
}

# How many treated counties come from cmhc_openings vs area treatment?
cat(sprintf("\nDirect CMHC counties (cmhc_openings): %d\n", nrow(cmhc_openings)))
cat(sprintf("Area-treated counties: %d\n", n_area))
cat(sprintf("Total 'treated' in category 1: %d\n", n_direct))

# Check: does n_direct == nrow(cmhc_openings)?
cat(sprintf("\nDoes category 1 match cmhc_openings? %s\n",
            ifelse(n_direct == nrow(cmhc_openings), "YES", "NO — MISMATCH")))

# If mismatch, check why
if (n_direct != nrow(cmhc_openings)) {
  in_openings_not_data <- setdiff(cmhc_openings$fips, counties$fips)
  in_data_not_openings <- counties %>%
    filter(grepl("^1\\.", category)) %>%
    filter(!fips %in% cmhc_openings$fips) %>%
    pull(fips)
  cat(sprintf("  In cmhc_openings but not in mortality data: %d\n", length(in_openings_not_data)))
  cat(sprintf("  In category 1 but not in cmhc_openings: %d\n", length(in_data_not_openings)))
}