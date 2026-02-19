#!/usr/bin/env Rscript
# ============================================================================
# Process priority ranking data: add FIPS, analyze rank vs construction,
# and prepare placebo-test variables for event study
# ============================================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# ============================================================================
# STEP 1: Load data sources
# ============================================================================

priority <- read_csv("cmhc_data/priorityranking.csv", show_col_types = FALSE)
cmhc_openings <- read_csv("cmhc_data/cmhc_openings.csv", show_col_types = FALSE)

# Get FIPS lookup from incarceration data
inc <- fst::read_fst("incarceration_trends_county.fst")
fips_lookup <- inc %>%
  select(fips, county_name, state_abbr) %>%
  distinct()

cat("Priority ranking rows:", nrow(priority), "\n")
cat("FIPS lookup rows:", nrow(fips_lookup), "\n")
cat("CMHC openings rows:", nrow(cmhc_openings), "\n\n")

# ============================================================================
# STEP 2: Clean county names for matching
# ============================================================================

# State name to abbreviation mapping
state_map <- data.frame(
  state_full = c("alabama","alaska","arizona","arkansas","california","colorado",
                 "connecticut","delaware","florida","georgia","hawaii","idaho",
                 "illinois","indiana","iowa","kansas","kentucky","louisiana",
                 "maine","maryland","massachusetts","michigan","minnesota",
                 "mississippi","missouri","montana","nebraska","nevada",
                 "new hampshire","new jersey","new mexico","new york",
                 "north carolina","north dakota","ohio","oklahoma","oregon",
                 "pennsylvania","rhode island","south carolina","south dakota",
                 "tennessee","texas","utah","vermont","virginia","washington",
                 "west virginia","wisconsin","wyoming","district of columbia"),
  state_abbr = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID",
                 "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
                 "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK",
                 "OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
                 "WI","WY","DC"),
  stringsAsFactors = FALSE
)

# Add state abbreviation to priority data
priority <- priority %>%
  mutate(state_lower = tolower(trimws(State))) %>%
  left_join(state_map, by = c("state_lower" = "state_full"))

cat("States matched:", sum(!is.na(priority$state_abbr)), "of", nrow(priority), "\n")
unmatched_states <- priority %>% filter(is.na(state_abbr)) %>% pull(state_lower) %>% unique()
if (length(unmatched_states) > 0) {
  cat("Unmatched states:", paste(unmatched_states, collapse = ", "), "\n")
}

# Clean priority county names
priority <- priority %>%
  mutate(county_clean = tolower(trimws(County)))

# Clean FIPS lookup county names: strip "County", "Parish", "city" suffixes
fips_lookup <- fips_lookup %>%
  mutate(
    county_clean = county_name,
    # Remove common suffixes
    county_clean = str_replace(county_clean, "\\s+County$", ""),
    county_clean = str_replace(county_clean, "\\s+Parish$", ""),
    county_clean = str_replace(county_clean, "\\s+Borough$", ""),
    county_clean = str_replace(county_clean, "\\s+Census Area$", ""),
    county_clean = str_replace(county_clean, "\\s+Municipality$", ""),
    county_clean = str_replace(county_clean, "\\s+City and Borough$", ""),
    county_clean = str_replace(county_clean, "\\s+city$", ""),
    county_clean = tolower(trimws(county_clean))
  )

# ============================================================================
# STEP 3: Match priority data to FIPS codes
# ============================================================================

# First pass: exact match on state_abbr + county_clean
priority_matched <- priority %>%
  left_join(fips_lookup %>% select(fips, state_abbr, county_clean),
            by = c("state_abbr", "county_clean"))

n_matched_exact <- sum(!is.na(priority_matched$fips))
cat("\nExact match:", n_matched_exact, "of", nrow(priority), "\n")

# Identify unmatched
unmatched <- priority_matched %>% filter(is.na(fips))
cat("Unmatched:", nrow(unmatched), "\n")

if (nrow(unmatched) > 0) {
  cat("\nSample unmatched counties:\n")
  print(unmatched %>% select(State, County, county_clean, state_abbr) %>% head(20))
}

# Second pass: fuzzy fixes for common discrepancies
# Common patterns: "de kalb" vs "dekalb", "st" vs "saint", etc.
priority <- priority %>%
  mutate(
    county_clean2 = county_clean,
    # "de kalb" -> "dekalb"
    county_clean2 = str_replace(county_clean2, "^de kalb$", "dekalb"),
    # "de soto" -> "desoto" 
    county_clean2 = str_replace(county_clean2, "^de soto$", "desoto"),
    # "la porte" -> "laporte"
    county_clean2 = str_replace(county_clean2, "^la porte$", "laporte"),
    # "la salle" -> "lasalle"
    county_clean2 = str_replace(county_clean2, "^la salle$", "lasalle"),
    # "o brien" -> "o'brien"
    county_clean2 = str_replace(county_clean2, "^o brien$", "o'brien"),
    # "st " -> "st. " 
    county_clean2 = str_replace(county_clean2, "^st ", "st. "),
    # "ste " -> "ste. "
    county_clean2 = str_replace(county_clean2, "^ste ", "ste. "),
    # Handle "new orleans" -> "orleans"  (Louisiana)
    county_clean2 = ifelse(county_clean2 == "new orleans" & state_abbr == "LA", "orleans", county_clean2),
    # "mongomery" -> "montgomery" (typo in Alabama)
    county_clean2 = str_replace(county_clean2, "^mongomery$", "montgomery"),
    # "livingson" -> "livingston" (typo in Louisiana)
    county_clean2 = str_replace(county_clean2, "^livingson$", "livingston"),
    # "pocahantas" -> "pocahontas" (typo in Iowa)
    county_clean2 = str_replace(county_clean2, "^pocahantas$", "pocahontas"),
    # "pottwattamie" -> "pottawattamie" (typo in Iowa)
    county_clean2 = str_replace(county_clean2, "^pottwattamie$", "pottawattamie"),
    # "monana" -> "monona" (typo in Iowa)
    county_clean2 = str_replace(county_clean2, "^monana$", "monona"),
    # Baltimore city special case  
    county_clean2 = ifelse(county_clean2 == "baltimore city" & state_abbr == "MD", "baltimore", county_clean2),
    # St. Louis city
    county_clean2 = ifelse(county_clean2 == "st louis city" & state_abbr == "MO", "st. louis", county_clean2),
    # "jeff davis" -> "jeff davis" should match
    # District of Columbia
    county_clean2 = ifelse(county_clean2 == "district of columbia", "district of columbia", county_clean2)
  )

fips_lookup <- fips_lookup %>%
  mutate(
    county_clean2 = county_clean,
    county_clean2 = str_replace(county_clean2, "^de kalb$", "dekalb"),
    county_clean2 = str_replace(county_clean2, "^de soto$", "desoto"),
    county_clean2 = str_replace(county_clean2, "^la porte$", "laporte"),
    county_clean2 = str_replace(county_clean2, "^la salle$", "lasalle"),
    county_clean2 = str_replace(county_clean2, "^st ", "st. "),
    county_clean2 = str_replace(county_clean2, "^ste ", "ste. ")
  )

# Re-match with cleaned names
priority_matched <- priority %>%
  left_join(fips_lookup %>% select(fips, state_abbr, county_clean2),
            by = c("state_abbr", "county_clean2"))

n_matched <- sum(!is.na(priority_matched$fips))
cat("\nAfter fuzzy fixes:", n_matched, "of", nrow(priority), "\n")

# Show remaining unmatched
unmatched2 <- priority_matched %>% filter(is.na(fips) & !is.na(state_abbr))
if (nrow(unmatched2) > 0) {
  cat("\nStill unmatched:\n")
  print(unmatched2 %>% select(State, County, county_clean2, state_abbr) %>% 
          arrange(State, County))
  
  # Try to find close matches in FIPS lookup for unmatched
  cat("\nAttempting to find close matches...\n")
  for (i in seq_len(min(nrow(unmatched2), 30))) {
    st <- unmatched2$state_abbr[i]
    cn <- unmatched2$county_clean2[i]
    candidates <- fips_lookup %>% 
      filter(state_abbr == st) %>%
      mutate(dist = adist(cn, county_clean2)) %>%
      arrange(dist) %>% head(3)
    cat(sprintf("  '%s' (%s) -> closest: %s\n", cn, st, 
                paste(sprintf("'%s' (d=%d)", candidates$county_clean2, candidates$dist), collapse=", ")))
  }
}

# Drop rows without FIPS or without rank
priority_final <- priority_matched %>%
  filter(!is.na(fips) & !is.na(rank)) %>%
  select(fips, state_abbr, county = County, area, rank)

cat("\n\nFinal priority dataset:", nrow(priority_final), "counties with FIPS and rank\n")

# Save the matched data
write_csv(priority_final, "cmhc_data/priorityranking_fips.csv")
cat("Saved to cmhc_data/priorityranking_fips.csv\n")

# ============================================================================
# STEP 4: Classify rank type by state (true rank vs score)
# ============================================================================

cat("\n==========================================================\n")
cat("CLASSIFYING RANK TYPE BY STATE\n")
cat("==========================================================\n\n")

state_rank_info <- priority_final %>%
  group_by(state_abbr) %>%
  summarize(
    n_counties = n(),
    min_rank = min(rank, na.rm = TRUE),
    max_rank = max(rank, na.rm = TRUE),
    n_unique_ranks = n_distinct(rank),
    # Check if ranks are integer-like
    all_integer = all(rank == round(rank), na.rm = TRUE),
    starts_with_1 = (min(rank, na.rm = TRUE) == 1),
    .groups = "drop"
  ) %>%
  mutate(
    # True rank: starts at 1, integers, goes up by natural numbers
    rank_type = case_when(
      starts_with_1 & all_integer ~ "true_rank",
      TRUE ~ "score"
    )
  )

cat("State-by-state rank classification:\n")
print(state_rank_info %>% select(state_abbr, n_counties, min_rank, max_rank, 
                                  n_unique_ranks, rank_type), n = 50)

# ============================================================================
# STEP 5: Standardize priority into a comparable measure
# ============================================================================

# For true_rank: lower number = higher priority (rank 1 is best)
#   -> priority = max_rank - rank + 1 (so rank 1 gets highest value)
# For score: higher score = higher priority
#   -> priority = rank (already correct direction)

# Within each state, convert to a percentile rank (0 = lowest priority, 1 = highest)
priority_final <- priority_final %>%
  left_join(state_rank_info %>% select(state_abbr, rank_type), by = "state_abbr") %>%
  group_by(state_abbr) %>%
  mutate(
    # Standardize: for true_rank, invert; for score, keep as is
    priority_raw = case_when(
      rank_type == "true_rank" ~ -rank,   # negate so higher = better
      rank_type == "score" ~ rank          # already higher = better
    ),
    # Convert to within-state percentile (0 to 1)
    priority_pctile = percent_rank(priority_raw),
    # Also create within-state terciles
    priority_tercile = ntile(priority_raw, 3),
    # And a "high priority" flag (top tercile)
    high_priority = as.integer(priority_tercile == 3)
  ) %>%
  ungroup()

cat("\n\nPriority distribution:\n")
cat("  High priority (top tercile):", sum(priority_final$high_priority == 1), "\n")
cat("  Medium priority:", sum(priority_final$priority_tercile == 2), "\n")
cat("  Low priority:", sum(priority_final$priority_tercile == 1), "\n")

# ============================================================================
# STEP 6: Merge with CMHC openings and analyze
# ============================================================================

cat("\n\n==========================================================\n")
cat("ANALYZING PRIORITY RANK vs ACTUAL CMHC CONSTRUCTION\n")
cat("==========================================================\n\n")

priority_cmhc <- priority_final %>%
  left_join(cmhc_openings, by = "fips") %>%
  mutate(
    got_cmhc = as.integer(!is.na(cmhc_year_exp)),
    # For analysis: early adopter = opened by 1975
    early_cmhc = as.integer(!is.na(cmhc_year_exp) & cmhc_year_exp <= 1975)
  )

# Area-level treatment: a county is effectively treated if ANY county
# in its state x area group received a CMHC (since the CMHC served the
# entire catchment area, not just the county where it was physically built)
priority_cmhc <- priority_cmhc %>%
  group_by(state_abbr, area) %>%
  mutate(
    area_has_cmhc = as.integer(any(got_cmhc == 1)),
    area_cmhc_year = suppressWarnings(min(cmhc_year_exp, na.rm = TRUE)),
    area_cmhc_year = ifelse(is.infinite(area_cmhc_year), NA_real_, area_cmhc_year)
  ) %>%
  ungroup() %>%
  mutate(
    # Key group: high priority area but NO county in the area got a CMHC
    high_priority_no_cmhc = as.integer(high_priority == 1 & area_has_cmhc == 0)
  )

cat("\nArea-level CMHC coverage:\n")
cat("  Total state x area groups:", n_distinct(paste(priority_cmhc$state_abbr, priority_cmhc$area)), "\n")
cat("  Areas where at least 1 county got CMHC:",
    n_distinct(paste(priority_cmhc$state_abbr[priority_cmhc$area_has_cmhc == 1],
                     priority_cmhc$area[priority_cmhc$area_has_cmhc == 1])), "\n")
cat("  Areas with NO CMHC:",
    n_distinct(paste(priority_cmhc$state_abbr[priority_cmhc$area_has_cmhc == 0],
                     priority_cmhc$area[priority_cmhc$area_has_cmhc == 0])), "\n")
cat("  Counties in areas with CMHC:", sum(priority_cmhc$area_has_cmhc == 1), "\n")
cat("  Counties in areas without CMHC:", sum(priority_cmhc$area_has_cmhc == 0), "\n")

# Summary table
cat("Cross-tabulation: Priority Tercile x CMHC Status\n")
cat("-----------------------------------------------------------\n")
xtab <- priority_cmhc %>%
  group_by(priority_tercile, got_cmhc) %>%
  summarize(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = got_cmhc, values_from = n, values_fill = 0, 
              names_prefix = "cmhc_")
print(xtab)

cat("\nProportion receiving CMHC by priority tercile:\n")
prop_tab <- priority_cmhc %>%
  group_by(priority_tercile) %>%
  summarize(
    n_counties = n(),
    n_cmhc = sum(got_cmhc),
    pct_cmhc = mean(got_cmhc) * 100,
    n_early_cmhc = sum(early_cmhc),
    pct_early = mean(early_cmhc) * 100,
    mean_year = mean(cmhc_year_exp, na.rm = TRUE),
    .groups = "drop"
  )
print(prop_tab)

# Correlation between priority and opening year (conditional on receiving CMHC)
cat("\n\nAmong counties that received CMHCs:\n")
cmhc_counties <- priority_cmhc %>% filter(got_cmhc == 1)
cat("  N:", nrow(cmhc_counties), "\n")

if (nrow(cmhc_counties) > 10) {
  cor_test <- cor.test(cmhc_counties$priority_pctile, cmhc_counties$cmhc_year_exp)
  cat(sprintf("  Correlation (priority percentile vs opening year): %.3f (p = %.4f)\n",
              cor_test$estimate, cor_test$p.value))
  
  # Regression: opening year ~ priority percentile
  fit <- lm(cmhc_year_exp ~ priority_pctile, data = cmhc_counties)
  cat(sprintf("  OLS: opening_year = %.2f + %.2f * priority_pctile\n",
              coef(fit)[1], coef(fit)[2]))
  cat(sprintf("  SE of slope: %.3f, p = %.4f\n",
              summary(fit)$coefficients[2, 2], summary(fit)$coefficients[2, 4]))
  
  # By state
  cat("\n  State-level correlations (priority vs opening year):\n")
  state_cors <- cmhc_counties %>%
    group_by(state_abbr) %>%
    filter(n() >= 5 & n_distinct(cmhc_year_exp) > 1) %>%
    summarize(
      n = n(),
      cor = cor(priority_pctile, cmhc_year_exp, use = "complete.obs"),
      mean_year = mean(cmhc_year_exp),
      .groups = "drop"
    ) %>%
    arrange(cor)
  print(state_cors, n = 40)
}

# Mean opening year by priority group
cat("\n\nMean CMHC opening year by priority group (among recipients):\n")
year_by_priority <- cmhc_counties %>%
  group_by(priority_tercile) %>%
  summarize(
    n = n(),
    mean_year = mean(cmhc_year_exp),
    sd_year = sd(cmhc_year_exp),
    pct_early = mean(cmhc_year_exp <= 1975) * 100,
    .groups = "drop"
  )
print(year_by_priority)

# T-test: opening year for high vs low priority
if (sum(cmhc_counties$priority_tercile == 3) > 5 & sum(cmhc_counties$priority_tercile == 1) > 5) {
  tt <- t.test(cmhc_year_exp ~ I(priority_tercile == 3),
               data = cmhc_counties %>% filter(priority_tercile %in% c(1, 3)))
  cat(sprintf("\n  T-test (high vs low priority opening year): diff = %.2f, p = %.4f\n",
              diff(tt$estimate), tt$p.value))
}

# Key placebo group summary
cat("\n\n-----------------------------------------------------------\n")
cat("PLACEBO GROUP: High Priority Area, but No CMHC in the Area\n")
cat("-----------------------------------------------------------\n")
cat("  Counties in high priority tercile:", sum(priority_cmhc$high_priority == 1), "\n")
cat("  Of those, county itself got CMHC:", sum(priority_cmhc$high_priority == 1 & priority_cmhc$got_cmhc == 1), "\n")
cat("  Of those, county has no CMHC but another county in its area does:",
    sum(priority_cmhc$high_priority == 1 & priority_cmhc$got_cmhc == 0 & priority_cmhc$area_has_cmhc == 1), "\n")
cat("  Of those, NO county in the area got a CMHC (PLACEBO):",
    sum(priority_cmhc$high_priority_no_cmhc == 1), "\n")
cat("  These counties were in high-priority areas but no CMHC was built\n")
cat("  anywhere in their catchment area. They serve as a placebo: if the\n")
cat("  effect is causal, these counties should NOT show the same mortality\n")
cat("  decline as area-treated counties.\n")

# Save the full merged dataset for use in event study
# Some counties appear in multiple areas — deduplicate by keeping the row
# where area_has_cmhc == 1 (if any area they're in is treated, they're treated),
# and among ties, keep the highest priority rank
priority_for_merge <- priority_cmhc %>%
  select(fips, state_abbr, area, rank, rank_type, priority_pctile, priority_tercile, 
         high_priority, got_cmhc, area_has_cmhc, area_cmhc_year, high_priority_no_cmhc) %>%
  arrange(fips, desc(area_has_cmhc), area_cmhc_year, desc(priority_pctile)) %>%
  distinct(fips, .keep_all = TRUE)

cat("\nAfter deduplication (counties in multiple areas): ", nrow(priority_for_merge), " unique FIPS\n")

write_csv(priority_for_merge, "cmhc_data/priority_ranks_for_eventstudy.csv")
cat("\n\nSaved priority data for event study merge: cmhc_data/priority_ranks_for_eventstudy.csv\n")

cat("\n==========================================================\n")
cat("DONE: Priority Ranking Analysis\n")
cat("==========================================================\n")
