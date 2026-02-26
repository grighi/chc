#!/usr/bin/env Rscript
# ============================================================================
# 15b_tab_cmhc_es_ad_cause.R — CMHC Event Study by Cause of Death & Race
#   Uses NBER Vital Statistics micro-data (cause-specific county-level deaths)
#   Ages 25-44, split by race (white/nonwhite) and cause (external/cardiovascular)
# ============================================================================
log_section("15b: CMHC Event Study — Cause-of-Death Decomposition")

warning("This decomposition should show which conditions deaths are driven by, but 
  we don't see any consistent patterns yet.")

# ── 1. Load NBER mortality micro-data ────────────────────────────────────────
cat("  Loading NBER mortality data...\n")
nber <- arrow::read_parquet(file.path(NBER_DIR, "nber_mortality_counts.parquet")) %>%
  as.data.table()

# Year mapping: NBER encodes years as 0-8 (=1970-78), 79-95 (=1979-95), or literal
nber_year_map <- c(
  setNames(1970:1978, as.character(0:8)),
  setNames(1979:1995, as.character(79:95))
)
nber[, year_raw := year]
nber[, year := as.integer(year)]
nber[as.character(year_raw) %in% names(nber_year_map),
     year := as.integer(nber_year_map[as.character(year_raw)])]

cat(sprintf("  NBER raw: %s rows, years %d-%d\n",
            format(nrow(nber), big.mark = ","), min(nber$year), max(nber$year)))

# ── 2. Filter: ages 25-44, years within CMHC panel range ────────────────────
nber <- nber[age_bin %in% c("25-34", "35-44")]
nber <- nber[year >= 1959 & year <= CMHC_PANEL_END]
cat(sprintf("  After age/year filter: %s rows\n", format(nrow(nber), big.mark = ",")))

# ── 3. Standardize countyrs to 5-char NCHS format ───────────────────────────
# 1959-1961: 4-char format "SSCC" → "SS0CC" (pad county from 2 to 3 digits)
# 1962+: already 5-char "SSCCC"
nber[nchar(countyrs) == 4,
     countyrs := paste0(substr(countyrs, 1, 2), "0", substr(countyrs, 3, 4))]
nber <- nber[nchar(countyrs) == 5 & !grepl("[^0-9]", countyrs)]

# ── 4. UCOD classification ──────────────────────────────────────────────────
# E-codes stored without "E" prefix, so numeric extraction covers all ICD revisions.
# Suicide:  950-959 (ICD-7/8) / E950-E959 (ICD-9, stored as 950-959)
# Homicide: 960-969 / E960-E969
# Alcohol:  303 (alcoholism)

# ── 2. ICD cause-of-death codes ───────────────────────────────────────
# Suicide: ICD-8A 950-959, ICD-9 E950-E959
sui_icd78 <- c("950-", paste0("950", 0:9),
               "951", "951-", paste0("951", 0:8),
               "952-", paste0("952", 0:9),
               "953", "953-", paste0("953", 0:9),
               "954", "954-",
               "955", "955-", paste0("955", 0:9),
               "956", "956-",
               "957", "957-", paste0("957", 0:9),
               "958", "958-", paste0("958", 0:9),
               "959", "959-")
sui_icd9  <- paste0("E95", 0:9)
SUICIDE_CODES <- c(sui_icd78, sui_icd9)

# Homicide: ICD-8A 960-969, ICD-9 E960-E969
hom_icd78 <- c("960", "960-", paste0("960", 0:9),
               "961", "961-", paste0("961", 0:9),
               "962", "962-", paste0("962", 0:9),
               "963", "963-", "964", "964-",
               "965", "965-", paste0("965", 0:9),
               "966", "966-",
               "967", paste0("967", 0:9),
               "968", paste0("968", 0:9),
               "969", "969-", paste0("969", 0:9))
hom_icd9  <- paste0("E96", 0:9)
HOMICIDE_CODES <- c(hom_icd78, hom_icd9)

# Alcohol-related: A&L main specification uses ONLY ICD-8/9 code 303 (alcoholism)
# Alt alcohol (robustness) adds 571.0-571.3, 155.0
alc_icd78 <- c("303", "303-", paste0("303", 0:9))
alc_icd9  <- c("303",  paste0("303", 0:9))
ALCOHOL_CODES <- unique(c(alc_icd78, alc_icd9))


SUICIDE_CODES  <- 950:959
HOMICIDE_CODES <- 960:969
ALCOHOL_CODES  <- 303

classify_ucod <- function(ucod) {
  code3 <- as.numeric(substr(ucod, 1, 3))
  fifelse(code3 %in% as.numeric(SUICIDE_CODES), "suicide",
    fifelse(code3 %in% as.numeric(HOMICIDE_CODES), "homicide",
      fifelse(code3 %in% as.numeric(ALCOHOL_CODES), "alcohol", "other")))
}

nber[, cause_type := classify_ucod(ucod)]

cat("  UCOD classification:\n")
cat(sprintf("    Suicide:  %s deaths\n",
            format(sum(nber$deaths[nber$cause_type == "suicide"]),  big.mark = ",")))
cat(sprintf("    Homicide: %s deaths\n",
            format(sum(nber$deaths[nber$cause_type == "homicide"]), big.mark = ",")))
cat(sprintf("    Alcohol:  %s deaths\n",
            format(sum(nber$deaths[nber$cause_type == "alcohol"]),  big.mark = ",")))
cat(sprintf("    Other:    %s deaths\n",
            format(sum(nber$deaths[nber$cause_type == "other"]),    big.mark = ",")))


# ── 5. Race categories ──────────────────────────────────────────────────────
nber[, race_num := as.integer(race)]
cat(sprintf("  White (race=1): %s rows, Nonwhite (race=2): %s rows\n",
            format(nrow(nber[race_num == 1]), big.mark = ","),
            format(nrow(nber[race_num == 2]), big.mark = ",")))

# ── 6. Aggregate by (year, countyrs, race, cause_type) ──────────────────────
CAUSE_TYPES <- c("suicide", "homicide", "alcohol")

agg_fn <- function(dt, race_val, race_label) {
  dt[race_num == race_val & cause_type %in% CAUSE_TYPES,
     .(deaths = sum(deaths, na.rm = TRUE)),
     by = .(year, countyrs, cause_type)
  ][, race_cat := race_label]
}

agg_white    <- agg_fn(nber, 1L, "white")
agg_nonwhite <- agg_fn(nber, 2L, "nonwhite")

# "all" race: aggregate across race=1 and race=2 for cause-specific
agg_all_race <- nber[race_num %in% c(1L, 2L) & cause_type %in% CAUSE_TYPES,
  .(deaths = sum(deaths, na.rm = TRUE)),
  by = .(year, countyrs, cause_type)
][, race_cat := "all"]

# "all" cause: aggregate across suicide + homicide + alcohol (race-specific)
agg_all_cause_w <- nber[race_num == 1L & cause_type %in% CAUSE_TYPES,
  .(deaths = sum(deaths, na.rm = TRUE)),
  by = .(year, countyrs)
][, `:=`(cause_type = "all", race_cat = "white")]

agg_all_cause_nw <- nber[race_num == 2L & cause_type %in% CAUSE_TYPES,
  .(deaths = sum(deaths, na.rm = TRUE)),
  by = .(year, countyrs)
][, `:=`(cause_type = "all", race_cat = "nonwhite")]

# Baseline: all race, all cause (suicide + homicide + alcohol combined)
agg_baseline <- nber[race_num %in% c(1L, 2L) & cause_type %in% CAUSE_TYPES,
  .(deaths = sum(deaths, na.rm = TRUE)),
  by = .(year, countyrs)
][, `:=`(cause_type = "all", race_cat = "all")]

agg <- rbindlist(list(
  agg_white, agg_nonwhite, agg_all_race,
  agg_all_cause_w, agg_all_cause_nw, agg_baseline
), use.names = TRUE)

cat(sprintf("  Aggregated: %s county-year-race-cause cells\n",
            format(nrow(agg), big.mark = ",")))

# ── 7. Map NCHS county codes → FIPS via crosswalk ───────────────────────────
cat("  Loading NCHS→FIPS crosswalk...\n")
xwalk <- fread(file.path(DATA_DIR, "nchs_fips_crosswalk.csv"))
xwalk[, nchs_countyrs := sprintf("%05d", nchs_countyrs)]

agg <- merge(agg, xwalk, by.x = "countyrs", by.y = "nchs_countyrs", all.x = FALSE)
cat(sprintf("  After FIPS merge: %s rows (%d unique FIPS)\n",
            format(nrow(agg), big.mark = ","), uniqueN(agg$fips)))

# ── 8. Merge onto CMHC panel ────────────────────────────────────────────────
cat("  Loading CMHC panel and merging...\n")
panel <- arrow::read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet")) %>%
  as.data.table()

# Keep panel columns needed for regression
panel_cols <- c("fips", "year", "stfips", "cofips", "copop_ad",
                "event_time_binned", "cmhc_treated", "cmhc_year_exp",
                "D_tot_act_md_t", "H_bpc", "Durb", "popwt_ad")
panel_sub <- panel[, ..panel_cols]

# Create all (fips, year, race_cat, cause_type) combinations we need
combos <- CJ(
  fips = unique(panel_sub$fips),
  year = unique(panel_sub$year),
  race_cat = c("white", "nonwhite", "all"),
  cause_type = c("suicide", "homicide", "alcohol", "all")
)

# Merge panel vars onto combos
combos <- merge(combos, panel_sub, by = c("fips", "year"), all.x = TRUE)

# Merge death counts
combos <- merge(combos, agg[, .(fips, year, race_cat, cause_type, deaths)],
                by = c("fips", "year", "race_cat", "cause_type"), all.x = TRUE)

# Missing death counts → 0 (county-year-race-cause with no recorded deaths)
combos[is.na(deaths), deaths := 0]

# Compute death rate per 100,000 (using copop_ad as denominator)
combos[, death_rate := deaths / copop_ad * 100000]
combos[!is.finite(death_rate), death_rate := NA_real_]

cat(sprintf("  Final dataset: %s rows, %d counties, years %d-%d\n",
            format(nrow(combos), big.mark = ","),
            uniqueN(combos$fips),
            min(combos$year), max(combos$year)))

# ── 9. Print summary statistics ─────────────────────────────────────────────
cat("\n  ── Summary statistics ──\n")
summary_stats <- combos[!is.na(death_rate),
  .(mean_rate = mean(death_rate, na.rm = TRUE),
    sd_rate   = sd(death_rate, na.rm = TRUE),
    mean_deaths = mean(deaths, na.rm = TRUE),
    n_county_years = .N,
    n_counties = uniqueN(fips)),
  by = .(race_cat, cause_type)]
print(summary_stats)

# Check year coverage
cat("\n  Year coverage:\n")
yr_cov <- combos[deaths > 0, .(n_counties_with_deaths = uniqueN(fips)), by = year]
print(yr_cov[order(year)])

# ── 10. Event study regressions ─────────────────────────────────────────────
cat("\n  ── Event study regressions ──\n")

run_es <- function(dt, race, cause) {
  sub <- dt[race_cat == race & cause_type == cause & !is.na(death_rate)]
  cat(sprintf("\n  --- %s × %s (N=%d, counties=%d) ---\n",
              race, cause, nrow(sub), uniqueN(sub$fips)))

  if (nrow(sub) < 100) {
    cat("    Skipping: too few observations\n")
    return(NULL)
  }

  model <- tryCatch(
    feols(
      death_rate ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
      | fips + year + year^Durb + year^stfips,
      data = sub,
      weights = ~popwt_ad,
      cluster = ~fips
    ),
    error = function(e) { cat(sprintf("    ERROR: %s\n", e$message)); NULL }
  )

  if (is.null(model)) return(NULL)

  cat("    Model estimated.\n")

  # Extract coefficients
  coef_df <- extract_es_coefs(model, max_et = CMHC_PLOT_MAX)

  # Joint tests
  tests <- run_joint_tests(model,
    pre_range  = CMHC_EVENT_MIN:-2,
    post_range = 0:CMHC_PLOT_MAX
  )

  # Post-treatment summary
  post <- coef_df[coef_df$event_time >= 0, ]
  cat(sprintf("    Post-treatment mean: %.3f (SE range: %.3f-%.3f)\n",
              mean(post$coefficient), min(post$se), max(post$se)))

  coef_df$race_cat   <- race
  coef_df$cause_type <- cause
  coef_df$pre_p      <- tests$pre_p %||% NA
  list(coef_df, model)
}

results <- list()

# Baseline: all races, all causes
res <- run_es(combos, "all", "all")
if (!is.null(res)) results[["all_all"]] <- res[[1]]

# Decompositions
for (rc in c("all")) {
  for (ct in c("suicide", "homicide", "alcohol")) {
    res <- run_es(combos, rc, ct)
    if (!is.null(res)) {
      results[[paste(rc, ct, sep = "_")]] <- res[[1]]
    }
  }
}

if (length(results) == 0) {
  cat("\n  WARNING: No models could be estimated. Check data merge.\n")
} else {
  all_coefs <- rbindlist(results)

  # ── 11. Print coefficient table ────────────────────────────────────────────
  cat("\n  ── Coefficient summary table ──\n")
  coef_summary <- all_coefs[event_time >= 0,
    .(mean_coef = mean(coefficient),
      min_coef  = min(coefficient),
      max_coef  = max(coefficient)),
    by = .(race_cat, cause_type)]
  print(coef_summary)

  # ── 12. Faceted event study plot ───────────────────────────────────────────
  cat("\n  Plotting...\n")

  all_coefs[, panel_label := paste0(
    toupper(substr(race_cat, 1, 1)), substr(race_cat, 2, nchar(race_cat)),
    " - ",
    toupper(substr(cause_type, 1, 1)), substr(cause_type, 2, nchar(cause_type))
  )]

  p <- ggplot(all_coefs, aes(x = event_time, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
    geom_point(color = "steelblue", size = 1.5) +
    geom_line(color = "steelblue", linewidth = 0.6) +
    facet_wrap(~panel_label, scales = "free_y", ncol = 2) +
    labs(
      title = "Effect of CMHC Opening on Cause-Specific Mortality (Ages 25-44)",
      subtitle = "Event study by race and cause (suicide/homicide/alcohol). NBER Vital Statistics, 1960-1988.",
      x = "Years Relative to CMHC Opening",
      y = "Change in Deaths per 100,000"
    ) +
    theme_paper()

  print(p)
}

cat("\n  Done.\n")
