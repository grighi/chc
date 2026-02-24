#!/usr/bin/env Rscript
# ============================================================================
# cmhc_poisson_replication.R
# Poisson PPML estimation — replicating Avery & LaVoice (2023), Table 3
#
# Specification (from A&L):
#   E[y] = exp(β·CMHC + ln(pop) + θ_c + γ_t + λ_s·t + ξ X)
#
# Table structure:
#   Columns: (1) Suicide, (2) Homicide, (3) Alcohol
#   Panel A: Total population
#   Panel B: White population
#   Panel C: Non-white population
#
# Controls (A&L spec):
#   - pct < HS × t, pct HS × t, unemployment × t, LFPR × t, MDs/1000 × t
#   - Urban quintile × year FE
#   - State-specific linear time trends
# Weighted by respective county population
# SE clustered at county level
#
# Also includes AER-data adult AMR (ages 20-49) estimates:
#   (1) Linear TWFE — BGB specification (matches 15_fig05)
#   (2) Poisson PPML — BGB spec on death counts
#   (3) Poisson PPML — A&L spec on death counts
# ============================================================================

library(arrow)
library(data.table)
library(dplyr)
library(fixest)
library(readr)

# setFixest_nthreads(parallel::detectCores())

cat("NOTE: Targeting A&L Table 3 — 60,316 obs across ~3,034 counties × 20 years\n")

cat("============================================================\n")
cat("POISSON PPML: Avery & LaVoice Replication\n")
cat("============================================================\n\n")

# ── 1. Year mapping for NBER mortality data ────────────────────────────
# NBER files store year as: 4-digit (1959-1967), single-digit 0-8 (1970-1978),
# or 2-digit 79-95 (1979-1995). Some years (1968-1969) may appear as 2-digit "68"/"69".
nber_year_map <- c(
  setNames(1959:1969, as.character(59:69)),   # 2-digit for 1959-1969
  setNames(1970:1978, as.character(0:8)),     # single-digit for 1970-1978
  setNames(1979:1999, as.character(79:99))    # 2-digit for 1979-1999
)

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

# ── 3. Load NBER mortality micro-data ──────────────────────────────────
cat("Loading NBER mortality counts...\n")
nber <- as.data.table(read_parquet("raw/nber_mortality/nber_mortality_counts.parquet"))

# Fix years
nber[, year_raw := year]
nber[year %in% names(nber_year_map), year := nber_year_map[year]]
nber[, year := as.integer(year)]
nber <- nber[!is.na(year) & year >= 1969 & year <= 1988]

# Construct 5-digit FIPS from staters (2-digit state) + countyrs (3-digit county)
# Note: countyrs sometimes has the state prefix embedded (e.g., staters='01',
# countyrs='0101' → strip leading '01' → county '01' → fips '01001')
# This logic matches combine_raw.py's approach.
nber[, staters_pad := formatC(as.integer(staters), width = 2, flag = "0")]
nber[, countyrs_str := as.character(countyrs)]
nber[, has_prefix := substr(countyrs_str, 1, 2) == staters_pad &
       nchar(countyrs_str) > 3]
nber[, county_part := fifelse(has_prefix,
                               substr(countyrs_str, 3, nchar(countyrs_str)),
                               countyrs_str)]
nber[, fips := as.integer(paste0(
  staters_pad,
  formatC(as.integer(county_part), width = 3, flag = "0")
))]
# Clean up temp columns
nber[, c("staters_pad", "countyrs_str", "has_prefix", "county_part") := NULL]

# Race grouping: white / nonwhite
nber[, race_group := fifelse(race == "1", "white", "nonwhite")]

# Flag causes
nber[, cause := fcase(
  ucod %in% SUICIDE_CODES,  "suicide",
  ucod %in% HOMICIDE_CODES, "homicide",
  ucod %in% ALCOHOL_CODES,  "alcohol",
  default = "other"
)]

cat(sprintf("  NBER: %d rows, years %d-%d, %d counties\n",
            nrow(nber), min(nber$year), max(nber$year), uniqueN(nber$fips)))

# ── 4. Aggregate deaths: county × year × race × cause ─────────────────
cat("Aggregating death counts...\n")

# By race
sui_deaths <- nber[cause == "suicide",
  .(deaths = sum(deaths, na.rm = TRUE)), by = .(fips, year, race_group)]
hom_deaths <- nber[cause == "homicide",
  .(deaths = sum(deaths, na.rm = TRUE)), by = .(fips, year, race_group)]
alc_deaths <- nber[cause == "alcohol",
  .(deaths = sum(deaths, na.rm = TRUE)), by = .(fips, year, race_group)]

# Total (across both races) for Panel A
sui_deaths_tot <- nber[cause == "suicide",
  .(deaths = sum(deaths, na.rm = TRUE)), by = .(fips, year)]
hom_deaths_tot <- nber[cause == "homicide",
  .(deaths = sum(deaths, na.rm = TRUE)), by = .(fips, year)]
alc_deaths_tot <- nber[cause == "alcohol",
  .(deaths = sum(deaths, na.rm = TRUE)), by = .(fips, year)]

cat(sprintf("  Suicide:  %d deaths\n", sum(sui_deaths$deaths)))
cat(sprintf("  Homicide: %d deaths\n", sum(hom_deaths$deaths)))
cat(sprintf("  Alcohol:  %d deaths\n", sum(alc_deaths$deaths)))

# ── 5. Load population denominators ────────────────────────────────────
cat("Loading population data...\n")
pop <- as.data.table(read_parquet("raw/county_pop_demo.parquet"))
pop[, race_group := fifelse(race == 1, "white", "nonwhite")]
pop[, fips := as.integer(fips)]

# By race
pop_race <- pop[, .(population = sum(pop, na.rm = TRUE)),
                by = .(fips, year, race_group)]

# Total (both races)
pop_total <- pop[, .(population = sum(pop, na.rm = TRUE)),
                 by = .(fips, year)]

cat(sprintf("  Population: years %d-%d\n", min(pop_race$year), max(pop_race$year)))

# ── 6. Load CMHC openings ─────────────────────────────────────────────
cat("Loading CMHC openings...\n")
cmhc_openings <- as.data.table(read_csv("cmhc_data/cmhc_openings.csv",
                                         show_col_types = FALSE))
cmhc_openings[, fips := as.integer(fips)]
cat(sprintf("  %d counties with CMHC openings\n", nrow(cmhc_openings)))

# ── 7. Load AER covariates + pscore data for A&L controls ─────────────
cat("Loading covariates...\n")
aer <- as.data.table(fst::read_fst("aer_data/aer_data.fst"))
aer[, fips := as.integer(fips)]

# Get state FIPS from AER (one row per county)
aer_ids <- unique(aer[, .(fips, stfips, cofips)])

# Drop NYC, LA, Chicago (as in BGB)
aer_ids <- aer_ids[!(stfips == 36 & cofips == 61) &
                    !(stfips == 6  & cofips == 37) &
                    !(stfips == 17 & cofips == 31)]

# Load pscore data for A&L controls (1960 baseline characteristics)
pscore <- as.data.table(haven::read_dta("aer_data/aer_pscore_data.dta"))
pscore[, fips := as.integer(paste0(
  formatC(as.integer(stfips), width = 2, flag = "0"),
  formatC(as.integer(cofips), width = 3, flag = "0")
))]

# A&L controls (Table 3 spec):
#   _pctschlt4:   % < 4 yrs schooling (≈ pct < HS, 1960)
#   _pctschlgt12: % 12+ yrs schooling (≈ pct HS+, 1960)
#   _pctlfue:     unemployment rate (1960)
#   _lfp:         labor force participation rate (1960)
#   _md_per1000:  MDs per 1000 pop (1960) — medical workers
#   _pcturb:      % urban (1960) — for urban quintile × year FE
pscore_controls <- pscore[, .(fips, `_pctschlt4`, `_pctschlgt12`, `_pctlfue`, `_lfp`,
                              `_md_per1000`, `_pcturb`)]

# Create urban quintile (A&L: "quintile of county's share of urban residents")
pscore_controls[, urb_qtile := as.factor(dplyr::ntile(`_pcturb`, 5))]

cat(sprintf("  Pscore controls: %d counties\n", nrow(pscore_controls)))

# ── 8. Build analysis panels ──────────────────────────────────────────

build_panel <- function(deaths_dt, cause_label, race_val) {
  cat(sprintf("  Building %s × %s panel...\n", cause_label, race_val))

  grid <- CJ(fips = aer_ids$fips, year = 1969:1988)

  # Merge deaths (fill 0 for missing)
  if (race_val == "total") {
    panel <- merge(grid, deaths_dt, by = c("fips", "year"), all.x = TRUE)
  } else {
    d <- deaths_dt[race_group == race_val, .(fips, year, deaths)]
    panel <- merge(grid, d, by = c("fips", "year"), all.x = TRUE)
  }
  panel[is.na(deaths), deaths := 0L]

  # Merge population
  if (race_val == "total") {
    panel <- merge(panel, pop_total, by = c("fips", "year"), all.x = TRUE)
  } else {
    p <- pop_race[race_group == race_val, .(fips, year, population)]
    panel <- merge(panel, p, by = c("fips", "year"), all.x = TRUE)
  }

  # Merge CMHC treatment (ALL cohorts — A&L doesn't restrict)
  panel <- merge(panel, cmhc_openings[, .(fips, cmhc_year_exp)],
                 by = "fips", all.x = TRUE)
  panel[, cmhc_post := as.integer(!is.na(cmhc_year_exp) & year >= cmhc_year_exp)]

  # Merge state FIPS
  panel <- merge(panel, aer_ids[, .(fips, stfips)], by = "fips", all.x = TRUE)

  # Merge A&L controls
  panel <- merge(panel, pscore_controls, by = "fips", all.x = TRUE)

  # Create linear time trend interactions: baseline × (year - 1960)
  panel[, t := year - 1960L]
  panel[, D_pctschlt4_t   := `_pctschlt4`   * t]
  panel[, D_pctschlgt12_t := `_pctschlgt12` * t]
  panel[, D_pctlfue_t     := `_pctlfue`     * t]
  panel[, D_lfp_t         := `_lfp`         * t]
  panel[, D_md_t          := `_md_per1000`  * t]  # medical workers × trend

  # Log population offset
  panel[, log_pop := log(pmax(population, 1))]

  # Drop obs with missing population
  panel <- panel[!is.na(population) & population > 0]

  cat(sprintf("    %d obs, %d counties, %d deaths\n",
              nrow(panel), uniqueN(panel$fips), sum(panel$deaths)))
  panel
}

# Build 9 panels: 3 causes × 3 race groups
cat("\nBuilding panels...\n")
panels <- list()
for (cause in c("suicide", "homicide", "alcohol")) {
  deaths_race <- switch(cause,
    suicide  = sui_deaths,
    homicide = hom_deaths,
    alcohol  = alc_deaths
  )
  deaths_tot <- switch(cause,
    suicide  = sui_deaths_tot,
    homicide = hom_deaths_tot,
    alcohol  = alc_deaths_tot
  )
  for (rv in c("total", "white", "nonwhite")) {
    key <- paste0(cause, "_", rv)
    if (rv == "total") {
      panels[[key]] <- build_panel(deaths_tot, cause, rv)
    } else {
      panels[[key]] <- build_panel(deaths_race, cause, rv)
    }
  }
}

# ── 9. Run Poisson PPML (A&L specification) ───────────────────────────
cat("\n============================================================\n")
cat("ESTIMATION: Poisson PPML (Avery & LaVoice spec)\n")
cat("  County FE + Year FE + State linear trends + Controls\n")
cat("  Weighted by county population, clustered at county\n")
cat("============================================================\n\n")

run_al_poisson <- function(panel, label) {
  cat(sprintf("  %s: %d obs, %d deaths\n",
              label, nrow(panel), sum(panel$deaths)))

  m <- tryCatch(
    fepois(deaths ~ cmhc_post +
             D_pctschlt4_t + D_pctschlgt12_t + D_pctlfue_t + D_lfp_t + D_md_t
           | fips + year^urb_qtile + stfips[year],
           data = panel, offset = ~log_pop,
           weights = ~population,
           cluster = ~fips),
    error = function(e) {
      cat(sprintf("    FAILED: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(m)) {
    b  <- coef(m)["cmhc_post"]
    se <- sqrt(diag(vcov(m)))["cmhc_post"]
    p  <- 2 * pnorm(-abs(b / se))
    cat(sprintf("    β = %.4f (SE = %.4f), p = %.4f, N = %d\n", b, se, p, nobs(m)))
  }
  m
}

models <- list()
for (cause in c("suicide", "homicide", "alcohol")) {
  for (rv in c("total", "white", "nonwhite")) {
    key <- paste0(cause, "_", rv)
    cat(sprintf("\n── %s ──\n", key))
    models[[key]] <- run_al_poisson(panels[[key]], key)
  }
}

# ── 10. Also run unweighted for robustness ─────────────────────────────
cat("\n\n── UNWEIGHTED (for comparison) ──\n")
models_uw <- list()
for (cause in c("suicide", "homicide", "alcohol")) {
  for (rv in c("total", "white", "nonwhite")) {
    key <- paste0(cause, "_", rv)
    m <- tryCatch(
      fepois(deaths ~ cmhc_post +
               D_pctschlt4_t + D_pctschlgt12_t + D_pctlfue_t + D_lfp_t + D_md_t
             | fips + year^urb_qtile + stfips[year],
             data = panels[[key]], offset = ~log_pop,
             cluster = ~fips),
      error = function(e) { NULL }
    )
    models_uw[[key]] <- m
    if (!is.null(m)) {
      b  <- coef(m)["cmhc_post"]
      se <- sqrt(diag(vcov(m)))["cmhc_post"]
      cat(sprintf("  %s: β = %.4f (SE = %.4f)\n", key, b, se))
    }
  }
}

# ── 10b. AER-data adult AMR: BGB TWFE + BGB Poisson + A&L Poisson ─────
cat("\n============================================================\n")
cat("AER DATA: Adult AMR (ages 20-49)\n")
cat("  (1) Linear TWFE — BGB specification\n")
cat("  (2) Poisson PPML — BGB specification\n")
cat("  (3) Poisson PPML — A&L specification\n")
cat("============================================================\n\n")

# Build panel from aer_data (already loaded as `aer`)
cat("Building AER adult AMR panel...\n")
aer_panel <- copy(aer)

# Filter to sample counties (drop NYC, LA, Chicago — already in aer_ids)
aer_panel <- aer_panel[fips %in% aer_ids$fips]

# Full year range (matching BGB panel: 1959-1988)
aer_panel <- aer_panel[year <= 1988]

# Merge CMHC openings
aer_panel <- merge(aer_panel, cmhc_openings[, .(fips, cmhc_year_exp)],
                   by = "fips", all.x = TRUE)

# For BGB spec: restrict CMHC cohorts to <= 1975
aer_panel[, cmhc_year_bgb := fifelse(!is.na(cmhc_year_exp) & cmhc_year_exp <= 1975,
                                      cmhc_year_exp, NA_real_)]
aer_panel[, cmhc_post_bgb := as.integer(!is.na(cmhc_year_bgb) & year >= cmhc_year_bgb)]

# For A&L spec: all CMHC cohorts (no restriction)
aer_panel[, cmhc_post_al := as.integer(!is.na(cmhc_year_exp) & year >= cmhc_year_exp)]

# Construct Durb (urban quintile, BGB style)
aer_panel[, urb60 := sum(`_60pcturban` * (year == 1960), na.rm = TRUE), by = fips]
aer_panel[, Durb := cut(urb60, breaks = c(-Inf, 1, 25, 50, 75, Inf),
                         labels = c("0", "1-25", "25-50", "50-75", "75+"),
                         include.lowest = TRUE)]

# Construct death counts (for Poisson); AMR = deaths per 100k
aer_panel[, deaths_ad := amr_ad * copop_ad / 1e5]
aer_panel[, log_pop_ad := log(pmax(copop_ad, 1))]

# Merge pscore controls (for A&L spec — need trend interactions)
aer_panel <- merge(aer_panel, pscore_controls[, .(fips, `_pctschlt4`, `_pctschlgt12`,
                                                   `_pctlfue`, `_lfp`, `_md_per1000`,
                                                   `_pcturb`, urb_qtile)],
                   by = "fips", all.x = TRUE)
aer_panel[, t := year - 1960L]
aer_panel[, D_pctschlt4_t   := `_pctschlt4`   * t]
aer_panel[, D_pctschlgt12_t := `_pctschlgt12` * t]
aer_panel[, D_pctlfue_t     := `_pctlfue`     * t]
aer_panel[, D_lfp_t         := `_lfp`         * t]
aer_panel[, D_md_t          := `_md_per1000`  * t]

# Drop missing outcome/population
aer_panel <- aer_panel[!is.na(amr_ad) & !is.na(copop_ad) & copop_ad > 0]

cat(sprintf("  AER panel: %d obs, %d counties, years %d-%d\n",
            nrow(aer_panel), uniqueN(aer_panel$fips),
            min(aer_panel$year), max(aer_panel$year)))
cat(sprintf("  CMHC post (BGB, cohorts<=1975): %d\n", sum(aer_panel$cmhc_post_bgb)))
cat(sprintf("  CMHC post (A&L, all cohorts):   %d\n", sum(aer_panel$cmhc_post_al)))

# --- (1) Linear TWFE — BGB specification ---
# Matches 15_fig05_cmhc_es_ad.R exactly (static DD instead of event study)
cat("\n── (1) Linear TWFE — BGB spec ──\n")
m_aer_twfe <- tryCatch(
  feols(amr_ad ~ cmhc_post_bgb + D_tot_act_md_t + H_bpc
        | fips + year + year^Durb + year^stfips,
        data = aer_panel,
        weights = ~popwt_ad,
        cluster = ~fips),
  error = function(e) {
    cat(sprintf("  FAILED: %s\n", e$message))
    NULL
  }
)
if (!is.null(m_aer_twfe)) {
  b  <- coef(m_aer_twfe)["cmhc_post_bgb"]
  se <- sqrt(diag(vcov(m_aer_twfe)))["cmhc_post_bgb"]
  p  <- 2 * pnorm(-abs(b / se))
  cat(sprintf("  TWFE β = %.4f (SE = %.4f), p = %.4f, N = %d\n",
              b, se, p, nobs(m_aer_twfe)))
}

# --- (2) Poisson PPML — BGB specification ---
# Same controls/FE/weights as TWFE, but Poisson on death counts
cat("\n── (2) Poisson PPML — BGB spec ──\n")
m_aer_pois_bgb <- tryCatch(
  fepois(deaths_ad ~ cmhc_post_bgb + D_tot_act_md_t + H_bpc
         | fips + year + year^Durb + year^stfips,
         data = aer_panel, offset = ~log_pop_ad,
         weights = ~popwt_ad,
         cluster = ~fips),
  error = function(e) {
    cat(sprintf("  FAILED: %s\n", e$message))
    NULL
  }
)
if (!is.null(m_aer_pois_bgb)) {
  b  <- coef(m_aer_pois_bgb)["cmhc_post_bgb"]
  se <- sqrt(diag(vcov(m_aer_pois_bgb)))["cmhc_post_bgb"]
  p  <- 2 * pnorm(-abs(b / se))
  cat(sprintf("  Poisson(BGB) β = %.4f (SE = %.4f), p = %.4f, N = %d\n",
              b, se, p, nobs(m_aer_pois_bgb)))
}

# --- (3) Poisson PPML — A&L specification ---
# A&L controls, state linear trends, urban quintile × year FE,
# contemporaneous pop weights, all CMHC cohorts, years 1969-1988
cat("\n── (3) Poisson PPML — A&L spec ──\n")
aer_al <- aer_panel[year >= 1969]  # A&L uses 1969-1988

m_aer_pois_al <- tryCatch(
  fepois(deaths_ad ~ cmhc_post_al +
           D_pctschlt4_t + D_pctschlgt12_t + D_pctlfue_t + D_lfp_t + D_md_t
         | fips + year^urb_qtile + stfips[year],
         data = aer_al, offset = ~log_pop_ad,
         weights = ~copop_ad,
         cluster = ~fips),
  error = function(e) {
    cat(sprintf("  FAILED: %s\n", e$message))
    NULL
  }
)
if (!is.null(m_aer_pois_al)) {
  b  <- coef(m_aer_pois_al)["cmhc_post_al"]
  se <- sqrt(diag(vcov(m_aer_pois_al)))["cmhc_post_al"]
  p  <- 2 * pnorm(-abs(b / se))
  cat(sprintf("  Poisson(A&L) β = %.4f (SE = %.4f), p = %.4f, N = %d\n",
              b, se, p, nobs(m_aer_pois_al)))
}

# ── 11. Results summary ───────────────────────────────────────────────
cat("\n============================================================\n")
cat("RESULTS: A&L Table Format\n")
cat("============================================================\n\n")

fmt <- function(m) {
  if (is.null(m)) return(list(coef = NA, se = NA, p = NA, n = NA, str = "---"))
  b  <- coef(m)["cmhc_post"]
  se <- sqrt(diag(vcov(m)))["cmhc_post"]
  p  <- 2 * pnorm(-abs(b / se))
  st <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
  list(coef = unname(b), se = unname(se), p = p,
       n = nobs(m),
       str = sprintf("%.3f%s (%.3f)", b, st, se))
}

cat("WEIGHTED (A&L spec):\n")
cat("                      (1) Suicide    (2) Homicide    (3) Alcohol\n")
cat("  Panel A: Total      ",
    fmt(models$suicide_total)$str, "  ",
    fmt(models$homicide_total)$str, "  ",
    fmt(models$alcohol_total)$str, "\n")
cat("  Panel B: White      ",
    fmt(models$suicide_white)$str, "  ",
    fmt(models$homicide_white)$str, "  ",
    fmt(models$alcohol_white)$str, "\n")
cat("  Panel C: Nonwhite   ",
    fmt(models$suicide_nonwhite)$str, "  ",
    fmt(models$homicide_nonwhite)$str, "  ",
    fmt(models$alcohol_nonwhite)$str, "\n")

cat("\n  A&L targets:\n")
cat("  Panel A: Total       -0.013 (0.016)   -0.092 (0.070)   -0.021 (0.074)\n")
cat("  Panel B: White       -0.003 (0.015)   -0.004 (0.048)    0.007 (0.094)\n")
cat("  Panel C: Nonwhite   -0.085* (0.051)  -0.153** (0.072)  -0.018 (0.058)\n")

cat("\nUNWEIGHTED:\n")
cat("                      (1) Suicide    (2) Homicide    (3) Alcohol\n")
cat("  Panel A: Total      ",
    fmt(models_uw$suicide_total)$str, "  ",
    fmt(models_uw$homicide_total)$str, "  ",
    fmt(models_uw$alcohol_total)$str, "\n")
cat("  Panel B: White      ",
    fmt(models_uw$suicide_white)$str, "  ",
    fmt(models_uw$homicide_white)$str, "  ",
    fmt(models_uw$alcohol_white)$str, "\n")
cat("  Panel C: Nonwhite   ",
    fmt(models_uw$suicide_nonwhite)$str, "  ",
    fmt(models_uw$homicide_nonwhite)$str, "  ",
    fmt(models_uw$alcohol_nonwhite)$str, "\n")

# N obs
cat("\nObservations:\n")
for (rv in c("total", "white", "nonwhite")) {
  key <- paste0("suicide_", rv)
  cat(sprintf("  %s: %s\n", rv,
              if (!is.null(models[[key]])) nobs(models[[key]]) else "NA"))
}

# AER-data adult AMR results
fmt_custom <- function(m, varname = "cmhc_post") {
  if (is.null(m)) return("---")
  b  <- coef(m)[varname]
  se <- sqrt(diag(vcov(m)))[varname]
  p  <- 2 * pnorm(-abs(b / se))
  st <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
  sprintf("%.3f%s (%.3f)", b, st, se)
}

cat("\n── AER DATA: Adult AMR (ages 20-49) ──\n")
cat(sprintf("  (1) Linear TWFE (BGB):    %s\n",
    fmt_custom(m_aer_twfe, "cmhc_post_bgb")))
cat(sprintf("  (2) Poisson PPML (BGB):   %s\n",
    fmt_custom(m_aer_pois_bgb, "cmhc_post_bgb")))
cat(sprintf("  (3) Poisson PPML (A&L):   %s\n",
    fmt_custom(m_aer_pois_al, "cmhc_post_al")))
cat(sprintf("  N (TWFE):          %s\n",
    if (!is.null(m_aer_twfe)) nobs(m_aer_twfe) else "NA"))
cat(sprintf("  N (Poisson BGB):   %s\n",
    if (!is.null(m_aer_pois_bgb)) nobs(m_aer_pois_bgb) else "NA"))
cat(sprintf("  N (Poisson A&L):   %s\n",
    if (!is.null(m_aer_pois_al)) nobs(m_aer_pois_al) else "NA"))

# ── 12. Save results CSV ──────────────────────────────────────────────
results <- data.frame()
for (cause in c("suicide", "homicide", "alcohol")) {
  for (rv in c("total", "white", "nonwhite")) {
    key <- paste0(cause, "_", rv)
    for (wt in c("weighted", "unweighted")) {
      m <- if (wt == "weighted") models[[key]] else models_uw[[key]]
      if (!is.null(m)) {
        f <- fmt(m)
        results <- rbind(results, data.frame(
          cause = cause, race = rv, weighted = wt,
          coef = f$coef, se = f$se, pval = f$p, n_obs = f$n,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}
# Add AER-data adult AMR results
aer_models <- list(
  list(m = m_aer_twfe,     var = "cmhc_post_bgb", spec = "linear_twfe_bgb"),
  list(m = m_aer_pois_bgb, var = "cmhc_post_bgb", spec = "poisson_ppml_bgb"),
  list(m = m_aer_pois_al,  var = "cmhc_post_al",  spec = "poisson_ppml_al")
)
for (am in aer_models) {
  if (!is.null(am$m)) {
    b  <- coef(am$m)[am$var]
    se <- sqrt(diag(vcov(am$m)))[am$var]
    p  <- 2 * pnorm(-abs(b / se))
    results <- rbind(results, data.frame(
      cause = "adult_amr", race = "total", weighted = am$spec,
      coef = unname(b), se = unname(se), pval = p, n_obs = nobs(am$m),
      stringsAsFactors = FALSE
    ))
  }
}

write.csv(results, "replication/cmhc_poisson_results.csv", row.names = FALSE)
cat("\nResults saved: replication/cmhc_poisson_results.csv\n")

# ── 13. LaTeX table (A&L format) ──────────────────────────────────────
cat("Generating LaTeX table...\n")

tex_coef <- function(m) {
  if (is.null(m)) return("---")
  b  <- coef(m)["cmhc_post"]
  se <- sqrt(diag(vcov(m)))["cmhc_post"]
  p  <- 2 * pnorm(-abs(b / se))
  st <- ifelse(p < 0.01, "$^{***}$", ifelse(p < 0.05, "$^{**}$",
               ifelse(p < 0.10, "$^{*}$", "")))
  sprintf("$%.3f$%s", b, st)
}
tex_se <- function(m) {
  if (is.null(m)) return("")
  se <- sqrt(diag(vcov(m)))["cmhc_post"]
  sprintf("$(%.3f)$", se)
}
tex_n <- function(m) {
  if (is.null(m)) return("---")
  formatC(nobs(m), format = "d", big.mark = ",")
}

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Poisson PPML Estimates: CMHC Effect on Cause-Specific Mortality}",
  "\\label{tab:poisson_cmhc}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & (1) & (2) & (3) \\\\",
  " & \\textbf{Suicide} & \\textbf{Homicide} & \\textbf{Alcohol} \\\\",
  "\\midrule",
  # Panel A
  "\\multicolumn{4}{l}{Panel A: Total population} \\\\",
  sprintf("\\quad CMHC & %s & %s & %s \\\\",
          tex_coef(models$suicide_total),
          tex_coef(models$homicide_total),
          tex_coef(models$alcohol_total)),
  sprintf(" & %s & %s & %s \\\\[6pt]",
          tex_se(models$suicide_total),
          tex_se(models$homicide_total),
          tex_se(models$alcohol_total)),
  # Panel B
  "\\multicolumn{4}{l}{Panel B: White population} \\\\",
  sprintf("\\quad CMHC & %s & %s & %s \\\\",
          tex_coef(models$suicide_white),
          tex_coef(models$homicide_white),
          tex_coef(models$alcohol_white)),
  sprintf(" & %s & %s & %s \\\\[6pt]",
          tex_se(models$suicide_white),
          tex_se(models$homicide_white),
          tex_se(models$alcohol_white)),
  # Panel C
  "\\multicolumn{4}{l}{Panel C: Non-white population} \\\\",
  sprintf("\\quad CMHC & %s & %s & %s \\\\",
          tex_coef(models$suicide_nonwhite),
          tex_coef(models$homicide_nonwhite),
          tex_coef(models$alcohol_nonwhite)),
  sprintf(" & %s & %s & %s \\\\",
          tex_se(models$suicide_nonwhite),
          tex_se(models$homicide_nonwhite),
          tex_se(models$alcohol_nonwhite)),
  "\\midrule",
  # Footer
  "County fixed effects & Yes & Yes & Yes \\\\",
  "Year fixed effects & Yes & Yes & Yes \\\\",
  "State linear time trend & Yes & Yes & Yes \\\\",
  "Controls & Yes & Yes & Yes \\\\",
  sprintf("Observations & %s & %s & %s \\\\",
          tex_n(models$suicide_total),
          tex_n(models$homicide_total),
          tex_n(models$alcohol_total)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\vspace{6pt}",
  "\\footnotesize",
  paste0("\\textit{Note:} Dependent variable is age-adjusted mortality counts. ",
         "Poisson regressions include controls for the natural log of the ",
         "respective population with the regression coefficient restricted ",
         "to one and linear time trends for percent less than high school ",
         "education, percent high school education, unemployment rate, and ",
         "labor force participation rate. Regressions are weighted by the ",
         "respective county population. Standard errors clustered at the ",
         "county level are in parenthesis. ",
         "$^*p < 0.10$, $^{**}p < 0.05$, $^{***}p < 0.01$. ",
         "Years included: 1969--1988."),
  "\\end{minipage}",
  "\\end{table}"
)

writeLines(tex, "replication/cmhc_poisson_table.tex")
cat("LaTeX table saved: replication/cmhc_poisson_table.tex\n")

cat("\n\nDone.\n")
