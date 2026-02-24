#!/usr/bin/env Rscript
# ============================================================================
# cmhc_poisson_v2.R — A&L Table 3 replication with corrected FIPS crosswalk
#
# Key fix: NBER mortality data uses SEQUENTIAL state/county codes (alphabetical),
# NOT standard FIPS codes. We build a crosswalk from the incarceration dataset
# (which has county_name + FIPS) to map NBER sequential codes → real FIPS.
# ============================================================================

library(arrow)
library(data.table)
library(dplyr)
library(fixest)
library(readr)

cat("============================================================\n")
cat("POISSON PPML: Avery & LaVoice Table 3 Replication (v2)\n")
cat("============================================================\n\n")

# ══════════════════════════════════════════════════════════════════════
# STEP 0: Build NBER sequential → FIPS crosswalk
# ══════════════════════════════════════════════════════════════════════

cat("── Building NBER → FIPS crosswalk from incarceration data ──\n")

# NBER sequential state codes → FIPS state codes (alphabetical ordering)
state_alpha <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
                 "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
                 "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
                 "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
                 "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
state_fips  <- c("01","02","04","05","06","08","09","10","11","12",
                 "13","15","16","17","18","19","20","21","22","23",
                 "24","25","26","27","28","29","30","31","32","33",
                 "34","35","36","37","38","39","40","41","42","44",
                 "45","46","47","48","49","50","51","53","54","55","56")
nber_seq_st <- formatC(1:51, width = 2, flag = "0")
state_map   <- data.table(nber_st = nber_seq_st, fips_st = state_fips, abbr = state_alpha)

# Load incarceration data for county names + FIPS
incarc <- fread("raw/incarceration/incarceration_trends_county.csv",
                select = c("year", "quarter", "county_name", "fips"),
                showProgress = FALSE)
earliest_yr <- min(incarc$year)
incarc_yr   <- incarc[year == earliest_yr]
incarc_yr   <- incarc_yr[, .SD[1], by = fips]
incarc_yr   <- incarc_yr[!is.na(fips) & fips > 0]
incarc_yr[, fips_str := formatC(as.integer(fips), width = 5, flag = "0")]
incarc_yr[, fips_st  := substr(fips_str, 1, 2)]
incarc_yr[, fips_co  := substr(fips_str, 3, 5)]
setorder(incarc_yr, fips_st, county_name)

# ── Combine NYC boroughs for crosswalk matching ──
# NBER treats NYC as 1 county; incarceration data has 5 boroughs.
# Combine them so NY county counts match (62 → 58 = NBER's 58).
nyc_fips <- c(36005L, 36047L, 36061L, 36081L, 36085L)
n_nyc_before <- nrow(incarc_yr[fips %in% nyc_fips])
if (n_nyc_before > 1) {
  incarc_yr <- incarc_yr[!fips %in% nyc_fips]
  nyc_row <- data.table(
    year = incarc_yr$year[1], quarter = incarc_yr$quarter[1],
    county_name = "New York City", fips = 36061L,
    fips_str = "36061", fips_st = "36", fips_co = "061"
  )
  incarc_yr <- rbind(incarc_yr, nyc_row, fill = TRUE)
  setorder(incarc_yr, fips_st, county_name)
  cat(sprintf("  NY: Combined %d NYC boroughs into 1\n", n_nyc_before))
}

# For each FIPS state, assign a sequential index (sorted by county_name)
incarc_yr[, seq_idx := seq_len(.N), by = fips_st]
incarc_yr[, nber_co := formatC(seq_idx, width = 3, flag = "0")]

# Now load NBER data to get its county counts per (sequential) state
nber_raw <- as.data.table(read_parquet("raw/nber_mortality/nber_mortality_counts.parquet"))

nber_year_map <- c(
  setNames(1959:1969, as.character(59:69)),
  setNames(1970:1978, as.character(0:8)),
  setNames(1979:1999, as.character(79:99))
)
nber_raw[year %in% names(nber_year_map), year := nber_year_map[year]]
nber_raw[, year := as.integer(year)]
nber_raw <- nber_raw[is.finite(year) & year >= 1969 & year <= 1988]

nber_raw[, nber_st := substr(countyrs, 1, 2)]
nber_raw[, nber_co := substr(countyrs, 3, 5)]

# Unique NBER counties per sequential state (exclude 000, 999)
nber_counties <- unique(nber_raw[nber_co != "000" & nber_co != "999",
                                  .(nber_st, nber_co)])
setorder(nber_counties, nber_st, nber_co)
nber_counties <- merge(nber_counties, state_map[, .(nber_st, fips_st)],
                       by = "nber_st", all.x = TRUE)

# Build crosswalk: for matching states, map NBER sequential → incarceration FIPS
nber_ct  <- nber_counties[!is.na(fips_st), .(n_nber = .N), by = fips_st]
incarc_ct <- incarc_yr[, .(n_incarc = .N), by = fips_st]
comp <- merge(nber_ct, incarc_ct, by = "fips_st", all = TRUE)
comp[, match := (n_nber == n_incarc)]

cat(sprintf("  Exact-match states: %d / %d\n", sum(comp$match, na.rm = TRUE), nrow(comp)))

# ── Variance minimization for mismatched states ──
# For states where county counts differ by a small number, we try dropping
# candidates from the larger list and picking the drop that yields the
# smoothest death-rate mapping (minimum variance of deaths/pop).

# Precompute totals for variance minimization
nber_deaths_by_co <- nber_raw[nber_co != "000" & nber_co != "999",
                               .(total_deaths = sum(deaths, na.rm = TRUE)),
                               by = .(nber_st, nber_co)]
# Load SEER population data (19 age groups → matches NBER age bins exactly)
cat("  Loading SEER population data...\n")
seer <- fread("raw/population_seer/uswbo19agesadj.csv")
seer <- seer[year >= 1969 & year <= 1988]
seer[, fips := as.integer(county)]
seer[, race_group := fifelse(race == 1, "white", "nonwhite")]

# Map SEER 19 age codes (0-18) → NBER age bins
# 0=<1, 1=1-4, 2=5-9, 3=10-14, 4=15-19, 5=20-24, 6=25-29, 7=30-34,
# 8=35-39, 9=40-44, 10=45-49, 11=50-54, 12=55-59, 13=60-64,
# 14=65-69, 15=70-74, 16=75-79, 17=80-84, 18=85+
seer_age_map <- data.table(
  seer_age = 0:18,
  nber_age_bin = c("0", "1-4", "5-14", "5-14", "15-24", "15-24",
                   "25-34", "25-34", "35-44", "35-44", "45-54", "45-54",
                   "55-64", "55-64", "65-74", "65-74", "75-84", "75-84", "85+")
)
seer <- merge(seer, seer_age_map, by.x = "age", by.y = "seer_age", all.x = TRUE)
cat(sprintf("  SEER: %d rows, %d counties, years %d-%d\n",
            nrow(seer), uniqueN(seer$fips), min(seer$year), max(seer$year)))

# Aggregations from SEER
pop_total_by_fips <- seer[, .(total_pop = sum(as.numeric(pop), na.rm = TRUE)), by = .(fips)]

# Age-specific population for direct age standardization
pop_race_age  <- seer[, .(pop = sum(as.numeric(pop), na.rm = TRUE)),
                       by = .(fips, year, race_group, nber_age_bin)]
pop_total_age <- seer[, .(pop = sum(as.numeric(pop), na.rm = TRUE)),
                       by = .(fips, year, nber_age_bin)]

# Standard population: mean county pop by age group (fixed reference)
std_pop <- pop_total_age[, .(std_pop = mean(pop)), by = nber_age_bin]
cat("  Standard population for age adjustment:\n")
print(std_pop)

# Total pop for panel offset/weights (summed across all ages)
pop_race  <- seer[, .(population = sum(as.numeric(pop), na.rm = TRUE)),
                   by = .(fips, year, race_group)]
pop_total <- seer[, .(population = sum(as.numeric(pop), na.rm = TRUE)),
                   by = .(fips, year)]

# Single-drop variance minimization
varmin_drop <- function(st_fips, nber_cos, incarc_rows, drop_from) {
  nber_st_code <- state_map[fips_st == st_fips, nber_st]
  n_nber <- length(nber_cos)
  best_var <- Inf
  best_xw <- NULL

  if (drop_from == "incarc") {
    for (i in 1:nrow(incarc_rows)) {
      remaining <- incarc_rows[-i][order(county_name)]
      if (nrow(remaining) != n_nber) next
      xw <- data.table(nber_st = nber_st_code, nber_co = nber_cos,
                        fips = remaining$fips)
      xw <- merge(xw, nber_deaths_by_co, by = c("nber_st", "nber_co"), all.x = TRUE)
      xw[is.na(total_deaths), total_deaths := 0]
      xw <- merge(xw, pop_total_by_fips, by = "fips", all.x = TRUE)
      xw <- xw[!is.na(total_pop) & total_pop > 0]
      if (nrow(xw) < 2) next
      v <- var(xw$total_deaths / xw$total_pop)
      if (v < best_var) {
        best_var <- v
        best_xw <- data.table(nber_st = nber_st_code, nber_co = nber_cos,
                              fips = remaining$fips)
      }
    }
  } else {
    for (i in 1:n_nber) {
      remaining_nber <- nber_cos[-i]
      incarc_sorted <- incarc_rows[order(county_name)]
      if (length(remaining_nber) != nrow(incarc_sorted)) next
      xw <- data.table(nber_st = nber_st_code, nber_co = remaining_nber,
                        fips = incarc_sorted$fips)
      xw <- merge(xw, nber_deaths_by_co, by = c("nber_st", "nber_co"), all.x = TRUE)
      xw[is.na(total_deaths), total_deaths := 0]
      xw <- merge(xw, pop_total_by_fips, by = "fips", all.x = TRUE)
      xw <- xw[!is.na(total_pop) & total_pop > 0]
      if (nrow(xw) < 2) next
      v <- var(xw$total_deaths / xw$total_pop)
      if (v < best_var) {
        best_var <- v
        best_xw <- data.table(nber_st = nber_st_code, nber_co = remaining_nber,
                              fips = incarc_sorted$fips)
      }
    }
  }

  if (!is.null(best_xw)) {
    cat(sprintf("    %s (%s): matched via varmin (var = %.2e)\n",
                state_map[fips_st == st_fips, abbr], st_fips, best_var))
  }
  best_xw
}

# Greedy multi-drop (for AK: drop 5 from incarceration side)
greedy_multi_drop <- function(st_fips, nber_cos, incarc_rows, n_drops) {
  nber_st_code <- state_map[fips_st == st_fips, nber_st]
  n_nber <- length(nber_cos)
  current <- copy(incarc_rows)

  for (d in 1:n_drops) {
    best_var <- Inf
    best_idx <- NULL
    for (i in 1:nrow(current)) {
      remaining <- current[-i][order(county_name)]
      matched <- remaining[1:min(n_nber, nrow(remaining))]
      nber_use <- nber_cos[1:nrow(matched)]
      xw <- data.table(nber_st = nber_st_code, nber_co = nber_use,
                        fips = matched$fips)
      xw <- merge(xw, nber_deaths_by_co, by = c("nber_st", "nber_co"), all.x = TRUE)
      xw[is.na(total_deaths), total_deaths := 0]
      xw <- merge(xw, pop_total_by_fips, by = "fips", all.x = TRUE)
      xw <- xw[!is.na(total_pop) & total_pop > 0]
      if (nrow(xw) < 2) next
      v <- var(xw$total_deaths / xw$total_pop)
      if (v < best_var) { best_var <- v; best_idx <- i }
    }
    if (is.null(best_idx)) {
      # Fallback: drop county with smallest/no population in SEER
      pops <- merge(current, pop_total_by_fips, by = "fips", all.x = TRUE)
      pops[is.na(total_pop), total_pop := 0]
      best_idx <- which.min(pops$total_pop)
      cat(sprintf("    AK drop %d/%d: FALLBACK '%s' (FIPS %d, smallest pop)\n",
                  d, n_drops, current$county_name[best_idx],
                  current$fips[best_idx]))
    } else {
      cat(sprintf("    AK drop %d/%d: '%s' (FIPS %d), var = %.2e\n",
                  d, n_drops, current$county_name[best_idx],
                  current$fips[best_idx], best_var))
    }
    current <- current[-best_idx]
  }

  final <- current[order(county_name)]
  if (nrow(final) != n_nber) {
    cat(sprintf("    WARNING: AK final %d counties vs %d NBER (skipping AK)\n",
                nrow(final), n_nber))
    return(data.table())
  }
  data.table(nber_st = nber_st_code, nber_co = nber_cos, fips = final$fips)
}

# Process mismatched states
mismatch_xw <- data.table()
cat("  Handling mismatched states (variance minimization)...\n")

# Single-county mismatches: drop 1 from whichever side is larger
single_mismatch <- list(
  list(st = "04", drop = "incarc"),   # AZ: NBER=14, incarc=15
  list(st = "08", drop = "incarc"),   # CO: NBER=63, incarc=64
  list(st = "30", drop = "nber"),     # MT: NBER=57, incarc=56
  list(st = "46", drop = "nber"),     # SD: NBER=67, incarc=66
  list(st = "56", drop = "nber")      # WY: NBER=24, incarc=23
)

for (info in single_mismatch) {
  st <- info$st
  row <- comp[fips_st == st]
  if (nrow(row) == 0) next
  if (!is.na(row$match) && row$match) next
  if (abs(row$n_nber - row$n_incarc) != 1) {
    cat(sprintf("    WARNING: %s diff = %d, skipping single-drop\n",
                st, row$n_nber - row$n_incarc))
    next
  }
  nber_co_sorted <- sort(nber_counties[fips_st == st, nber_co])
  incarc_sub <- incarc_yr[fips_st == st]
  xw <- varmin_drop(st, nber_co_sorted, incarc_sub, info$drop)
  if (!is.null(xw)) mismatch_xw <- rbind(mismatch_xw, xw)
}

# AK (fips_st=02): NBER=24, incarc=29 → drop 5 from incarc (greedy)
ak_row <- comp[fips_st == "02"]
if (nrow(ak_row) > 0 && !is.na(ak_row$match) && !ak_row$match) {
  ak_nber_cos <- sort(nber_counties[fips_st == "02", nber_co])
  ak_incarc   <- incarc_yr[fips_st == "02"]
  ak_n_drop   <- nrow(ak_incarc) - length(ak_nber_cos)
  if (ak_n_drop > 0) {
    cat(sprintf("  AK: dropping %d counties (greedy varmin)\n", ak_n_drop))
    xw_ak <- greedy_multi_drop("02", ak_nber_cos, ak_incarc, ak_n_drop)
    mismatch_xw <- rbind(mismatch_xw, xw_ak)
  }
}

cat(sprintf("  Variance-minimized states: %d additional (%d counties)\n",
            uniqueN(mismatch_xw$nber_st), nrow(mismatch_xw)))

# ── Build crosswalk: exact-match states ──
crosswalk <- data.table()
for (st in comp[match == TRUE, fips_st]) {
  nber_co_sorted  <- sort(nber_counties[fips_st == st, nber_co])
  incarc_co_sorted <- incarc_yr[fips_st == st][order(county_name)]
  xw <- data.table(
    nber_st  = state_map[fips_st == st, nber_st],
    nber_co  = nber_co_sorted,
    fips     = incarc_co_sorted$fips
  )
  crosswalk <- rbind(crosswalk, xw)
}

# Add variance-minimized mismatched states
if (nrow(mismatch_xw) > 0) {
  crosswalk <- rbind(crosswalk, mismatch_xw)
}

n_xw_states <- uniqueN(crosswalk$nber_st)
cat(sprintf("  Crosswalk states: %d / 51 (all except VA)\n", n_xw_states))
cat(sprintf("  Crosswalk entries: %d counties\n", nrow(crosswalk)))

# Apply crosswalk to NBER data
nber_raw <- merge(nber_raw, crosswalk[, .(nber_st, nber_co, fips)],
                  by = c("nber_st", "nber_co"), all.x = TRUE)

mapped   <- sum(!is.na(nber_raw$fips))
unmapped <- sum(is.na(nber_raw$fips))
cat(sprintf("  Mapped: %d rows (%.1f%%)  |  Unmapped: %d rows\n",
            mapped, 100 * mapped / nrow(nber_raw), unmapped))
cat(sprintf("  Unique FIPS: %d\n", uniqueN(nber_raw[!is.na(fips), fips])))

# Keep only mapped rows
nber <- nber_raw[!is.na(fips)]

# ══════════════════════════════════════════════════════════════════════
# STEP 1: ICD codes & cause flagging
# ══════════════════════════════════════════════════════════════════════

# Suicide: ICD-8A 950-959
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

# Homicide: ICD-8A 960-969
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

# Alcohol: A&L main = ICD-8/9 code 303 only
alc_icd78 <- c("303", "303-", paste0("303", 0:9))
alc_icd9  <- c("303",  paste0("303", 0:9))
ALCOHOL_CODES <- unique(c(alc_icd78, alc_icd9))

# Race grouping
nber[, race_group := fifelse(race == "1", "white", "nonwhite")]

# Flag causes
nber[, cause := fcase(
  ucod %in% SUICIDE_CODES,  "suicide",
  ucod %in% HOMICIDE_CODES, "homicide",
  ucod %in% ALCOHOL_CODES,  "alcohol",
  default = "other"
)]

# NBER age bins match SEER aggregations exactly — use directly
nber[, pop_age_bin := age_bin]

cat(sprintf("\n  NBER (mapped): %d rows, years %d-%d, %d counties\n",
            nrow(nber), min(nber$year), max(nber$year), uniqueN(nber$fips)))
cat(sprintf("  Age bin mapping: %d mapped, %d unmapped (nan)\n",
            sum(!is.na(nber$pop_age_bin)), sum(is.na(nber$pop_age_bin))))

# ══════════════════════════════════════════════════════════════════════
# STEP 2: Aggregate deaths
# ══════════════════════════════════════════════════════════════════════

cat("Computing age-adjusted death counts (direct standardization)...\n")

# Direct age standardization function
# For each county-year: adj_count = Σ_a (deaths_a / pop_a) × std_pop_a
age_adjust <- function(deaths_age_dt, pop_age_dt, std_pop_dt) {
  merged <- merge(deaths_age_dt, pop_age_dt,
                  by.x = c("fips", "year", "pop_age_bin"),
                  by.y = c("fips", "year", "nber_age_bin"),
                  all.x = TRUE)
  merged <- merged[!is.na(pop) & pop > 0]
  merged[, rate := deaths / pop]
  merged <- merge(merged, std_pop_dt, by.x = "pop_age_bin", by.y = "nber_age_bin")
  merged[, adj := rate * std_pop]
  merged[, .(deaths = sum(adj, na.rm = TRUE)), by = .(fips, year)]
}

for (cause_val in c("suicide", "homicide", "alcohol")) {
  prefix <- substr(cause_val, 1, 3)

  # Deaths by age group × race
  d_race_age <- nber[cause == cause_val & !is.na(pop_age_bin),
                      .(deaths = sum(deaths, na.rm = TRUE)),
                      by = .(fips, year, race_group, pop_age_bin)]
  adj_list <- list()
  for (rv in c("white", "nonwhite")) {
    d_sub <- d_race_age[race_group == rv, .(fips, year, pop_age_bin, deaths)]
    p_sub <- pop_race_age[race_group == rv, .(fips, year, nber_age_bin, pop)]
    adj_sub <- age_adjust(d_sub, p_sub, std_pop)
    adj_sub[, race_group := rv]
    adj_list[[rv]] <- adj_sub
  }
  assign(paste0(prefix, "_deaths"), rbindlist(adj_list))

  # Deaths by age group × total (all races)
  d_tot_age <- nber[cause == cause_val & !is.na(pop_age_bin),
                     .(deaths = sum(deaths, na.rm = TRUE)),
                     by = .(fips, year, pop_age_bin)]
  assign(paste0(prefix, "_deaths_tot"),
         age_adjust(d_tot_age, pop_total_age, std_pop))
}

cat(sprintf("  Suicide:  %.0f age-adjusted deaths\n", sum(sui_deaths$deaths)))
cat(sprintf("  Homicide: %.0f age-adjusted deaths\n", sum(hom_deaths$deaths)))
cat(sprintf("  Alcohol:  %.0f age-adjusted deaths\n", sum(alc_deaths$deaths)))

# ══════════════════════════════════════════════════════════════════════
# STEP 3: Load population, CMHC openings, controls
# ══════════════════════════════════════════════════════════════════════

# pop_race, pop_total already computed from SEER in STEP 0

cat("Loading CMHC openings...\n")
cmhc_openings <- as.data.table(read_csv("cmhc_data/cmhc_openings.csv", show_col_types = FALSE))
cmhc_openings[, fips := as.integer(fips)]
cat(sprintf("  %d counties with CMHC openings\n", nrow(cmhc_openings)))

cat("Loading covariates...\n")
aer <- as.data.table(fst::read_fst("aer_data/aer_data.fst"))
aer[, fips := as.integer(fips)]
aer_ids <- unique(aer[, .(fips, stfips, cofips)])
# Drop NYC, LA, Chicago
aer_ids <- aer_ids[!(stfips == 36 & cofips == 61) &
                    !(stfips == 6  & cofips == 37) &
                    !(stfips == 17 & cofips == 31)]

# 1970 Census controls from NHGIS (replaces 1960 AER pscore controls)
nhgis <- as.data.table(fst::read_fst("replication/nhgis0003_csv/nhgis_1970_controls.fst"))
nhgis[, urb_qtile := as.factor(dplyr::ntile(pct_urban, 5))]
cat(sprintf("  NHGIS 1970 controls: %d counties\n", nrow(nhgis)))

# ══════════════════════════════════════════════════════════════════════
# STEP 4: Build analysis panels
# ══════════════════════════════════════════════════════════════════════

build_panel <- function(deaths_dt, cause_label, race_val) {
  cat(sprintf("  Building %s × %s panel...\n", cause_label, race_val))

  # AER county set × years 1969-1988 (A&L sample period)
  grid <- CJ(fips = aer_ids$fips, year = 1969:1988)

  if (race_val == "total") {
    panel <- merge(grid, deaths_dt, by = c("fips", "year"), all.x = TRUE)
  } else {
    d <- deaths_dt[race_group == race_val, .(fips, year, deaths)]
    panel <- merge(grid, d, by = c("fips", "year"), all.x = TRUE)
  }
  panel[is.na(deaths), deaths := 0]

  if (race_val == "total") {
    panel <- merge(panel, pop_total, by = c("fips", "year"), all.x = TRUE)
  } else {
    p <- pop_race[race_group == race_val, .(fips, year, population)]
    panel <- merge(panel, p, by = c("fips", "year"), all.x = TRUE)
  }

  panel <- merge(panel, cmhc_openings[, .(fips, cmhc_year_exp)],
                 by = "fips", all.x = TRUE)
  panel[, cmhc_post := as.integer(!is.na(cmhc_year_exp) & year >= cmhc_year_exp)]
  panel <- merge(panel, aer_ids[, .(fips, stfips)], by = "fips", all.x = TRUE)
  panel <- merge(panel, nhgis, by = "fips", all.x = TRUE)

  panel[, t := year - 1960L]
  panel[, D_pctltHS_t := pct_lt_hs  * t]
  panel[, D_pctHS_t   := pct_hs     * t]
  panel[, D_unemp_t   := unemp_rate * t]
  panel[, D_lfp_t     := lfp_rate   * t]

  panel[, log_pop := log(pmax(population, 1))]
  panel <- panel[!is.na(population) & population > 0]

  cat(sprintf("    %d obs, %d counties, %.0f deaths\n",
              nrow(panel), uniqueN(panel$fips), sum(panel$deaths)))
  panel
}

cat("\nBuilding panels...\n")
panels <- list()
for (cause in c("suicide", "homicide", "alcohol")) {
  deaths_race <- switch(cause, suicide=sui_deaths, homicide=hom_deaths, alcohol=alc_deaths)
  deaths_tot  <- switch(cause, suicide=sui_deaths_tot, homicide=hom_deaths_tot, alcohol=alc_deaths_tot)
  for (rv in c("total", "white", "nonwhite")) {
    key <- paste0(cause, "_", rv)
    if (rv == "total") {
      panels[[key]] <- build_panel(deaths_tot, cause, rv)
    } else {
      panels[[key]] <- build_panel(deaths_race, cause, rv)
    }
  }
}

# ══════════════════════════════════════════════════════════════════════
# STEP 5: Estimate Poisson PPML
# ══════════════════════════════════════════════════════════════════════

cat("\n============================================================\n")
cat("ESTIMATION: Poisson PPML (A&L specification)\n")
cat("  County FE + Year FE + State linear trends + Controls\n")
cat("  Weighted by county population, clustered at county\n")
cat("============================================================\n\n")

run_al_poisson <- function(panel, label) {
  cat(sprintf("  %s: %d obs, %.0f deaths\n", label, nrow(panel), sum(panel$deaths)))
  m <- tryCatch(
    fepois(deaths ~ cmhc_post +
             D_pctltHS_t + D_pctHS_t + D_unemp_t + D_lfp_t
           | fips + year^urb_qtile + stfips[year],
           data = panel, offset = ~log_pop,
           weights = ~population,
           cluster = ~fips,
           fixef.rm = "singleton"),
    error = function(e) { cat(sprintf("    FAILED: %s\n", e$message)); NULL }
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

# ══════════════════════════════════════════════════════════════════════
# STEP 6: Print results table
# ══════════════════════════════════════════════════════════════════════

fmt <- function(m) {
  if (is.null(m)) return(list(coef = NA, se = NA, p = NA, n = NA, str = "---"))
  b  <- coef(m)["cmhc_post"]
  se <- sqrt(diag(vcov(m)))["cmhc_post"]
  p  <- 2 * pnorm(-abs(b / se))
  st <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
  list(coef = unname(b), se = unname(se), p = p, n = nobs(m),
       str = sprintf("%.3f%s (%.3f)", b, st, se))
}

cat("\n============================================================\n")
cat("RESULTS: A&L Table 3 Replication\n")
cat("============================================================\n\n")

cat("OUR ESTIMATES:\n")
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

cat("\nA&L TARGETS:\n")
cat("                      (1) Suicide    (2) Homicide    (3) Alcohol\n")
cat("  Panel A: Total       -0.013 (0.016)   -0.092 (0.070)   -0.021 (0.074)\n")
cat("  Panel B: White       -0.003 (0.015)   -0.004 (0.048)    0.007 (0.094)\n")
cat("  Panel C: Nonwhite   -0.085* (0.051)  -0.153** (0.072)  -0.018 (0.058)\n")
cat("  N                    60,316           60,316           60,316\n")

cat("\nOUR N (panel → estimation, fixef.rm = 'singleton'):\n")
for (rv in c("total", "white", "nonwhite")) {
  ns <- c()
  for (cause in c("suicide", "homicide", "alcohol")) {
    key <- paste0(cause, "_", rv)
    n_panel <- nrow(panels[[key]])
    n_est   <- if (!is.null(models[[key]])) nobs(models[[key]]) else NA
    ns <- c(ns, sprintf("%s: %d→%s", cause, n_panel, n_est))
  }
  cat(sprintf("  %s: %s\n", rv, paste(ns, collapse = "  |  ")))
}

cat(sprintf("\nCrosswalk: %d states / 51 (skipped VA)\n", n_xw_states))
cat(sprintf("Unique FIPS in analysis: %d counties\n", uniqueN(panels$suicide_total$fips)))

cat("\n\nDone.\n")
