#!/usr/bin/env Rscript
# ============================================================================
# 19a_table_cause_nber.R — NBER Cause-Specific AMR Event Studies (Ages 20-49)
#
#   Builds age-adjusted mortality rates from NBER micro-data, merges with the
#   CMHC panel, and runs event studies by cause of death.
#
#   Panels:
#     A: AER cause-specific AMRs (same-sample benchmark)
#     B: NBER-derived cause-specific AMRs (age-adjusted, 1960 weights)
#     C: External cause decomposition (suicide, homicide, poisoning, etc.)
#     D: Race-stratified external causes
#
#   Outputs:
#     - LaTeX tables: table_cause_nber_{a,b,c,d}.tex
#     - Intermediates: nber_deaths_pop.rds, panel_nber_cause.fst (for 19b, 19c)
#
#   Adapted from: server_cmhc/aer_data/15a_cmhc_es_ad_cause_nber.R
# ============================================================================
log_section("19a: NBER Cause-Specific Event Studies")

# ============================================================================
# 1. CONFIGURATION
# ============================================================================

ANALYSIS_YEARS <- setdiff(1959:1988, c(1972, 1978))
ICD9_START <- 1979

# NBER sequential state (01-51) -> FIPS state code
STATE_SEQ_TO_FIPS <- c(
  "01"="01","02"="02","03"="04","04"="05","05"="06","06"="08","07"="09",
  "08"="10","09"="11","10"="12","11"="13","12"="15","13"="16","14"="17",
  "15"="18","16"="19","17"="20","18"="21","19"="22","20"="23","21"="24",
  "22"="25","23"="26","24"="27","25"="28","26"="29","27"="30","28"="31",
  "29"="32","30"="33","31"="34","32"="35","33"="36","34"="37","35"="38",
  "36"="39","37"="40","38"="41","39"="42","40"="44","41"="45","42"="46",
  "43"="47","44"="48","45"="49","46"="50","47"="51","48"="53","49"="54",
  "50"="55","51"="56"
)

# States to skip (crosswalk unreliable): VA, AK, HI, MD, MO, NV
SKIP_FIPS_ST <- c("51", "02", "15", "24", "29", "32")

# NBER 10-year age bins overlapping ages 20-49
NBER_AD_BINS <- c("15-24", "25-34", "35-44", "45-54")

# SEER age codes mapping to NBER bins
SEER_TO_NBER <- data.table(
  seer_age = c(4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L),
  nber_bin = c("15-24","15-24","25-34","25-34","35-44","35-44","45-54","45-54")
)

# 1960 national population shares within ages 20-49
W_1960 <- data.table(
  nber_bin = c("15-24", "25-34", "35-44", "45-54"),
  w_1960   = c(0.157452, 0.332723, 0.351184, 0.158642)
)

# ============================================================================
# 2. CAUSE CODE MAPPING — Table A1, BaileyGoodmanBacon appendix
# ============================================================================

assign_cause <- function(icd_num, is_ecode, year) {
  cause <- integer(length(icd_num))
  ICD8_START <- 1968L

  cause[icd_num >= 800 & icd_num <= 949] <- 7L
  cause[icd_num >= 980 & icd_num <= 999] <- 7L

  ne <- !is_ecode

  # ICD-7 (1959-1967)
  p7 <- ne & year < ICD8_START
  cause[p7 & icd_num >= 400 & icd_num <= 434] <- 2L
  cause[p7 & icd_num == 440]                   <- 2L
  cause[p7 & icd_num >= 330 & icd_num <= 334] <- 3L
  cause[p7 & icd_num >= 441 & icd_num <= 447] <- 3L
  cause[p7 & icd_num >= 140 & icd_num <= 205] <- 4L
  cause[p7 & icd_num >= 1   & icd_num <= 138] <- 5L
  cause[p7 & icd_num >= 490 & icd_num <= 493] <- 5L
  cause[p7 & icd_num == 260]                   <- 6L

  # ICD-8 (1968-1978)
  p8 <- ne & year >= ICD8_START & year < ICD9_START
  cause[p8 & icd_num >= 390 & icd_num <= 429] <- 2L
  cause[p8 & icd_num == 440]                   <- 2L
  cause[p8 & icd_num >= 430 & icd_num <= 438] <- 3L
  cause[p8 & icd_num >= 441 & icd_num <= 448] <- 3L
  cause[p8 & icd_num >= 140 & icd_num <= 209] <- 4L
  cause[p8 & icd_num >= 0   & icd_num <= 136] <- 5L
  cause[p8 & icd_num >= 480 & icd_num <= 486] <- 5L
  cause[p8 & icd_num == 250]                   <- 6L

  # ICD-9 (1979-1988)
  p9 <- ne & year >= ICD9_START
  cause[p9 & icd_num >= 390 & icd_num <= 429] <- 2L
  cause[p9 & icd_num == 440]                   <- 2L
  cause[p9 & icd_num >= 430 & icd_num <= 438] <- 3L
  cause[p9 & icd_num >= 441 & icd_num <= 448] <- 3L
  cause[p9 & icd_num >= 140 & icd_num <= 208] <- 4L
  cause[p9 & icd_num >= 1   & icd_num <= 139] <- 5L
  cause[p9 & icd_num >= 480 & icd_num <= 487] <- 5L
  cause[p9 & icd_num == 250]                   <- 6L

  cause
}

# ============================================================================
# 3. LOAD AND PROCESS NBER DEATHS
# ============================================================================
cat("Loading NBER mortality data ...\n")
nber <- as.data.table(read_parquet(file.path(NBER_DIR, "nber_mortality_counts_clean.parquet")))
cat(sprintf("  Raw: %s rows\n", format(nrow(nber), big.mark = ",")))

nber <- nber[year %in% ANALYSIS_YEARS & age_bin %in% NBER_AD_BINS]

nber[, co_seq := suppressWarnings(as.integer(substr(countyrs, 3, 5)))]
nber <- nber[!is.na(co_seq) & co_seq > 0 & co_seq < 900]

nber[, uc := toupper(trimws(gsub("-$", "", as.character(ucod))))]
nber[, is_e := substr(uc, 1, 1) == "E"]
nber[, icd_num := suppressWarnings(as.integer(
  ifelse(is_e, substr(uc, 2, 4), substr(uc, 1, 3))
))]
nber[is.na(icd_num), icd_num := -1L]

nber[, cause := assign_cause(icd_num, is_e, year)]

# External cause flags
nber[, is_external := (icd_num >= 800 & icd_num <= 999)]
nber[, is_suicide  := (icd_num >= 950 & icd_num <= 959)]
nber[, is_homicide := (icd_num >= 960 & icd_num <= 969)]
nber[, is_poison   := (icd_num >= 850 & icd_num <= 869)]
nber[, is_accident := (icd_num >= 800 & icd_num <= 949) | (icd_num >= 980 & icd_num <= 999)]

# Alcohol-related deaths
nber[, is_alcohol := FALSE]
nber[year >= 1968L & !is_e & icd_num %in% c(291L, 303L, 571L), is_alcohol := TRUE]
nber[year <  1968L & !is_e & icd_num %in% c(307L, 322L, 581L), is_alcohol := TRUE]
nber[icd_num == 860L, is_alcohol := TRUE]

cat(sprintf("  Filtered to adult bins: %s rows\n", format(nrow(nber), big.mark = ",")))

# ============================================================================
# 4. BUILD CROSSWALK: NBER sequential -> FIPS (using SEER county list)
# ============================================================================
cat("\nBuilding NBER -> FIPS crosswalk (SEER-based) ...\n")

SEER_PATH <- file.path(ROOT, "raw", "population_seer", "pop_seer.csv")
seer_sample <- fread(SEER_PATH, select = c("county", "year"))
seer_sample <- seer_sample[year == 1970]
seer_counties <- unique(seer_sample$county)
seer_by_st <- data.table(
  fips = seer_counties,
  st_fips = sprintf("%02d", seer_counties %/% 1000)
)
seer_by_st <- seer_by_st[order(st_fips, fips)]
rm(seer_sample)

nber_by_st <- nber[, .(co_list = list(sort(unique(co_seq)))), by = staters]
nber_by_st[, fips_st := STATE_SEQ_TO_FIPS[staters]]

xwalk_list <- list()
for (i in 1:nrow(nber_by_st)) {
  nber_st <- nber_by_st$staters[i]
  fips_st <- nber_by_st$fips_st[i]
  if (is.na(fips_st) || fips_st %in% SKIP_FIPS_ST) next

  nber_cos <- nber_by_st$co_list[[i]]
  fips_cos <- seer_by_st[st_fips == fips_st]$fips

  n_min <- min(length(nber_cos), length(fips_cos))
  if (n_min == 0) next

  if (length(nber_cos) != length(fips_cos)) {
    cat(sprintf("  Partial match: FIPS %s — NBER %d vs SEER %d counties\n",
                fips_st, length(nber_cos), length(fips_cos)))
  }

  xwalk_list[[length(xwalk_list) + 1]] <- data.table(
    staters = nber_st,
    co_seq  = nber_cos[1:n_min],
    fips    = fips_cos[1:n_min]
  )
}
xwalk <- rbindlist(xwalk_list)
cat(sprintf("  Crosswalk: %d county mappings, %d states\n",
            nrow(xwalk), uniqueN(xwalk$staters)))

nber <- merge(nber, xwalk, by = c("staters", "co_seq"), all.x = FALSE)
cat(sprintf("  Mapped NBER rows: %s\n", format(nrow(nber), big.mark = ",")))

# ============================================================================
# 5. LOAD POPULATION DENOMINATORS (SEER + Census interpolation)
# ============================================================================
cat("\nLoading population denominators ...\n")

# 5a. SEER populations (1969-1988)
cat("  Loading SEER (1969-1988) ...\n")
seer <- fread(SEER_PATH)
seer <- seer[year %in% 1969:1988]
seer_pop <- seer[, .(pop = sum(pop)), by = .(county, year, age)]
rm(seer)

pop_seer <- merge(seer_pop[age %in% 4:11], SEER_TO_NBER, by.x = "age", by.y = "seer_age")
pop_seer <- pop_seer[, .(pop_bin = sum(pop)), by = .(county, year, nber_bin)]
setnames(pop_seer, "county", "fips")
rm(seer_pop)

cat(sprintf("    SEER pop by bin: %s rows, %d counties, years %d-%d\n",
            format(nrow(pop_seer), big.mark = ","),
            uniqueN(pop_seer$fips),
            min(pop_seer$year), max(pop_seer$year)))

# 5b. Census populations (1950, 1960)
cat("  Loading census data (1950, 1960) ...\n")
CEN_DIR <- file.path(ROOT, "raw", "population_cen")
cen50 <- as.data.table(read_dta(file.path(CEN_DIR, "cen1950_icpsr.dta")))
cen60 <- as.data.table(read_dta(file.path(CEN_DIR, "cen1960_icpsr.dta")))

cen50 <- cen50[level == 1 & !is.na(fips)]
cen60 <- cen60[level == 1 & !is.na(fips)]
cen50[, fips := as.integer(fips)]
cen60[, fips := as.integer(fips)]

for (col in c("pop1519","pop2024","pop2529","pop3034","pop3539","pop4044","pop4549","pop5054")) {
  if (col %in% names(cen50)) cen50[, (col) := as.numeric(get(col))]
  if (col %in% names(cen60)) cen60[, (col) := as.numeric(get(col))]
}

cen50_bins <- cen50[, .(
  fips    = fips,
  `15-24` = pop1519 + pop2024,
  `25-34` = pop2529 + pop3034,
  `35-44` = pop3539 + pop4044,
  `45-54` = pop4549 + pop5054
)]
cen50_long <- melt(cen50_bins, id.vars = "fips",
                   variable.name = "nber_bin", value.name = "pop_1950")
cen50_long[, nber_bin := as.character(nber_bin)]

cen60_bins <- cen60[, .(
  fips    = fips,
  `15-24` = pop1519 + pop2024,
  `25-34` = pop2529 + pop3034,
  `35-44` = pop3539 + pop4044,
  `45-54` = pop4549 + pop5054
)]
cen60_long <- melt(cen60_bins, id.vars = "fips",
                   variable.name = "nber_bin", value.name = "pop_1960")
cen60_long[, nber_bin := as.character(nber_bin)]

cat(sprintf("    Census 1950: %d counties, Census 1960: %d counties\n",
            uniqueN(cen50_long$fips), uniqueN(cen60_long$fips)))

# 5c. Interpolate populations for 1959-1968
cat("  Interpolating populations (1959-1968) ...\n")
seer_1969 <- pop_seer[year == 1969, .(fips, nber_bin, pop_1969 = pop_bin)]

benchmarks <- merge(cen50_long, cen60_long, by = c("fips", "nber_bin"), all = TRUE)
benchmarks <- merge(benchmarks, seer_1969, by = c("fips", "nber_bin"), all = TRUE)

interp_list <- list()
for (yr in 1959:1968) {
  if (yr <= 1959) {
    alpha <- (yr - 1950) / 10
    yr_pop <- benchmarks[!is.na(pop_1950) & !is.na(pop_1960),
                         .(fips, nber_bin, year = as.integer(yr),
                           pop_bin = pop_1950 + alpha * (pop_1960 - pop_1950))]
  } else if (yr == 1960) {
    yr_pop <- benchmarks[!is.na(pop_1960),
                         .(fips, nber_bin, year = 1960L, pop_bin = as.double(pop_1960))]
  } else {
    alpha <- (yr - 1960) / 9
    yr_pop <- benchmarks[!is.na(pop_1960) & !is.na(pop_1969),
                         .(fips, nber_bin, year = as.integer(yr),
                           pop_bin = pop_1960 + alpha * (pop_1969 - pop_1960))]
  }
  interp_list[[length(interp_list) + 1]] <- yr_pop
}
pop_interp <- rbindlist(interp_list)

cat(sprintf("    Interpolated: %s rows, %d counties, years 1959-1968\n",
            format(nrow(pop_interp), big.mark = ","),
            uniqueN(pop_interp$fips)))

# 5d. Combine all population sources
pop_by_bin <- rbind(
  pop_interp,
  pop_seer[year >= 1969, .(fips, nber_bin, year, pop_bin)]
)

cat(sprintf("  Combined pop series: %s rows, %d counties, years %d-%d\n",
            format(nrow(pop_by_bin), big.mark = ","),
            uniqueN(pop_by_bin$fips),
            min(pop_by_bin$year), max(pop_by_bin$year)))

rm(pop_seer, cen50, cen60, cen50_bins, cen60_bins, cen50_long, cen60_long,
   seer_1969, benchmarks, pop_interp, interp_list)

# ============================================================================
# 7. AGGREGATE NBER DEATHS BY FIPS x YEAR x BIN x CAUSE
# ============================================================================
cat("\nAggregating deaths by county x year x bin x cause ...\n")

deaths_by_bin <- nber[, .(deaths = sum(as.double(deaths))),
                       by = .(fips, year, age_bin, cause)]
deaths_by_bin[cause == 0L, cause := 8L]

deaths_all <- nber[, .(deaths = sum(as.double(deaths))),
                    by = .(fips, year, age_bin)]
deaths_all[, cause := 0L]
deaths_by_bin <- rbind(deaths_by_bin, deaths_all)

cat(sprintf("  Deaths: %s county-year-bin-cause rows\n",
            format(nrow(deaths_by_bin), big.mark = ",")))

# Panel C: External cause deaths
cat("  Aggregating Panel C (external cause) deaths ...\n")
ext_cause_flags <- list(
  external = "is_external", suicide = "is_suicide", homicide = "is_homicide",
  poison = "is_poison", accident = "is_accident", alcohol = "is_alcohol"
)
deaths_ext_list <- list()
for (ec_name in names(ext_cause_flags)) {
  flag_col <- ext_cause_flags[[ec_name]]
  d_ext <- nber[get(flag_col) == TRUE, .(deaths = sum(as.double(deaths))),
                by = .(fips, year, age_bin)]
  d_ext[, ext_cause := ec_name]
  deaths_ext_list[[ec_name]] <- d_ext
}
deaths_ext <- rbindlist(deaths_ext_list)
cat(sprintf("    Panel C deaths: %s rows\n", format(nrow(deaths_ext), big.mark = ",")))

# Panel D: Race-stratified cause deaths
cat("  Aggregating Panel D (race-stratified) deaths ...\n")
race_cause_combos <- list(
  w_external  = list(race_cond = quote(race == "1"), flag = "is_external"),
  nw_external = list(race_cond = quote(race != "1"), flag = "is_external"),
  w_suicide   = list(race_cond = quote(race == "1"), flag = "is_suicide"),
  nw_suicide  = list(race_cond = quote(race != "1"), flag = "is_suicide"),
  w_homicide  = list(race_cond = quote(race == "1"), flag = "is_homicide"),
  nw_homicide = list(race_cond = quote(race != "1"), flag = "is_homicide"),
  w_poison    = list(race_cond = quote(race == "1"), flag = "is_poison"),
  nw_poison   = list(race_cond = quote(race != "1"), flag = "is_poison"),
  w_alcohol   = list(race_cond = quote(race == "1"), flag = "is_alcohol"),
  nw_alcohol  = list(race_cond = quote(race != "1"), flag = "is_alcohol")
)
deaths_race_list <- list()
for (rc_name in names(race_cause_combos)) {
  rc <- race_cause_combos[[rc_name]]
  d_race <- nber[eval(rc$race_cond) & get(rc$flag) == TRUE,
                 .(deaths = sum(as.double(deaths))),
                 by = .(fips, year, age_bin)]
  d_race[, race_cause := rc_name]
  deaths_race_list[[rc_name]] <- d_race
}
deaths_race <- rbindlist(deaths_race_list)
cat(sprintf("    Panel D deaths: %s rows\n", format(nrow(deaths_race), big.mark = ",")))

# Save intermediates for 19b and 19c
saveRDS(list(
  deaths_by_bin = deaths_by_bin,
  deaths_ext    = deaths_ext,
  deaths_race   = deaths_race,
  pop_by_bin    = pop_by_bin
), file.path(DATA_DIR, "nber_deaths_pop.rds"))
cat(sprintf("  Saved %s\n", file.path(DATA_DIR, "nber_deaths_pop.rds")))

# ============================================================================
# 8. COMPUTE AGE-ADJUSTED AMRs
# ============================================================================
cat("\nComputing age-adjusted AMRs (1960 national population weights) ...\n")

asmr <- merge(deaths_by_bin, pop_by_bin,
              by.x = c("fips", "year", "age_bin"),
              by.y = c("fips", "year", "nber_bin"),
              all.x = TRUE)
asmr[, asmr_val := fifelse(pop_bin > 0, deaths / pop_bin, 0)]
asmr <- merge(asmr, W_1960, by.x = "age_bin", by.y = "nber_bin")

amr_dt <- asmr[, .(amr = 1e5 * sum(asmr_val * w_1960)), by = .(fips, year, cause)]

amr_wide <- dcast(amr_dt[cause > 0], fips + year ~ cause, value.var = "amr", fill = 0)
for (cc in as.character(2:8)) {
  if (cc %in% names(amr_wide)) {
    setnames(amr_wide, cc, paste0("amr_ad_", cc, "_nber"))
  }
}

amr_allcause <- amr_dt[cause == 0, .(fips, year, amr_ad_nber = amr)]
amr_wide <- merge(amr_wide, amr_allcause, by = c("fips", "year"), all.x = TRUE)

cat(sprintf("  NBER age-adjusted rates: %s county-years, %d counties\n",
            format(nrow(amr_wide), big.mark = ","),
            uniqueN(amr_wide$fips)))

# Panel C AMRs
cat("  Computing Panel C (external cause) AMRs ...\n")
asmr_ext <- merge(deaths_ext, pop_by_bin,
                  by.x = c("fips", "year", "age_bin"),
                  by.y = c("fips", "year", "nber_bin"),
                  all.x = TRUE)
asmr_ext[, asmr_val := fifelse(pop_bin > 0, deaths / pop_bin, 0)]
asmr_ext <- merge(asmr_ext, W_1960, by.x = "age_bin", by.y = "nber_bin")
amr_ext <- asmr_ext[, .(amr = 1e5 * sum(asmr_val * w_1960)),
                     by = .(fips, year, ext_cause)]

amr_ext_wide <- dcast(amr_ext, fips + year ~ ext_cause, value.var = "amr", fill = 0)
for (ec_name in names(ext_cause_flags)) {
  if (ec_name %in% names(amr_ext_wide)) {
    setnames(amr_ext_wide, ec_name, paste0("amr_ad_", ec_name, "_nber"))
  }
}
cat(sprintf("    Panel C AMRs: %s county-years\n", format(nrow(amr_ext_wide), big.mark = ",")))

# Panel D AMRs
cat("  Computing Panel D (race-stratified) AMRs ...\n")
asmr_race <- merge(deaths_race, pop_by_bin,
                   by.x = c("fips", "year", "age_bin"),
                   by.y = c("fips", "year", "nber_bin"),
                   all.x = TRUE)
asmr_race[, asmr_val := fifelse(pop_bin > 0, deaths / pop_bin, 0)]
asmr_race <- merge(asmr_race, W_1960, by.x = "age_bin", by.y = "nber_bin")
amr_race <- asmr_race[, .(amr = 1e5 * sum(asmr_val * w_1960)),
                       by = .(fips, year, race_cause)]

amr_race_wide <- dcast(amr_race, fips + year ~ race_cause, value.var = "amr", fill = 0)
for (rc_name in names(race_cause_combos)) {
  if (rc_name %in% names(amr_race_wide)) {
    setnames(amr_race_wide, rc_name, paste0("amr_ad_", rc_name, "_nber"))
  }
}
cat(sprintf("    Panel D AMRs: %s county-years\n", format(nrow(amr_race_wide), big.mark = ",")))

# ============================================================================
# 9. LOAD PANEL AND MERGE
# ============================================================================
cat("\nLoading cmhc_panel and merging NBER rates ...\n")
panel <- as.data.table(read_parquet(file.path(DATA_DIR, "cmhc_panel.parquet")))

old_nber_cols <- grep("_nber$", names(panel), value = TRUE)
if (length(old_nber_cols) > 0) panel[, (old_nber_cols) := NULL]

panel <- merge(panel, amr_wide, by = c("fips", "year"), all.x = TRUE)
panel <- merge(panel, amr_ext_wide, by = c("fips", "year"), all.x = TRUE)
panel <- merge(panel, amr_race_wide, by = c("fips", "year"), all.x = TRUE)

n_matched <- sum(!is.na(panel$amr_ad_nber))
cat(sprintf("  Panel rows with NBER rates: %s / %s (%.1f%%)\n",
            format(n_matched, big.mark = ","),
            format(nrow(panel), big.mark = ","),
            100 * n_matched / nrow(panel)))

# Scale check
cat("\n  Scale check (pop-weighted mean rates, matched rows):\n")
matched <- panel[!is.na(amr_ad_nber) & !is.na(amr_ad)]
wt <- matched$popwt_ad
cat(sprintf("    amr_ad AER mean:  %.1f (unwt: %.1f)\n",
            weighted.mean(matched$amr_ad, wt, na.rm = TRUE),
            mean(matched$amr_ad, na.rm = TRUE)))
cat(sprintf("    amr_ad NBER mean: %.1f (unwt: %.1f)\n",
            weighted.mean(matched$amr_ad_nber, wt, na.rm = TRUE),
            mean(matched$amr_ad_nber, na.rm = TRUE)))

# Crosswalk quality filter
cat("\n  Crosswalk quality: within-county correlation (NBER vs AER all-cause)...\n")
corr_dt <- panel[!is.na(amr_ad_nber) & !is.na(amr_ad),
                 .(rho = if (.N >= 5) cor(amr_ad, amr_ad_nber) else NA_real_,
                   n = .N),
                 by = fips]
corr_dt <- corr_dt[!is.na(rho)]
cat(sprintf("    Counties with >=5 obs: %d\n", nrow(corr_dt)))
cat(sprintf("    Median within-county rho: %.3f\n", median(corr_dt$rho)))

RHO_THRESHOLD <- 0.5
good_fips <- corr_dt[rho >= RHO_THRESHOLD]$fips
panel[, good_xwalk := fips %in% good_fips]
cat(sprintf("    Keeping %d counties (rho >= %.1f), dropping %d\n",
            length(good_fips), RHO_THRESHOLD, nrow(corr_dt) - length(good_fips)))

# Normalize NBER rates to match AER levels
cat("\n  Normalizing NBER rates to match AER levels ...\n")
matched_rows <- panel[!is.na(amr_ad_nber) & !is.na(amr_ad)]
wt <- matched_rows$popwt_ad

aer_wm <- weighted.mean(matched_rows$amr_ad, wt, na.rm = TRUE)
nber_wm <- weighted.mean(matched_rows$amr_ad_nber, wt, na.rm = TRUE)
sf_all <- aer_wm / nber_wm
panel[, amr_ad_nber := amr_ad_nber * sf_all]
cat(sprintf("    All-cause: AER=%.1f NBER=%.1f -> scale=%.4f\n", aer_wm, nber_wm, sf_all))

for (cc in 2:7) {
  aer_col  <- paste0("amr_ad_", cc)
  nber_col <- paste0("amr_ad_", cc, "_nber")
  if (aer_col %in% names(panel) && nber_col %in% names(panel)) {
    am <- weighted.mean(matched_rows[[aer_col]], wt, na.rm = TRUE)
    nm <- weighted.mean(matched_rows[[nber_col]], wt, na.rm = TRUE)
    sf <- am / nm
    panel[, (nber_col) := get(nber_col) * sf]
    cat(sprintf("    %s: AER=%.2f NBER=%.2f -> scale=%.4f\n", aer_col, am, nm, sf))
  }
}
panel[, amr_ad_8_nber := amr_ad_8_nber * sf_all]

# ============================================================================
# 10. EVENT STUDIES
# ============================================================================

causes <- c(
  amr_ad   = "All Causes",
  amr_ad_2 = "Cardiovascular",
  amr_ad_3 = "Cerebrovascular",
  amr_ad_4 = "Cancer",
  amr_ad_5 = "Infectious Disease",
  amr_ad_6 = "Diabetes",
  amr_ad_7 = "Accidents",
  amr_ad_8 = "Other"
)

time_bins <- list(
  "[-6,-1]"  = -6:-1,
  "[0,3]"    = 0:3,
  "[4,7]"    = 4:7,
  "[8,13]"   = 8:13
)

nber_extract_es_coefs <- function(mod, max_et = 13) {
  cf <- coef(mod)
  se <- sqrt(diag(vcov(mod)))
  nms <- names(cf)
  idx <- grepl("^event_time_binned::", nms)
  et_vals <- as.integer(sub("^event_time_binned::(-?[0-9]+)$", "\\1", nms[idx]))
  df <- data.frame(event_time = et_vals, coefficient = unname(cf[idx]),
                   se = unname(se[idx]), stringsAsFactors = FALSE)
  df <- rbind(df, data.frame(event_time = -1L, coefficient = 0, se = 0))
  df <- df[order(df$event_time), ]
  df <- df[df$event_time <= max_et, ]
  rownames(df) <- NULL
  df
}

star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

run_panel <- function(dv_names, label, cause_dict = causes, data = panel) {
  cat(sprintf("\n  -- %s --\n", label))
  results <- list()
  for (dv in dv_names) {
    if (!(dv %in% names(data)) || all(is.na(data[[dv]]))) {
      cat(sprintf("  Skipping %s (not found or all NA)\n", dv))
      next
    }
    cause_label <- cause_dict[sub("_nber$", "", dv)]
    cat(sprintf("  Estimating: %s (%s) ...\n", dv, cause_label))
    fml <- as.formula(sprintf(
      "%s ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc | fips + year + year^Durb + year^stfips + fips[year]",
      dv
    ))
    mod <- feols(fml, data = data, weights = ~popwt_ad, cluster = ~fips)
    coef_df <- nber_extract_es_coefs(mod, max_et = 13)
    bin_vals <- sapply(time_bins, function(et_range) {
      rows <- coef_df[coef_df$event_time %in% et_range, ]
      if (nrow(rows) == 0) return(list(mean = NA, se = NA, p = NA))
      avg    <- mean(rows$coefficient)
      avg_se <- sqrt(mean(rows$se^2))
      t_stat <- avg / avg_se
      p_val  <- 2 * pnorm(-abs(t_stat))
      list(mean = avg, se = avg_se, p = p_val)
    }, simplify = FALSE)
    results[[dv]] <- bin_vals
  }
  results
}

# ── Restrict to same sample ──
panel_full <- copy(panel)
panel <- panel[!is.na(amr_ad_nber) & !is.na(amr_ad) & good_xwalk == TRUE]
cat(sprintf("\n  Same-sample restriction: %s rows, %d counties\n",
            format(nrow(panel), big.mark = ","),
            uniqueN(panel$fips)))

# Save restricted panel for downstream scripts (19b, 19c)
fst::write_fst(panel, file.path(DATA_DIR, "panel_nber_cause.fst"))
cat(sprintf("  Saved %s\n", file.path(DATA_DIR, "panel_nber_cause.fst")))

# ── Run panels ──
dv_aer  <- names(causes)
dv_nber <- paste0(names(causes), "_nber")

results_a <- run_panel(dv_aer, "Panel A: AER Variables (same-sample)")
results_b <- run_panel(dv_nber, "Panel B: NBER Variables (same-sample)")

causes_c <- c(
  amr_ad_external = "All External",
  amr_ad_suicide  = "Suicide",
  amr_ad_homicide = "Homicide",
  amr_ad_poison   = "Poisoning",
  amr_ad_accident = "Accidents",
  amr_ad_alcohol  = "Alcohol"
)
causes_d <- c(
  amr_ad_w_external  = "White External",
  amr_ad_nw_external = "Nonwhite External",
  amr_ad_w_suicide   = "White Suicide",
  amr_ad_nw_suicide  = "Nonwhite Suicide",
  amr_ad_w_homicide  = "White Homicide",
  amr_ad_nw_homicide = "Nonwhite Homicide",
  amr_ad_w_poison    = "White Poisoning",
  amr_ad_nw_poison   = "Nonwhite Poisoning",
  amr_ad_w_alcohol   = "White Alcohol",
  amr_ad_nw_alcohol  = "Nonwhite Alcohol"
)

dv_ext  <- paste0(names(causes_c), "_nber")
dv_race <- paste0(names(causes_d), "_nber")

results_c <- run_panel(dv_ext, "Panel C: External Causes (NBER)", cause_dict = causes_c)
results_d <- run_panel(dv_race, "Panel D: Race-Stratified (NBER)", cause_dict = causes_d)

panel <- panel_full

# ============================================================================
# 11. GENERATE LATEX TABLES
# ============================================================================
cat("\nGenerating LaTeX tables ...\n")

make_tex_table <- function(results, dv_names, cause_dict, caption, label,
                            note = NULL) {
  tex <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{tab:%s}", label),
    "\\small",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & $[-6,-1]$ & $[0,3]$ & $[4,7]$ & $[8,13]$ \\\\",
    "\\midrule"
  )

  for (dv in dv_names) {
    if (is.null(results[[dv]])) next
    cause_label <- cause_dict[dv]
    if (is.na(cause_label)) cause_label <- cause_dict[sub("_nber$", "", dv)]

    coef_vals <- character(4)
    se_vals   <- character(4)
    for (j in seq_along(names(time_bins))) {
      bn <- names(time_bins)[j]
      bv <- results[[dv]][[bn]]
      stars <- star(bv$p)
      coef_vals[j] <- sprintf("%.3f%s", bv$mean, stars)
      se_vals[j]   <- sprintf("(%.3f)", bv$se)
    }

    tex <- c(tex,
      sprintf("%s & %s \\\\", cause_label, paste(coef_vals, collapse = " & ")),
      sprintf(" & %s \\\\", paste(se_vals, collapse = " & "))
    )
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")

  if (!is.null(note)) {
    tex <- c(tex,
      sprintf("\\begin{minipage}{\\textwidth}"),
      sprintf("\\vspace{0.5em}\\footnotesize \\textit{Notes:} %s", note),
      "\\end{minipage}"
    )
  }

  tex <- c(tex, "\\end{table}")
  tex
}

note_common <- paste(
  "Each cell reports the mean event-study coefficient across event times in the indicated bin.",
  "Standard errors (in parentheses) computed as $\\sqrt{\\text{mean}(se_i^2)}$ within each bin.",
  "All specifications include county, year, urban$\\times$year, and state$\\times$year fixed effects,",
  "county-specific linear trends, and controls for physician supply and hospital beds.",
  "Population-weighted (1960 base). Clustered at county level.",
  "* $p<0.10$, ** $p<0.05$, *** $p<0.01$."
)

# Panel A+B
tex_ab <- make_tex_table(
  c(results_a, results_b),
  c(dv_aer, dv_nber),
  c(setNames(paste0(causes, " (AER)"), names(causes)),
    setNames(paste0(causes, " (NBER)"), paste0(names(causes), "_nber"))),
  "Cause-Specific Mortality: AER vs NBER-Derived AMRs (Ages 20--49)",
  "cause_nber_ab",
  note = note_common
)
writeLines(tex_ab, file.path(TABLE_DIR, "table_cause_nber_ab.tex"))
cat(sprintf("  Saved %s\n", file.path(TABLE_DIR, "table_cause_nber_ab.tex")))

# Panel C
tex_c <- make_tex_table(
  results_c, dv_ext, causes_c,
  "External Cause Decomposition: NBER-Derived AMRs (Ages 20--49)",
  "cause_nber_ext",
  note = note_common
)
writeLines(tex_c, file.path(TABLE_DIR, "table_cause_nber_ext.tex"))
cat(sprintf("  Saved %s\n", file.path(TABLE_DIR, "table_cause_nber_ext.tex")))

# Panel D
tex_d <- make_tex_table(
  results_d, dv_race, causes_d,
  "Race-Stratified External Causes: NBER-Derived AMRs (Ages 20--49)",
  "cause_nber_race",
  note = note_common
)
writeLines(tex_d, file.path(TABLE_DIR, "table_cause_nber_race.tex"))
cat(sprintf("  Saved %s\n", file.path(TABLE_DIR, "table_cause_nber_race.tex")))

# Save CSV for reference
cause_csv <- data.table()
for (panel_name in c("A", "B", "C", "D")) {
  res <- switch(panel_name, A = results_a, B = results_b, C = results_c, D = results_d)
  dvs <- switch(panel_name, A = dv_aer, B = dv_nber, C = dv_ext, D = dv_race)
  for (dv in dvs) {
    if (is.null(res[[dv]])) next
    for (bn in names(time_bins)) {
      bv <- res[[dv]][[bn]]
      cause_csv <- rbind(cause_csv, data.table(
        panel = panel_name, outcome = dv, bin = bn,
        mean_coef = bv$mean, se = bv$se, p = bv$p
      ))
    }
  }
}
save_csv(cause_csv, "table_cause_nber_all.csv")
