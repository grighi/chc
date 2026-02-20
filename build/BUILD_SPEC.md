# BUILD SPEC: "Community Mental Health Centers: Then and Now"
## Full Reproducible Pipeline — From Raw Data to All Figures and Tables

**Author**: Gio Righi, UCLA  
**Date**: February 2026  
**Target**: Top-5 Journal (AER, QJE, JPE, Econometrica, REStud)

This spec describes every raw data source, every intermediate dataset, every
analysis, and every output. A single `build.R` script in `build/` will
execute the entire pipeline from top to bottom.

---

## 0. DIRECTORY STRUCTURE

```
build/
├── build.R                        # Master build script (sources everything in order)
├── 00_setup.R                     # Package loading, paths, global parameters
├── 01_prep_cmhc_openings.R        # CMHC treatment timing
├── 02_prep_aer_panel.R            # Historical county-year panel (1959–1988)
├── 03_prep_incarceration.R        # Incarceration data (1970–2024)
├── 04_prep_ccbhc_grants.R         # CCBHC treatment timing from USAspending
├── 05_prep_ccbhc_mortality.R      # WONDER mortality for CCBHC era
├── 06_prep_ccbhc_panel.R          # Contemporary county-year panel (2010–2024)
├── 07_prep_national_mortality.R   # National mortality time series (1959–2023)
├── 10_fig01_federal_budgets.R     # Figure 1
├── 11_fig02_national_mortality.R  # Figure 2 (NEW)
├── 12_fig03_cmhc_rollout_map.R    # Figure 3 (NEW)
├── 13_table01_balance.R           # Table 1 (NEW)
├── 14_fig04_timing_exogeneity.R   # Figure 4
├── 15_fig05_cmhc_es_ad.R          # Figure 5 (main CMHC result)
├── 16_fig06_cmhc_es_eld.R         # Figure 6 (CMHC placebo: 50+)
├── 17_table02_dd_robustness.R     # Table 2 (NEW)
├── 18_fig07_cmhc_chc_robust.R     # Figure 7
├── 19_table03_cause_decomp.R      # Table 3 (NEW — CMHC cause decomposition)
├── 20_fig_cmhc_race.R             # Figure: CMHC by race (NEW)
├── 21_fig_cmhc_heterogeneity.R    # Figure: CMHC heterogeneity (NEW)
├── 22_table_cmhc_incarceration.R  # Table: CMHC incarceration long-diff
├── 23_fig08_ccbhc_es_mort.R       # Figure 8
├── 24_fig09_ccbhc_robust.R        # Figure 9
├── 25_fig10_ccbhc_es_inc.R        # Figure 10
├── 26_table_ccbhc_heterogeneity.R # Table: CCBHC heterogeneity
├── 27_fig_side_by_side.R          # Figure: CMHC vs CCBHC (NEW)
├── 28_appendix_poisson.R          # Appendix: Poisson specification
├── 29_appendix_bacon.R            # Appendix: Bacon decomposition (NEW)
├── 30_appendix_ccbhc_placebo.R    # Appendix: CCBHC 50+ placebo (NEW)
├── 31_appendix_dod_decomp.R       # Appendix: CCBHC deaths-of-despair decomp (NEW)
├── data/                          # Intermediate / master datasets (generated)
│   ├── cmhc_panel.parquet         # Master CMHC-era panel
│   ├── ccbhc_panel.parquet        # Master CCBHC-era panel
│   ├── cmhc_openings.csv          # Cleaned CMHC treatment timing
│   ├── ccbhc_treatment.csv        # Cleaned CCBHC treatment timing
│   ├── national_mortality.parquet # National time series
│   └── ...
├── output/                        # All figures and tables
│   ├── figures/
│   │   ├── fig01_federal_budgets.pdf
│   │   ├── fig02_national_mortality.pdf
│   │   ├── fig03_cmhc_rollout_map.pdf
│   │   ├── fig04_timing_exogeneity.pdf
│   │   ├── fig05_cmhc_es_ad.pdf
│   │   ├── fig06_cmhc_es_eld.pdf
│   │   ├── fig07_cmhc_chc_robust.pdf
│   │   ├── fig08_ccbhc_es_mort.pdf
│   │   ├── fig09_ccbhc_robust.pdf
│   │   ├── fig10_ccbhc_es_inc.pdf
│   │   ├── fig_cmhc_race.pdf
│   │   ├── fig_cmhc_heterogeneity.pdf
│   │   ├── fig_side_by_side.pdf
│   │   ├── fig_bacon_decomp.pdf
│   │   ├── fig_ccbhc_placebo_50plus.pdf
│   │   └── fig_ccbhc_dod_decomp.pdf
│   └── tables/
│       ├── table01_balance.tex
│       ├── table02_dd_robustness.tex
│       ├── table03_cause_decomp_cmhc.tex
│       ├── table_cmhc_incarceration_ld.tex
│       ├── table_ccbhc_heterogeneity.tex
│       ├── table_cmhc_race.tex
│       └── table_appendix_poisson.tex
└── logs/                          # Build logs
```

---

## 1. RAW DATA SOURCES

### 1.1 `aer_data/aer_data.fst` — Bailey & Goodman-Bacon (2015) Replication Panel

**Source**: AER replication package for Bailey & Goodman-Bacon (2015).  
**Format**: FST (originally Stata `.dta`, converted).  
**Primary key**: `(fips, year)` — 5-digit county FIPS × calendar year.  
**Coverage**: ~3,044 counties × 30 years (1959–1988) = ~91,320 rows.  
**Excludes**: Independent cities in VA are merged with parent counties.

**Key variables used**:

| Variable | Description |
|----------|-------------|
| `fips` | 5-digit county FIPS |
| `stfips`, `cofips` | State and county FIPS components |
| `year` | Calendar year (1959–1988) |
| `chc_year_exp` | Year of first non-planning CHC grant |
| `amr` | Age-adjusted mortality rate, all ages |
| `amr_ad` | AMR, ages 20–49 |
| `amr_eld` | AMR, ages 50+ |
| `amr_w_eld` | AMR, white, 50+ |
| `amr_nw_eld` | AMR, nonwhite, 50+ |
| `amr_ad_2` through `amr_ad_7` | AMR 20–49 by cause (CVD, cerebro, cancer, infectious, diabetes, accidents) |
| `copop`, `copop_ad`, `copop_eld` | Contemporary population counts (total, 20–49, 50+) |
| `popwt`, `popwt_ad`, `popwt_eld` | 1960 population weights |
| `_60pcturban` | % urban 1960 |
| `_60pctrurf` | % rural farm 1960 |
| `_60pctnonwhit` | % nonwhite 1960 |
| `_pct59inclt3k` | % income < $3k 1960 |
| `_tot_act_md` | Total active MDs 1960 |
| `_60pctmt64years` | % aged 65+ 1960 |
| `_60medschlmt24` | Median years schooling 1960 |
| `D_tot_act_md_t` | MDs × linear trend |
| `H_bpc` | Hospital beds per capita (AHA) |
| `pcrfund_chc` | Real per-capita CHC funding |
| `grant_chc` | CHC grant dummy |
| `tranpcmcare`, `tranpcmcaid` | Medicare/Medicaid transfers per capita |

**Notes**: This dataset already contains all mortality outcomes, CHC timing, 1960 census characteristics, and AHA hospital data. It is the workhorse for the CMHC-era analysis. Race-specific AMR (`amr_w_eld`, `amr_nw_eld`) is available for 50+ only in the base dataset; **race-specific AMR for ages 20–49 must be computed from NBER Vital Statistics** (see §1.6).

### 1.2 `aer_data/aer_pscore_data.dta` — Propensity Score Covariates

**Source**: AER replication package.  
**Primary key**: `(stfips, cofips)` — cross-sectional, one row per county.  
**Coverage**: ~3,044 counties.

**Key variables used** (all measured in 1960):

| Variable | Description |
|----------|-------------|
| `_popden` | Population density |
| `_pctpopgro` | % population growth 1950–1960 |
| `_pctnw` | % nonwhite |
| `_pcturb`, `_pctrur` | % urban, % rural farm |
| `_pctinclt3k`, `_pctincgt10k` | % income < $3k, > $10k |
| `_pctschlt4`, `_pctschlgt12` | % schooling < 4 yrs, > 12 yrs |
| `_lfp`, `_pctlfue`, `_pctlfmale` | Labor force stats |
| `_pctrent`, `_pctplumb`, `_pcttv`, `_pcttel`, `_pctcar` | Housing characteristics |
| `_md_per1000` | MDs per 1,000 pop |
| `_govtexp_per1000` | Local govt expenditure per 1,000 pop |
| `region` | Census region |

### 1.3 `cmhc_data/cmhc_openings.csv` — CMHC Treatment Timing

**Source**: Constructed from NIMH Mental Health Directory cross-sections (XLS files for 1971, 1973, 1975, 1977, 1979, 1981). Each XLS has columns `GEOID` (county FIPS) and `count` (number of CMHCs). First appearance of a county across directories = opening year.  
**Format**: CSV.  
**Primary key**: `fips` (one row per county that ever had a CMHC).  
**Coverage**: 1,178 counties with CMHC openings.

| Variable | Description |
|----------|-------------|
| `fips` | 5-digit county FIPS |
| `cmhc_year_exp` | Year of first CMHC appearance in directory |

**Year distribution**:

| Year | Counties |
|------|----------|
| 1971 | 208 |
| 1973 | 58 |
| 1975 | 77 |
| 1977 | 715 |
| 1979 | (in 1981 file) |
| 1981 | 119 |

**Notes**: The analysis restricts to CMHCs opening ≤ 1975 for the main event-study specification (to have a sufficiently long post-treatment window within the 1959–1988 panel). This gives 343 treated counties. Later cohorts (1977, 1981) are coded as never-treated in the main spec but used in robustness.

### 1.4 `cmhc_data/priority_ranks_for_eventstudy.csv` — CMHC Priority Rankings

**Source**: Constructed from state-level CMHC priority ranking documents (NARA).  
**Primary key**: `fips`.  
**Coverage**: 2,360 counties with priority rank data.

| Variable | Description |
|----------|-------------|
| `fips` | 5-digit county FIPS |
| `state_abbr` | State abbreviation |
| `area` | Catchment area number |
| `rank` | Priority rank score (continuous) |
| `priority_pctile` | Percentile within state (0–1) |
| `priority_tercile` | 1/2/3 (low/medium/high priority) |
| `high_priority` | = 1 if top tercile |
| `got_cmhc` | = 1 if county received a CMHC |
| `area_has_cmhc` | = 1 if any county in catchment area got CMHC |
| `area_cmhc_year` | Earliest CMHC year in catchment area |
| `high_priority_no_cmhc` | = 1 if high priority but area never got CMHC |

### 1.5 `incarceration_trends_county.csv` / `.fst` — Vera Institute Incarceration Trends

**Source**: Vera Institute of Justice, "Incarceration Trends" dataset.  
**Format**: CSV (245,841 rows) and FST cache.  
**Primary key**: `(fips, year, quarter)`.  
**Coverage**: ~3,142 counties × years 1970–2024 × 4 quarters.

**Key variables used**:

| Variable | Description |
|----------|-------------|
| `fips` | 5-digit county FIPS (needs zero-padding) |
| `year`, `quarter` | Time identifiers |
| `total_pop`, `total_pop_15to64` | Population denominators |
| `total_jail_pop` | Total jail population |
| `total_jail_pop_rate` | Jail pop per 100k (15–64) |
| `black_jail_pop`, `black_jail_pop_rate` | Black jail pop and rate |
| `total_prison_pop`, `total_prison_pop_rate` | Prison pop and rate |
| `total_incarceration`, `total_incarceration_rate` | Jail + prison combined |
| `urbanicity`, `region`, `division` | Geographic classifiers |
| `female_jail_pop`, `male_jail_pop` | Gender decomposition |
| `total_jail_pretrial` | Pre-trial jail pop |
| `total_jail_adm`, `total_jail_dis` | Admissions and discharges |

**Notes**: Quarterly data is averaged to annual county means. The `.fst` cache has pre-computed per-capita rates.

### 1.6 `nber_mortality/nber_mortality_counts.parquet` — NBER Vital Statistics Micro-Data Counts

**Source**: NBER Vital Statistics Multiple Cause of Death files, 1959–2004. Aggregated by `mortality_analysis.py`.  
**Format**: Parquet.  
**Primary key**: `(year, race, age_bin, ucod)`.  
**Coverage**: National-level counts by year × race × age group × underlying cause of death.

**Variables**:

| Variable | Description |
|----------|-------------|
| `year` | Calendar year |
| `race` | Race code |
| `age_bin` | Age group (0, 1–4, 5–14, 15–24, 25–34, 35–44, 45–54, 55–64, 65–74, 75–84, 85+) |
| `ucod` | ICD underlying cause of death code |
| `count` | Number of deaths |

**Notes**: This is aggregated at the national level for the national mortality time-series figure. For county-level race-specific mortality, the raw NBER micro-data must be re-aggregated at the county level (see §1.7). The AER data provides race-stratified mortality only for 50+ age groups.

### 1.7 Race-Specific County Mortality (NEEDS CONSTRUCTION)

**Source**: NBER Vital Statistics micro-data OR CDC WONDER restricted-use files.  
**Needed for**: CMHC event study by race (Figure: CMHC by race, ages 20–49).  
**Construction**: Aggregate NBER mortality micro-data at the county × year × race × age-group level to obtain `amr_w_ad` (white, 20–49) and `amr_nw_ad` (nonwhite, 20–49). This variable does NOT exist in the AER replication data.

**Fallback**: If county-level race-specific AMR for 20–49 cannot be constructed from available micro-data, use the Avery & LaVoice (2023) approach: restrict the age range to match their specification and compare white vs. nonwhite effects within that framework.

**Action item**: Check whether the AER `aer_data.dta` has latent race × age mortality variables not included in the description file. If not, pull from NBER micro-data at county level (requires the fixed-width `.dat` files, not just the aggregated parquet).

### 1.8 CCBHC Grant Data — USAspending.gov

**Source**: USAspending.gov bulk download, CFDA 93.829 and 93.696.  
**Files**:
- `ccbhc/Assistance_PrimeAwardSummaries_2026-02-09_H21M27S10_1_93.829.csv` (822 rows, 100 columns) — CCBHC expansion grants
- `ccbhc/Assistance_PrimeAwardSummaries_2026-02-11_H16M34S02_1_93.696.csv` (509 rows) — additional grants

**Primary key**: `assistance_award_unique_key`.

**Key variables used**:

| Variable | Description |
|----------|-------------|
| `period_of_performance_start_date` | Grant start date (→ treatment year) |
| `prime_award_summary_recipient_county_fips_code` | County FIPS of recipient |
| `prime_award_summary_recipient_state_fips_code` | State FIPS |
| `total_obligated_amount` | Grant amount |

**Processing**:
1. Drop planning grants from 93.829 (start date = 2015-10-23, before 2017).
2. Extract `treat_year = year(period_of_performance_start_date)`.
3. Keep first grant per county: `treat_year = min(treat_year) by fips`.
4. Restrict to `treat_year ∈ 2018:2024`.

**Derived variables**:
- Planning-grant states: states with grants starting 2015-10-23 (used as robustness comparison group).
- Medicaid waiver states: MN, MO, NV, NJ, NY, OK, OR, PA (FIPS: 27, 29, 32, 34, 36, 40, 41, 42).

### 1.9 CCBHC-Era Mortality — CDC WONDER

**Source**: CDC WONDER Multiple Cause of Death (1999–2020, final) and Provisional Mortality (2018–present).  
**Files** (in `ccbhc/`):
- `Deaths_2010-2015_25-44_MCOD.csv` — County × year, ages 25–44, all-cause, 2010–2015
- `Deaths_2016-2020_25-44_MCOD.csv` — Same, 2016–2020
- `Deaths_2018-2023_25-44_MCOD.csv` — Same, 2018–2023 (provisional for post-2020)
- `Provisional Mortality Statistics, 2018 through Last Week.csv` — 2018–2025, ages 25–44
- `Mortality_ICD_F.csv` — ICD-F codes (mental/behavioral), 2012–2017
- `Provisional_Mortality_ICD_F.csv` — Same, provisional
- `Mortality_U_V_codes.csv` — External causes of death
- `Mortality_U_V_codes_2018-25.csv` — Same, provisional
- `Mortality_Covid.csv`, `Mortality_non_F_or_Covid.csv` — Cause decomposition files

**Format**: CDC WONDER CSVs with columns `County`, `County Code` (5-digit FIPS), `Year Code`, `Deaths`, `Population`, `Crude Rate`. Values may be `"Suppressed"` (Deaths < 10) or `"Unreliable"`.

**Primary key**: `(County Code, Year Code)`.  
**Coverage**: ~800–1,800 counties per year (depending on suppression threshold).

**Additional WONDER data** (in `wonder_mortality/data/`):
- `county_sex_race_hispanic_origin_adults_all_races_all_causes.parquet` — County-level, sex × race × Hispanic origin, adult age groups, 1999–2020
- `county_sex_race_hispanic_origin_20-24_..._provisional.parquet` — Same, provisional era

### 1.10 `ccbhc/directories/facilities_geocoded.rds` — SAMHSA MH Directory Facilities

**Source**: SAMHSA Mental Health Directory PDFs (2017–2020), parsed → geocoded.  
**Format**: RDS (data.table).  
**Primary key**: `(facility_name, year)`.  
**Coverage**: ~10,000 facilities per year, mapped to county FIPS.

**Key variables**:

| Variable | Description |
|----------|-------------|
| `county_fips` | 5-digit county FIPS |
| `year` | Directory year (2017–2020) |
| `facility_name` | Facility name |
| `services` | Space-delimited service codes (OMH, CMHC, ACT, CBT, DBT, etc.) |

**Used for**: Matching CCBHC-treated counties to controls based on baseline mental health infrastructure (2017 cross-section).

### 1.11 `federal_budgets/mental_health_budgets.csv` — Federal MH Spending

**Source**: Manually extracted from OMB Budget Appendix PDFs (SAMHSA line items), 1965–2025.  
**Format**: CSV, 157 rows.  
**Primary key**: `(program, year)`.

| Variable | Description |
|----------|-------------|
| `program` | Budget line name (Early CMHC, ADAMHA Mental Health, ADMS Block Grant, Mental Health Block Grant, Mental Health) |
| `year` | Fiscal year |
| `amount` | Nominal amount |
| `unit` | "millions" or "thousands" |

---

## 2. INTERMEDIATE / MASTER DATASETS

### 2.1 `build/data/cmhc_panel.parquet` — CMHC-Era Master Panel

**Unit of observation**: county × year.  
**Coverage**: ~3,044 counties × 30 years (1959–1988).  
**Construction**: Merge of `aer_data.fst` + `cmhc_openings.csv` + `priority_ranks` + `aer_pscore_data` + `incarceration_trends` (1970–1988 subset).

**Steps** (in `02_prep_aer_panel.R`):
1. Read `aer_data/aer_data.fst`.
2. Left join `cmhc_openings.csv` on `fips`.
3. Left join `priority_ranks_for_eventstudy.csv` on `fips`.
4. Left join `aer_pscore_data.dta` on `(stfips, cofips)`.
5. Drop NYC (36061), LA (6037), Chicago (17031).
6. Filter `year <= 1988`.
7. Create treatment indicator: `cmhc_treated = !is.na(cmhc_year_exp) & cmhc_year_exp <= 1975`.
8. Create event time: `event_time = year - cmhc_year_exp` (NA for never-treated).
9. Bin event time: clamp at [-6, +20], set never-treated to `-999`.
10. Create urban quartile `Durb` from `_60pcturban`.
11. Create population weights from 1960 values: `popwt`, `popwt_ad`, `popwt_eld`.
12. Create population quintile: `pop_quintile = ntile(popwt, 5)`.
13. Compute pre-treatment AMR quintile from mean AMR 1959–1964.
14. Left join incarceration data (annualized from quarterly) for 1970–1988.
15. Create balanced-jail-sample indicator.
16. Write to `build/data/cmhc_panel.parquet`.

**Key derived variables**:

| Variable | Type | Description |
|----------|------|-------------|
| `cmhc_treated` | binary | = 1 if CMHC opened ≤ 1975 |
| `cmhc_year_exp` | int | Year of CMHC opening |
| `event_time` | int | year − cmhc_year_exp |
| `event_time_binned` | int | Binned at [-6, +20]; -999 = never-treated |
| `Durb` | factor | Urban quartile (5 categories) |
| `popwt`, `popwt_ad`, `popwt_eld` | numeric | 1960 population weights |
| `pop_quintile` | int | Population size quintile |
| `pre_amr_mean`, `pre_amr_ad_mean` | numeric | Pre-treatment average AMR |
| `amr_quintile`, `amr_ad_quintile` | int | Pre-treatment mortality quintile |
| `chc_event_time_binned` | int | CHC event time (for horse race) |
| `ever_chc` | binary | = 1 if county ever got CHC |
| `jail_rate` | numeric | Annual mean jail pop rate per 100k (1970+) |
| `jail_bal_sample` | binary | = 1 if county has ≥ 7 years jail data 1965–1980 |
| All `aer_data` variables | — | Passed through |
| All `pscore` variables | — | Passed through |
| All `priority` variables | — | Passed through |

### 2.2 `build/data/ccbhc_panel.parquet` — CCBHC-Era Master Panel

**Unit of observation**: county × year.  
**Coverage**: ~3,142 counties × years 2010–2024.  
**Construction**: Merge of incarceration panel + WONDER mortality + CCBHC grants + SAMHSA directory facilities.

**Steps** (in `06_prep_ccbhc_panel.R`):
1. Load CCBHC treatment timing from `04_prep_ccbhc_grants.R` output.
2. Load incarceration data, annualize from quarterly: `mean(total_jail_pop_rate)` by `(fips, year)`.
3. Load WONDER mortality (all-cause, ages 25–44): stack the three MCOD CSVs + provisional file, deduplicate overlapping years, impute population for 2021+ using county-level linear trend from 2010–2020.
4. Load cause-specific WONDER mortality: ICD-F (mental/behavioral), external (U/V codes), COVID.
5. Load SAMHSA directory facilities (2017 cross-section) for matching covariates: counts of OMH, ACT, CMHC, therapy, case management, crisis, residential facilities per county.
6. Merge all on `(fips, year)`.
7. Create event time: `event_time = year - treat_year`.
8. Bin event time at [-5, +5]; -999 = never-treated.
9. Create matching variables for baseline (2015) characteristics.
10. Flag Medicaid waiver states and planning-grant states.
11. Write to `build/data/ccbhc_panel.parquet`.

**Key derived variables**:

| Variable | Type | Description |
|----------|------|-------------|
| `treat_year` | int | Year of first CCBHC grant (2018–2024) |
| `treated` | binary | = 1 if county ever received CCBHC grant |
| `event_time` | int | year − treat_year |
| `event_time_binned` | int | Binned at [-5, +5]; -999 = never-treated |
| `stfips` | char | 2-digit state FIPS |
| `mort_rate` | numeric | All-cause death rate per 100k, ages 25–44 |
| `mort_rate_f` | numeric | ICD-F death rate per 100k |
| `mort_rate_external` | numeric | External-cause death rate per 100k |
| `jail_pop_rate` | numeric | Total jail pop rate per 100k |
| `log_jail_pop_rate` | numeric | Log of jail rate |
| `total_pop` | numeric | County population |
| `waiver_state` | binary | = 1 if in Medicaid waiver state |
| `planning_state` | binary | = 1 if in planning-grant state |
| `n_omh_2017`, `n_cmhc_2017`, ... | int | Baseline facility counts |

### 2.3 `build/data/national_mortality.parquet` — National Mortality Time Series

**Unit of observation**: year × age group (× cause, optional).  
**Coverage**: 1959–2023.  
**Construction**: From NBER Vital Statistics (1959–2004) + CDC WONDER (1999–2023), with overlap reconciliation.

**Steps** (in `07_prep_national_mortality.R`):
1. For 1959–1998: aggregate NBER micro-data deaths by year × age bin. Denominator from Census/SEER intercensal estimates.
2. For 1999–2023: use CDC WONDER national queries (may need to pull if not already cached).
3. Reconcile overlap (1999–2004) by preferring WONDER (final) counts.
4. Compute age-specific mortality rates: `deaths / population * 100,000`.
5. Aggregate to age groups: 20–49, 50+, all ages.
6. Optionally decompose by cause: cardiovascular, cancer, suicide, overdose, homicide, accidents, alcohol-related.

---

## 3. GLOBAL PARAMETERS (in `00_setup.R`)

```r
# ── CMHC-era parameters ──
CMHC_MAX_COHORT      <- 1975      # Only CMHCs opening ≤ this year for main spec
CMHC_PANEL_END       <- 1988      # Last year of AER panel
CMHC_EVENT_MIN       <- -6        # Left bin endpoint
CMHC_EVENT_MAX       <- 20        # Right bin endpoint (truncated at 14 for plots)
CMHC_REF_PERIOD      <- -1        # Omitted event-time bin
CMHC_PLOT_MAX         <- 14       # Max event time shown in plots

# ── CCBHC-era parameters ──
CCBHC_EVENT_MIN      <- -5
CCBHC_EVENT_MAX      <- 5
CCBHC_REF_PERIOD     <- -1
CCBHC_PANEL_START    <- 2010
CCBHC_TREAT_YEARS    <- 2018:2024 # Valid treatment years

# ── Excluded counties ──
DROP_FIPS <- c(36061, 6037, 17031)  # NYC, LA, Chicago

# ── Medicaid waiver state FIPS ──
WAIVER_ST_FIPS <- c("27","29","32","34","36","40","41","42")

# ── Common regression specification ──
CMHC_CONTROLS <- "D_tot_act_md_t + H_bpc + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`"
CMHC_FE       <- "fips + year^Durb + year^stfips"

# ── Plotting defaults ──
PLOT_WIDTH  <- 10
PLOT_HEIGHT <- 6
PLOT_DPI    <- 300
OUTPUT_FORMAT <- "pdf"  # "pdf" for paper, "png" for slides
```

---

## 4. ANALYSES AND OUTPUTS

### 4.1 Figure 1: Federal Mental Health Budgets, 1965–2023

**Script**: `10_fig01_federal_budgets.R`  
**Source file to copy from**: `federal_budgets/plot_mental_health_budgets.R`  
**Input**: `federal_budgets/mental_health_budgets.csv`, CPI-U from FRED  
**Output**: `output/figures/fig01_federal_budgets.pdf`

**Specification**:
- Plot real (2023$) federal spending on community mental health over time.
- Four program series: Early CMHC, ADAMHA Mental Health, ADMS Block Grant, Mental Health Block Grant, Mental Health.
- Concatenate into a continuous series showing the lifecycle.
- Shade the CMHC era (1963–1981) and CCBHC era (2017–present).
- Y-axis: millions of 2023 dollars. X-axis: fiscal year.

---

### 4.2 Figure 2: National Mortality Trends by Age Group (NEW)

**Script**: `11_fig02_national_mortality.R`  
**Input**: `build/data/national_mortality.parquet`  
**Output**: `output/figures/fig02_national_mortality.pdf`

**Specification**:
- Two-panel figure.
- **Panel A**: AMR for ages 50+ (1959–2023). Steady cardiovascular-driven decline. Overlay shaded CMHC and CCBHC eras.
- **Panel B**: AMR for ages 20–49 (1959–2023). Show decline in 1960s–70s, flattening in 1980s–90s, rise of deaths of despair post-2000. Overlay same shaded eras.
- Optional: decompose Panel B by cause (suicide, overdose, alcohol, homicide, all other).
- Source: NBER Vital Statistics for 1959–1998, CDC WONDER for 1999–2023.

**Data needed**: If `national_mortality.parquet` does not exist, `07_prep_national_mortality.R` must construct it. This may require downloading NBER micro-data `.csv.zip` files OR using the pre-aggregated `nber_mortality_counts.parquet` for national totals, plus pulling WONDER data for 1999–2023 via the existing `pull_wonder_unified.py` script.

---

### 4.3 Figure 3: CMHC Rollout Map (NEW)

**Script**: `12_fig03_cmhc_rollout_map.R`  
**Input**: `cmhc_openings.csv`, county shapefile (from `tigris` R package)  
**Output**: `output/figures/fig03_cmhc_rollout_map.pdf`

**Specification**:
- Choropleth map of the contiguous U.S.
- Counties colored by CMHC opening year: 1971, 1973, 1975, 1977, 1981, never.
- Use a sequential color palette (e.g., viridis or Blues) with "never" in gray.
- Include counts in legend: "1971 (N=208)", etc.
- Requires `tigris::counties()` or a pre-downloaded shapefile.

---

### 4.4 Table 1: Balance Table — CMHC Counties vs. Non-CMHC Counties (NEW)

**Script**: `13_table01_balance.R`  
**Input**: `build/data/cmhc_panel.parquet` (cross-section at 1960)  
**Output**: `output/tables/table01_balance.tex`

**Specification**:
- Cross-sectional comparison of treated (CMHC ≤ 1975) vs. never-treated counties on 1960 observables.
- Variables: population, % urban, % rural farm, % nonwhite, % income < $3k, % income > $10k, median schooling, MDs per 1,000, hospital beds per capita, AMR (all ages), AMR (20–49), AMR (50+).
- Columns: Mean (Treated), Mean (Control), Difference, SE, p-value.
- Population-weighted and unweighted versions.
- Use `modelsummary` or `stargazer` for LaTeX output.
- Note: treated counties are larger and more urban — this is expected and addressed by the identification strategy.

---

### 4.5 Figure 4: Timing Exogeneity Scatter (CMHC Year vs. 1960 AMR)

**Script**: `14_fig04_timing_exogeneity.R`  
**Source**: Adapted from `cmhc_event_study.R` Step 12b.  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/figures/fig04_timing_exogeneity.pdf`

**Specification**:
- Two-panel figure (treated counties only).
- **Panel A**: Scatter of 1960 AMR (ages 20–49) vs. CMHC establishment year. Population-weighted OLS line. Report slope, SE, p-value in annotation.
- **Panel B**: Scatter of Δ AMR 1959–1960 (ages 20–49) vs. CMHC year. Same format.
- Point size proportional to 1960 county population.
- Robustness: residualized version conditioning on % urban, active MDs.

---

### 4.6 Figure 5: CMHC Event Study, Ages 20–49 (Main Result)

**Script**: `15_fig05_cmhc_es_ad.R`  
**Source**: Adapted from `cmhc_event_study.R`.  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/figures/fig05_cmhc_es_ad.pdf`, `output/data/cmhc_es_ad_coefs.csv`

**Specification**:
```r
feols(
  amr_ad ~ i(event_time_binned, ref = -1) + D_tot_act_md_t + H_bpc
         + `_60pcturban` + `_pct59inclt3k` + `_60pctnonwhit`
  | fips + year^Durb + year^stfips,
  data = panel,
  weights = ~popwt_ad,
  cluster = ~fips
)
```
- Treatment: CMHC opening ≤ 1975 (343 treated counties).
- Control: Never-treated counties (~2,700 counties).
- Event time: binned at [-6, +20], reference = -1. Plot truncated at +14.
- Fixed effects: county + (year × urban quartile) + (year × state).
- Controls: MDs × trend, beds per capita, % urban, % poverty, % nonwhite.
- Weights: 1960 population (ages 20–49).
- Clustering: county.
- Report: Joint pre-trend test (t = -6 to -2), joint post-treatment test (t = 0 to +14).

---

### 4.7 Figure 6: CMHC Event Study, Ages 50+ (Placebo)

**Script**: `16_fig06_cmhc_es_eld.R`  
**Source**: Same as above, replacing `amr_ad` with `amr_eld` and `popwt_ad` with `popwt_eld`.  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/figures/fig06_cmhc_es_eld.pdf`, `output/data/cmhc_es_eld_coefs.csv`

**Specification**: Identical to Figure 5 except:
- Outcome: `amr_eld` (AMR, ages 50+)
- Weights: `popwt_eld`
- Expected result: flat, noisy coefficients centered on zero.

---

### 4.8 Table 2: DD Estimates, Robustness Specifications (NEW)

**Script**: `17_table02_dd_robustness.R`  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/tables/table02_dd_robustness.tex`

**Specification**: A table with 6+ columns, each a different specification:

| Column | Specification |
|--------|---------------|
| (1) | County FE + Year FE only |
| (2) | + Urban × Year FE |
| (3) | + State × Year FE (main spec) |
| (4) | + State-specific linear trends |
| (5) | Propensity score reweighting (DFL weights from `aer_pscore_data`) |
| (6) | Callaway & Sant'Anna (2021) estimator |
| (7) | Sun & Abraham (2021) estimator |

- For each: report post-treatment average coefficient (mean of t = 0 to +14), SE, N, pre-trend F-test p-value.
- Rows: AMR 20–49 (main), AMR 50+ (placebo).
- Use `fixest::feols()` for (1)–(5), `did::att_gt()` for (6), `fixest::sunab()` for (7).

---

### 4.9 Figure 7: CMHC Robustness to CHC Controls

**Script**: `18_fig07_cmhc_chc_robust.R`  
**Source**: Adapted from `cmhc_event_study.R` Step 12c.  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/figures/fig07_cmhc_chc_robust.pdf`

**Specification**:
- Two-panel figure.
- **Panel A**: Baseline CMHC event study (blue) vs. + CHC per-capita funding control (orange). Same axis.
- **Panel B**: Baseline vs. horse-race with CHC event-time indicators as additional controls.
- Both panels: AMR ages 20–49. Show that CMHC coefficients are virtually unchanged.
- Optional third comparison: restrict to never-CHC counties.

---

### 4.10 Table 3: Cause-Specific Mortality Decomposition, CMHC (NEW)

**Script**: `19_table03_cause_decomp.R`  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/tables/table03_cause_decomp_cmhc.tex`

**Specification**:
- Dependent variables (all available in AER data as `amr_ad_2` through `amr_ad_7`):
  - `amr_ad_2`: Cardiovascular disease (expected: null or small)
  - `amr_ad_3`: Cerebrovascular disease (expected: null)
  - `amr_ad_4`: Cancer (expected: null)
  - `amr_ad_5`: Infectious disease (expected: small)
  - `amr_ad_6`: Diabetes (expected: null)
  - `amr_ad_7`: Accidents (expected: placebo — null)
  - `amr_ad`: All-cause (reference)
- For each: run the main spec (Figure 5), report post-treatment average coefficient, SE, pre-trend p-value.
- Note: suicide, substance, and homicide are NOT separately available in the AER ICD recoding. The residual (all-cause minus sum of 2–7) captures "other" including these mental-health-related causes.

**IMPORTANT**: The AER cause-of-death categories use an older ICD classification that does not isolate suicide, overdose, or homicide separately for the 20–49 age group. The six categories above are what's available. To get suicide/homicide/overdose, one would need to go back to NBER micro-data with raw ICD codes. Flag this as a possible extension if the raw data is accessible.

---

### 4.11 Figure: CMHC Event Study by Race (NEW)

**Script**: `20_fig_cmhc_race.R`  
**Input**: `build/data/cmhc_panel.parquet` (if race-specific AMR 20–49 exists) OR raw NBER data  
**Output**: `output/figures/fig_cmhc_race.pdf`, `output/tables/table_cmhc_race.tex`

**Specification**:
- Two-line event study: white AMR (20–49) and nonwhite AMR (20–49).
- Same main spec as Figure 5, run separately for each race group.
- **Data issue**: `amr_w_ad` and `amr_nw_ad` (white and nonwhite AMR, ages 20–49) do NOT exist in the AER data. The AER data has `amr_w_eld` and `amr_nw_eld` (race-specific for 50+ only).
- **Resolution options**:
  1. Construct from NBER Vital Statistics micro-data at county level (preferred).
  2. Use the AER data for 50+ race decomposition only and discuss 20–49 qualitatively.
  3. Check if Avery & LaVoice replication data provides these variables.

**If micro-data unavailable**: Run the 50+ race decomposition as a substitute (this is in the AER data: `amr_w_eld`, `amr_nw_eld`) and note the limitation.

---

### 4.12 Figure: CMHC Heterogeneity by Baseline Characteristics (NEW)

**Script**: `21_fig_cmhc_heterogeneity.R`  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/figures/fig_cmhc_heterogeneity.pdf`

**Specification**:
- Four-panel figure. Each panel: event study for AMR (20–49), split at median of a baseline characteristic.
- **Panel A**: Above vs. below median `_pct59inclt3k` (poverty rate).
- **Panel B**: Above vs. below median `_60pcturban` (urbanicity).
- **Panel C**: Above vs. below median `_md_per1000` (physician supply).
- **Panel D**: Above vs. below median distance to nearest state mental hospital (if available; otherwise `ps_popden` as proxy).
- For each: run main spec on each subsample, overlay two event-study lines.

---

### 4.13 Table: CMHC Incarceration Long-Difference Regressions

**Script**: `22_table_cmhc_incarceration.R`  
**Source**: Adapted from `cmhc_event_study.R` Step 13.  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/tables/table_cmhc_incarceration_ld.tex`

**Specification**:
- Cross-sectional long-difference: Δlog(jail_rate) from 1970 to end year.
- End years: 1978, 1983, 1988, 1993, 2000.
- Treatment: CMHC operating by end year.
- Columns: (1) no controls, (2) + 1960 baseline controls, (3) + state FE + pop weights (small counties < 30k), (4) same for large counties, (5) interaction with log(pop).
- Robustness rows: + CHC dummy, + CHC funding, never-CHC only.
- Report coefficient, robust SE, p-value, N for each cell.

---

### 4.14 Figure 8: CCBHC Mortality Event Study, Ages 25–44

**Script**: `23_fig08_ccbhc_es_mort.R`  
**Source**: Adapted from `ccbhc/ccbhc_grants_did.R` with `ANALYSIS_TYPE = "mortality"`.  
**Input**: `build/data/ccbhc_panel.parquet`  
**Output**: `output/figures/fig08_ccbhc_es_mort.pdf`, `output/data/ccbhc_mort_baseline_coefs.csv`

**Specification**:
```r
feols(
  mort_rate ~ i(event_time_binned, ref = -1) + total_pop
  | fips + year,
  data = panel[fips %in% matched_counties],
  weights = ~total_pop,
  cluster = ~fips
)
```
- Treatment: first CCBHC grant, 2018–2024.
- Matching: logit propensity score on 2017 facility counts (OMH, ACT, CMHC, therapy, case management, crisis, residential), nearest-neighbor 1:2.
- Event time: [-5, +5], reference = -1.
- Population weighted, county-clustered.
- Report pre-trend and post-treatment joint tests.

---

### 4.15 Figure 9: CCBHC Mortality Robustness

**Script**: `24_fig09_ccbhc_robust.R`  
**Source**: Adapted from `ccbhc/ccbhc_grants_did.R` sections 8–9.  
**Input**: `build/data/ccbhc_panel.parquet`  
**Output**: `output/figures/fig09_ccbhc_robust.pdf`

**Specification**:
- Two-panel figure.
- **Panel A**: Exclude Medicaid waiver states (MN, MO, NV, NJ, NY, OK, OR, PA).
- **Panel B**: Restrict to planning-grant states only (tighter counterfactual).
- Same spec as Figure 8 on each subsample.

---

### 4.16 Figure 10: CCBHC Incarceration Event Study

**Script**: `25_fig10_ccbhc_es_inc.R`  
**Source**: Adapted from `ccbhc/ccbhc_grants_did.R` with `ANALYSIS_TYPE = "incarceration"`.  
**Input**: `build/data/ccbhc_panel.parquet`  
**Output**: `output/figures/fig10_ccbhc_es_inc.pdf`, `output/data/ccbhc_inc_baseline_coefs.csv`

**Specification**:
- Same framework as Figure 8, but:
  - Outcome: `log(total_jail_pop_rate)`.
  - Event time window: [-5, +5].
- Report heterogeneity by county size (small < 30k vs. large ≥ 30k).

---

### 4.17 Table: CCBHC Heterogeneity by Pre-Treatment Opioid Overdose Rate (NEW)

**Script**: `26_table_ccbhc_heterogeneity.R`  
**Input**: `build/data/ccbhc_panel.parquet`  
**Output**: `output/tables/table_ccbhc_heterogeneity.tex`

**Specification**:
- Split counties at median pre-treatment (2015–2017) overdose death rate.
- Run CCBHC mortality event study separately on each subsample.
- Report post-treatment average coefficient, SE, for high vs. low overdose counties.
- Pre-treatment overdose rate source: CDC WONDER ICD-10 codes X40–X44, X60–X64, X85, Y10–Y14 (or ICD-F codes for substance use).

---

### 4.18 Figure: Side-by-Side CMHC and CCBHC Event Studies (NEW)

**Script**: `27_fig_side_by_side.R`  
**Input**: Coefficient CSVs from Figures 5 and 8  
**Output**: `output/figures/fig_side_by_side.pdf`

**Specification**:
- Two-panel figure with SAME y-axis scale.
- **Panel A**: CMHC event study (AMR 20–49), event time [-6, +14].
- **Panel B**: CCBHC event study (mortality 25–44), event time [-5, +5].
- Both panels use the same y-axis range so magnitudes are directly comparable.
- Annotation: treatment onset at vertical dashed line, coefficient scale in annotation.

---

### 4.19 Appendix: CMHC Poisson Specification

**Script**: `28_appendix_poisson.R`  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/tables/table_appendix_poisson.tex`

**Specification**:
- Poisson pseudo-maximum likelihood (PPML) version of the main spec.
- `fixest::fepois()` with deaths as outcome and log(population) as offset.
- Compare coefficients (as IRRs) with linear specification.
- This is for comparability with Avery & LaVoice (2023) who use Poisson.

---

### 4.20 Appendix: Bacon Decomposition (NEW)

**Script**: `29_appendix_bacon.R`  
**Input**: `build/data/cmhc_panel.parquet`  
**Output**: `output/figures/fig_bacon_decomp.pdf`

**Specification**:
- Use `bacondecomp::bacon()` to decompose the TWFE DD estimate into:
  - Treated vs. never-treated (expect ~80%+ of weight)
  - Earlier vs. later treated
  - Later vs. earlier treated
- Scatter plot: x = weight, y = 2×2 DD estimate, colored by comparison type.
- Report share of weight on each comparison type.
- Outcome: AMR 20–49. Binary treatment: post-CMHC indicator.

---

### 4.21 Appendix: CCBHC Placebo — Ages 50+ (NEW)

**Script**: `30_appendix_ccbhc_placebo.R`  
**Input**: WONDER mortality data for ages 50+ at county level (may need new pull)  
**Output**: `output/figures/fig_ccbhc_placebo_50plus.pdf`

**Specification**:
- Same CCBHC event study as Figure 8 but for ages 50+ (or 45–64 if 50+ not available in county-level WONDER).
- Expected: null effect, confirming CCBHC operates through mental health channels.
- **Data needed**: WONDER mortality at county level for ages 50+ or 45+, 2010–2024. If not already pulled, use `pull_wonder_unified.py` with appropriate age group config.

---

### 4.22 Appendix: CCBHC Deaths-of-Despair Decomposition (NEW)

**Script**: `31_appendix_dod_decomp.R`  
**Input**: `build/data/ccbhc_panel.parquet` (with cause-specific mortality)  
**Output**: `output/figures/fig_ccbhc_dod_decomp.pdf`

**Specification**:
- Three separate event studies:
  - **(a)** Drug overdose mortality (ICD-10: X40–X44, X60–X64, Y10–Y14).
  - **(b)** Suicide mortality (ICD-10: X60–X84, Y87.0, U03).
  - **(c)** Alcohol-related mortality (ICD-10: K70, K73–K74, X45, X65, Y15).
- Same spec as Figure 8 on each cause.
- Three-panel figure. Expectation: overdose shows sharpest break at treatment (MAT mandate).
- **Data needed**: Cause-specific WONDER pulls at county level, ages 25–44. The existing `Mortality_U_V_codes*.csv` files may partially cover this, but specific ICD ranges may need new pulls.

---

## 5. DATA DEPENDENCIES AND ACQUISITION

### Data Already in Hand
| Dataset | Location | Status |
|---------|----------|--------|
| AER panel (`aer_data.fst`) | `aer_data/` | ✅ Ready |
| AER pscore data | `aer_data/aer_pscore_data.dta` | ✅ Ready |
| CMHC openings | `cmhc_data/cmhc_openings.csv` | ✅ Ready |
| CMHC priority ranks | `cmhc_data/priority_ranks_for_eventstudy.csv` | ✅ Ready |
| Incarceration trends | `incarceration_trends_county.fst` | ✅ Ready |
| CCBHC grants (93.829) | `ccbhc/Assistance_...93.829.csv` | ✅ Ready |
| CCBHC grants (93.696) | `ccbhc/Assistance_...93.696.csv` | ✅ Ready |
| WONDER mortality 25–44 (2010–2023) | `ccbhc/Deaths_*.csv` + provisional | ✅ Ready |
| WONDER cause-specific | `ccbhc/Mortality_*.csv` | ✅ Ready |
| SAMHSA directories (geocoded) | `ccbhc/directories/facilities_geocoded.rds` | ✅ Ready |
| Federal budgets | `federal_budgets/mental_health_budgets.csv` | ✅ Ready |
| NBER mortality counts (national) | `nber_mortality/nber_mortality_counts.parquet` | ✅ Ready |
| WONDER county mortality (parquet) | `wonder_mortality/data/*.parquet` | ✅ Ready |

### Data Needing Construction or New Pulls
| Dataset | Needed For | Action |
|---------|------------|--------|
| National mortality time series (1959–2023) | Figure 2 | Aggregate from NBER counts + WONDER; may need WONDER national pull for 1999–2023 age-specific rates |
| Race-specific AMR (20–49) at county level | Figure: CMHC by race | Construct from NBER micro-data at county level, OR check if Avery & LaVoice replication has it. Fallback: use 50+ race decomp from AER data |
| WONDER mortality ages 50+ at county level (2010–2024) | CCBHC 50+ placebo | Pull via `pull_wonder_unified.py` with age ≥ 50 |
| Cause-specific WONDER for suicide/overdose/alcohol separately (25–44, county) | CCBHC DoD decomposition | Pull via `pull_wonder_unified.py` with specific ICD-10 ranges |
| County shapefiles | Rollout map | Download via `tigris::counties()` at runtime |

---

## 6. BUILD SCRIPT — `build.R`

```r
#!/usr/bin/env Rscript
# ============================================================================
# MASTER BUILD SCRIPT
# "Community Mental Health Centers: Then and Now"
# Gio Righi, UCLA
#
# Usage:  Rscript build/build.R
#         (or source from the project root)
#
# This script runs every step in order. Each sub-script is self-contained
# and reads from build/data/, writes to build/output/.
# ============================================================================

cat("================================================================\n")
cat("BUILD: Community Mental Health Centers — Then and Now\n")
cat(sprintf("Started: %s\n", Sys.time()))
cat("================================================================\n\n")

# Record start time
build_start <- Sys.time()

# ── Setup ──
source("build/00_setup.R")

# ── Data Preparation ──
cat("\n========== DATA PREPARATION ==========\n\n")
source("build/01_prep_cmhc_openings.R")
source("build/02_prep_aer_panel.R")
source("build/03_prep_incarceration.R")
source("build/04_prep_ccbhc_grants.R")
source("build/05_prep_ccbhc_mortality.R")
source("build/06_prep_ccbhc_panel.R")
source("build/07_prep_national_mortality.R")

# ── Figures and Tables: CMHC Era ──
cat("\n========== CMHC-ERA OUTPUTS ==========\n\n")
source("build/10_fig01_federal_budgets.R")
source("build/11_fig02_national_mortality.R")
source("build/12_fig03_cmhc_rollout_map.R")
source("build/13_table01_balance.R")
source("build/14_fig04_timing_exogeneity.R")
source("build/15_fig05_cmhc_es_ad.R")
source("build/16_fig06_cmhc_es_eld.R")
source("build/17_table02_dd_robustness.R")
source("build/18_fig07_cmhc_chc_robust.R")
source("build/19_table03_cause_decomp.R")
source("build/20_fig_cmhc_race.R")
source("build/21_fig_cmhc_heterogeneity.R")
source("build/22_table_cmhc_incarceration.R")

# ── Figures and Tables: CCBHC Era ──
cat("\n========== CCBHC-ERA OUTPUTS ==========\n\n")
source("build/23_fig08_ccbhc_es_mort.R")
source("build/24_fig09_ccbhc_robust.R")
source("build/25_fig10_ccbhc_es_inc.R")
source("build/26_table_ccbhc_heterogeneity.R")

# ── Cross-Era and Appendix ──
cat("\n========== CROSS-ERA AND APPENDIX ==========\n\n")
source("build/27_fig_side_by_side.R")
source("build/28_appendix_poisson.R")
source("build/29_appendix_bacon.R")
source("build/30_appendix_ccbhc_placebo.R")
source("build/31_appendix_dod_decomp.R")

# ── Done ──
build_end <- Sys.time()
cat("\n================================================================\n")
cat(sprintf("BUILD COMPLETE: %s\n", Sys.time()))
cat(sprintf("Total time: %.1f minutes\n", difftime(build_end, build_start, units = "mins")))
cat("================================================================\n")
```

---

## 7. R PACKAGE DEPENDENCIES

```r
# Core
library(data.table)
library(dplyr)
library(tidyr)
library(readr)

# Data I/O
library(fst)
library(haven)       # .dta files
library(readxl)      # CMHC Excel files
library(arrow)       # Parquet

# Estimation
library(fixest)      # feols, fepois, sunab
library(lmtest)      # coeftest
library(sandwich)    # vcovHC
library(MatchIt)     # Propensity score matching (CCBHC)

# Staggered DD
library(did)         # Callaway & Sant'Anna
library(bacondecomp) # Bacon decomposition

# Plotting
library(ggplot2)
library(patchwork)   # Multi-panel figures
library(sf)          # Spatial data
library(tigris)      # County shapefiles

# Tables
library(modelsummary) # LaTeX tables
library(kableExtra)   # Alternative table formatting
```

---

## 8. KEY ECONOMETRIC DECISIONS

### 8.1 CMHC Specification Choices

| Choice | Decision | Rationale |
|--------|----------|-----------|
| Treatment cohort cutoff | ≤ 1975 | Need ≥ 13 years post-treatment in 1959–1988 panel |
| Event-time bins | [-6, +20] | Bin endpoints, show [-6, +14] in plots |
| Reference period | t = -1 | Standard |
| Fixed effects | County + Year×Urban + Year×State | Controls for county-level unobservables, differential trends by urbanicity and state |
| Controls | MDs×trend, beds per capita, % urban, % poverty, % nonwhite | Time-varying and baseline×trend controls from Bailey & Goodman-Bacon |
| Weights | 1960 age-group-specific population | Consistent with AER replication |
| Clustering | County | Conservative given serial correlation |
| Linear vs. Poisson | Linear (Poisson in appendix) | All-cause AMR has sufficient within-county variation; Poisson for comparability with Avery & LaVoice |

### 8.2 CCBHC Specification Choices

| Choice | Decision | Rationale |
|--------|----------|-----------|
| Treatment years | 2018–2024 | Drop pre-2017 planning grants |
| Matching | NN 1:2 on 2017 facility counts | Controls for baseline MH infrastructure |
| Event-time bins | [-5, +5] | Short post-period given recent rollout |
| Fixed effects | County + Year | Simpler FE given shorter panel |
| Controls | Total population | Population size |
| Weights | Total population (optional) | Population weighted |
| Clustering | County | Standard |

---

## 9. NOTES FOR CLAUDE CODE IMPLEMENTATION

1. **Copy source code**: Claude Code should copy relevant functions from existing scripts (e.g., `cmhc_event_study.R`, `ccbhc/ccbhc_grants_did.R`) into the `build/` directory scripts, refactoring to use the master parquet panels rather than re-reading raw data in each script.

2. **Shared helpers**: Create helper functions in `00_setup.R` for:
   - `extract_event_study_coefs(model)` — extracts event-time coefficients into a tidy data.frame.
   - `plot_event_study(coef_df, ...)` — standardized event-study plotting function.
   - `run_joint_tests(model, pre_range, post_range)` — pre-trend and post-treatment F-tests.

3. **Data paths**: All raw data paths should be relative to the project root (`../aer_data/`, `../cmhc_data/`, etc.) since `build/` is a subdirectory. Or use `here::here()`.

4. **Parquet I/O**: Use `arrow::write_parquet()` and `arrow::read_parquet()` for intermediate datasets. This is faster than CSV and preserves types.

5. **Idempotency**: Each prep script should check whether its output already exists and skip if so (with a `FORCE_REBUILD` flag). Analysis scripts should always re-run.

6. **Figure format**: Default to PDF for paper; use the `OUTPUT_FORMAT` parameter to switch to PNG for slides.

7. **LaTeX tables**: Use `modelsummary` with `output = "latex"` for publication-quality tables. Include `\label{}` and `\caption{}`.

8. **Logging**: Each script should print its name, key sample sizes, and timing to stdout. Redirect all output to `build/logs/build.log`.

9. **Error handling**: Wrap data-dependent steps in `tryCatch()`. If race-specific AMR data is unavailable, skip Figure: CMHC by race with a warning rather than failing the build. If WONDER pulls are needed and the API is down, skip with a warning.

10. **Reproducibility**: Set `set.seed(42)` at the top of `00_setup.R`. Pin package versions in a `renv.lock` if possible.
