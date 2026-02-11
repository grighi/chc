# CCBHC — Mental Health Facilities and Incarceration

This project extracts, parses, and geocodes facility records from the SAMHSA
*Mental Health Directory* PDFs (2017--2020), then estimates the effect of
mental health facility openings on county-level incarceration rates using
two-way fixed effects event studies.

## Scripts

### Data Pipeline

| Script | Language | Description |
|--------|----------|-------------|
| `extract_pages.py` | Python | Reads the raw text export of each directory PDF (three fixed-width columns per page, form-feed separated) and converts it to a single-column long format. Column boundaries are auto-detected per page via space-distribution analysis. |
| `parse_directory.py` | Python | Parses the long-format text into a structured CSV with columns: `place`, `facility_name`, `address`, `phone`, `intake`, `services`. Uses a state machine that recognizes place names, address lines, phone numbers, intake lines, and service-code lines. |
| `geocode_facilities.R` | R | Reads all four facility CSVs (2017--2020), extracts 5-digit ZIP codes, maps each to county name / FIPS / state / lat-lon via `zipcodeR` (offline, no API), attaches county FIPS from `tigris::fips_codes`, and saves the stacked dataset as `facilities_geocoded.rds`. |

### Analysis

| Script | Language | Description |
|--------|----------|-------------|
| `facility_incarceration_analysis_1718.R` | R | Builds county-year counts of three facility types (all OMH, CMHC, ACT) from the geocoded data, merges with quarterly incarceration trends (averaged to county-year), and runs three two-way FE regressions (`feols`) of incarceration rate on facility counts with county and year fixed effects. Results are exported to `facility_incarceration_results.csv`. |
| `facility_event_study.R` | R | Difference-in-differences event study. Defines treatment as the first year a county gains at least one new facility of a given type. Runs separate event studies for all OMH, CMHC, and ACT facility types with two-way FE (county + year), population control, and standard errors clustered by county. Produces coefficient CSVs, event study plots, and histograms of non-zero facility-count changes per 100k population. |
| `facility_event_study_diagnostics.R` | R | Robustness checks for the CMHC event study: (1) baseline imbalance test on pre-period mean incarceration rates, (2) pre-trend slope comparison with density plot, (3) Spec A -- county-specific linear trends, (4) Spec B -- placebo with treatment shifted back one year, (5) Spec C -- restricted to large counties (pop >= 50k). Exports a comparison summary CSV and diagnostic plots. |
| `cmhc_event_study.R` | R | Replicates a Bailey & Goodman-Bacon (2015) style event study for historical Community Mental Health Center openings using the AER replication data. Estimates the effect of CMHC establishment (pre-1976) on age-adjusted mortality rates at the county level. |

## Pipeline

```
PDF (text layer)
  |  extract_pages.py
  v
directory_long_{year}.txt       (single-column, one record per block)
  |  parse_directory.py
  v
facilities_{year}.csv           (structured CSV)
  |  geocode_facilities.R
  v
facilities_geocoded.rds         (stacked data.table with county / FIPS / lat-lon)
  |
  |  facility_incarceration_analysis_1718.R   -->  facility_incarceration_results.csv
  |  facility_event_study.R                   -->  event study plots + coefficients
  |  facility_event_study_diagnostics.R       -->  diagnostic plots + comparison CSV
  v
```

## Data Quality

| Metric                       | 2017   | 2018  |
|------------------------------|--------|-------|
| Total facilities parsed      | 10,155 | 9,496 |
| F-codes leaking into names   | 0      | 0     |
| Empty facility names         | 0      | 0     |
| Empty addresses              | 0      | 0     |
| Empty services               | 0      | 28    |

## Results

### Event Studies: Facility Openings and Incarceration

#### All OMH Facilities (CMHC, OMH, MSNH)

![Event study — All OMH](event_study_all_omh.png)

![Histogram — All OMH non-zero facility changes per 100k](histogram_all_omh.png)

#### CMHC Facilities

![Event study — CMHC](event_study_cmhc.png)

![Histogram — CMHC non-zero facility changes per 100k](histogram_cmhc.png)

#### ACT Facilities

![Event study — ACT](event_study_act.png)

![Histogram — ACT non-zero facility changes per 100k](histogram_act.png)

### Diagnostics (CMHC)

#### Pre-trend Density

![Pre-trend density — treated vs never-treated](diagnostics_pretrend_density_cmhc.png)

#### Baseline Event Study

![Diagnostics — Baseline](diagnostics_es_baseline.png)

#### Spec A: County-Specific Linear Trends

![Diagnostics — County trends](diagnostics_es_spec_a__county_trends.png)

#### Spec B: Placebo Timing (Treatment Shifted Back 1 Year)

![Diagnostics — Placebo timing](diagnostics_es_spec_b__placebo_timing.png)

#### Spec C: Large Counties Only (Pop >= 50k)

![Diagnostics — Pop >= 50k](diagnostics_es_spec_c__pop____50k.png)

## Requirements

**Python:** Standard library only (no external packages).

**R:** `data.table`, `fixest`, `ggplot2`, `zipcodeR`, `tigris`, `haven`, `dplyr`, `readxl`, `tidyr`, `fst`, `readr`

## Source Data

| File | Description |
|------|-------------|
| `2017 MH Directory.pdf` -- `2020 MH Directory.pdf` | SAMHSA Mental Health Directory, 2017--2020 editions |
| `*.txt` versions | Text layer extracted from the PDFs |
| `../incarceration_trends_county.csv` | County-level quarterly incarceration trends |
| `../aer_data/aer_data.fst` | Bailey & Goodman-Bacon (2015) AER replication data |
