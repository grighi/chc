# Differences Between Replication and Main Analysis

Comparison of `replication/cmhc_replication.r` (Avery & LaVoice 2023 replication)
vs. `cmhc_event_study.R` (main analysis specification).

## Estimator

| | Replication | Main Analysis |
|---|---|---|
| Model | Poisson TWFE (`fepois`) | OLS linear (`feols`) |
| Interpretation | Semi-elasticity (% change) | Level change (deaths per 100,000) |
| Offset | Population enters as weight | Population enters as weight |

## Treatment Definition

| | Replication | Main Analysis |
|---|---|---|
| Treated counties | ~536 (all cohorts 1971-1981, dropping 1977-only catchment areas) | ~343 (1971-1975 cohorts only; 1977+ set to never-treated) |
| Treatment variable | Binary `cmhc_post` (1 if year >= cmhc_year_exp) | Event-time dummies `i(event_time_binned)` |
| Static estimate | Yes (Table 3 analog) | No (event study only) |

The main analysis is more conservative: it restricts to early cohorts (1971-1975)
that had the longest post-treatment observation window and were less likely to be
affected by the 1981 Omnibus Budget Reconciliation Act that ended the CMHC program.

## Event Study Specification

| | Replication | Main Analysis |
|---|---|---|
| Bin width | 2-year bins | 1-year bins |
| Pre-treatment range | [-8,-7] to [-2,-1] | [-6] to [-1] |
| Post-treatment range | [0,1] to [14,15] | [0] to [20+] |
| Reference period | [-2,-1] | -1 |
| Endpoint binning | [-8,-7] and [14,15+] | -6 and 20+ |

## Fixed Effects

| | Replication | Main Analysis |
|---|---|---|
| County FE | Yes (`fips`) | Yes (`fips`) |
| Year FE | Yes (`year`) | Absorbed into state-by-year |
| State time trends | State linear trend (`stfips[year]`) | **State-by-year FE** (`year^stfips`) |
| Urban-by-year FE | Yes (`Durb^year`) | Yes (`year^Durb`) |

The main analysis uses **state-by-year fixed effects** (fully non-parametric),
which is strictly more flexible than the replication's state-specific linear time trends.
State-by-year FE absorb any state-level time-varying confounders, not just those
that evolve linearly. This follows Bailey & Goodman-Bacon's (2015) preferred specification.

## Controls

| | Replication | Main Analysis |
|---|---|---|
| Specification | Baseline x year trend interactions | Level controls |
| Variables | `D_pct59inclt3k_t`, `D_60pctnonwhit_t`, `D_60pctrurf_t`, `D_60pcturban_t`, `D_tot_act_md_t` | `D_tot_act_md_t`, `H_bpc`, `_60pcturban`, `_pct59inclt3k`, `_60pctnonwhit` |

The replication uses Avery's control specification (baseline characteristics interacted
with a linear time trend), while the main analysis includes hospital beds per capita
(`H_bpc`) as a time-varying healthcare infrastructure control.

Note: With state-by-year FE, time-invariant county characteristics and state-level
time-varying characteristics are already absorbed. The remaining controls capture
county-level time trends correlated with baseline characteristics.

## Population Weights

| | Replication | Main Analysis |
|---|---|---|
| Weight source | Current-year population (`copop`, `copop_eld`, etc.) | 1960 baseline population (`popwt` = `copop` in 1960) |

The main analysis uses 1960 population to avoid endogenous weighting — if CMHCs
affect migration or survival, current population is a bad control. The replication
follows Avery's specification which uses contemporaneous population.

## Sample

| | Replication | Main Analysis |
|---|---|---|
| Years | 1969-1988 | 1959-1988 (all available years up to 1988) |
| Exclusions | NYC, LA, Chicago | NYC, LA, Chicago |
| Outcome variables | AMR (all ages), AMR by race (NW/W elderly), cause-specific AMR, IMR | AMR (all ages), AMR elderly (50+), AMR adults (20-49) |

The main analysis starts earlier (1959) to capture more pre-treatment variation,
particularly for the 1971 cohort which has only 2 pre-years in 1969-1988 but
12 pre-years in 1959-1988.

## Clustering

Both specifications cluster standard errors at the county level.

## Why the Linear TWFE Is the Preferred Specification

The three specification differences (Poisson vs OLS, bin width, FE structure)
are not equally consequential. The first two are minor; the third drives the
results.

**Poisson vs OLS**: Both are standard. OLS on mortality rates is the norm in this
literature — Bailey & Goodman-Bacon (2015) use OLS throughout. AMR is already a
rate, not a raw count, so Poisson's count-data motivation is weak. Wooldridge
(2021) further notes that Poisson TWFE has its own issues with heterogeneous
treatment effects in staggered designs.

**Bin width**: 1-year bins are more transparent than 2-year bins. The tradeoff
is precision vs resolution; neither is inherently more correct.

**State-by-year FE vs state linear trends**: This is what matters. State-by-year
FE is strictly more flexible — it absorbs any state-level time-varying confounder
nonparametrically, while state linear trends assume confounders evolve linearly.
That assumption is hard to maintain during a period with the crack epidemic,
deinstitutionalization, and Medicare expansion all evolving nonlinearly.

The elderly event study illustrates why this matters. Under the Poisson
replication (state linear trends), elderly AMR shows flat pre-trends and a
negative post-treatment effect. Under the linear TWFE (state-by-year FE), the
same outcome shows significant positive pre-trends (+15 to +27 deaths/100K at
t=-6 through t=-2), revealing that within states, soon-to-be-treated counties
had differentially higher elderly mortality years before treatment. State linear
trends partially absorb this pattern and produce a "clean" event study — but
that hides a parallel-trends violation rather than fixing it.

By contrast, the adult (20-49) event study has flat pre-trends and a strong
negative post-treatment decline in *both* specifications. This is the robust
result.

## Summary

The main analysis specification is more conservative and robust:
- State-by-year FE instead of linear trends (handles non-linear state confounders)
- 1960 population weights (avoids endogeneity)
- Only early cohorts (1971-1975, cleaner identification)
- Longer pre-period (more statistical power for pre-trend tests)
- 1-year event-time bins (higher time resolution)

The adult mortality result (20-49) is clean in both specifications. The elderly
result is sensitive to the FE structure, suggesting residual confounding from
county-level differential trends in elderly mortality.
