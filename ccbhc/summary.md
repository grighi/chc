# Mental Health Facilities and Incarceration: Data Summary and Findings

## 1. Data Construction

### Facility Data

We digitized the SAMHSA *Mental Health Directory* PDFs for 2017--2020.
Each PDF was converted from fixed-width multi-column text to structured
CSV records containing facility name, address, phone, intake info, and
service codes.  Facilities were geocoded to counties via 5-digit ZIP
lookup (`zipcodeR`) and assigned FIPS codes (`tigris`), yielding the
stacked panel `facilities_geocoded.rds`.

Three facility types were defined by grepping service codes:

| Type     | Service code match         | Interpretation                        |
|----------|----------------------------|---------------------------------------|
| all_omh  | CMHC, OMH, or MSNH        | Any outpatient mental health facility |
| cmhc     | CMHC                      | Community Mental Health Center        |
| act      | ACT                       | Assertive Community Treatment team    |

County-year facility counts were collapsed from the facility-level data.

### Incarceration Data

County-quarter incarceration data (`incarceration_trends_county.csv`)
was averaged across quarters within each county-year to produce a unique
county-year panel of `total_pop` and `total_incarceration_rate` (per
100k).  The analysis sample restricts to years after 2010.

### Merge

The two datasets were merged on 5-digit FIPS code and year.  Counties
absent from the facility data in a given year received facility counts
of zero.  Rows with missing incarceration rates were dropped.

---

## 2. Cross-Sectional Regressions (2017--2018)

The initial analysis (`facility_incarceration_analysis_1718.R`) ran
two-way FE regressions of incarceration rate on facility counts with
county and year fixed effects:

| Model   | Coefficient | SE    | t       | p        | N      |
|---------|-------------|-------|---------|----------|--------|
| all_omh | -12.59      | 1.32  | -9.53   | < 0.001  | 70,047 |
| cmhc    | -33.12      | 4.62  | -7.17   | < 0.001  | 70,047 |
| act     | -52.78      | 6.01  | -8.78   | < 0.001  | 70,047 |

All three facility types show large, highly significant negative
associations with incarceration rate.  An additional CMHC facility is
associated with a 33-point lower incarceration rate per 100k; an
additional ACT team with a 53-point reduction.  These are correlational
--- the event study below attempts causal identification.

---

## 3. Facility Change Distributions

Before defining treatment, we examined the distribution of non-zero
year-over-year changes in facility counts per 100k population.

![Histogram -- All OMH changes per 100k](histogram_all_omh.png)

![Histogram -- CMHC changes per 100k](histogram_cmhc.png)

![Histogram -- ACT changes per 100k](histogram_act.png)

The histograms show that most county-year changes are modest in absolute
terms but highly variable per capita, especially in small counties.
This motivates the robustness check restricting to counties with
population >= 50k (Spec C below).

---

## 4. Event Study Results

Treatment is defined as the first year (excluding 2017, which is the
panel start) a county's facility count of a given type increases by at
least one relative to the prior year.  Never-treated counties serve as
controls.  The specification is:

$$\text{incarceration\_rate}_{ct} = \sum_{\tau} \beta_\tau \cdot \mathbf{1}[\text{event\_time} = \tau] + \gamma \cdot \text{pop}_{ct} + \alpha_c + \delta_t + \varepsilon_{ct}$$

with county and year FE and SEs clustered by county.  Reference period:
$\tau = -1$.

### All OMH Facilities

![Event study -- All OMH](event_study_all_omh.png)

| Event time | Coef   | SE    | 95% CI             |
|------------|--------|-------|--------------------|
| -5         | 84.8   | 24.4  | [37.1, 132.6]**    |
| -4         | 23.5   | 11.9  | [0.1, 46.9]**      |
| -3         | 16.0   | 9.4   | [-2.4, 34.4]       |
| -2         | 7.3    | 6.0   | [-4.5, 19.1]       |
| -1         | 0      | --    | ref                |
| 0          | 1.1    | 11.2  | [-20.9, 23.0]      |
| 1          | -20.5  | 25.2  | [-69.9, 28.9]      |
| 2          | 19.7   | 34.0  | [-46.9, 86.4]      |
| 3          | 51.4   | 44.6  | [-36.2, 138.9]     |

Pre-treatment coefficients are positive and significant at t=-5 and
t=-4, suggesting treated counties had *rising* incarceration rates
relative to controls before gaining a facility.  Post-treatment
coefficients are noisy and not statistically significant.

### CMHC Facilities

![Event study -- CMHC](event_study_cmhc.png)

| Event time | Coef   | SE    | 95% CI             |
|------------|--------|-------|--------------------|
| -5         | 92.6   | 23.6  | [46.3, 138.9]***   |
| -4         | 33.7   | 12.2  | [9.7, 57.7]***     |
| -3         | 37.6   | 9.5   | [18.9, 56.3]***    |
| -2         | 16.8   | 6.8   | [3.4, 30.2]**      |
| -1         | 0      | --    | ref                |
| 0          | 8.3    | 11.7  | [-14.6, 31.3]      |
| 1          | -43.8  | 24.6  | [-92.0, 4.5]*      |
| 2          | 15.6   | 34.8  | [-52.6, 83.8]      |
| 3          | -9.4   | 42.1  | [-91.8, 73.0]      |

The CMHC results show a clear violation of parallel trends: all four
pre-treatment coefficients are positive and significant, indicating
counties that gain a CMHC were already on a different incarceration
trajectory.  At t=1 there is a marginally significant drop of ~44
points, but this must be interpreted cautiously given the pre-trend
pattern.

### ACT Facilities

![Event study -- ACT](event_study_act.png)

| Event time | Coef   | SE    | 95% CI             |
|------------|--------|-------|--------------------|
| -5         | 74.8   | 41.3  | [-6.2, 155.8]      |
| -4         | 5.3    | 16.9  | [-27.9, 38.4]      |
| -3         | 6.1    | 11.8  | [-16.9, 29.2]      |
| -2         | -22.9  | 14.5  | [-51.3, 5.6]       |
| -1         | 0      | --    | ref                |
| 0          | -7.5   | 14.4  | [-35.7, 20.7]      |
| 1          | 0.5    | 33.9  | [-66.1, 67.0]      |
| 2          | -0.9   | 46.4  | [-91.9, 90.0]      |
| 3          | 2.7    | 57.4  | [-109.9, 115.2]    |

ACT estimates are imprecise throughout, reflecting fewer treated
counties.  Pre-trends are generally not significant (except a marginal
negative at t=-2).  Post-treatment coefficients are close to zero with
wide confidence intervals --- no detectable effect.

---

## 5. Diagnostics (CMHC)

### Test 1: Baseline Imbalance

We compared mean pre-period (t <= -2) incarceration rates for treated
vs. never-treated counties.  The diagnostics script reports the
difference and a two-sample t-test.  Given the significant pre-period
coefficients in the event study, baseline imbalance is expected.

### Test 2: Pre-trend Slopes

For each county we estimated the slope of incarceration rate on year
using only pre-treatment observations, then compared mean slopes across
groups.

![Pre-trend density -- treated vs. never-treated](diagnostics_pretrend_density_cmhc.png)

The density plot shows the distribution of county-level pre-period
slopes.  Treated counties have a visibly different slope distribution,
confirming the pre-trend concern flagged in the event study.

### Robustness Specifications

| Spec | Description | N obs | Mean pre coef | Max |pre| | t=0 coef | t=0 SE |
|------|-------------|-------|---------------|------------|----------|--------|
| Baseline | County + year FE | 22,029 | 29.0 | 38.2 | 3.1 | 11.2 |
| A: County trends | + county-specific linear trends | 22,029 | -12.9 | 27.9 | 17.5 | 11.0 |
| B: Placebo timing | Treatment shifted back 1 year | 22,029 | 20.1 | 24.3 | -13.9 | 6.2 |
| C: Pop >= 50k | Large counties only | 7,441 | 19.9 | 25.0 | 1.5 | 5.9 |

#### Spec A: County-Specific Linear Trends

![Spec A -- County trends](diagnostics_es_spec_a__county_trends.png)

Adding county-specific linear trends flips the pre-period coefficients
negative (mean -12.9, down from +29.0 in baseline), indicating the
baseline pre-trends were largely linear.  The t=0 coefficient becomes
+17.5 (insignificant), suggesting that after absorbing county trends
there is no clear treatment effect.

#### Spec B: Placebo Timing

![Spec B -- Placebo timing](diagnostics_es_spec_b__placebo_timing.png)

Shifting treatment back one year produces a significant negative placebo
effect at t=0 (-13.9, SE 6.2, p < 0.05) and at t=2 (-44.3).  This is
difficult to interpret as a clean placebo failure; it may reflect the
actual treatment effect leaking into adjacent periods, or it may
indicate that the timing of facility openings is endogenous to
incarceration trends.

#### Spec C: Large Counties Only

![Spec C -- Pop >= 50k](diagnostics_es_spec_c__pop____50k.png)

Restricting to counties with population >= 50k (7,441 obs) reduces
the sample substantially.  Pre-trend coefficients remain positive
(mean 19.9) but standard errors tighten.  The t=0 effect is essentially
zero (1.5, SE 5.9), and post-treatment estimates are imprecise.

---

## 6. Summary of Findings

1. **Strong cross-sectional association:** Counties with more mental
   health facilities have substantially lower incarceration rates,
   across all three facility types.

2. **Pre-trends undermine causal claims:** The CMHC event study shows
   large, significant positive pre-treatment coefficients --- counties
   that gain facilities were already on a different trajectory.  This
   pattern is consistent with facilities opening in response to rising
   incarceration or correlated county-level changes (policy, funding).

3. **County trends absorb pre-trends but eliminate the effect:** Spec A
   shows that adding county-specific linear trends removes the
   pre-trend pattern, but also eliminates any post-treatment effect.

4. **ACT results are too noisy:** Too few ACT openings to detect an
   effect with this panel length.

5. **Large counties do not drive results differently:** Restricting to
   pop >= 50k does not materially change the picture.

The main takeaway is that the cross-sectional relationship between
mental health facilities and incarceration is not supported by a clean
DiD design in this panel.  The pre-trend violations suggest that
facility placement is endogenous to county-level trends, and
alternative identification strategies (e.g., instrumental variables,
staggered adoption with heterogeneity-robust estimators) may be needed.
