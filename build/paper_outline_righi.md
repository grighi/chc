# OUTLINE: "Community Mental Health Centers: Then and Now"
## Gio Righi, UCLA


---

## FIGURES AND TABLES — STATUS AND NEEDS
### In Hand is numbered, dashed is still needed

1. **Figure 1**: Federal Mental Health Budgets, 1965–2023 (descriptive)
-- **Figure: National mortality trends by age group** (20–49 vs. 50+, showing divergence and deaths-of-despair rise)
-- **Table: Balance table** (CMHC counties vs. non-CMHC counties on 1960 observables)
-- **Figure: CMHC rollout map** (geographic/temporal distribution)
2. **Figure 2/4**: Timing exogeneity scatter (CMHC year vs. 1960 AMR, residualized)
3. **Figure 5**: CMHC event study, ages 20–49 (main result)
-- **Table: DD estimates, robustness specifications** (county FE, state-year FE, trends, reweighting, C&S, S&A)
-- **Table: CMHC event study by race** (white vs. non-white, ages 20–49) (Reconciliation with Avery & LaVoice)
-- **Figure: CMHC heterogeneity by baseline poverty/urbanicity/physician supply
-- **Table: CMHC incarceration long difference regressions
4. **Figure 6**: CMHC event study, ages 50+ (placebo)
-- **Table: Cause-specific mortality decomposition (CMHC)** — suicide, substance, homicide, accidents (placebo)
5. **Figure 7**: CMHC robustness to CHC controls (two panels)
-- **CCBHC balance table** (treated vs. untreated counties)
6. **Figure 8**: CCBHC mortality event study, ages 25–44 (main result 2)
7. **Figure 9**: CCBHC mortality robustness (excl. Medicaid states; planning-grant comparison)
8. **Figure 10**: CCBHC incarceration event study (main result)
-- **Table: CCBHC heterogeneity by pre-treatment opioid overdose rate**
-- **Table: Appendix: CMHC event study, ages 20–49, Poisson specification** 

---

## SECTION 1: INTRODUCTION

**Paragraph 1 — The opening policy hook.**
Open with the scale of the mental health crisis and the recurring federal question: can community-based care improve the health of mentally ill Americans? Note that community mental health has had two distinct federal eras of expansion—the Community Mental Health Centers (CMHCs) of the 1960s and the Certified Community Behavioral Health Centers (CCBHCs) of the 2010s—separated by three decades of stagnation. This paper evaluates both.

**Paragraph 2 — What CMHCs and CCBHCs did.**
Describe the core model: outpatient mental health services delivered in the community at low or no cost, intended to replace or complement institutionalization. CMHCs were authorized by President Kennedy in 1963, rolled out through the 1960s–70s, and effectively defunded at the start of the Reagan Administration. CCBHCs, created following the ACA, adopted the same Medicaid Prospective Payment System (PPS) model that stabilized Federally Qualified Health Centers, adding requirements for 24/7 crisis services and medication-assisted treatment for opioid use disorder.

**Paragraph 3 — The historical narrative and why it matters now.**
The CMHC program is widely considered a failure (Grob 1991, 1994; Rose 1979), a view reinforced by qualitative accounts tying it to the deinstitutionalization debacle. This narrative shaped policy for decades. Yet the World Health Organization continues to recommend community-based mental health care (WHO 2001), and the federal government has returned to the model with CCBHCs. Quantitative evidence on whether community mental health actually works is thus of first-order policy importance—both for evaluating the historical experiment and for guiding the current expansion.

**Paragraph 4 — Limitations of the existing literature.**
The only quantitative study of CMHCs' mortality effects, Avery and LaVoice (2023), finds effects limited to the non-white population and restricted to specific causes of death (8% reduction in non-white suicide, 14% in non-white homicide). Their analysis covers 1971–1981—a period when CMHCs were already financially strained and the program was moving toward dissolution. Moreover, their study examines cause-specific mental health mortality rather than all-cause mortality, potentially missing channels through which community mental health improves survival (e.g., reduced overdose from co-occurring substance use treatment, reduced mortality from chronic conditions exacerbated by untreated mental illness). No study has examined the contemporary CCBHC program.

**Paragraph 5 — What this paper does.**
This paper uses the staggered rollout of CMHCs across U.S. counties in the 1960s and the staggered introduction of CCBHCs across counties in the 2010s to provide the first comprehensive evidence of community mental health's effects on all-cause mortality and incarceration. By studying both eras, the paper can assess whether the same model of care produces health benefits across different institutional contexts, funding structures, and disease environments.

**Paragraph 6 — Preview of results: CMHCs.**
The establishment of a CMHC reduced age-adjusted mortality among 20–49 year olds by approximately 10–12 deaths per 100,000 within a decade—a substantial effect given baseline mortality in this age group. Critically, there is no effect among those aged 50 and older, consistent with CMHCs operating through mental health channels rather than the cardiovascular and chronic disease channels relevant to older adults. The effect is robust to controlling for the concurrent rollout of Community Health Centers, whose mortality effects operated through primary care for older Americans (Bailey and Goodman-Bacon 2015).

**Paragraph 7 — Preview of results: CCBHCs.**
Turning to the contemporary era, the introduction of CCBHCs reduced mortality among 25–44 year olds by approximately 18 deaths per 100,000 within five years. This larger effect is consistent with the more severe baseline crisis (deaths of despair) and the expanded service model (24/7 crisis services, medication-assisted treatment for opioids). CCBHCs also reduced local jail incarceration rates by approximately 10 per 100,000, providing the first causal evidence on the mental health–criminal justice pipeline.

**Paragraph 8 — Contribution and roadmap.**
The paper makes three contributions. First, it provides the first estimates of community mental health's effect on all-cause mortality, finding effects that are substantially larger than those in prior work focused on narrow cause-specific outcomes. Second, it is the first evaluation of the CCBHC program, documenting that the rebuilt model produces health gains and reduces incarceration. Third, by studying both eras, the paper documents a striking pattern: federal investment in community mental health reduces mortality, and federal disinvestment coincides with its reversal. The remainder of the paper proceeds as follows. [Roadmap.]

---

## SECTION 2: BACKGROUND AND INSTITUTIONAL HISTORY

### 2.1 The First Era: Community Mental Health Centers (1963–1981)

**Paragraph 9 — Origins.**
Describe the political and medical context of the early 1960s: optimism about psychotropic medications, military experience with brief community interventions, and Kennedy's personal connection to mental illness. The Community Mental Health Centers Construction Act of 1963 authorized $150 million in construction grants for centers serving catchment areas of 75,000–200,000 people. Centers were required to provide five essential services: inpatient, outpatient, partial hospitalization, emergency, and consultation/education.

**Paragraph 10 — Rollout and funding.**
The OEO and later HEW administered the program. Staffing grants were added in 1965. Describe the seed-money model: federal grants covered 75% initially, declining on a sliding scale, with centers expected to secure alternative funding. In practice, as documented by the Comptroller General's 1971 report to Congress, there was considerable discrepancy between planned priorities and actual funding decisions—centers were frequently established with little regard to state-level priority rankings, a feature that aids identification. [Connect to Avery and LaVoice (2023) finding on priority rankings not predicting rollout; connect to Bailey and Goodman-Bacon (2015) on similar administrative confusion in CHC rollout.]

**Paragraph 11 — What CMHCs actually provided.**
CMHCs offered outpatient counseling, crisis intervention, partial hospitalization, and consultation/education services. They served primarily the less severely mentally ill—not the chronically institutionalized population—providing preventive services and treatment for depression, anxiety, substance use, and related conditions. Services were provided at low or no cost based on ability to pay. Patient demographics skewed young (42% under 25, 39% ages 25–45 per NIMH statistical notes) and non-white populations used centers at rates disproportionate to their county population share.

**Paragraph 12 — Defunding and the "failure" narrative.**
The 1981 Omnibus Budget Reconciliation Act converted CMHC funding to block grants, effectively ending the program. By its end, only 781 of a planned 2,000+ centers had been built, and total spending was $2.66 billion. The program was declared a failure—conflated with the separate failures of deinstitutionalization—despite never having been fully implemented. The qualitative literature emphasizes the inability of CMHCs to serve the seriously mentally ill being discharged from state hospitals, but this was not the population CMHCs were designed or equipped to treat.

**Paragraph 13 — Contrast with Community Health Centers.**
While CMHCs were defunded, Community Health Centers (CHCs) providing primary medical care evolved into Federally Qualified Health Centers (FQHCs) with a stable funding model through Medicaid PPS reimbursement and Section 330 grants. [Reference Figure 1 showing the divergence in funding trajectories.] This divergence is central to the paper's narrative: the same era that saw sustained investment in community primary care saw the abandonment of community mental health care.

### 2.2 The Gap: 1981–2014

**Paragraph 14 — Three decades of stagnation.**
Real federal spending on community mental health was flat from 1981 through the 2000s under the block grant model [Figure 1]. During this period, the mental health treatment burden shifted to emergency departments, jails, and prisons. The incarceration rate of people with serious mental illness rose dramatically. Beginning in the late 1990s, deaths of despair—from suicide, drug overdose, and alcohol-related causes—began rising sharply among working-age adults (Case and Deaton 2015, 2020), a trend most acute in areas with the fewest mental health resources.

### 2.3 The Second Era: Certified Community Behavioral Health Centers (2014–Present)

**Paragraph 15 — CCBHC origins and design.**
The Excellence in Mental Health Act of 2014 created the CCBHC demonstration program. Describe the key design improvement: rather than seed money, CCBHCs are funded through Medicaid PPS, mirroring the model that stabilized FQHCs. CCBHCs must provide nine categories of services including 24/7 crisis intervention, outpatient mental health and substance use treatment, screening and assessment, and medication-assisted treatment for opioid use disorder. The expanded service requirements and sustainable payment model address the two main weaknesses of the original CMHC program.

**Paragraph 16 — Rollout.**
The initial demonstration began in 2017 with eight states; subsequent expansion grants extended the program to additional states and counties. Describe the phased rollout that provides identifying variation. Distinguish between planning grants and expansion grants.

---

## SECTION 3: EXPECTED EFFECTS AND RELATIONSHIP TO PRIOR WORK

**Paragraph 17 — Why community mental health should reduce mortality.**
The age profile of mental health–related mortality is concentrated among 20–49 year olds: suicide, overdose, alcohol-related deaths, and homicide victimization (for the mentally ill) all peak in working-age populations. CMHCs and CCBHCs directly provide treatments—counseling, medication, crisis services—that address the proximate causes of these deaths. Unlike CHCs, whose mortality effects operated through management of chronic cardiovascular conditions in the elderly (Bailey and Goodman-Bacon 2015), community mental health centers should affect mortality in younger age groups through mental health and substance use channels.

**Paragraph 18 — Why the 50+ age group serves as a placebo.**
Mortality among those aged 50 and older in the 1960s–1970s was dominated by cardiovascular disease and cancer—conditions managed by primary medical care (anti-hypertensives, etc.), not by mental health treatment. A null effect on 50+ mortality would confirm that the estimated effects operate through mental health channels rather than general improvements in healthcare infrastructure.

**Paragraph 19 — Reconciling with Avery and LaVoice (2023).**
The differences between our results and those of Avery and LaVoice (2023) can be understood along four dimensions. First, *timing*: they study the 1971–1981 period when CMHCs were already financially strained and moving toward dissolution; we study the initial rollout in the 1960s when federal funding was strongest and centers were being newly established. Second, *outcome breadth*: they examine only three cause-specific mortality categories (suicide, homicide, alcohol), while we use all-cause mortality, capturing the full range of channels through which mental health treatment improves survival. Third, *age focus*: their analysis covers all ages, diluting effects among older populations for whom mental health treatment has limited mortality impact; we focus on 20–49 year olds, where effects concentrate. Fourth, *population heterogeneity*: their finding that effects were limited to non-whites is consistent with our results if the initial rollout period had larger effects across all groups (before centers became financially constrained and had to ration services), while the later period they study shows effects only among those with the least access to alternatives.

**Paragraph 20 — Why CCBHCs might have larger effects.**
The CCBHC-era mortality effects are larger than the CMHC-era effects, which is consistent with: (a) a more severe baseline crisis (deaths of despair epidemic), (b) expanded service requirements (24/7 crisis services, MAT for opioids), (c) a more sustainable funding model (Medicaid PPS vs. declining seed money), and (d) improved treatment modalities (evidence-based pharmacotherapy and psychotherapy vs. 1960s approaches).

---

## SECTION 4: DATA

**Paragraph 21 — CMHC establishment data.**
Describe the data on when and where CMHCs were established. If using the same archival sources as Avery and LaVoice (Mental Health Directories), note this; if using OEO/HEW grant records similar to Bailey and Goodman-Bacon's CHC data, describe the source. Map the rollout geographically and temporally. [Reference Figure 3: Rollout Map.]

**Paragraph 22 — CCBHC data.**
Describe the data on CCBHC establishment from SAMHSA records, including the distinction between planning and expansion grants.

**Paragraph 23 — Mortality data.**
Vital Statistics multiple cause of death files, county-level, with population denominators from SEER or Census/NCHS bridged estimates. Age-adjusted mortality rates computed for the relevant age groups (20–49 for CMHCs, 25–44 for CCBHCs). Describe the cause-of-death classifications for decomposition analysis.

**Paragraph 24 — Incarceration data.**
County-level jail incarceration data from the Bureau of Justice Statistics Annual Survey of Jails or Census of Jails. Describe coverage and timing.

**Paragraph 25 — County characteristics.**
1960 census characteristics for CMHC analysis; ACS characteristics for CCBHC analysis. Present balance table comparing treated vs. untreated counties on pre-treatment observables. [Reference Table 1.] Note differences in population size, urbanicity, and medical workforce between treated and untreated counties, and describe how the empirical strategy addresses these.

---

## SECTION 5: EMPIRICAL STRATEGY

### 5.1 Identification

**Paragraph 26 — Approach and identifying variation.**
The empirical strategy uses variation in the timing and location of CMHC/CCBHC establishment within a flexible event-study framework (Jacobson, LaLonde, and Sullivan 1993). The key identifying assumption is that, conditional on county and year fixed effects plus controls, the timing of CMHC/CCBHC establishment is uncorrelated with trends in mortality. Describe why this is plausible: administrative confusion in the rollout process (Comptroller General 1971 report), and the disconnect between state priority rankings and actual establishment (Avery and LaVoice 2023).

**Paragraph 27 — Timing exogeneity evidence: levels.**
Present evidence that pre-treatment county characteristics do not predict the timing of CMHC establishment, conditional on ever receiving a center. [Reference Figure 2: scatter of 1960 AMR levels vs. CMHC year, residualized on % urban, % nonwhite, % poverty.] The flat regression line provides no evidence of selection on pre-program mortality levels.

**Paragraph 28 — Timing exogeneity evidence: trends.**
Extend the analysis to pre-program mortality trends (1959–60 changes vs. establishment year). Flat relationship confirms no selection on pre-trends. [This could be Panel B of the same figure or a separate figure.]

### 5.2 Estimation

**Paragraph 29 — Event-study specification.**
Present the event-study model formally. For CMHCs:

y_{ct} = α + Σ_b π_b · CMHC_c · 1(t − T*_c ∈ b) + θ_c + γ_t + λ_s·t + ξX_{ct} + ε_{ct}

where y_{ct} is the age-adjusted mortality rate (ages 20–49) in county c, year t; CMHC_c indicates whether county c ever received a CMHC; T*_c is the year of establishment; θ_c are county fixed effects; γ_t are year fixed effects; λ_s·t are state-specific linear time trends; and X_{ct} are time-varying controls. The omitted bin is t = −1.

Justify the use of a linear specification (vs. Poisson as in Avery and LaVoice 2023) given that all-cause mortality rates in the 20–49 age group have sufficient within-county variation to support linear estimation. Note that Poisson specifications are used as robustness.

**Paragraph 30 — Difference-in-differences specification.**
Present the aggregated DD specification for summarizing magnitudes. Describe the grouping of event-year bins.

**Paragraph 31 — Addressing staggered treatment timing.**
Discuss the Goodman-Bacon (2021) decomposition and concerns about bias from heterogeneous treatment effects in TWFE. Note that the large share of never-treated counties limits the weight on problematic comparisons. Describe supplementary analyses using Callaway and Sant'Anna (2021) or Sun and Abraham (2021) estimators.

**Paragraph 32 — CCBHC specification.**
Present the analogous specification for the CCBHC analysis. Note differences: shorter post-treatment window, different control variables, and the use of planning-grant states as an alternative comparison group.

---

## SECTION 6: RESULTS — CMHCs

### 6.1 Main Results

**Paragraph 33 — Event study, ages 20–49.**
Present the headline CMHC event-study figure. [Figure 5.] Describe the pattern: flat pre-trends near zero, followed by a gradual decline in mortality beginning at treatment, reaching approximately −10 to −12 per 100,000 by event year +10 to +13. Discuss the partial rebound in the far-right tail (years 15+), which corresponds to the 1981 block-grant conversion for later-adopting CMHCs. This rebound is consistent with the funding withdrawal story and is itself informative about the role of sustained investment.

**Paragraph 34 — Placebo: ages 50+.**
Present the event-study for ages 50+. [Figure 6.] Flat and noisy coefficients centered on zero throughout the event window. This rules out that the 20–49 result is driven by general improvements in healthcare infrastructure and confirms that the effect operates through mental health channels rather than primary care or cardiovascular channels.

**Paragraph 35 — DD summary estimates.**
Present the aggregated DD estimates in a table. [Table 2.] Show robustness across specifications: county FE only, county + state-year FE, state-specific trends, propensity score reweighting, and staggered-DD estimators.

### 6.2 Robustness

**Paragraph 36 — Controlling for CHCs.**
The most important robustness check: are CMHC mortality effects confounded by the concurrent rollout of Community Health Centers? Present the comparison figure. [Figure 7.] Panel A: adding per-capita CHC funding as a time-varying control—the CMHC event-study line is virtually unchanged. Panel B: including CHC event-time indicators as controls in a horse race—the CMHC effect survives with minimal attenuation. This confirms that the mortality reductions from CMHCs are distinct from the CHC effects documented by Bailey and Goodman-Bacon (2015), consistent with the fact that the two programs operated through different channels (mental health vs. primary medical care) and affected different age groups (20–49 vs. 50+).

**Paragraph 37 — Additional robustness.**
Describe additional checks: different control groups (boundary analysis, propensity-score trimmed samples), alternative specifications (Poisson, Callaway-Sant'Anna, Sun-Abraham), and controlling for other concurrent programs (Medicaid, Medicare, SSDI/SSI).

### 6.3 Heterogeneity and Mechanisms

**Paragraph 38 — Cause-specific decomposition.**
Decompose the all-cause mortality effect by cause of death. [Table 3.] Expect the effect to be concentrated in suicide, substance-related deaths, and homicide—the causes most directly linked to mental illness and its treatment. Accidents should serve as a placebo cause (expect null). The finding that effects extend beyond the narrow set of causes examined by Avery and LaVoice (2023) explains why all-cause effects are larger than the sum of their cause-specific estimates.

**Paragraph 39 — Heterogeneity by race.**
Examine whether effects differ by race, connecting to Avery and LaVoice's finding that CMHC effects were limited to non-whites. If our results show effects for both white and non-white populations in the initial rollout period, this is consistent with the hypothesis that the earlier, better-funded era of CMHCs produced broader benefits, while the later period studied by Avery and LaVoice saw effects only among those with the least access to alternatives.

**Paragraph 40 — Heterogeneity by county characteristics.**
Examine effect heterogeneity by pre-treatment poverty rate, urbanicity, physician supply, and distance to the nearest state mental hospital. Larger effects in areas with fewer pre-existing mental health resources would support the access mechanism. Interaction with deinstitutionalization proximity would shed light on whether CMHCs served as a substitute for institutional care.

**Paragraph 41 — The funding withdrawal and the late-period rebound.**
Discuss the partial rebound in mortality effects visible in the far-right tail of the event study. For late-adopting CMHCs, event years 10–15 correspond to the post-1981 block grant era. This pattern—initial improvement followed by partial reversal when funding is cut—is itself evidence of a causal relationship between community mental health investment and mortality.

---

## SECTION 7: RESULTS — CCBHCs

### 7.1 Main Results

**Paragraph 42 — Event study, ages 25–44.**
Present the headline CCBHC event-study. [Figure 8.] Beautifully flat pre-trends, sharp break at treatment, with mortality declining approximately 18 per 100,000 by year +5. Discuss the larger magnitude compared to CMHCs in the context of the more severe baseline crisis (deaths of despair), expanded service requirements, and improved treatment modalities.

**Paragraph 43 — Incarceration results.**
Present the CCBHC incarceration event study. [Figure 10.] Jail population rates decline approximately 10 per 100,000 by year +3 to +5, with flat pre-trends. This is the first causal evidence on whether community-based mental health treatment reduces incarceration—a central question in criminal justice reform. The result is consistent with a diversion mechanism: individuals receiving community treatment are less likely to enter the criminal justice system, which itself reduces mortality risk.

### 7.2 Robustness

**Paragraph 44 — Excluding Medicaid waiver states.**
Present robustness to excluding states with concurrent Medicaid expansion waivers. [Figure 9, Panel A.] The effect attenuates somewhat (~11 per 100k vs. ~18) but remains clearly present, confirming that the mortality reduction is not solely driven by Medicaid expansion.

**Paragraph 45 — Planning-grant comparison group.**
Present estimates using planning-grant states as the comparison group. [Figure 9, Panel B.] These states expressed interest in the program but did not receive the expansion grant, making them a tighter counterfactual. The effect is actually *larger* in this specification (~14 per 100k), alleviating concerns about selection into CCBHC status.

### 7.3 Heterogeneity and Mechanisms

**Paragraph 46 — Cause-specific decomposition.**
Decompose CCBHC mortality effects by cause. [Table 4.] Expect contributions from overdose (given MAT requirements), suicide (crisis services), and alcohol-related mortality.

**Paragraph 47 — Heterogeneity by opioid crisis severity.**
Examine whether effects are larger in counties with higher pre-treatment opioid overdose rates, which would be consistent with the MAT requirement driving part of the mortality reduction.

---

## SECTION 8: CONNECTING THE TWO ERAS

**Paragraph 48 — The natural experiment of disinvestment.**
Bring the two sets of results together. The paper documents a striking pattern visible in Figure 1: when the federal government invested in community mental health (CMHCs in the 1960s, CCBHCs in the 2010s), mortality among working-age adults fell; when it disinvested (1981), improvements stalled or reversed. While the two eras differ in many ways—disease environment, treatment technology, funding model, service requirements—the fundamental finding is consistent: access to community-based mental health care reduces mortality.

**Paragraph 49 — What the CCBHC model learned from CMHC failures.**
Discuss how the CCBHC design addresses the two main criticisms of CMHCs: (a) the seed-money funding model that led to financial instability is replaced by Medicaid PPS, and (b) the lack of mandate to serve those with serious mental illness is addressed by expanded service requirements including 24/7 crisis care. The fact that CCBHCs produce larger mortality effects per year of exposure is consistent with these design improvements.

---

## SECTION 9: COST-EFFECTIVENESS

**Paragraph 50 — CMHC cost-effectiveness.**
Translate the mortality estimates into cost-per-life-saved. Compare to Bailey and Goodman-Bacon's estimates for CHCs and to Medicare implementation estimates (Chay, Kim, and Swaminathan 2011). Given the younger age of deaths prevented by CMHCs, the life-years saved per death are substantially larger than for programs targeting the elderly.

**Paragraph 51 — CCBHC cost-effectiveness.**
Similar calculation for CCBHCs. Include the additional benefit of reduced incarceration, which carries its own substantial fiscal savings (cost per jail bed-year).

**Paragraph 52 — Lower bounds.**
Note that these cost ratios understate the broader effects because mortality and incarceration fail to capture changes in morbidity, disability, employment, homelessness, and other quality-of-life improvements. Mortality is the most extreme outcome of untreated mental illness; improvements on less severe margins are expected to be larger.

---

## SECTION 10: CONCLUSION

**Paragraph 53 — Summary.**
The United States has run the community mental health experiment twice. Both times, the investment reduced mortality among working-age adults. The first time, the experiment was abandoned before completion; the second time, it was designed with a more sustainable funding and service model. The evidence from both eras supports the WHO recommendation that community-based mental health care can be effective at reducing the most severe consequences of mental illness.

**Paragraph 54 — Policy implications.**
The results imply that CCBHC expansion is a cost-effective investment in public health and public safety. The incarceration results are particularly relevant to ongoing debates about alternatives to incarceration for people with mental illness. The CMHC results, including the late-period rebound when funding was cut, suggest that the sustainability of effects depends on sustained investment—a lesson embodied in the CCBHC's Medicaid PPS funding model.

**Paragraph 55 — Broader lessons.**
The paper also illustrates how the evaluation of a policy can depend on when and whom one studies. The CMHC program was declared a failure based on qualitative assessments of its inability to serve the deinstitutionalized—a population it was never designed to treat. Quantitative evidence on the population it *was* designed to serve—working-age adults with moderate mental illness, substance use disorders, and limited access to care—tells a different story. The historical narrative, shaped by the most visible failures, obscured the real benefits of community mental health care for three decades.

---


### Previously Generated Ideas for Outputs Still Needed:

| # | Figure/Table | Purpose | Priority |
|---|---|---|---|
| 1 | **Table 1: Balance table** (CMHC counties vs. non-CMHC counties on 1960 observables) | Descriptive, identification support | High |
| 2 | **Figure: National mortality trends by age group** (20–49 vs. 50+, showing divergence and deaths-of-despair rise) | Motivation — bridges Figure 1's funding gap to the outcome of interest | High |
| 3 | **Figure: CMHC rollout map** (geographic/temporal distribution) | Descriptive | High |
| 4 | **Table 2: DD estimates, robustness specifications** (county FE, state-year FE, trends, reweighting, C&S, S&A) | Main result summary | High |
| 5 | **Table 3: Cause-specific mortality decomposition (CMHC)** — suicide, substance, homicide, accidents (placebo) | Mechanism | High |
| 6 | **Table 4: Cause-specific mortality decomposition (CCBHC)** — overdose, suicide, alcohol | Mechanism | High |
| 7 | **Figure: CMHC event study by race** (white vs. non-white, ages 20–49) | Reconciliation with Avery & LaVoice | High |
| 8 | **CCBHC balance table** (treated vs. untreated counties) | Descriptive | Medium |
| 9 | **CCBHC rollout map** | Descriptive | Medium |
| 10 | **Figure: CMHC heterogeneity by baseline poverty/urbanicity/physician supply** | Mechanism | Medium |
| 11 | **Figure: CMHC heterogeneity by distance to state hospital** | Deinstitutionalization interaction | Medium |
| 12 | **Figure: CCBHC heterogeneity by pre-treatment opioid overdose rate** | Mechanism | Medium |
| 13 | **Figure: CMHC event study, ages 20–49, Poisson specification** | Robustness / comparability with Avery & LaVoice | Medium |
| 14 | **Table: CMHC cost-per-life-saved vs. CHC and Medicare benchmarks** | Cost-effectiveness | Medium |

### New Graphs to Construct (not currently mentioned in manuscript):

1. **National mortality time series by age group and cause (1960–2023)**: This is the single most important missing descriptive figure. It should show: (a) a panel for ages 50+ with steady cardiovascular-driven decline, and (b) a panel for ages 20–49 showing the decline in the 1960s–70s, the flattening in the 1980s–90s, and the rise of deaths of despair post-2000. Overlay the CMHC and CCBHC eras (shaded bands or vertical lines). This motivates the entire paper and connects the funding story (Figure 1) to the health outcomes.

2. **CMHC event study by race (ages 20–49)**: Critical for the dialogue with Avery & LaVoice. If you find effects for both white and non-white populations in the 1960s rollout, this directly speaks to why their later-period analysis found effects only for non-whites. If you find effects only for non-whites even in the 1960s, the story changes and the all-cause result is driven by non-white mortality channels.

3. **Side-by-side CMHC and CCBHC event studies on a common scale**: A single figure with two panels using the same y-axis. This makes the "then and now" comparison vivid—the reader can see that both interventions produce the same qualitative pattern (flat pre-trends, post-treatment decline) but that the CCBHC effects are steeper. This would be a signature figure for the paper.

4. **Bacon decomposition diagnostic (CMHC)**: Show the weights on different 2×2 DD comparisons. Given 80%+ never-treated counties, problematic comparisons should receive little weight, but showing this explicitly preempts a referee concern.

5. **CCBHC event study for ages 50+ (placebo)**: The manuscript appendix mentions the CMHC 50+ null but does not indicate whether the CCBHC 50+ placebo exists. If it does not, construct it. For the same reason you need the CMHC placebo, you need it for CCBHCs.

6. **Deaths of despair decomposition for CCBHCs**: Separate event studies for (a) drug overdose, (b) suicide, (c) alcohol-related mortality. Given the CCBHC's MAT mandate, overdose should show the sharpest break at treatment. This decomposition makes the mechanism story concrete.

---

## KEY STRATEGIC NOTES FOR TOP-5 TARGETING

**Framing**: The "then and now" structure is the paper's unique selling point. No other paper can tell this story. Frame it not as two separate analyses stapled together, but as a unified test of a recurring policy question: does public investment in community mental health care improve health? The answer from both eras is yes.

**Dialogue with Bailey & Goodman-Bacon (2015)**: This is the natural comparison paper. They studied CHCs (physical health, 50+ mortality); you study CMHCs (mental health, 20–49 mortality). The CHC-CMHC robustness check is central—it shows the two programs operated through different channels on different populations. The cost-effectiveness comparison is also natural: CHCs were cost-effective for the elderly; CMHCs appear cost-effective for working-age adults.

**Dialogue with Avery & LaVoice (2023)**: Acknowledge their contribution as the first quantitative study. The differences in results are interpretable and informative, not contradictory. Their smaller effects are consistent with studying a later period (financial strain), narrower outcomes (specific causes vs. all-cause), and all ages (diluting age-specific effects). Your paper complements theirs by studying the initial rollout and broadening the outcome.

**The incarceration result**: This is a standalone policy contribution. Emphasize it. The mental health → criminal justice pipeline is one of the most discussed policy questions in the U.S. right now. Causal evidence that community mental health investment reduces incarceration will attract attention from both health economists and criminal justice researchers.
