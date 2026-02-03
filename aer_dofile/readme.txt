readme file for Bailey and Goodman-Bacon (2014)
The War on Poverty's Experiment in Public Medicine: 
The Impact of Community Health Centers on the Mortality of Older Americans


*datasets

  74.5M  10/10/14 22:10  aer_data.dta      	- MAIN ANALYSIS FILE (1959-1998): mortality rates, covariates, weights, CHC variables
  68.2M  10/10/14 23:07  aer_cen70.dta    	- IPUMS extract (1970) Census with CHC variables 
9318.6k  10/10/14 20:58  aer_chc_aha.dta   	- AHA data by county (1948-1990) with CHC variables
 236.2k  10/10/14 22:37  aer_nhc.dta       	- Selected variables from the OEO 11-City Survey
 729.0k  10/10/14 20:58  aer_pscore_data.dta	- 1960 Characteristics with CHC variables (ICPSR 2896)
1230.8k  10/10/14 20:58  aer_shsue.dta   	- Selected variables from 1963 and 1970 Surveys of Health Services Utilization and Expenditure (ICPSR 7740, 7741)


*variables in main analysis file

Contains data from aer_data.dta
  obs:        96,185                          
 vars:           138                          11 Oct 2014 01:06
 size:    78,102,220                          (_dta has notes)
-------------------------------------------------------------------------------------------------------------------------------
              storage   display    value
variable name   type    format     label      variable label
-------------------------------------------------------------------------------------------------------------------------------
fips            float   %9.0g                 FIPS
stfips          float   %8.0g                 State FIPS
cofips          float   %9.0g               * County FIPS
year            float   %ty                   Calendar Year
chc_year_exp    int     %9.0g                 Year of first non-planning CHC grant
exp1            byte    %9.0g                 event-time, 1965-1974 CHCs
exp2            byte    %9.0g                 event-time, all CHCs
exp1_1998       float   %9.0g                 event-time, 1965-1974 CHCs, 1959-1998 data
did1            byte    %9.0g                 diff-in-diff groups, 1965-1974 CHCs
did2            byte    %9.0g                 diff-in-diff groups, all CHCs
samp8998        float   %9.0g                 counties observed 1959-1998
amr_eld_2       double  %9.0g                 AMR, 50+, Cardiovascular Disease
amr_eld_3       double  %9.0g                 AMR, 50+, Cerebrovascular Disease
amr_eld_4       double  %9.0g                 AMR, 50+, Cancer
amr_eld_5       double  %9.0g                 AMR, 50+, Infectious Disease
amr_eld_6       double  %9.0g                 AMR, 50+, Diabetes
amr_eld_7       double  %9.0g                 AMR, 50+, Accidents
amr_eld         double  %9.0g                 AMR, 50+
amr_w_eld       double  %9.0g                 AMR, White, 50+
amr_nw_eld      double  %9.0g                 AMR, Nonwhite, 50+
amr_ad_2        double  %9.0g                 AMR, Adults, Cardiovascular Disease
amr_ad_3        double  %9.0g                 AMR, Adults, Cerebrovascular Disease
amr_ad_4        double  %9.0g                 AMR, Adults, Cancer
amr_ad_5        double  %9.0g                 AMR, Adults, Infectious Disease
amr_ad_6        double  %9.0g                 AMR, Adults, Diabetes
amr_ad_7        double  %9.0g                 AMR, Adults, Accidents
amr_ad          double  %9.0g                 AMR, 20-49
amr_ch_2        double  %9.0g                 AMR, Children, Cardiovascular Disease
amr_ch_3        double  %9.0g                 AMR, Children, Cerebrovascular Disease
amr_ch_4        double  %9.0g                 AMR, Children, Cancer
amr_ch_5        double  %9.0g                 AMR, Children, Infectious Disease
amr_ch_6        double  %9.0g                 AMR, Children, Diabetes
amr_ch_7        double  %9.0g                 AMR, Children, Accidents
amr_ch          double  %9.0g                 AMR, 1-19
amr             double  %9.0g                 AMR, All Ages
imr             float   %9.0g                 Infant Mortality Rate
nnmr            float   %9.0g                 Neonatal Infant Mortality Rate
pnmr            float   %9.0g                 Postneonatal Infant Mortality Rate
asmr_5064_2     double  %9.0g                 ASMR, 50-64, Cardiovascular Disease
asmr_6500_2     double  %9.0g                 ASMR, 65+, Cardiovascular Disease
asmr_5064_3     double  %9.0g                 ASMR, 50-64, Cerebrovascular Disease
asmr_6500_3     double  %9.0g                 ASMR, 65+, Cerebrovascular Disease
asmr_5064_4     double  %9.0g                 ASMR, 50-64, Cancer
asmr_6500_4     double  %9.0g                 ASMR, 65+, Cancer
asmr_5064_5     double  %9.0g                 ASMR, 50-64, Infectious Disease
asmr_6500_5     double  %9.0g                 ASMR, 65+, Infectious Disease
asmr_5064_6     double  %9.0g                 ASMR, 50-64, Diabetes
asmr_6500_6     double  %9.0g                 ASMR, 65+, Diabetes
asmr_5064_7     double  %9.0g                 ASMR, 50-64, Accidents
asmr_6500_7     double  %9.0g                 ASMR, 65+, Accidents
asmr_6579       double  %9.0g                 ASMR, 65-79
asmr_8000       double  %9.0g                 ASMR, 80+
asmr_5064       double  %9.0g                 ASMR, 50-64
asmr_6500       double  %9.0g                 ASMR, 65+
copop_6579      double  %9.0g                 Population, 65-79
copop_8000      double  %9.0g                 Population, 80+
copop_5064      double  %9.0g                 Population, 50-64
copop_6500      double  %9.0g                 Population, 65+
copop_w_eld     double  %9.0g                 Population, White, 50+
copop_nw_eld    double  %9.0g                 Population, Nonwhite, 50+
copop_eld       double  %9.0g                 Population, 50+
copop_ad        double  %9.0g                 Population, 20-49
copop_ch        double  %9.0g                 Population, 1-19
copop           double  %9.0g                 Population, Total
births          float   %9.0g                 Live Births
popwt_6579      double  %9.0g                 Population (1960), 65-79
popwt_8000      double  %9.0g                 Population (1960), 80+
popwt_5064      long    %9.0g                 Population (1960), 50-64
popwt_6500      long    %9.0g                 Population (1960), 65+
popwt_w_eld     long    %9.0g                 Population (1960), White, 50+
popwt_nw_eld    long    %9.0g                 Population (1960), Nonwhite, 50+
popwt_eld       long    %9.0g                 Population (1960), 50+
popwt_ad        long    %9.0g                 Population (1960), 20-49
popwt_ch        long    %9.0g                 Population (1960), 1-19
popwt           long    %9.0g                 Population (1960), Total
popwt60         float   %9.0g                 (mean) totcopop
bwt_w           float   %9.0g                 White Live Births (1960)
bwt_nw          int     %9.0g                 Nonwhite Live Births (1960)
bwt             float   %9.0g                 Live Births (1960)
dflpopwgt1_6579 float   %9.0g                 DFLxPop Weight, Early CHCs, 65-79
dflpopwgt1_8000 float   %9.0g                 DFLxPop Weight, Early CHCs, 80+
dflpopwgt1_5064 float   %9.0g                 DFLxPop Weight, Early CHCs, 50-64
dflpopwgt1_6500 float   %9.0g                 DFLxPop Weight, Early CHCs, 65+
dflpopwgt1_eld  float   %9.0g                 DFLxPop Weight, Early CHCs, 50+
dflpopwgt2_eld  float   %9.0g                 DFLxPop Weight, All CHCs, 50+
dflpopwgt1_ad   float   %9.0g                 DFLxPop Weight, Early CHCs, 20-49
dflpopwgt1_ch   float   %9.0g                 DFLxPop Weight, Early CHCs, 1-19
dflpopwgt1_inf  float   %9.0g                 
dflpopwgt1      float   %9.0g                 DFLxPop Weight, Early CHCs, All Ages
dflpopwgt2      float   %9.0g                 DFLxPop Weight, All CHCs, All Ages
pscore1         float   %9.0g                 Propensity Score, 1965-1974 CHCs
pscore2         float   %9.0g                 Propensity Score, All CHCs
D_pct59inclt3~t float   %9.0g                 1959: Inc<3k x Trend
D_60pctnonwhi~t float   %9.0g                 1960: Nonwhite x Trend
D_60pctrurf_t   float   %9.0g                 1960: Rural x Trend
D_60pcturban_t  float   %9.0g                 1960: Urban x Trend
D_tot_act_md_t  float   %9.0g                 1960: Total Active MDs x Trend
_60pcturban     double  %10.0g                % urban 1960
_60pctrurf      double  %10.0g                % rural farm 1960
_60pctnonwhit   double  %10.0g                % nonwhite 1960
_60pct04years   double  %10.0g                % population aged 0-4 (1960)
_60pctmt64years double  %10.0g                % population aged 65+ (1960)
_59medfaminc    float   %12.0g                Median family income, 1959
_pct59inclt3k   double  %10.0g                % w/ 1959 family income <$3000 (1960)
_pct59incmt10k  double  %10.0g                % w/ 1959 family income $10,000+ (1960)
_60medschlmt24  double  %10.0g                Median years schooling/persons 25+ (1960)
_60pctlt4schl   double  %10.0g                % persons 25+ w/ <4 yrs schooling (1960)
_60pctmt12schl  double  %10.0g                % persons 25+ w/ 12+ yrs schooling (1960)
_tot_act_md     double  %10.0g                (sum) tot_act_md
_tot_med_stud   double  %10.0g                (sum) tot_med_stud_69
R_tranpcret     double  %10.0g                Retirement Transfers PC (REIS)
R_tranpcpa1     double  %10.0g                Public Assistance Transfers PC (REIS)
tranpcmcare1    double  %10.0g                Medicare + Military Medical PC (REIS)
tranpcmcare     double  %10.0g                Medicare PC (REIS)
tranpcmcaid     double  %10.0g                Medicaid PC (REIS)
H_bpc           float   %9.0g                 Beds per Capita (AHA)
H_hpc           float   %9.0g                 Hospitals per Capita (AHA)
er_a            float   %9.0g                 Medicare Part A Enrollment Rate
er_b            float   %9.0g                 Medicare Part B Enrollment Rate
er_ab           float   %9.0g                 Medicare Part A+B Enrollment Rate
rpe_amt_a       float   %9.0g                 Medicare Part A Per-Recipient Expenditures
rpe_amt_b       float   %9.0g                 Medicare Part B Per-Recipient Expenditures
rpe_amt_ab      float   %9.0g                 Medicare Part A+B Per-Recipient Expenditures
grant_hs        float   %9.0g                 Head Start Grant
grant_capadmin  float   %9.0g                 CAP Admin Grant
grant_legal     float   %9.0g                 LSP Grant
grant_health    float   %9.0g                 CAP Health Grant
grant_sen       float   %9.0g                 CAP Seniors Program Grant
grant_fs        float   %9.0g                 Received ANY grant for Food Stamps?
grant_chc       float   %9.0g                 CHC Grant
pcrfund_hs      float   %9.0g                 Real PC HS Funds
pcrfund_capad~n float   %9.0g                 Real PC CAP Admin Funds
pcrfund_legal   float   %9.0g                 Real PC LSP Funds
pcrfund_health  float   %9.0g                 Real PC CAP Health Funds
pcrfund_sen     float   %9.0g                 Real PC CAP Seniors Funds
pcrfund_chc     float   %9.0g                 Real PC CHC Funds
imr_w           float   %9.0g                 White, Infant Mortality Rate
imr_nw          float   %9.0g                 Nonwhite Infant Mortality Rate



*do files
There are dofiles that correspond to every figure and table in the main text and appendix except tables describing the data (Appendix A, tables F1 and F2),
and those reproduced from other sources (Appendix B).  D4.  Most files also use the  "outreg2.ado".  There are also excel files that format the outreg file
to match the tables in the published paper/appendices.  The fixed effects plotted in figure D2 will be in the log file after generating figure 5.  We also 
incude a dofile that generates the propensity scores.  This code is called in the dofiles that create table 1 and figure The analysis was done in Stata/MP 13.1.  



