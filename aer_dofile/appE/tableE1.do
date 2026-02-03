/*****************************************************
TABLE E1: Trend-Break Results for Older Adult Mortality, Treated Counties Only
******************************************************/
clear
clear matrix
set mat 1000
clear mata
set maxvar 10000
set matsize 10000
set more off, perm
pause on
capture log close
log using "$output/log_tableE1", replace text	

use "$data/aer_data" if year<=1988 & chc_year_exp<=1974, clear
*Drop NY/LA/Chicago
drop if stfips==36 & cofips==61
drop if stfips==6  & cofips==37
drop if stfips==17 & cofips==31	

*Make fixed effects
*urban categories by which to generate year-FE
cap drop _urb
cap drop Durb
egen _urb = total(D_60pcturban*(year==1960)), by(fips)
egen Durb = cut(_urb), at(0, 1, 25, 50, 75, 110)		/*quarters with a zero*/

gen treat1 = chc_year_exp<=1974
*make the zero in the index come at -1, then multiply by a treatment dummy so that all other counties get zero
gen pre_exp1 = (exp1*(exp1>=-6)*(exp1<=14) + 1)
replace pre_exp1 = 0 if treat1~=1 | (exp1<-6 | exp1>14)
gen dcons_pre = (exp1>=-6) & (exp1<-1)
*make the zero in the index come at -1, then multiply by a treatment dummy so that all other counties get zero		
gen post_exp1 = (exp1*(exp1>-1)+1)*(exp1<=14)
replace post_exp1 = 0 if treat1~=1 | (exp1<=-1 | exp1>14)
gen dcons_post = exp1>-1 & exp1<=14		

char exp1 [omit] -1
xi i.exp1, pref(_T)


/*************************************************
2. Region-by-Year and Year-by-Urban Effects (and county FE)
*************************************************/
recode stfips (9 23 25 33 44 50 34 36 42 = 1) (18 17 26 39 55 19 20 27 29 31 38 46 = 2) (10 11 12 13 24 37 45 51 54 1 21 28 47 5 22 40 48 = 4) (4 8 16 35 30 49 32 56 6 41 53 = 5), gen(region)
xi i.year*i.Durb i.year*i.region

*ESTIMATION
xtreg amr_eld _I*  _Texp1_1 dcons_pre pre_exp dcons_post post_exp _Texp1_23 [aw=popwt_eld], cluster(fips) fe

*OUTREG (specifications to different files)
outreg2 dcons_pre pre_exp post_exp dcons_post using "$output/tableE1.xls", append noparen noaster title(`e(depvar)') ctitle("UxY & RxY FE: `e(cmdline)'")


/*************************************************
3. Region-by-Year and Year-by-Urban Effects , trends(and county FE)
*************************************************/
xi i.year*i.Durb i.year*i.region i.fips*year

*ESTIMATION
xtreg amr_eld _I*  _Texp1_1 dcons_pre pre_exp dcons_post post_exp _Texp1_23 [aw=popwt_eld], cluster(fips) fe

*OUTREG (specifications to different files)
outreg2 dcons_pre pre_exp post_exp dcons_post using "$output/tableE1.xls", append noparen noaster title(`e(depvar)') ctitle("UxY & RxY FE: `e(cmdline)'")




