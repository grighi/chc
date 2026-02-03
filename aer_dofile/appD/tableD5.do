/******************************************************
TABLE D5: DD Estimates by Specification, IPW weighted
******************************************************/
clear
clear matrix
set mat 5000
clear mata
set maxvar 10000
set matsize 10000
set more off, perm
pause on
capture log close
log using "$output/log_tableD5", replace text	

*do $dofile/pscore

use "$data/aer_data" if year<=1988, clear
merge m:1 stfips cofips using "$output/pscore_temp"
keep if _merge==3
drop _merge

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

*make year dummies by urban category, state FE and county trends
xi	i.year*i.Durb i.year*i.stfips			/*this makes county trends, state-by-year FE and urban-by-year FE*/
cap drop _Ifips*
cap drop _IDurb*
cap drop _Istfi*	

*TREATMENT VARS
char did1[omit] -1
xi i.did1, pref(_DD)
		

*adjust hospital variables to be per-50+ year old
replace H_bpc = H_bpc*(copop/copop_eld)
replace H_hpc = H_hpc*(copop/copop_eld)

*baseline spec
xtreg amr_eld _Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=dflpopwgt1], cluster(fips) fe
outreg2 using "$output/tableD5.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("Baseline, IPW: `e(cmdline)'") 
local r append


*region-by-year effects spec
cap recode stfips (9 23 25 33 44 50 34 36 42 = 1) (18 17 26 39 55 19 20 27 29 31 38 46 = 2) (10 11 12 13 24 37 45 51 54 1 21 28 47 5 22 40 48 = 4) (4 8 16 35 30 49 32 56 6 41 53 = 5), gen(region)
xi	i.year*i.Durb i.year*i.region			/*this makes county trends, state-by-year FE and urban-by-year FE*/

xtreg amr_eld _Iyear* _IyeaXDu* _IyeaXr* D_* R_* H_* _DD* [aw=dflpopwgt1], cluster(fips) fe
outreg2 using "$output/tableD5.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("RxY, IPW: `e(cmdline)'") 
local r append


*region-by-year effects spec, trimmed sample
drop if pscore1<.1|pscore1>.9
xi	i.year*i.Durb i.year*i.region			/*this makes county trends, state-by-year FE and urban-by-year FE*/

xtreg amr_eld _Iyear* _IyeaXDu* _IyeaXr* D_* R_* H_* _DD* [aw=dflpopwgt1], cluster(fips) fe
outreg2 using "$output/tableD5.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("RxY, IPW, Trimmed: `e(cmdline)'") 
local r append


log close
exit
