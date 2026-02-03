/******************************************************
TABLE 3: Cause-specific DD Estimates for older adults (50+) and elderly/nonelderly
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
log using "$output/log_table3", replace text	

use "$data/aer_data" if year<=1988, clear
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
xi	i.year*i.Durb i.year*i.stfips	
cap drop _IDurb*
cap drop _Istfi*	

*TREATMENT VARS
char did1[omit] -1
xi i.did1, pref(_DD)

*adjust hospital variables to be per-50+ year old
replace H_bpc = H_bpc*(copop/copop_eld)
replace H_hpc = H_hpc*(copop/copop_eld)
		
***************
*By-Cause 50+
***************
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt_eld]"	

local r replace
*all cause
xtreg amr_eld `X', cluster(fips) fe
		
*OUTREG RESULTS
outreg2 using "$output/table3.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR, 50+: `e(cmdline)'") 
local r append
forval c = 2/7{		
	xtreg amr_eld_`c' `X', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/table3.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR, 50+, Cause `c': `e(cmdline)'") 
}

local r append
***************
*By-Cause 50-64
***************
*adjust hospital variables to be per-50-64 year old
replace H_bpc = H_bpc*(copop_eld/copop_5064)
replace H_hpc = H_hpc*(copop_eld/copop_5064)

local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt_5064]"	

*all cause
xtreg asmr_5064 `X', cluster(fips) fe
		
*OUTREG RESULTS
outreg2 using "$output/table3.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR, 50-64: `e(cmdline)'") 

forval c = 2/7{		
	xtreg asmr_5064_`c' `X', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/table3.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR, 50-64, Cause `c': `e(cmdline)'") 
	local r append
}

*************
*By-Cause 65+
*************
*adjust hospital variables to be per-65+ year old
replace H_bpc = H_bpc*(copop_5064/copop_6500)
replace H_hpc = H_hpc*(copop_5064/copop_6500)

local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt_6500]"	

*all cause
xtreg asmr_6500 `X', cluster(fips) fe
		
*OUTREG RESULTS
outreg2 using "$output/table3.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR, 65+: `e(cmdline)'") 

forval c = 2/7{		
	xtreg asmr_6500_`c' `X', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/table3.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR, 65+, Cause `c': `e(cmdline)'") 
	local r append
}

log close
exit
