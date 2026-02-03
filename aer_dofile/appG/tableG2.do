/******************************************************
TABLE G2: Cause-specific DD Estimates for older adults (50+) and elderly/nonelderly
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
log using "$output/log_tableG2", replace text	

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
replace H_bpc = H_bpc*(copop/copop_ch)
replace H_hpc = H_hpc*(copop/copop_ch)
		
***************
*By-Cause Children
***************
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt_ch]"	

local r replace
forval c = 2/7{		
	xtreg amr_ch_`c' `X', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/tableG2.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR, Children, Cause `c': `e(cmdline)'") 
	local r append
}

local r append
***************
*By-Cause Adults
***************
*adjust hospital variables to be per-50-64 year old
replace H_bpc = H_bpc*(copop_ch/copop_ad)
replace H_hpc = H_hpc*(copop_ch/copop_ad)

local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt_ad]"	


forval c = 2/7{		
	xtreg amr_ad_`c' `X', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/tableG2.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR, Adults, Cause `c': `e(cmdline)'") 
	local r append
}
log close
exit

