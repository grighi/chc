/******************************************************
TABLE 2: DD Estimates by Specification
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
log using "$output/log_table2", replace text	

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
xi	i.year*i.Durb i.fips*year i.year*i.stfips			/*this makes county trends, state-by-year FE and urban-by-year FE*/
cap drop _Ifips*
cap drop _IDurb*
cap drop _Istfi*	

*TREATMENT VARS
char did1[omit] -1
xi i.did1, pref(_DD)
		
		
*PANEL A		
*define the specifications
local X1 "_Iyear* _IyeaXDu* _DD* [aw=popwt]"										
local X2 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt]"	
local X3 "_Iyear* _IyeaXDu* _IyeaXst* _IfipXy* R_* H_* _DD* [aw=popwt]"								
local X4 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=dflpopwgt1]"								

*for display in the log file
local DisX1 "FE + UxY FE"	
local DisX2 "FE + UxY FE + X + STxY FE"
local DisX3 "FE + UxY FE + X + STxY FE + County Trends"	
local DisX4 "FE + UxY FE + X + STxY FE, DFL Weights"

local r replace
foreach i of numlist 1 2 3 4{		
	di "amr: `DisX`i''"
	xtreg amr  `X`i'', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/table2.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR: `e(cmdline)'") 
	local r append
}


*PANEL B
*define the specifications
local X1 "_Iyear* _IyeaXDu* _DD* [aw=popwt_eld]"										
local X2 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt_eld]"	
local X3 "_Iyear* _IyeaXDu* _IyeaXst* _IfipXy* R_* H_* _DD* [aw=popwt_eld]"								
local X4 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=dflpopwgt1_eld]"								

*for display in the log file
local DisX1 "FE + UxY FE"	
local DisX2 "FE + UxY FE + X + STxY FE"
local DisX3 "FE + UxY FE + X + STxY FE + County Trends"	
local DisX4 "FE + UxY FE + X + STxY FE, DFL Weights"

*adjust hospital variables to be per-50+ year old
replace H_bpc = H_bpc*(copop/copop_eld)
replace H_hpc = H_hpc*(copop/copop_eld)

foreach i of numlist 1 2 3 4{		
	di "amr_eld: `DisX`i''"
	xtreg amr_eld `X`i'', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/table2.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR 50+: `e(cmdline)'") 
	local r append
}

log close
exit
