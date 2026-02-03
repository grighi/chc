/******************************************************
TABLE G1: DD Estimates by Specification and Age Group
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
log using "$output/log_tableG1", replace text	

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
local X1 "_Iyear* _IyeaXDu* _DD* [aw=bwt]"										
local X2 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=bwt]"	
local X3 "_Iyear* _IyeaXDu* _IyeaXst* _IfipXy* R_* H_* _DD* [aw=bwt]"								
local X4 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=dflpopwgt1_inf]"								

*for display in the log file
local DisX1 "FE + UxY FE"	
local DisX2 "FE + UxY FE + X + STxY FE"
local DisX3 "FE + UxY FE + X + STxY FE + County Trends"	
local DisX4 "FE + UxY FE + X + STxY FE, DFL Weights"

local r replace
foreach i of numlist 1 2 3 4{		
	di "amr: `DisX`i''"
	xtreg imr  `X`i'', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/tableG1.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("IMR: `e(cmdline)'") 
	local r append
}


*PANEL B
*define the specifications
local X1 "_Iyear* _IyeaXDu* _DD* [aw=popwt_ch]"										
local X2 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt_ch]"	
local X3 "_Iyear* _IyeaXDu* _IyeaXst* _IfipXy* R_* H_* _DD* [aw=popwt_ch]"								
local X4 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=dflpopwgt1_ch]"								

*for display in the log file
local DisX1 "FE + UxY FE"	
local DisX2 "FE + UxY FE + X + STxY FE"
local DisX3 "FE + UxY FE + X + STxY FE + County Trends"	
local DisX4 "FE + UxY FE + X + STxY FE, DFL Weights"

*adjust hospital variables to be per-50+ year old
replace H_bpc = H_bpc*(copop/copop_ch)
replace H_hpc = H_hpc*(copop/copop_ch)

foreach i of numlist 1 2 3 4{		
	di "amr_ch: `DisX`i''"
	xtreg amr_ch `X`i'', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/tableG1.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR 1-14: `e(cmdline)'") 
	local r append
}

*PANEL C
*define the specifications
local X1 "_Iyear* _IyeaXDu* _DD* [aw=popwt_ad]"										
local X2 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt_ad]"	
local X3 "_Iyear* _IyeaXDu* _IyeaXst* _IfipXy* R_* H_* _DD* [aw=popwt_ad]"								
local X4 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=dflpopwgt1_ad]"								

*for display in the log file
local DisX1 "FE + UxY FE"	
local DisX2 "FE + UxY FE + X + STxY FE"
local DisX3 "FE + UxY FE + X + STxY FE + County Trends"	
local DisX4 "FE + UxY FE + X + STxY FE, DFL Weights"

*adjust hospital variables to be per-50+ year old
replace H_bpc = H_bpc*(copop_ch/copop_ad)
replace H_hpc = H_hpc*(copop_ch/copop_ad)

foreach i of numlist 1 2 3 4{		
	di "amr_ad: `DisX`i''"
	xtreg amr_ad `X`i'', cluster(fips) fe
			
	*OUTREG RESULTS
	outreg2 using "$output/tableG1.xls", `r' keep(_DDdid1_2 _DDdid1_4 _DDdid1_5 _DDdid1_6) noparen noaster ctitle("AMR 15-49: `e(cmdline)'") 
	local r append
}


*PANEL D
*this file does not run the regressions for panel D because it is the same as panel B of table 2.  

log close
exit
