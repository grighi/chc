/******************************************************
Table G3: Estimates for Cumulative Grant Funds
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
log using "$output/log_tableG3", replace text	

use "$data/aer_data.dta" if year<=1988, clear

*Drop NY/LA/Chicago instead of the pscore restrictions
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

*treatment vars
recode exp1 (7/100=7)
char exp1[omit] -1
xi i.exp1, pref(_T)	

gen fund_chc = copop/1000*pcrfund_chc
recode fund_chc (.=0)
sort fips year
bys fips: gen cfund_chc = sum(fund_chc)
replace cfund_chc = cfund_chc/1000000

*PANEL A		
*define the specifications
local X1 "_Iyear* _IyeaXDu* cfund_chc [aw=popwt]"										
local X2 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* cfund_chc [aw=popwt]"	
local X3 "_Iyear* _IyeaXDu* _IyeaXst* _IfipXy* R_* H_* cfund_chc [aw=popwt]"								
local X4 "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* cfund_chc [aw=dflpopwgt1]"								

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
	outreg2 using "$output/tableG3.xls", `r' keep(cfund_chc) noparen noaster ctitle("AMR: `e(cmdline)'") 
	local r append
}

log close
exit
