/******************************************************
TABLE H3: Heterogeneity in older adult DD Estimates, ALL CENTERS
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
log using "$output/log_tableH3", replace text	

use "$data/aer_data" if year<=1988, clear

*define above/below median 
gen treat2 = chc_year_exp<.
sum amr_eld if year==1960 & treat2, det
local m = r(p50)
egen hamr = total(treat2*(year==1960)*(amr_eld>`m')), by(fips)

gen pcmd = _tot_act_md/copop
sum pcmd if year==1960 & treat2, det
local m = r(p50)
egen hpcmd = total(treat2*(year==1960)*(pcmd>`m')), by(fips)

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

*adjust hospital variables to be per-50+ year old
replace H_bpc = H_bpc*(copop/copop_eld)
replace H_hpc = H_hpc*(copop/copop_eld)

*TREATMENT VARS
char did2[omit] -1
xi i.did2, pref(_DD)

*By Pre-Treatment Characteristics
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* Low_DD* High_DD* [aw=popwt_eld]"	
local r replace
foreach var of varlist hamr hpcmd{
		*split up treatment dummies
		cap drop High* Low*
		for var _DD*: gen HighX = X*`var'
		for var _DD*: gen LowX = X*(1-`var')
		recode High* Low* (.=-1)	

		*pre-treatment AMR means by group
		sum amr_eld if `var' & chc_year_exp<. & exp1==-1 [aw=copop_eld]
		local prehigh = r(mean)
		sum amr_eld if ~`var' & chc_year_exp<. & exp1==-1 [aw=copop_eld]
		local prelow = r(mean)
		
		*regression
		xtreg amr_eld `X', cluster(fips) fe
		test Low_DDdid2_2 == High_DDdid2_2
		test Low_DDdid2_4 == High_DDdid2_4, accumulate
		test Low_DDdid2_5 == High_DDdid2_5, accumulate
		local hp = r(p)
		outreg2 using "$output/tableH3.xls", `r' keep(Low_DDdid2_2 Low_DDdid2_4 Low_DDdid2_5 High_DDdid2_2 High_DDdid2_4 High_DDdid2_5) noparen noaster ctitle("`var': `e(cmdline)'") addstat(lowmean, `prelow', highmean, `prehigh', htest, `hp')
		local r append
}
		

		
*STACK BY RACE		
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

ren amr_w_eld _amr0
ren amr_nw_eld _amr1
ren popwt_w_eld _popwt0
ren popwt_nw_eld _popwt1
ren copop_w_eld _copop0
ren copop_nw_eld _copop1

drop amr* asmr* popwt* copop* dflpop*
ren	_amr0	amr0
ren	_amr1	amr1
ren	_popwt0	popwt0
ren	_popwt1	popwt1
ren	_copop0	copop0
ren	_copop1	copop1

*generate population restriction
egen nrp = total((copop0<100) | (copop1<100)), by(fips)
gen d100r = nrp==0
drop nrp

*stack by race
reshape long amr copop popwt, i(stfips cofips year) j(nonwhite)
egen rco = group(nonwhite fips)
xtset rco year

*TREATMENT VARS
char did2[omit] -1
xi i.did2, pref(_DD)

*interact everything with the nonwhite variable (but separate effects rather than interactions for treatment)
for var _DD*: gen NWX = X*nonwhite
for var _DD*: gen WX = X*(1 - nonwhite)
for var _I*: gen NWX = X*nonwhite
for var D* R* H*: gen NWX = X*nonwhite

*specification
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* NW* W_DD* [aw=popwt]"	

*pre-treatment AMR means by group
sum amr if chc_year_exp<. & exp1==-1 & ~nonwhite [aw=copop]
local prew = r(mean)
sum amr if chc_year_exp<. & exp1==-1 & nonwhite [aw=copop]
local prenw = r(mean)

xtreg amr `X' if d100r, cluster(fips) fe
test W_DDdid2_2 == NW_DDdid2_2
test W_DDdid2_4 == NW_DDdid2_4, accumulate
test W_DDdid2_5 == NW_DDdid2_5, accumulate
local rp = r(p)

outreg2 using "$output/tableH3.xls", `r' keep(W_DDdid2_2 W_DDdid2_4 W_DDdid2_5 NW_DDdid2_2 NW_DDdid2_4 NW_DDdid2_5) noparen noaster title(`e(depvar)') ctitle("`e(cmdline)'") addstat(whitemean, `prew', nonwhitemean, `prenw', rtest, `rp')






**dropping regions
use "$data/aer_data" if year<=1988, clear

*Drop NY/LA/Chicago
drop if stfips==36 & cofips==61
drop if stfips==6  & cofips==37
drop if stfips==17 & cofips==31	
recode stfips (9 23 25 33 44 50 34 36 42 = 1) (18 17 26 39 55 19 20 27 29 31 38 46 = 2) (10 11 12 13 24 37 45 51 54 1 21 28 47 5 22 40 48 = 3) (4 8 16 35 30 49 32 56 6 41 53 = 4), gen(region)				
				
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
char did2[omit] -1
xi i.did2, pref(_DD)


local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _DD* [aw=popwt_eld]"

local Reg1 = "Northeast"
local Reg2 = "Midwest"
local Reg3 = "South"
local Reg4 = "West"


forval rg = 1/4{

	sum amr_eld if chc_year_exp<. & exp1==-1 & region~=`rg' [aw=copop_eld]
	local pre = r(mean) 
	
	xtreg amr_eld `X' if region~=`rg', cluster(fips) fe
	
	*OUTREG RESULTS
	outreg2 using "$output/tableH3.xls", `r' keep(_DDdid2_2 _DDdid2_4 _DDdid2_5 ) noparen noaster ctitle("Dropping the `Reg`rg'': `e(cmdline)'") addstat(premean, `pre')
}


