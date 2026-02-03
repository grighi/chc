/***********************
Table 5: SHSUE DD Estimates for Utilization
***********************/

clear
clear matrix
set mem 400m
set more off, perm
pause on
capture log close

log using "$output/log_table5", replace text	
use "$data/aer_shsue" if agecat>=50, clear	

*Covariates
xi i.Dwhite i.agecat i.area i.famsize2
gen frac_pov = rfinc/pov*100
egen PF = cut(frac_pov), at(0,100,300,10000)

local X  "_I* D70 NHC D70xNHC"

local r replace
	foreach var of varlist YBP_rsrce YCP_tot_vis YBP_dphys YBP_pdrug_oop{
		*POOLED MODEL TO GET p-value on TEST of DD coefs across income groups
		cap drop POOL*
		cap drop PFpsu
		xi i.PF, pref(POOL)
		for var `X': gen POOL100X = POOLPF_100*X
		for var `X': gen POOL300X = POOLPF_300*X
		egen PFpsu = group(PF psu)
		areg `var' `X' POOL*  [aw=wgt], cluster(PFpsu) a(PFpsu)	
		test POOL300D70xNHC
		local p300 = r(p)

		*RUN THE SEPARATE MODELS TO GET COEFS
		foreach pov of numlist 0 100 300{
			local varlab: variable label `var'	
			xi, prefix(N): areg `var' `X' if PF==`pov' [aw=wgt], cluster(psu) a(psu)
			sum `var' [aw=wgt] if D70!=1 & NHC==1  & PF==`pov'
			outreg2 D70xNHC using "$output/table5.xls", `r' noparen noaster ctitle("POV`pov': `varlab'") addstat(MDV, r(mean), "A=C (p-val)", `p300')
			local r append
		}
	}

exit


