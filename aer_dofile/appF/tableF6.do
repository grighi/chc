/***********************
Table F6: Clinic as a Regular Source of care
***********************/
clear
clear matrix
set mem 400m
set more off, perm
pause on
capture log close

log using "$output/log_tableF6", replace text	
use "$data/aer_shsue", clear	

*Covariates
xi i.Dwhite i.agecat i.area i.famsize2
gen frac_pov = rfinc/pov*100
egen PF = cut(frac_pov), at(0,100,300,10000)

local var "YBP_rsrce_clin"
local r replace
	*Pooled Estimate
		xi, prefix(N): areg `var' _I* D70 NHC D70xNHC [aw=wgt], cluster(psu) a(psu)							
		sum `var' [aw=wgt] if D70!=1 & NHC==1 
		outreg2 D70xNHC using "$output/tableF6.xls", `r' noparen noaster ctitle("ALL: `varlab'") addstat(MDV, r(mean))		
		local r append
		
	*Urban Estimate
		xi, prefix(N): areg `var' _I* D70 NHC D70xNHC if area<=2 [aw=wgt], cluster(psu) a(psu)		
		sum `var' [aw=wgt] if D70!=1 & NHC==1 & area<=2
		outreg2 D70xNHC using "$output/tableF6.xls", `r' noparen noaster ctitle("URBAN: `varlab'") addstat(MDV, r(mean))				

	*Rural Estimate
		xi, prefix(N): areg `var' _I* D70 NHC D70xNHC if area>2 [aw=wgt], cluster(psu) a(psu)	
		sum `var' [aw=wgt] if D70!=1 & NHC==1 & area<=2
		outreg2 D70xNHC using "$output/tableF6.xls", `r' noparen noaster ctitle("RURAL: `varlab'") addstat(MDV, r(mean))						
	
	*POOLED MODEL by poverty group
	cap drop POOL*
	cap drop PFpsu
	xi i.PF, pref(POOL)
	for var _I* D70 NHC D70xNHC: gen POOL100X = POOLPF_100*X
	for var _I* D70 NHC D70xNHC: gen POOL300X = POOLPF_300*X
	egen PFpsu = group(PF psu)
	areg `var' _I*  D70 NHC D70xNHC POOL*  [aw=wgt], cluster(PFpsu) a(PFpsu)	
	test POOL300D70xNHC
	local p300 = r(p)
	test POOL100D70xNHC
	local p100 = r(p)
	outreg2 *D70xNHC* using "$output/tableF6.xls", `r' noparen noaster ctitle("BY POV: `varlab'") addstat("A=B (p-val)", `p100', "A=C (p-val)", `p300')		
exit




		*****RUN THE SEPARATE MODELS TO GET COEFS
		foreach pov of numlist 0 100 300{
			local varlab: variable label `var'												/*put variable label for DV in a local to add to output dataset*/	
			xi, prefix(N): areg `var' _I* D70 NHC D70xNHC if PF==`pov' [aw=wgt], cluster(psu) a(psu)							/*LPMs*/
			sum `var' [aw=wgt] if D70!=1 & NHC==1  & PF==`pov'
			outreg2 D70xNHC using "$output/tableF6.xls", `r' noparen noaster ctitle("POV`pov': `varlab'") addstat(MDV, r(mean), p100, `p100', p300, `p300', obs , `obs', r2, `r2')
			local r append
		}
