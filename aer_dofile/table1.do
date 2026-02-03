/******************************************************
TABLE 1: Mean 1960 Characteristics 
******************************************************/
clear
clear matrix
clear mata
set mat 1000
set mem 5000m
set maxvar 10000
set matsize 10000
set more off, perm
pause on
capture log close
log using "$output/table1", replace text	

use "$data/aer_data.dta", clear
egen amr65 = total(amr*(year==1965)),by(fips)
cap drop _59medfaminc 
gen dms = _tot_med_stud>0
drop _tot_med_stud
recode stfips (9 23 25 33 44 50 34 36 42 = 1) ///
			  (18 17 26 39 55 19 20 27 29 31 38 46 = 2) ///
			  (10 11 12 13 24 37 45 51 54 1 21 28 47 5 22 40 48 = 4) ///
			  (4 8 16 35 30 49 32 56 6 41 53 = 5), gen(region)
xi i.region, noomit
drop if fips==6037|fips==17031|fips==36061|fips==3011
xi i.exp1, pref(_T)
qui xtreg amr D* R* H* _Texp* [aw=popwt], fe 		/*to get paper estimation sample*/
keep if e(sample)
keep if year==1960
keep *fips* chc_year_exp _* amr dms copop year region
drop _60medschlmt24
replace _tot_act_md = _tot_act_md/copop*1000
drop _Texp*

merge 1:1 stfips cofips using "$output/pscore_temp"
keep if _merge==3
drop _merge

*rescale DFL weights
egen totdfl = total(dflwgt1), by(treat1)
replace dflwgt1 = dflwgt1/totdfl
count if treat1
replace dflwgt1 = 1/r(N) if treat1

*RESCALING HAS A HUGE EFFECT ON THE T-STAT!  IN MY ORIGINAL CODE I DIDN'T DO A GOOD JOB RESCALING THINGS
	
	
*by timing
preserve
	egen chc = cut(chc_year_exp), at(1965, 1968, 1971, 1975, 1981)
	keep if chc<.
	collapse (mean) _* copop amr dms (count) fips, by(chc)
	xpose, varname clear
	ren v1 _65_67
	ren v2 _68_70
	ren v3 _71_74
	ren v4 _75_80
	save "$output/table1", replace
restore

*by treatment
preserve
	gen chc = chc_year_exp<=1974
	collapse (mean) _* copop amr dms (count) fips, by(chc)
	xpose, varname clear
	ren v1 _other
	ren v2 _pre74
	merge 1:1 _varname using "$output/table1"
	drop _merge
	save "$output/table1", replace
restore

preserve
	gen chc = chc_year_exp<=1974
	gen p = .
	gen varname = ""
	local i = 2
	foreach var of varlist _* copop dms amr{
		reg `var' chc
		testparm chc
		replace p = r(p) in `i'
		replace varname = "`var'" in `i'
		local i = `i'+1
	}
	ren varname _varname
	keep if p<.
	keep p _varname
	merge 1:1 _varname using "$output/table1"
	drop _merge
	save "$output/table1", replace
restore


*reweighted means
preserve
	gen chc = chc_year_exp<=1974
	xi i.region, noomit

	collapse (mean) _* dms amr copop (count) fips [aw=dflwgt1], by(chc)
	drop if chc

	xpose, clear varname
	ren v1 dfl_
	merge 1:1 _varname using "$output/table1"
	drop _merge
	save "$output/table1", replace
restore 

*do the DFL-weighted t-tests
preserve
	*resample pscores 1000 times and calculate a t-stat for EACH covariate on EACH rep
	cap gen chc = chc_year_exp<=1974
	xi i.region, noomit
	gen _varname = ""
	gen pdfl = .

	set seed 12345
	forval i = 1/1000{
		cap drop b_*
		*draw new error terms and apply them to the index and feed through normal CDF
		gen b_pscore = normal(index1 + rnormal())
		*gen dfl weights
		sum chc
		local ED 					= r(mean)
		gen b_dflwgt				= (b_pscore/`ED')*((1-`ED')/(1-b_pscore))*(1-chc)			/*see DiNardo (2002) about only applying these weights to the control group for TOT*/
		egen b_totdfl 				= total(b_dflwgt), by(chc)
		replace b_dflwgt			= b_dflwgt/b_totdfl
		count if chc
		replace b_dflwgt 			= 1/r(N) if chc
		*do 1000 t-test of differences in dfl weighted means to get a bootstrap t-distribution
		foreach var of varlist copop _60pcturban _60pctrurf _I* _60pct04years _60pctmt64years _60pctnonwhit _60pctlt4schl	_60pctmt12schl _pct59inclt3k _pct59incmt10k _tot_act_md dms amr{
			reg `var' chc [aw=b_dflwgt]
			testparm chc
			cap gen pct_`var' = sqrt(r(F)) in `i'
			replace pct_`var' = sqrt(r(F)) in `i'
		}
	}
	gen ind = _n
	sum pct*
	keep ind pct*
	keep if pct_copop<.
	save "$data/pct_t_bootstrap", replace
restore



*do t-test and compare to the bootstrap t-distributions (not tabulated t-dist)
gen chc = chc_year_exp<=1974
xi i.region, noomit

gen _varname = ""
gen pdfl = .
gen ind = .
local i = 2
foreach var of varlist copop _60pcturban _60pctrurf _Iregion_1 _Iregion_2 _Iregion_4 _Iregion_5 _60pct04years _60pctmt64years _60pctnonwhit _60pctlt4schl	_60pctmt12schl _pct59inclt3k _pct59incmt10k _tot_act_md dms amr{
	reg `var' chc [aw=dflwgt1]
	testparm chc
	local t = sqrt(r(F))
	preserve
		use "$data/pct_t_bootstrap", replace
		count if pct_`var'>=abs(`t')
		local p = r(N)/1000
	restore
	replace pdfl = `p' in `i'
	replace _varname = "`var'" in `i'
	replace ind = `i' if _varname=="`var'"
	local i = `i'+1
}
keep if pdfl<.
keep pdfl _varname ind
merge 1:1 _varname using "$output/table1"
drop _merge
order _varname _65 _68 _71 _75 _pre _other p dfl_ pdfl
replace ind = 1 if _varname=="fips"
replace ind = 0 if _varname=="chc"
sort ind
save "$output/table1", replace

export excel using "$output/table1.xls", replace firstr(var)

erase "$output/table1.dta"
erase "$data/pct_t_bootstrap.dta"

log close
