/*****************************************
Table F3: Knowledge of NHC by Race and Age
*****************************************/
clear
clear matrix
set mem 400m
set more off, perm
pause on
capture log close

log using "$output/log_tableF3", replace text	

use "$data/aer_nhc.dta", clear
*chc knowledge
tab hh_knew_comphc int_race_r if hh_knew_comphc>-99, col nofre
gen white = int_race_r==1
tab hh_knew_comphc white if hh_knew_comphc>-99, col nofre
egen agecat = cut(age), at(0,1,15,50,100)
gen knew = hh_knew_comphc==1 if hh_knew_comphc>-99 & hh_knew_comphc<.
tab agecat white, sum(knew) means
egen agecat2 = cut(age), at(0(5)70,80,100)

foreach a of numlist 0 1 15 50{
	ttest knew if agecat==`a', by(white)
	local r`a' = floor(r(p)*100)/100
}

drop if agecat==.
collapse (mean) knew, by(agecat white)
reshape wide knew, i(agecat) j(white)
gen p = `r0' in 1
replace p = `r1' in 2
replace p = `r15' in 3
replace p = `r50' in 4
format p %12.2f
export excel "$output/tableF3.xls", replace firstr(var)


	

