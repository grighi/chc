/******************************************************
TABLE D1: Predicting the timing of CHC grant 
******************************************************/
clear
clear matrix
clear mata
set mat 1000
set more off, perm
pause on
capture log close
log using "$output/log_tableD1", replace text	


use "$data/aer_data" if chc_year_exp<=1974, clear
*Drop NY/LA/Chicago
drop if stfips==36 & cofips==61
drop if stfips==6  & cofips==37
drop if stfips==17 & cofips==31	

egen amr65 = total(amr*(year==1965)),by(fips)
gen damr = amr65-amr

keep if year==1960

replace _tot_act_md = _tot_act_md/1000

*1. WEIGHTED - ALL
reg chc_year_exp	_60pcturban _60pctrurf _60pct04years _60pctmt64years _60pctnonwhit _60pctmt12schl _60pctlt4schl _pct59inclt3k _pct59incmt10k _tot_act_md  amr damr [aw = copop], robust

*F-test, weighted model, all non-urban  variables
test _60pctrurf _60pct04years _60pctmt64years _60pctnonwhit _60pctmt12schl _60pctlt4schl _pct59inclt3k _pct59incmt10k _tot_act_md  amr damr
local p1 = r(p)
*F-test, weighted model, all non-urban, non-MD variables
test _60pctrurf _60pct04years _60pctmt64years _60pctnonwhit _60pctmt12schl _60pctlt4schl _pct59inclt3k _pct59incmt10k amr damr
local p2 = r(p)
outreg2 using "$output/tableD1.xls", replace noparen noaster adds(Test1, `p1', Test2, `p2') title(`e(depvar)') ctitle("Weighted LPM")


*1. WEIGHTED - ALL
reg chc_year_exp	_60pcturban _60pctrurf _60pct04years _60pctmt64years _60pctnonwhit _60pctmt12schl _60pctlt4schl _pct59inclt3k _pct59incmt10k _tot_act_md  amr damr , robust

*F-test, weighted model, all non-urban  variables
test _60pctrurf _60pct04years _60pctmt64years _60pctnonwhit _60pctmt12schl _60pctlt4schl _pct59inclt3k _pct59incmt10k _tot_act_md  amr damr
local p1 = r(p)
*F-test, weighted model, all non-urban, non-MD variables
test _60pctrurf _60pct04years _60pctmt64years _60pctnonwhit _60pctmt12schl _60pctlt4schl _pct59inclt3k _pct59incmt10k amr damr
local p2 = r(p)
outreg2 using "$output/tableD1.xls", append noparen noaster adds(Test1, `p1', Test2, `p2') title(`e(depvar)') ctitle("Unweighted LPM")


