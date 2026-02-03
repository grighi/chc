/****************************************************************
Table D2: Probability of Migration to House or State, 1965-1970
****************************************************************/
clear
clear matrix
clear mata
set mat 1000
set more off, perm
pause on
capture log close
log using "$output/log_tableD2", replace text	

use "$data/aer_cen70", clear
xi i.sex i.agecat i.race i.edcat i.nch i.dpov
local X "_I* ch5 dmil dcoll"

local r replace
foreach dv in DHMIG DSMIG{
		*no covariates overall
		reg `dv' treat  [aw=hhwt], cluster(fips)
		outreg2 using "$output/tableD2.xls", `r' keep(*tre*) noparen noaster title(`e(depvar)') ctitle("`e(cmdline)'")
		/*!*/		local r append														

		*covariates overall
		reg `dv' `X' treat  [aw=hhwt], cluster(fips)
		outreg2 *tre* using "$output/tableD2.xls", `r' keep(*tre*) noparen noaster title(`e(depvar)') ctitle("`e(cmdline)'")
}


