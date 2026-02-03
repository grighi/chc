/**************************************************************
Table D4: Marginal effects (mean derivatives) from p-score equation
**************************************************************/
clear
clear matrix
clear mata
set mat 1000
set more off, perm
pause on
capture log close
log using "$output/log_tableD4", replace text	

use "$data/aer_pscore_data", clear

*Drop NY/LA/Chicago
drop if stfips==36 & cofips==61
drop if stfips==6  & cofips==37
drop if stfips==17 & cofips==31	

drop if stfips==2|stfips==15

/************************************
 ESTIMATE PROPENSITY SCORES EQUATION
*************************************/
ren _copop x_copop
ren _copop2 x_copop2

*1. 
local early "NO POPULATION, LINEARLY, WEIGHTED, EARLY CENTERS"
probit treat1 _* [pw=x_copop]
margins, dydx(*) post
outreg2 using "$output/tableD4.xls", replace noparen noaster ctitle("Mean Derivatives")

