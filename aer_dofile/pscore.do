use $data/aer_pscore_data, clear

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
estimates store ps1


*2. 
local all "NO POPULATION, LINEARLY, WEIGHTED, ALL CENTERS"
probit treat2 _* [pw=x_copop]
estimates store ps2

*give missing values the state mean in order to predict pscores...imputation not used in the estimation...this may be wrong, but its only for 36 small counties
foreach var of varlist _*{
	egen testo = mean(`var'),by(stfips)
	replace `var' = testo if `var'==.
	drop testo
}



/*************************
PREDICT PROPENSITY SCORES
**************************/
forval i = 1/2{
	estimates restore ps`i'
	predict index`i', xb
	gen pscore`i' = normal(index`i')
}
label var pscore1 early
label var pscore2 late

keep *fips treat? pscore? ind*


/*************************
CREATE DFL WEIGHTS
**************************/
sum treat1
local ED 					= r(mean)
gen dflwgt1 			= (pscore1/`ED')*((1-`ED')/(1-pscore1))*(1-treat1)	

sum treat2
local ED 					= r(mean)
gen dflwgt2 			= (pscore2/`ED')*((1-`ED')/(1-pscore2))*(1-treat2)	

sort stfips cofips
save "$output/pscore_temp", replace
