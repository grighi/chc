/******************************************************
FIGURE D5: ES Results for Transfer Payments
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
log using "$output/log_figureD5", replace text	

*save a file to hold the coefficients for stata graphs
set obs 15
gen time = _n - 8
save "$output/chc_reis_es", replace emptyok

use "$data/aer_data.dta" if year<=1980, clear

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

*make year dummies by urban category
xi i.year*i.Durb i.year*i.stfips 						/*this makes state-by-year FE and urban-by-year FE*/
cap drop _IDurb*
cap drop _Istfi*

*treatment vars
recode exp1 (7/100=7)
char exp1[omit] -1
xi i.exp1, pref(_T)


*REIS Public Assistance
	local prog pa
	gen lnpa = ln(R_tranpcpa1)
	*PC PA (take out REIS covariates, obviously)
	xtreg lnpa _Iyear* _IyeaXDu* _IyeaXst* D_* H_* _Texp* [aw=popwt], cluster(fips) fe			/*RUN IT ON THE ESTIMATION SAMPLE FROM THE PAPER*/

	preserve

	*STORE RESULTS IN A STATA FILE
	use "$output/chc_reis_es", clear
	quietly{
		gen b_`prog'_lnpc						= .
		gen se_`prog'_lnpc 						= .	
		forval h = 1/15{
				if `h'==7{
					replace b_`prog'_lnpc		= 0 in `h'
				}
				else{
					replace b_`prog'_lnpc		 		= _b[_Texp1_`h'] in `h'
					replace se_`prog'_lnpc 			= _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/chc_reis_es", replace

	restore
	
	
*REIS Retirement Payments
	local prog ret
	gen lnret = ln(R_tranpcret)	
	*PC PA (take out REIS covariates, obviously)
	xtreg lnret _Iyear* _IyeaXDu* _IyeaXst* D_* H_* _Texp* [aw=popwt], cluster(fips) fe			/*RUN IT ON THE ESTIMATION SAMPLE FROM THE PAPER*/

	preserve

	*STORE RESULTS IN A STATA FILE
	use "$output/chc_reis_es", clear
	quietly{
		gen b_`prog'_lnpc						= .
		gen se_`prog'_lnpc 						= .	
		forval h = 1/15{
				if `h'==7{
					replace b_`prog'_lnpc		= 0 in `h'
				}
				else{
					replace b_`prog'_lnpc		 		= _b[_Texp1_`h'] in `h'
					replace se_`prog'_lnpc 			= _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/chc_reis_es", replace

	restore


use "$output/chc_reis_es", clear
graph set window fontface "Times New Roman"			/*make everything always show up as Times New Roman*/

gen ubpa = b_pa_lnpc + 1.96*se_pa_lnpc 
gen lbpa = b_pa_lnpc - 1.96*se_pa_lnpc 

gen ubret = b_ret_lnpc + 1.96*se_ret_lnpc 
gen lbret = b_ret_lnpc - 1.96*se_ret_lnpc 


#delimit ;
scatter 
b_pa_lnpc b_ret_lnpc ubpa lbpa ubret lbret
time if time>=-6 & time<=6, 	/*just the coefficients, not the confidence intervals...scatter allows markers, and is actually equivalent to "line" with the connect option*/
xline(-1, lcolor(black)) yline(0, lcolor(black)) 	/*axis lines (vertical line at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
connect(l l l l l l l)										/*add lines (of course)*/
msymbol(Oh T i i i i )										/*Model 1 has no marker, Model 2 has open circles, Model 3 has open diamonds, Model 4 has open triangles and Model 5 has solid squares*/
mcolor(navy maroon navy navy maroon maroon)
msize(medlarge medlarge)								/*identically sized markers, so that the open ones can actually be seen*/
lpattern(solid solid dash dash dot dot)
lwidth(medthick medthick medium medium medium medium)									/*line widths...make preferred specification really thick*/
lcolor(navy maroon navy navy maroon maroon)
cmissing(n n n n n n)
legend(order(1 2) rows(1) label(1 "Public Assistance") label(2 "Retirement and Disability Assistance") region(style(none)) size(medsmall))
xtitle("Years Since CHC Establishment", size(medsmall))
ytitle("log Payments per Capita", size(medsmall) margin(0 2 10 10))
xlabel(-6(2)6, labsize(small))
title("", size(medium) color(black))
graphregion(fcolor(white) color(white) icolor(white) margin(none)) ;
#delimit cr;

graph export "$output/figureD5.wmf", replace

erase "$output/chc_reis_es.dta"

