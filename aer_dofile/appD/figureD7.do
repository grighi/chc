/******************************************************
FIGURE D7: ES Results for Other Funding Amounts
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
log using "$output/log_figureD7", replace text	

*save a file to hold the coefficients for stata graphs
set obs 15
gen time = _n - 8
save "$output/chc_otherfundamounts_es", replace emptyok

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

*preferred spec
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* [aw=popwt]"

foreach prog in hs capadmin health legal sen chc{
	
	*Grant Dummy
	xtreg pcrfund_`prog' `X', cluster(fips) fe	

	preserve

	*STORE RESULTS IN A STATA FILE
	use "$output/chc_otherfundamounts_es", clear
	quietly{
		gen b_`prog'_dgrant							= .
		gen se_`prog'_dgrant 						= .	
		forval h = 1/15{
				if `h'==7{
					replace b_`prog'_dgrant			= 0 in `h'
				}
				else{
					replace b_`prog'_dgrant 		= _b[_Texp1_`h'] in `h'
					replace se_`prog'_dgrant		= _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/chc_otherfundamounts_es", replace

	restore
}



use "$output/chc_otherfundamounts_es", clear
#delimit ;
twoway (scatter 
	b_hs_dgrant b_capadmin_dgrant b_sen_dgrant 
	b_health_dgrant b_legal_dgrant b_chc_dgrant 
	time if time>=-6 & time<=6, 	/*just the coefficients, not the confidence intervals...scatter allows markers, and is actually equivalent to "line" with the connect option*/
	xline(-1, lcolor(black)) yline(0, lcolor(black)) 	/*axis lines (vertical line at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
	connect(l l l l l l l)										/*add lines (of course)*/
	msymbol(Oh T S Dh X i)										/*Model 1 has no marker, Model 2 has open circles, Model 3 has open diamonds, Model 4 has open triangles and Model 5 has solid squares*/
	mcolor(gray maroon forest_green dkorange red navy)
	msize(medium medium medium medium medium medium )								/*identically sized markers, so that the open ones can actually be seen*/
	lpattern(solid solid solid solid solid )
	lwidth(medium medium medium medium medium  thick)									/*line widths...make preferred specification really thick*/
	lcolor(gray maroon forest_green dkorange red navy)
	cmissing(n n n n n n)
	legend(order(1 2 3 4 5 6) rows(2) label(1 "Head Start") label(2 "CAP Admin.") label(3 "Elderly Prog.") label(4 "Other CAP Health") label(5 "Legal Services") label(6 "CHC") region(style(none)) size(medium))
	xtitle("Years Since CHC Establishment", size(medsmall))
	ytitle("Real Per-Capita Grant", margin(0 2 10 10) size(medsmall))
	xlabel(-6(2)6, labsize(small))
	ylabel(-5000(5000)15000, labsize(small))
	title("", size(medlarge) color(black))
	graphregion(fcolor(white) color(white) icolor(white) margin(small)))
	;
#delimit cr;


graph export "$output/figureD7.wmf", replace

erase "$output/chc_otherfundamounts_es.dta"



