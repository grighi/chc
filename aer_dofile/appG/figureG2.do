/******************************************************
FIGURE G2: Event-Study Results for AMR by Urbanicity in 1960, WLS/OLS, Early/All Centers
******************************************************/
clear
clear matrix
set mat 1000
clear mata
set maxvar 10000
set matsize 10000
set more off, perm
pause on
capture log close
log using "$output/log_figureG2", replace text	

*save a file to hold the coefficients for stata graphs
set obs 23
gen time = _n - 8
save "$output/amr_chc_es_urban_results_wls_ols", replace emptyok


use "$data/aer_data" if year<=1988, clear
*Drop NY/LA/Chicago
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

*define urban (split treatment group in half)
egen u60 = total(D_60pcturban_t*(year==1960)), by(fips)
sum u60 if chc_year_exp<=1974, det
gen hu = u60>r(p50)

*urban shares in high/low groups noted on fig
	sum u60 if chc_year_exp<=1974 & hu [aw=copop]
	local hshare : display %02.0f round(r(mean))
	sum u60 if chc_year_exp<=1974 & ~hu [aw=copop]
	local lshare : display %02.0f round(r(mean)*100)


***************
*EARLY CENTERS
***************
*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* HU_Texp* [aw=popwt]"	

*treatment variables (separate event-time dummies for high- and low-urban counties)
char exp1[omit] -1
xi i.exp1, pref(_T)
for var _Texp1*: gen HUX = hu*X		
for var _Texp1*: replace X = (1-hu)*X
xtreg amr `X' if year<=1988, cluster(fips) fe
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_urban_results_wls_ols", clear
	quietly{
		gen b_w_high				= .
		gen se_w_high				= .	
		gen b_w_low					= .
		gen se_w_low				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_w_low = 0 in `h'
					replace b_w_high = 0 in `h'
				}
				else{
					replace b_w_low = _b[_Texp1_`h'] in `h'
					replace se_w_low = _se[_Texp1_`h'] in `h'
					replace b_w_high = _b[HU_Texp1_`h'] in `h'
					replace se_w_high = _se[HU_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_es_urban_results_wls_ols", replace
restore

*unweighted spec
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* HU_Texp*"	
xtreg amr `X' if year<=1988, cluster(fips) fe
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_urban_results_wls_ols", clear
	quietly{
		gen b_uw_high				= .
		gen se_uw_high				= .	
		gen b_uw_low					= .
		gen se_uw_low				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_uw_low = 0 in `h'
					replace b_uw_high = 0 in `h'
				}
				else{
					replace b_uw_low = _b[_Texp1_`h'] in `h'
					replace se_uw_low = _se[_Texp1_`h'] in `h'
					replace b_uw_high = _b[HU_Texp1_`h'] in `h'
					replace se_uw_high = _se[HU_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_es_urban_results_wls_ols", replace
restore





***************
*LATER CENTERS
***************
*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* HU_Texp* [aw=popwt]"	

*treatment variables (separate event-time dummies for high- and low-urban counties)
cap drop HU*
cap drop *exp1*
char exp2[omit] -1
xi i.exp2, pref(_T)
for var _Texp2*: gen HUX = hu*X		
for var _Texp2*: replace X = (1-hu)*X
xtreg amr `X' if year<=1988, cluster(fips) fe
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_urban_results_wls_ols", clear
	quietly{
		gen b_w_high_all				= .
		gen se_w_high_all				= .	
		gen b_w_low_all					= .
		gen se_w_low_all				= .	
		forval h = 1/17{
				if `h'==7{
					replace b_w_low_all = 0 in `h'
					replace b_w_high_all = 0 in `h'
				}
				else{
					replace b_w_low_all = _b[_Texp2_`h'] in `h'
					replace se_w_low_all = _se[_Texp2_`h'] in `h'
					replace b_w_high_all = _b[HU_Texp2_`h'] in `h'
					replace se_w_high_all = _se[HU_Texp2_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_es_urban_results_wls_ols", replace
restore

*unweighted spec
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* HU_Texp*"	
xtreg amr `X' if year<=1988, cluster(fips) fe
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_urban_results_wls_ols", clear
	quietly{
		gen b_uw_high_all				= .
		gen se_uw_high_all				= .	
		gen b_uw_low_all					= .
		gen se_uw_low_all				= .	
		forval h = 1/17{
				if `h'==7{
					replace b_uw_low_all = 0 in `h'
					replace b_uw_high_all = 0 in `h'
				}
				else{
					replace b_uw_low_all = _b[_Texp2_`h'] in `h'
					replace se_uw_low_all = _se[_Texp2_`h'] in `h'
					replace b_uw_high_all = _b[HU_Texp2_`h'] in `h'
					replace se_uw_high_all = _se[HU_Texp2_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_es_urban_results_wls_ols", replace
restore

use "$output/amr_chc_es_urban_results_wls_ols", clear 
		#delimit ;					
		cap drop ub* lb*;
		gen ub 			= b_w_high + 1.96*se_w_high;
		gen lb 			= b_w_high - 1.96*se_w_high;		
		
		scatter b_w_high b_uw_high b_w_high_all b_uw_high_all ub lb
				time if time>=-6 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
				xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
				yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
				connect(l l l l l l l l l)								/*LINES: add lines (of course)*/
				cmissing(n n n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
				msymbol(X i Th O i i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
				msize(large . large large large  . . . .)						/*MARKER SIZE: only matters when a marker is specified*/
				mcolor(navy maroon forest_green dkorange navy navy blue blue)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
				lpattern( solid solid solid solid  dash dash dot dot)					/*LINE PATTERN: estimates solid, CI dashed*/
				lwidth(thick thick thick thick medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
				lcolor(navy maroon forest_green dkorange  navy navy blue blue)						/*LINE COLOR: all-cause results are always navy*/ 		
				legend(order(- "Early Centers: " 1 2 - "All Centers: " 3 4) 	rows(1) 	label(1 "WLS") 	label(2 "OLS") label(3 "WLS") label(4 "OLS")
					region(style(none))  size(medium))				
				xlabel(-6(3)14, labsize(medium))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
				ylabel(,  labsize(medium))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
				xtitle("Years Since CHC Establishment", size(medlarge))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
				ytitle("Deaths per 100,000 Residents", size(medlarge))
				title("{it: A. Urban Counties}", size(large) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
				graphregion(fcolor(white) color(white) icolor(white)) saving("$output/panela.gph", replace); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
				#delimit cr;

		#delimit ;					
		cap drop ub* lb*;
		gen ub 			= b_w_low + 1.96*se_w_low;
		gen lb 			= b_w_low - 1.96*se_w_low;		
		
		scatter b_w_low b_uw_low b_w_low_all b_uw_low_all ub lb
				time if time>=-6 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
				xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
				yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
				connect(l l l l l l l l l)								/*LINES: add lines (of course)*/
				cmissing(n n n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
				msymbol(X i Th O i i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
				msize(large . large large large  . . . .)						/*MARKER SIZE: only matters when a marker is specified*/
				mcolor(navy maroon forest_green dkorange navy navy blue blue)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
				lpattern( solid solid solid solid dash dash dot dot)					/*LINE PATTERN: estimates solid, CI dashed*/
				lwidth(thick thick thick thick medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
				lcolor(navy maroon forest_green dkorange  navy navy blue blue)						/*LINE COLOR: all-cause results are always navy*/ 		
				legend(order(- "Early Centers: " 1 2 - "All Centers: " 3 4) 	rows(1) 	label(1 "WLS") 	label(2 "OLS") label(3 "WLS") label(4 "OLS")
					region(style(none))  size(medium))				
				xlabel(-6(3)14, labsize(medium))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
				ylabel(,  labsize(medium))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
				xtitle("Years Since CHC Establishment", size(medlarge))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
				ytitle("", size(medlarge))
				title("{it: B. Non-Urban Counties}", size(large) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
				graphregion(fcolor(white) color(white) icolor(white)) saving("$output/panelb.gph", replace); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
				#delimit cr;

grc1leg "$output/panela.gph" "$output/panelb.gph", col(2) legendfrom("$output/panela.gph") imargin(tiny) xsize(8.5) ysize(5.5) graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) 	
graph display, xsize(7.5) ysize(3.5)
graph export "$output/figureG2.wmf", replace

erase "$output/amr_chc_es_urban_results_wls_ols.dta"
log close
exit



