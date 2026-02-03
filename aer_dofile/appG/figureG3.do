/******************************************************
FIGURE G3: Event-Study Results for IMR by Race
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
log using "$output/log_figureG3", replace text	

*save a file to hold the coefficients for stata graphs
set obs 23
gen time = _n - 8
save "$output/imr_byrace_chc_es_results", replace emptyok

use "$data/aer_data", clear
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

char exp1[omit] -1
xi i.exp1, pref(_T)


*************************
*White IMR
*************************
xtreg imr_w _Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* [aw=bwt_w] if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/imr_byrace_chc_es_results", clear
	quietly{
		gen b_X_w				= .
		gen se_X_w				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_w = 0 in `h'
				}
				else{
					replace b_X_w = _b[_Texp1_`h'] in `h'
					replace se_X_w = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/imr_byrace_chc_es_results", replace
restore


*************************
*White IMR
*************************
char exp1[omit] -1
xi i.exp1, pref(_T)
xtreg imr_nw _Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* [aw=bwt_nw] if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/imr_byrace_chc_es_results", clear
	quietly{
		gen b_X_nw				= .
		gen se_X_nw				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_nw = 0 in `h'
				}
				else{
					replace b_X_nw = _b[_Texp1_`h'] in `h'
					replace se_X_nw = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/imr_byrace_chc_es_results", replace
restore


use "$output/imr_byrace_chc_es_results", clear

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
		

#delimit ;				
cap drop ub* lb*;
gen ubw 		= b_X_w + 1.96*se_X_w ;
gen lbw 		= b_X_w - 1.96*se_X_w ;		
gen ubnw 		= b_X_nw + 1.96*se_X_nw ;
gen lbnw 		= b_X_nw - 1.96*se_X_nw ;		
		
scatter b_X_w b_X_nw ubw lbw ubnw lbnw
		time if time>=-6 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(Th O i i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
		msize(medium medium . . )						/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(maroon navy maroon maroon navy navy)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern( solid solid dot dot dash dash)					/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth( medthick medthick medium medium medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
		lcolor( maroon navy maroon maroon navy navy)						/*LINE COLOR: all-cause results are always navy*/ 		
		legend(order(1 2) rows(1) label(1 "White") label(2 "Nonwhite"))				
		xlabel(-6(3)12 14, labsize(small))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(,  labsize(small))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
		xtitle("Years Since CHC Establishment", size(medium))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
		ytitle("Deaths per 1,000 Live Births", size(medium))
		title("", size(medium) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
		graphregion(fcolor(white) color(white) icolor(white)) ; /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		#delimit cr;

		graph export "$output/figureG3.wmf", replace

		
		
erase "$output/imr_byrace_chc_es_results.dta"
log close
exit


