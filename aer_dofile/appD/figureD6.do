/******************************************************
FIGURE D6: Event-Study Results for Populations
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
log using "$output/log_figureD6", replace text	

*save a file to hold the coefficients for stata graphs
set obs 23
gen time = _n - 8
save "$output/pop_chc_es_results", replace emptyok

*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp*"	

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

*************************
*Older Adults: 50+
*************************
char exp1[omit] -1
xi i.exp1, pref(_T)

xtreg copop_eld `X' if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/pop_chc_es_results", clear
	quietly{
		gen b_X_eld				= .
		gen se_X_eld				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_eld = 0 in `h'
				}
				else{
					replace b_X_eld = _b[_Texp1_`h'] in `h'
					replace se_X_eld = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/pop_chc_es_results", replace
restore



*************************
*Older Adults: 50-64
*************************
char exp1[omit] -1
xi i.exp1, pref(_T)

xtreg copop_5064 `X' if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/pop_chc_es_results", clear
	quietly{
		gen b_X_5064				= .
		gen se_X_5064				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_5064 = 0 in `h'
				}
				else{
					replace b_X_5064 = _b[_Texp1_`h'] in `h'
					replace se_X_5064 = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/pop_chc_es_results", replace
restore



*************************
*Elderly: 65+
*************************
char exp1[omit] -1
xi i.exp1, pref(_T)

xtreg copop_6500 `X' if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/pop_chc_es_results", clear
	quietly{
		gen b_X_6500				= .
		gen se_X_6500				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_6500 = 0 in `h'
				}
				else{
					replace b_X_6500 = _b[_Texp1_`h'] in `h'
					replace se_X_6500 = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/pop_chc_es_results", replace
restore



use "$output/pop_chc_es_results", clear
graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
		
#delimit ;					
for var *_X_*:  replace X = . if time>14;

cap drop ub* lb*;
gen ub 			= b_X_eld + 1.96*se_X_eld ;
gen lb 			= b_X_eld - 1.96*se_X_eld ;		

twoway (scatter b_X_eld b_X_5064 b_X_6500 ub lb
		time if time>=-6 & time<=14,							/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 								/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so all "pre" periods have NO CHC*/
		yline(0, lcolor(black)) 								/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(i O Th i i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification be a solid line*/
		msize(. medium medium medium . . . .)			/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(navy forest_green dkorange navy navy blue blue)	/*MARKER COLOR: */
		lpattern( solid solid solid dash dash dot dot)			/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth(vthick medthick medthick medthick medium medium medium medium)			/*LINE WIDTHS: make preferred specification thick*/
		lcolor(navy forest_green dkorange  navy navy blue blue)	/*LINE COLOR: main results are always navy*/ 		
		legend(order(1 2 3) row(1) label(1 "50+") label(2 "50-54") label(3 "65+"))	
		xlabel(-6(3)12 14, labsize(medium))  						/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(,  labsize(medium))								/*YLABEL*/
		xtitle("Years Since CHC Establishment", size(medium))	/*XTITLE*/
		ytitle("County Population" " ", size(medium))
		title("", size(medium) color(black))	
		graphregion(fcolor(white) color(white) icolor(white) margin(medlarge)) 
		plotregion(margin(vsmall)))
		(pcarrowi -2500 2 -2500 -.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(-2500 7 "Year Before CHCs Began Operating", j(left)))
		; /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		#delimit cr;
		
graph export "$output/figureD6.wmf", replace

erase "$output/pop_chc_es_results.dta"
log close
exit
