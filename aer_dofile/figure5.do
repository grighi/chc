/******************************************************
FIGURE 5: Main Event-Study Results for AMR
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
log using "$output/log_figure5", replace text	

*save a file to hold the coefficients for stata graphs
set obs 33
gen time = _n - 8
save "$output/amr_chc_es_results", replace emptyok

*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* [aw=popwt]"	

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

*************************
*Early CHCs, All Counties
*************************
char exp1[omit] -1
xi i.exp1, pref(_T)
xtreg amr `X' if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_results", clear
	quietly{
		gen b_X_early_all				= .
		gen se_X_early_all				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_early_all = 0 in `h'
				}
				else{
					replace b_X_early_all = _b[_Texp1_`h'] in `h'
					replace se_X_early_all = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_es_results", replace
restore

*************************
*All CHCs, All Counties
*************************
char exp2[omit] -1
xi i.exp2, pref(_T)

xtreg amr `X' if year<=1988, cluster(fips) fe
testparm _Texp2_2-_Texp2_6
local ppre = r(p)
testparm _Texp2_8-_Texp2_16
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_results", clear
	quietly{
		gen b_X_all_all				= .
		gen se_X_all_all				= .	
		forval h = 1/17{
				if `h'==7{
					replace b_X_all_all = 0 in `h'
				}
				else{
					replace b_X_all_all = _b[_Texp2_`h'] in `h'
					replace se_X_all_all = _se[_Texp2_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_es_results", replace
restore




**********************************
*Early CHCs, All Years (1959-1998)
**********************************
*Baltimore and Arlington can't be combined with counties after 1988
drop if fips==24510
drop if fips==51013

*can't include AHA or REIS covariates in the later years
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* _Texp* [aw=popwt]"	

char exp1_1998[omit] -1
xi i.exp1_1998, pref(_T)

xtreg amr `X' if samp8998, cluster(fips) fe
testparm _Texp1_1998_2-_Texp1_1998_6
local ppre = r(p)
testparm _Texp1_1998_8-_Texp1_1998_32
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_results", clear
	quietly{
		gen b_X_early_5998				= .
		gen se_X_early_5998				= .	
		forval h = 1/33{
				if `h'==7{
					replace b_X_early_5998 = 0 in `h'
				}
				else{
					replace b_X_early_5998 = _b[_Texp1_1998_`h'] in `h'
					replace se_X_early_5998 = _se[_Texp1_1998_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_es_results", replace
restore



use "$output/amr_chc_es_results", clear
graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
		
#delimit ;					
for var *all_all: replace X = . if time>8;
for var *early_all:  replace X = . if time>14;
for var *early_5998:  replace X = . if time>23;

cap drop ub* lb*;
gen ub 			= b_X_early_all + 1.96*se_X_early_all ;
gen lb 			= b_X_early_all - 1.96*se_X_early_all ;		

twoway (scatter b_X_all_all b_X_early_all b_X_early_5998 ub lb
		time if time>=-6 & time<=24,							/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 								/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so all "pre" periods have NO CHC*/
		yline(0, lcolor(black)) 								/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(O i Th i i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification be a solid line*/
		msize(medium . medium medium medium . . . .)			/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(maroon navy forest_green navy navy blue blue)	/*MARKER COLOR: */
		lpattern( solid solid solid dash dash dot dot)			/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth( medthick vthick medthick medthick medthick medium medium medium medium)			/*LINE WIDTHS: make preferred specification thick*/
		lcolor(maroon navy forest_green navy navy blue blue)	/*LINE COLOR: main results are always navy*/ 		
		legend(off)				
		xlabel(-6(3)24, labsize(medium))  						/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(,  labsize(medium))								/*YLABEL*/
		xtitle("Years Since CHC Establishment", size(medium))	/*XTITLE*/
		ytitle("Deaths per 100,000 Residents" " ", size(medium))
		title("", size(medium) color(black))	
		graphregion(fcolor(white) color(white) icolor(white) margin(small))  /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		plotregion(margin(vsmall)))
		(pcarrowi 7 2 7 -.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(7 9 "Year Before CHCs Began Operating", j(left)))
		(pcarrowi -6 6 -9 5 , lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(-5 8 "All CHCs, 1959-1988", j(left)))		
		(pcarrowi -12 14 -17 11 , lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(-10 18 "Early CHCs (funded 1965-1974);" "3,044 counties observed 1959-88", j(left)))		
		(pcarrowi -23 22 -21 21 , lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(-25 18 "Early CHCs (funded 1965-1974);" "388 counties observed 1959-98", j(left)))
		; 
		#delimit cr;
		
graph export "$output/figure5.wmf", replace

*erase "$output/amr_chc_es_results.dta"
log close
exit
