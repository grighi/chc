/******************************************************
FIGURE D4: Event-Study Results with pscore-trimmed sample (and IPW)
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
log using "$output/log_figureD4", replace text	

*save a file to hold the coefficients for stata graphs
set obs 33
gen time = _n - 8
save "$output/amr_chc_es_results_dfltrim", replace emptyok

do $dofile/pscore

use "$data/aer_data" if year<=1988, clear
merge m:1 stfips cofips using "$output/pscore_temp"
keep if _merge==3
drop _merge

keep if pscore1>=.1 & pscore1<=.9

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
*Overall AMR
*************************
*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* [aw=dflpopwgt1]"	
char exp1[omit] -1
xi i.exp1, pref(_T)
xtreg amr `X' if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_results_dfltrim", clear
	quietly{
		gen b_X				= .
		gen se_X				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X = 0 in `h'
				}
				else{
					replace b_X = _b[_Texp1_`h'] in `h'
					replace se_X = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_es_results_dfltrim", replace
restore


*************************
*Older Adult AMR
*************************
*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* [aw=dflpopwgt1_eld]"	
char exp1[omit] -1
xi i.exp1, pref(_T)
xtreg amr_eld `X' if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_results_dfltrim", clear
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
	save "$output/amr_chc_es_results_dfltrim", replace
restore



use "$output/amr_chc_es_results_dfltrim", clear

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
		
#delimit ;					
cap drop ub* lb*;
gen ub 			= b_X + 1.96*se_X ;
gen lb 			= b_X - 1.96*se_X ;				

scatter b_X ub lb
		time if time>=-6 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(Th i i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X3 be a solid line (as well as the CI)..."i" means no marker*/
		msize(medium . medium medium medium . . . .)						/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(navy navy navy blue blue)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern( solid dash dash dot dot)					/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth( medthick  medium medium medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
		lcolor(navy navy navy blue blue)						/*LINE COLOR: all-cause results are always navy*/ 		
		legend(off)				
		xlabel(-6(3)14, labsize(small))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(,  labsize(small))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
		xtitle("", size(medium))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
		ytitle("Deaths per 100,000 Residents" " " , size(medium))
		title("{it: A. Age-Adjusted Mortality}", size(medium) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
		graphregion(fcolor(white) color(white) icolor(white) margin(vsmall)) 
		plotregion(margin(vsmall))
		saving("$output/panel_a", replace); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		#delimit cr;
		
				
#delimit ;					
cap drop ub* lb*;
gen ub 			= b_X_eld + 1.96*se_X_eld ;
gen lb 			= b_X_eld - 1.96*se_X_eld ;	

scatter b_X_eld ub lb
		time if time>=-6 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(Th i i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X3 be a solid line (as well as the CI)..."i" means no marker*/
		msize(medium . medium medium medium . . . .)						/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(navy navy navy blue blue)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern( solid dash dash dot dot)					/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth( medthick  medium medium medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
		lcolor(navy navy navy blue blue)						/*LINE COLOR: all-cause results are always navy*/ 		
		legend(off)				
		xlabel(-6(3)14, labsize(small))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(,  labsize(small))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
		xtitle(" " "Years Since CHC Establishment", size(medium))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
		ytitle("Deaths per 100,000 Residents" " " , size(medium))
		title("{it: B. Older Adult Mortality (50+)}", size(medium) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
		graphregion(fcolor(white) color(white) icolor(white) margin(vsmall)) 
		plotregion(margin(vsmall))
		saving("$output/panel_b", replace); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		#delimit cr;
		
				
graph combine "$output/panel_a.gph" "$output/panel_b.gph", col(1) imargin(tiny) xcommon xsize(8.5) ysize(5.5) graphregion(fcolor(white) color(white) icolor(white))
graph display, xsize(7.5) ysize(10)
graph export "$output/figureD4.wmf", replace

erase "$output/amr_chc_es_results_dfltrim.dta"

exit
