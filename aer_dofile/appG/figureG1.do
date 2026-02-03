/******************************************************
FIGURE G1: Age-Group-Specific Event-Study Results for ASMR, 9 pre-periods
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
log using "$output/log_figureG1", replace text	

*save a file to hold the coefficients for stata graphs
set obs 26
gen time = _n - 11
save "$output/asmr_chc_es_results_9pre", replace emptyok

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

*drop places treated before 1967 to get longer pre-period
drop if chc_year_exp<=1967
drop exp1
gen exp1 = year  - chc_year_exp if chc_year_exp<=1974
tab exp1
replace exp1						= -1  if exp1==.
replace exp1						= -10 if exp1<-9
replace exp1						= 15 if exp1>14 & exp1!=.

**********
*Infants
**********
*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* [aw=bwt]"	

char exp1[omit] -1
xi i.exp1, pref(_T)
xtreg imr `X', cluster(fips) fe

*STORE RESULTS IN A STATA FILE
preserve
	use "$output/asmr_chc_es_results_9pre", clear
	quietly{
		gen b_X_inf				= .
		gen se_X_inf				= .	
		forval h = 1/26{
				if `h'==10{
					replace b_X_inf = 0 in `h'
				}
				else{
					replace b_X_inf = _b[_Texp1_`h'] in `h'
					replace se_X_inf = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/asmr_chc_es_results_9pre", replace
restore

******************************
*Children, Adults, Older Adults
******************************
*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp*"	

char exp1[omit] -1
xi i.exp1, pref(_T)

foreach g in ch ad eld{
xtreg amr_`g' `X' [aw=popwt_`g'], cluster(fips) fe

*STORE RESULTS IN A STATA FILE
preserve
	use "$output/asmr_chc_es_results_9pre", clear
	quietly{
		gen b_X_`g'				= .
		gen se_X_`g'				= .	
		forval h = 1/26{
				if `h'==10{
					replace b_X_`g' = 0 in `h'
				}
				else{
					replace b_X_`g' = _b[_Texp1_`h'] in `h'
					replace se_X_`g' = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/asmr_chc_es_results_9pre", replace
restore
}






use "$output/asmr_chc_es_results_9pre", clear

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/		

*infant panel
#delimit ;				
cap drop ub* lb*;
gen ub 			= b_X_inf + 1.96*se_X_inf;
gen lb 			= b_X_inf - 1.96*se_X_inf;	

twoway (scatter b_X_inf ub lb
		time if time>=-9 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
		msize(medium . medium medium . . )						/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(navy navy navy)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern( solid dash dash)					/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth(vthick  medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
		lcolor( navy navy navy)						/*LINE COLOR: all-cause results are always navy*/ 		
		legend(off)											/*LEGEND: none here because of grc1leg*/
		xlabel(-9(3)12 14, labsize(small))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(,  labsize(small))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
		xtitle("")											/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
		ytitle("Deaths per 1,000 Live Births", size(medium))
		title("{it:A. Infants}", size(medlarge) color(black))	/*TITLE: panel titles are defined in the locals above*/
		graphregion(fcolor(white) color(white) icolor(white)) )
		(pcarrowi 1.25 1 1.25 -.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(1.25 5 "Year Before CHCs" "Began Operating", j(left) size(medlarge))), 
		saving("$output/panel_inf", replace); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		#delimit cr;

*child panel		
#delimit ;				
cap drop ub* lb*;
gen ub 			= b_X_ch + 1.96*se_X_ch ;
gen lb 			= b_X_ch - 1.96*se_X_ch ;		

twoway (scatter b_X_ch ub lb
		time if time>=-9 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
		msize(medium . medium medium . . )						/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(navy navy navy)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern( solid dash dash)					/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth(vthick  medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
		lcolor( navy navy navy)						/*LINE COLOR: all-cause results are always navy*/ 		
		legend(off)				
		xlabel(-9(3)12 14, labsize(small))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(,  labsize(small))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
		xtitle("", size(small))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
		ytitle("Deaths per 100,000 Residents", size(medium))
		title("{it:B. Children (1-14)}", size(medlarge) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
		graphregion(fcolor(white) color(white) icolor(white)) )
		(pcarrowi -3.5 1.5 -3.5 -.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(-3.5 5 "Year Before CHCs" "Began Operating", j(left) size(medlarge))),
		saving("$output/panel_ch", replace); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		#delimit cr;

*adult panel		
#delimit ;				
cap drop ub* lb*;
gen ub 			= b_X_ad + 1.96*se_X_ad;
gen lb 			= b_X_ad - 1.96*se_X_ad;		
		
twoway (scatter b_X_ad ub lb
		time if time>=-9 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
		msize(medium . medium medium . . )						/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(navy navy navy)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern( solid dash dash)					/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth(vthick  medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
		lcolor( navy navy navy)						/*LINE COLOR: all-cause results are always navy*/ 		
		legend(off)				
		xlabel(-9(3)12 14, labsize(small))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(,  labsize(small))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
		xtitle("Years Since CHC Establishment", size(medium))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
		ytitle("Deaths per 100,000 Residents", size(medium))
		title("{it:C. Adults (15-49)}", size(medlarge) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
		graphregion(fcolor(white) color(white) icolor(white)) )
		(pcarrowi -15 1.5 -15 -.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(-15 5 "Year Before CHCs" "Began Operating", j(left) size(medlarge))), 
		saving("$output/panel_ad", replace); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		#delimit cr;

		
*older adults panel
		#delimit ;					
		cap drop ub* lb*;
		gen ub 			= b_X_eld + 1.96*se_X_eld;
		gen lb 			= b_X_eld - 1.96*se_X_eld;		
	
		twoway (scatter b_X_eld ub lb	
				time if time>=-9 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
				xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
				yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
				connect(l l l l l l l)								/*LINES: add lines (of course)*/
				cmissing(n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
				msymbol(i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
				msize(medium . medium medium . . )						/*MARKER SIZE: only matters when a marker is specified*/
				mcolor(navy navy navy)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
				lpattern( solid dash dash)					/*LINE PATTERN: estimates solid, CI dashed*/
				lwidth(vthick  medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
				lcolor( navy navy navy)						/*LINE COLOR: all-cause results are always navy*/ 		
				legend(off)				
					xlabel(-9(3)12 14, labsize(small))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
				ylabel(,  labsize(small))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
				xtitle("Years Since CHC Establishment", size(medium))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
				ytitle("Deaths per 100,000 Residents", size(medium))
				title("{it:D. Older Adults (50+)}", size(medlarge) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
				graphregion(fcolor(white) color(white) icolor(white)) )
				(pcarrowi 25 1.5 25 -.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
				text(25 6 "Year Before CHCs" "Began Operating", j(left) size(medlarge))),
				saving("$output/panel_eld", replace); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
				#delimit cr;

	graph combine "$output/panel_inf.gph" "$output/panel_ch.gph" "$output/panel_ad.gph" "$output/panel_eld.gph", col(2) xsize(8.5) ysize(8.5) imargin(zero) graphregion(fcolor(white) color(white) icolor(white) margin(zero)) plotregion(margin(small))
		graph display, xsize(8.5) ysize(5.5) 	
	graph export "$output/figureG1.wmf", replace

	erase "$output/asmr_chc_es_results.dta"
	erase "$output/panel_inf.gph"
	erase "$output/panel_ch.gph"
	erase "$output/panel_ad.gph"
	erase "$output/panel_eld.gph"
	log close
	exit

