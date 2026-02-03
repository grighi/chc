/******************************************************
FIGURE E1: Event-Study Results for Older Adult Mortality, Treated Counties Only
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
log using "$output/log_figureE1", replace text	

*save a file to hold the coefficients for stata graphs
set obs 23
gen time = _n - 8
save "$output/amr_chc_treatonly_es_results", replace emptyok

use "$data/aer_data" if year<=1988 & chc_year_exp<=1974, clear
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

char exp1[omit] -1
xi i.exp1, pref(_T)

/*************************************************
2. Region-by-Year and Year-by-Urban Effects (and county FE)
*************************************************/
recode stfips (9 23 25 33 44 50 34 36 42 = 1) (18 17 26 39 55 19 20 27 29 31 38 46 = 2) (10 11 12 13 24 37 45 51 54 1 21 28 47 5 22 40 48 = 4) (4 8 16 35 30 49 32 56 6 41 53 = 5), gen(region)
xi i.year*i.Durb i.year*i.region

*ESTIMATION
xtreg amr_eld _I*  _Texp* [aw=popwt_eld], cluster(fips) fe

*STORE RESULTS IN A STATA FILE
preserve
use "$output/amr_chc_treatonly_es_results", clear
quietly{
local i 2
gen b_X`i' 								= .
gen se_X`i' 							= .	
forval h = 1/23{
		if `h'==7{
			replace b_X`i' 				= 0 in `h'
		}
		else{
			replace b_X`i' 				= _b[_Texp1_`h'] in `h'
			replace se_X`i' 			= _se[_Texp1_`h'] in `h'
		}
}
}	 
label var b_X`i' "Region-by-Year and Year-by-Urban Effects (and county FE)"
save "$output/amr_chc_treatonly_es_results", replace
restore


/*************************************************
3. Region-by-Year and Year-by-Urban Effects , trends(and county FE)
*************************************************/
xi i.year*i.Durb i.year*i.region i.fips*year

*ESTIMATION
xtreg amr_eld _I*  _Texp* [aw=popwt_eld], cluster(fips) fe

*STORE RESULTS IN A STATA FILE
preserve
use "$output/amr_chc_treatonly_es_results", clear
quietly{
local i 3
gen b_X`i' 								= .
gen se_X`i' 							= .	
forval h = 1/23{
		if `h'==7{
			replace b_X`i' 				= 0 in `h'
		}
		else{
			replace b_X`i' 				= _b[_Texp1_`h'] in `h'
			replace se_X`i' 			= _se[_Texp1_`h'] in `h'
		}
}
}	
label var b_X`i' "Region-by-Year, Year-by-Urban Effects (and county FE), county trends"
save "$output/amr_chc_treatonly_es_results", replace
restore



use "$output/amr_chc_treatonly_es_results", clear
graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/


#delimit ;
cap drop ub* lb*;
gen ub 			= b_X3 + 1.96*se_X3 ;
gen lb 			= b_X3 - 1.96*se_X3 ;		

scatter b_X2 b_X3 ub lb  		
		time if time>=-6 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(i Oh i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
		msize(medium medium . . . )						/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(navy maroon navy navy )						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern( solid solid dash dash)					/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth( vthick medthick medthin medthin)			/*LINE WIDTHS: make preferred specification really thick*/
		lcolor(navy maroon maroon maroon)						/*LINE COLOR: all-cause results are always navy*/ 					
		legend(off)				
		xlabel(-6(2)14, labsize(small))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(, labsize(small))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
		xtitle("Years Since Treatment", size(small))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
		ytitle("")
		title("", size(medium) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
		graphregion(fcolor(white) color(white) icolor(white)); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		#delimit cr;
	graph export "$output/figureE1.wmf", replace
	
	
erase "$output/amr_chc_treatonly_es_results.dta"	
log close
exit

