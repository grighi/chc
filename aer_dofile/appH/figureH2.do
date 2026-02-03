/******************************************************
FIGURE H2: Event-Study Results for AMR by Urbanicity in 1960, ALL CENTERS
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
log using "$output/log_figureH2", replace text	

*save a file to hold the coefficients for stata graphs
	set obs 17
	gen time = _n - 8
save "$output/amr_chc_es_urban_results_allchc", replace emptyok

*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* HU_Texp* [aw=popwt]"	

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

*define urban (split treatment group in half)
egen u60 = total(D_60pcturban_t*(year==1960)), by(fips)
sum u60 if chc_year_exp<=1974, det
gen hu = u60>r(p50)

*urban shares in high/low groups noted on fig
	sum u60 if chc_year_exp<=1974 & hu [aw=copop]
	local hshare : display %02.0f round(r(mean))
	sum u60 if chc_year_exp<=1974 & ~hu [aw=copop]
	local lshare : display %02.0f round(r(mean))


*treatment variables (separate event-time dummies for high- and low-urban counties)
char exp2[omit] -1
xi i.exp2, pref(_T)
for var _Texp2*: gen HUX = hu*X		
for var _Texp2*: replace X = (1-hu)*X
xtreg amr `X' if year<=1988, cluster(fips) fe
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_es_urban_results_allchc", clear
	quietly{
		gen b_X_high				= .
		gen se_X_high				= .	
		gen b_X_low					= .
		gen se_X_low				= .	
		forval h = 1/17{
				if `h'==7{
					replace b_X_low = 0 in `h'
					replace b_X_high = 0 in `h'
				}
				else{
					replace b_X_low = _b[_Texp2_`h'] in `h'
					replace se_X_low = _se[_Texp2_`h'] in `h'
					replace b_X_high = _b[HU_Texp2_`h'] in `h'
					replace se_X_high = _se[HU_Texp2_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_es_urban_results_allchc", replace
restore



use "$output/amr_chc_es_urban_results_allchc", clear
#delimit ;					
cap drop ub* lb*;
gen ublo			= b_X_low + 1.96*se_X_low ;
gen lblo 			= b_X_low - 1.96*se_X_low ;		
gen ubhi			= b_X_high + 1.96*se_X_high ;
gen lbhi			= b_X_high - 1.96*se_X_high ;	

twoway (scatter b_X_high b_X_low *bhi *blo
		time if time>=-6 & time<=15,					/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
		xline(-1, lcolor(black)) 						/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1*/
		yline(0, lcolor(black)) 						/*YLINE: horizontal black line at 0*/
		connect(l l l l l l l l l)						/*LINES: add lines (of course)*/
		cmissing(n n n n n n n n n)						/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(Sh D i i i i)							/*MARKER SYMBOL:*/
		msize(vlarge large . . . .)						/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(forest_green dkorange forest_green forest_green dkorange dkorange)						/*MARKER COLOR: */
		lpattern( solid solid dash dash dot dot)		/*LINE PATTERN: estimates solid, CI dashed*/
		lwidth( vthick thick  medthick medthick medthick medthick)			/*LINE WIDTHS: make preferred specification really thick*/
		lcolor(forest_green dkorange forest_green forest_green dkorange dkorange)		/*LINE COLOR: high urban in green, low urban in organge*/ 		
		legend(off)
		xlabel(-6(3)6 8, labsize(medlarge))  			/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(-40(20)40,  labsize(medlarge))			/*YLABEL: */
		xtitle("Years Since CHC Establishment", size(medlarge))		/*XTITLE:*/
		ytitle("Deaths per 100,000 Residents", size(medlarge))
		title("", size(vlarge) color(black))			/*TITLE: "$output/panel titles are defined in the locals above*/
		graphregion(fcolor(white) color(white) icolor(white)))
		(pcarrowi 35 2 35 -.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(35 6 "Year Before CHCs Began Operating", j(left)))
		(pcarrowi 16 6 1 5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(22 6 "Effects of CHCs in counties with below" "median share urban population (average" "`lshare'% in non-urban areas)", j(left)))
		(pcarrowi -25 -2 -20.5 4, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick)
		text(-30 -3.5 "Effects of CHCs in" "counties with above" "median share urban" "population (average" "`hshare'% in urban areas)", j(left)))
		; /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
		#delimit cr;
		

graph export "$output/figureH2.wmf", replace

erase "$output/amr_chc_es_urban_results.dta"
log close
exit



