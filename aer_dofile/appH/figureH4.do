/******************************************************
FIGURE H4: ES results for hospital capacity, ALL CENTERS 
******************************************************/
clear
clear matrix
clear mata
set mat 1000
set mem 6000m
set maxvar 10000
set matsize 10000
set more off, perm
pause on
capture log close
log using "$output/log_figureH4", replace text	

*save a file to hold the coefficients for stata graphs
set obs 17
gen time = _n - 8
save "$output/aha_chc_es_results_allchc", replace emptyok

use "$data/aer_chc_aha.dta" if year>=1959 & year<=1988, clear
merge 1:1 fips year using "$data/aer_data"
keep if _merge==3
drop _merge

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
recode exp2 (9/50=9) (-50/-7=-7)
char exp2[omit] -1
xi i.exp2, pref(_T)

local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* _Texp* [aw=popwt]"

foreach y in bpc hpc{
	xtreg `y' `X', cluster(fips) fe	

	preserve
	*STORE RESULTS IN A STATA FILE
	use "$output/aha_chc_es_results_allchc", clear
	quietly{
		gen b_`y'							= .
		gen se_`y' 							= .	
		forval h = 1/17{
				if `h'==7{
					replace b_`y' 			= 0 in `h'		/*coefficient "16" is the omitted category*/
				}
				else{
					replace b_`y' 			= _b[_Texp2_`h'] in `h'
					replace se_`y' 			= _se[_Texp2_`h'] in `h'
				}
		}
	}
	save "$output/aha_chc_es_results_allchc", replace
	restore
}


*trend breaks
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* _Texp2_1 pre_exp2 dcons_post post_exp2 _Texp2_17"

cap drop treat2
cap drop *pre*
cap drop *post*
gen treat2 = chc_year_exp<=1980
*make the zero in the index come at -1, then multiply by a treatment dummy so that all other counties get zero
gen pre_exp2 = (exp2*(exp2>=-6)*(exp2<9) + 1)
replace pre_exp2 = 0 if treat2~=1 | (exp2<-6 | exp2>=9)
*make the zero in the index come at -1, then multiply by a treatment dummy so that all other counties get zero		
gen post_exp2 = (exp2*(exp2>-1)+1)*(exp2<9)
replace post_exp2 = 0 if treat2~=1 | (exp2<=-1 | exp2>=9)
gen dcons_post = exp2>-1 & exp2<9	

xtreg bpc `X', cluster(fips) fe	
global tb_bpc_b: display %4.3f _b[post_exp2]
global tb_bpc_se: display %4.3f _se[post_exp2]	

xtreg hpc `X', cluster(fips) fe	
global tb_hpc_b: display %6.5f _b[post_exp2]
global tb_hpc_se: display %6.5f _se[post_exp2]



use "$output/aha_chc_es_results_allchc", clear
graph set window fontface "Times New Roman"			/*make everything always show up as Times New Roman*/


global tb_bpc_b: display %4.3f 0.0121834    
global tb_bpc_se: display %4.3f 0.024959

global tb_hpc_b: display %5.4f 0.0001655
global tb_hpc_se: display %5.4f 0.0003318


/*Hospitals Per Capita*/		
		#delimit ;
		cap drop ub* lb*;
		gen ub_hpc 			= b_hpc + 1.96*se_hpc;
		gen lb_hpc 			= b_hpc - 1.96*se_hpc;
		gen ub_bpc 			= b_bpc + 1.96*se_bpc;
		gen lb_bpc 			= b_bpc - 1.96*se_bpc;
		
	twoway (scatter b_hpc ub*hpc* lb*hpc* time if time>=-15 & time<=15,	/*just the coefficients, not the confidence intervals...scatter allows markers, and is actually equivalent to "line" with the connect option*/
		yaxis(1)
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(Sh i i )									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
		msize(medlarge medarge . .)							/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(maroon maroon maroon)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern(solid dash dash)							/*LINE PATTERN: estimates solid, DI dashed*/
		lwidth(thick medthick medthick)						/*LINE WIDTHS: make preferred specification really thick*/
		lcolor(maroon maroon maroon)						/*LINE COLOR: all-cause results are always navy*/ 
		legend(order(1 4) 	rows(1) 	label(1 "Hospitals per 1,000 (Left)") 	/*LEGEND: put it here to be used in the grc1leg command.*/
										label(4 "Beds per 1,000 (Right)") 
					region(style(none))  size(medium))
		xlabel(-6(3)6, labsize(medium))  				/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(-.0125(.00625).0125, labsize(medium) axis(1))	/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
		xtitle("Years Since CHC Establishment")											/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
		ytitle("Hospitals per 1,000 Residents",axis(1) size(medium))
		title("", size(medium) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(medium)) )
			
		(scatter b_bpc ub*bpc* lb*bpc* time if time>=-15 & time<=15,	/*just the coefficients, not the confidence intervals...scatter allows markers, and is actually equivalent to "line" with the connect option*/
		yaxis(2)
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(i i i )										/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
		msize(medlarge medlarge . .)						/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(navy navy navy)								/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern(solid dot dot)								/*LINE PATTERN: estimates solid, DI dashed*/
		lwidth( thick medthick medthick )					/*LINE WIDTHS: make preferred specification really thick*/
		lcolor(navy navy navy)								/*LINE COLOR: all-cause results are always navy*/ 
		legend(order(1 4) 	rows(1) 	label(1 "Hospitals per 1,000 (Left)") 	/*LEGEND: put it here to be used in the grc1leg command.*/
										label(4 "Beds per 1,000 (Right)") 
					region(style(none))  size(medium))
		xlabel(-6(3)6, labsize(medium))  				/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(-1.25(.625)1.25, labsize(medium) axis(2))		/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
		xtitle("Years Since CHC Establishment")											/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
		ytitle("Hospital Beds per 1,000 Residents",axis(2) size(medium))
		title("", size(medium) color(black))
		text(-.6 3.1  "Post-CHC Trend-Break Estimates:" "    Hospitals = $tb_hpc_b (s.e. =  $tb_hpc_se)" "    Beds =  $tb_bpc_b (s.e. =  $tb_bpc_se)", j(left) size(medsmall) yaxis(2))
		graphregion(fcolor(white) color(white) icolor(white) margin(medium)) )
		
		(pcarrowi 1 -3 1 -1.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick) yaxis(2)
		text(1 -5 "Year Before" "CHC Began", j(left) size(medium) yaxis(2))
		);
		#delimit cr;
		
graph export "$output/figureH4.wmf", replace


erase "$output/aha_chc_es_results.dta"

log close
exit
