/******************************************************
FIGURE 8: ES Results for Medicare Variables
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
log using "$output/log_figure8", replace text	

*save a file to hold the coefficients for stata graphs
set obs 23
gen time = _n - 8
save "$output/medicare_chc_es_results", replace emptyok

use "$data/aer_data.dta" if year<=1982, clear

*drop VA
drop if stfips==51

*Drop NY/LA/Chicago instead of the pscore restrictions
drop if stfips==36 & cofips==61
drop if stfips==6  & cofips==37
drop if stfips==17 & cofips==31				
	
*adjust hospital variables to be per-65+ year old
replace H_bpc = H_bpc*(copop/copop_6500)
replace H_hpc = H_hpc*(copop/copop_6500)
	
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

*treatment vars for Medicare data outcomes
*recode exp1 (9/100 = 9)
char exp1[omit] -1
xi i.exp1, pref(_T)
gen pre = exp1<-1 & chc_year_exp<=1974
drop _Texp1_1-_Texp1_6

*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* pre _Texp* [aw=popwt_6500]"	

foreach var of varlist er_ab rpe_amt_ab{
	xtreg `var' `X', cluster(fips) fe
		
	*STORE RESULTS IN A STATA FILE
	preserve
		use "$output/medicare_chc_es_results", clear
		quietly{
			gen b_X_`var'				= .
			gen se_X_`var'				= .	
			forval h = 6/17{
					if `h'==6{
						replace b_X_`var' = _b[pre] in `h'
						replace se_X_`var' = _se[pre] in `h'
					}
					if `h'==7{
						replace b_X_`var' = 0 in `h'
					}
					if `h'>7{
						replace b_X_`var' = _b[_Texp1_`h'] in `h'
						replace se_X_`var' = _se[_Texp1_`h'] in `h'
					}
			}
		}
		save "$output/medicare_chc_es_results", replace
	restore
}


use "$data/aer_data.dta" if year<=1988, clear

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

*treatment vars for medicare+military medical
char exp1[omit] -1
xi i.exp1, pref(_T)
*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _Texp* [aw=popwt]"	

xtreg tranpcmcare1 `X', cluster(fips) fe
	
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/medicare_chc_es_results", clear
	quietly{
		gen b_X_tranpcmcare1				= .
		gen se_X_tranpcmcare1				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_tranpcmcare1 = 0 in `h'
				}
				if `h'~=7{
					replace b_X_tranpcmcare1 = _b[_Texp1_`h'] in `h'
					replace se_X_tranpcmcare1 = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/medicare_chc_es_results", replace
restore





use "$output/medicare_chc_es_results", clear

/*Medicare Enrollment Rate, 65+ */		
		#delimit ;
		cap drop ub* lb*;
		for var *_er* : replace X = . if time>8 | time<=-2;
		for var *_rpe*: replace X = . if time>7 | time<=-2;
		gen ub 				= b_X_er_ab + 1.96*se_X_er_ab;
		gen lb 				= b_X_er_ab - 1.96*se_X_er_ab;
				
		twoway (scatter b_X_er_ab ub lb
		time if time>=-6 & time<=14,	/*just the coefficients, not the confidence intervals...scatter allows markers, and is actually equivalent to "line" with the connect option*/
		yaxis(1)
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(Th i i )									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
		msize(large large . .)							/*MARKER SIZE: only matters when a marker is specified*/
		mcolor( navy navy navy)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern(solid dash dash)							/*LINE PATTERN: estimates solid, DI dashed*/
		lwidth(medthick medium medium )						/*LINE WIDTHS: make preferred specification really thick*/
		lcolor(navy navy navy)							/*LINE COLOR: all-cause results are always navy*/ 
		legend(order(1) 	rows(1)  label(1 "Parts A + B") 
					region(style(none))  size(medlarge))
		xlabel(-6(3)14, labsize(large))  				/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel(-.015(.005).015, labsize(large))  				/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/		xtitle("Years Since CHC Establishment", size(medlarge))											/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
		xtitle("Years Since CHC Establishment", size(large))											/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
		ytitle("Medicare Enrollment (per resident 65+)" " ", size(large))
		title("{it: B. Medicare Enrollment}", size(vlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(small)) )
		(pcarrowi .01 1.5 .01 -.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick) 
		text(.01 6 "Year Before CHC Began", j(left) size(large)) ),
		saving("$output/fig_8b", replace);
		#delimit cr;
		
/*Medicare Spending Per Enrollee ($2012)*/		
		#delimit ;
		cap drop ub* lb*;
		
		gen ubpe 				= b_X_rpe_amt_ab + 1.96*se_X_rpe_amt_ab;
		gen lbpe 				= b_X_rpe_amt_ab - 1.96*se_X_rpe_amt_ab;
		
		gen ubpc 				= b_X_tranpcmcare1 + 1.96*se_X_tranpcmcare1;
		gen lbpc	 			= b_X_tranpcmcare1 - 1.96*se_X_tranpcmcare1;		
		
		twoway (scatter b_X_rpe_amt_ab b_X_tranpcmcare1 ubpe lbpe ubpc lbpc
		time if time>=-6 & time<=14,	/*just the coefficients, not the confidence intervals...scatter allows markers, and is actually equivalent to "line" with the connect option*/
		yaxis(1)
		xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
		yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
		connect(l l l l l l)								/*LINES: add lines (of course)*/
		cmissing(n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
		msymbol(Sh O i i i i )									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
		msize(large large . .)							/*MARKER SIZE: only matters when a marker is specified*/
		mcolor(navy maroon navy navy maroon maroon)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
		lpattern(solid solid dot dot dash dash)							/*LINE PATTERN: estimates solid, DI dashed*/
		lwidth(medthick medthick medthick medthick medium medium )						/*LINE WIDTHS: make preferred specification really thick*/
		lcolor(navy maroon navy navy maroon maroon)							/*LINE COLOR: all-cause results are always navy*/ 
		legend(order(1 2) 	rows(1) 	label(1 "Per Enrollee") 	label(2 "Per Capita, Medicare+Military Medical") 
					region(style(none))  size(medlarge))
		xlabel(-6(3)14, labsize(large))  				/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
		ylabel( ,labsize(large))
		xtitle("Years Since CHC Establishment", size(large))											/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
		ytitle("Medicare Expenditures ($2012)" " ", size(large))
		title("{it: A. Medicare Spending}", size(vlarge) color(black))
		graphregion(fcolor(white) color(white) icolor(white) margin(medsmall)) )
		(pcarrowi 50 1.5 50 -.5, lcolor(black) mcolor(black) lwidth(medthick) mlwidth(medthick) 
		text(50 6 "Year Before CHC Began", j(left) size(large)) ),
		saving("$output/fig_8a", replace);
		#delimit cr;
		
		
	graph combine "$output/fig_8a.gph" "$output/fig_8b.gph", col(2) imargin(tiny) xsize(8.5) ysize(4.5) graphregion(fcolor(white) color(white) icolor(white) margin(tiny)) 
	graph display, xsize(7.5) ysize(3.5) 			
	graph export "$output/figure8.wmf", replace
	
*	erase "$output/medicare_chc_es_results.dta"
	erase "$output/fig_8a.gph"
	erase "$output/fig_8b.gph"
	log close
	exit

	
