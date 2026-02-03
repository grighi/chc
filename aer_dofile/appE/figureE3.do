/******************************************************
FIGURE E3: Event-Study Estimates with county-characteristics/Medicaid interactions
******************************************************/
clear
clear matrix
set mat 5000
clear mata
set maxvar 10000
set matsize 10000
set more off, perm
pause on
capture log close
log using "$output/log_figureE3", replace text	

*save a file to hold the coefficients for stata graphs
set obs 23
gen time = _n - 8
save "$output/amr_chc_mcaid_es_results", replace emptyok


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

*medicaid timing
recode stfips (1=1970) (2=1972) (4=1982) (5=1970) (6=1966) (8=1969) (9=1966) (10=1966) (11=1968) ///
(12=1970) (13=1967) (15=1966) (16=1966) (17=1966) (18=1970) (19=1967) (20=1967) (21=1966) ///
(22=1966) (23=1966) (24=1966) (25=1966) (26=1966) (27=1966) (28=1970) (29=1967) (30=1967) ///
(31=1966) (32=1967) (33=1967) (34=1970) (35=1966) (36=1966) (37=1970) (38=1966) (39=1966) ///
(40=1966) (41=1967) (42=1966) (44=1966) (45=1968) (46=1967) (47=1969) (48=1967) (49=1966) ///
(50=1966) (51=1969) (53=1966) (54=1966) (55=1966) (56=1967), gen(ymcaid)

gen mexp = year - ymcaid
char mexp[omit] -1
xi i.mexp, pref(M)


***************************************
*Controlling for Medicaid*High-Poverty
***************************************
gen pov = _pct59inclt3k>45
for var Mmexp*: gen _MCX = X*pov

*preferred specification: year FE, urban-by-year FE, state-by-year effects, 1960 char trends, REIS vars, AHA varls
local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _MC* _Texp* [aw=popwt_eld]"	

char exp1[omit] -1
xi i.exp1, pref(_T)
xtreg amr_eld `X' if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_mcaid_es_results", clear
	quietly{
		gen b_X_pov				= .
		gen se_X_pov				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_pov = 0 in `h'
				}
				else{
					replace b_X_pov = _b[_Texp1_`h'] in `h'
					replace se_X_pov = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_mcaid_es_results", replace
restore





***************************************
*Controlling for Medicaid*High-PCMD
***************************************
gen pcmd = _tot_act_md/popwt_eld
sum pcmd, det
gen hpcmd = pcmd>r(p50)
for var Mmexp*: gen _MDX = X*hpcmd

local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _MD* _Texp* [aw=popwt_eld]"	

char exp1[omit] -1
xi i.exp1, pref(_T)
xtreg amr_eld `X' if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_mcaid_es_results", clear
	quietly{
		gen b_X_pcmd				= .
		gen se_X_pcmd				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_pcmd = 0 in `h'
				}
				else{
					replace b_X_pcmd = _b[_Texp1_`h'] in `h'
					replace se_X_pcmd = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_mcaid_es_results", replace
restore



***************************************
*Controlling for Medicaid*Med School
***************************************
egen ms = total((_tot_med_stud>0)*(year==1960)), by(fips)
for var Mmexp*: gen _MSX = X*ms

local X "_Iyear* _IyeaXDu* _IyeaXst* D_* R_* H_* _MS* _Texp* [aw=popwt_eld]"	

char exp1[omit] -1
xi i.exp1, pref(_T)
xtreg amr_eld `X' if year<=1988, cluster(fips) fe
testparm _Texp1_2-_Texp1_6
local ppre = r(p)
testparm _Texp1_8-_Texp1_22
local ppost = r(p)
*STORE RESULTS IN A STATA FILE
preserve
	use "$output/amr_chc_mcaid_es_results", clear
	quietly{
		gen b_X_ms				= .
		gen se_X_ms				= .	
		forval h = 1/23{
				if `h'==7{
					replace b_X_ms = 0 in `h'
				}
				else{
					replace b_X_ms = _b[_Texp1_`h'] in `h'
					replace se_X_ms = _se[_Texp1_`h'] in `h'
				}
		}
	}
	save "$output/amr_chc_mcaid_es_results", replace
restore




use "$output/amr_chc_mcaid_es_results",clear

graph set window fontface "Times New Roman"							/*FONT: make everything always show up as Times New Roman*/
		
		#delimit ;					
		cap drop ub* lb*;
		gen ub 			= b_X_pcmd + 1.96*se_X_pcmd ;
		gen lb 			= b_X_pcmd - 1.96*se_X_pcmd ;				
		
		scatter b_X_pcmd b_X_ms b_X_pov ub lb
				time if time>=-6 & time<=14,						/*SCATTER: allows markers, and is actually equivalent to "line" with the connect option*/
				xline(-1, lcolor(black)) 							/*XLINE: refers to a vertical line that crosses the x-axis. Put one at the omitted category, -1, so that all post periods have some CHC grant even if its not for a whole year at time 0*/
				yline(0, lcolor(black)) 							/*YLINE: horizontal black line at 0*/
				connect(l l l l l l l l l)								/*LINES: add lines (of course)*/
				cmissing(n n n n n n n n n)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
				msymbol(X i Th i i i i)									/*MARKER SYMBOL: Th is open triangle, X are x's, and make the preferred specification, X2 be a solid line (as well as the CI)..."i" means no marker*/
				msize(medium . medium medium medium . . . .)						/*MARKER SIZE: only matters when a marker is specified*/
				mcolor(navy maroon forest_green navy navy blue blue)						/*MARKER COLOR: forest green and purple are not used elsewhere so that the scheme for men/women/overall is consistent*/
				lpattern( solid solid solid dash dash dot dot)					/*LINE PATTERN: estimates solid, CI dashed*/
				lwidth( medthick medthick medthick medthick medthick medium medium medium medium)			/*LINE WIDTHS: make preferred specification really thick*/
				lcolor(navy maroon forest_green navy navy blue blue)						/*LINE COLOR: all-cause results are always navy*/ 		
				legend(order(- "Controls for Medicaid Interacted With: " 1 2 3)  colgap(10)	rows(2) 	label(1 "High Per-Capita MDs, 1960") 	label(2 "    Medical School, 1969") label(3 "High Poverty, 1959")
					region(style(none))  size(medsmall))				
					xlabel(-6(3)14, labsize(small))  					/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
				ylabel(,  labsize(small))							/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
				xtitle(" " "Years Since CHC Establishment", size(medium))		/*XTITLE: title on this one, because in the combine command its on the bottom.*/
				ytitle("Deaths per 100,000 Residents" " " , size(medium))
				title("", size(medium) color(black))	/*TITLE: "$output/panel titles are defined in the locals above*/
				graphregion(fcolor(white) color(white) icolor(white) margin(small)) 
				plotregion(margin(small)); /*BACKGROUNDS: this code takes away all the borders and color from the stata graph background and maximizes graph size within its region*/
				#delimit cr;
				
	graph export "$output/figureE3.wmf", replace

	erase "$output/amr_chc_mcaid_es_results.dta"	
	log close
	
	exit



