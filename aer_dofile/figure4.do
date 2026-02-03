/*********************************************************************
FIGURE 4: Predicting the timing of CHC grant with 1960 Characteristics
*********************************************************************/
clear
clear matrix
clear mata
set mat 1000
set more off, perm
pause on
capture log close
log using "$output/log_figure4", replace text	

use *fips* year chc_year_exp amr _* copop using "$data/aer_data" if chc_year_exp<=1974, clear
drop if fips==6037|fips==17031|fips==36061

*add the 1965 AMR value to all obs and then keep year 1960
egen amr65 = total(amr*(year==1965)), by(fips)
keep if year==1960
gen damr = amr65-amr


************
* 1965 AMR *
************
*univariate regression
reg amr65 chc_year_exp [aw = copop], robust
predict amrhat
local bu = floor(_b[chc_year_exp]*10)/10
local seu = floor(_se[chc_year_exp]*10)/10

*multivariate
#delimit ;
reg amr65	
_60pcturban _60pctrurf _60pct04years 
_60pctmt64years _60pctnonwhit _60pctmt12schl 
_60pctlt4schl _pct59inclt3k _pct59incmt10k 
_tot_act_md  chc_year_exp [aw = copop] , robust;
#delimit cr;
local ba = floor(_b[chc_year_exp]*10)/10
local sea = floor(_se[chc_year_exp]*10)/10

*get best-fit line for average x's and variable chc_year
preserve
foreach var of varlist _60pcturban _60pctrurf _60pct04years _60pctmt64years _60pctnonwhit _60pctmt12schl _60pctlt4schl _pct59inclt3k _pct59incmt10k _tot_act_md{
	egen testo = mean(`var')
	replace `var' = testo
	drop testo
}
predict amrhatadj
keep stfips cofips  amrhatadj
save "amrhatadj", replace
restore
merge 1:1 stfips cofips  using "amrhatadj"
drop _merge

******
*DAMR*
******
*univariate regression
reg damr chc_year_exp [aw = copop] 
predict damrhat
bys chc_year_exp: replace damrhat=. if _n>1
local bdu = floor(_b[chc_year_exp]*10)/10
local sedu = floor(_se[chc_year_exp]*10)/10

*multivariate
#delimit ;
reg damr	
_60pcturban _60pctrurf _60pct04years 
_60pctmt64years _60pctnonwhit _60pctmt12schl 
_60pctlt4schl _pct59inclt3k _pct59incmt10k 
_tot_act_md  chc_year_exp [aw = copop] , robust;
#delimit cr;
local bda = floor(_b[chc_year_exp]*10)/10
local seda = floor(_se[chc_year_exp]*10)/10

*get best-fit line for average x's and variable chc_year
preserve
foreach var of varlist _60pcturban _60pctrurf _60pct04years _60pctmt64years _60pctnonwhit _60pctmt12schl _60pctlt4schl _pct59inclt3k _pct59incmt10k _tot_act_md{
	egen testo = mean(`var')
	replace `var' = testo
	drop testo
}
predict damrhatadj
bys chc_year_exp: replace damrhatadj=. if _n>1
keep stfips cofips  damrhatadj
save "damrhatadj", replace
restore
merge 1:1 stfips cofips  using "damrhatadj"
drop _merge 


*MAKE GRAPHS
#delimit ;
twoway scatter amr chc_year_exp [aw=copop] ,
connect(n)										/*LINES: add lines to the fitted values, but not to the ASMR scatters*/
cmissing(n) 							/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
msymbol(Oh)									/*MARKER SYMBOL: O is closed circle, T is closed triangle, Oh and Th are open circle and triangle*/
legend(off)										/*LEGEND: off because grc1leg only needs one legend to work*/
xlabel(1965(3)1974, labsize(large))				/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
ylabel(200(400)1400, labsize(large) axis(1))						/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
xtitle("")										/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
legend(off)
graphregion(fcolor(white) color(white) icolor(white)) 
yaxis(1)
||
scatter amrhat amrhatadj chc_year_exp ,
connect(l l)										/*LINES: add lines to the fitted values, but not to the ASMR scatters*/
cmissing(y y)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
msymbol(i T)									/*MARKER SYMBOL: O is closed circle, T is closed triangle, Oh and Th are open circle and triangle*/
msize( . medlarge)
mcolor( forest_green maroon)								/*MARKER COLOR:*/ 
lpattern( solid solid)							/*LINE PATTERN:*/
lwidth( medthick medthick)						/*LINE WIDTHS:*/
lcolor(forest_green maroon)									/*LINE COLOR:*/ 
legend(off)										/*LEGEND: off because grc1leg only needs one legend to work*/
xlabel(1965(3)1974, labsize(large))				/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
ylabel(200(400)1400, labsize(large) axis(1))						/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
xtitle("")										/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
ytitle("Deaths per 100,000 Residents", size(large))
title("{it: A. 1965 AMR}", size(vlarge) color(black))	/*TITLE: panel titles are defined in the locals above*/
legend(off)
graphregion(fcolor(white) color(white) icolor(white)) 
yaxis(1) saving("$output/panel_a.gph", replace);


#delimit ;
twoway scatter damr chc_year_exp [aw=copop] ,
connect(n)										/*LINES: add lines to the fitted values, but not to the ASMR scatters*/
cmissing(n) 							/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
msymbol(Oh)									/*MARKER SYMBOL: O is closed circle, T is closed triangle, Oh and Th are open circle and triangle*/
legend(off)										/*LEGEND: off because grc1leg only needs one legend to work*/
xlabel(1965(3)1974, labsize(large))				/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
ylabel(-400(200)200, labsize(large) axis(1))						/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
xtitle("")										/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
ytitle("Change in Deaths per 100,000 Residents", size(large))
title("{it: B. 1960-1965 Change in AMR}", size(vlarge) color(black))	/*TITLE: panel titles are defined in the locals above*/
legend(off)
graphregion(fcolor(white) color(white) icolor(white)) 
yaxis(1)
||
scatter damrhat damrhatadj chc_year_exp ,
connect(l l)										/*LINES: add lines to the fitted values, but not to the ASMR scatters*/
cmissing(y y)								/*MISSING: do not connect lines between missing points (ie. confidence intervals)*/
msymbol(i T)									/*MARKER SYMBOL: O is closed circle, T is closed triangle, Oh and Th are open circle and triangle*/
msize( . medlarge)
mcolor( forest_green maroon)								/*MARKER COLOR:*/ 
lpattern( solid solid)							/*LINE PATTERN:*/
lwidth( medthick medthick)						/*LINE WIDTHS:*/
lcolor(forest_green maroon)									/*LINE COLOR:*/ 
xlabel(1965(3)1974, labsize(large))				/*XLABEL: this makes the axis tight [it doesn't work with xscale(range()), only xlabel]*/
ylabel(-400(200)200, labsize(large) axis(1))						/*YLABEL: make there be 4 ticks so they don't overlap and make the tick-mark text small*/
xtitle("")										/*XTITLE: no title on this one, only on the bottom panels to save vertical space*/
title("{it: B. 1960-1965 Change in AMR}", size(vlarge) color(black))	/*TITLE: panel titles are defined in the locals above*/
legend(order(- "Fitted Values: " 2 3) rows(1) label(2 "Univariate") label(3 "Multivariate") size(medium) region(style(none)))
graphregion(fcolor(white) color(white) icolor(white)) 
yaxis(1) saving("$output/panel_b", replace);
#delimit cr;	

		di "Univariate Levels Slope: `bu'"
		di "Univariate Levels SE: `seu'"		
		di "Multivariate Levels Slope: `ba'"
		di "Multivariate Levels SE: `sea'"
		
		di "Univariate Changes Slope: `bdu'"
		di "Univariate Changes SE: `sedu'"		
		di "Multivariate Changes Slope: `bda'"
		di "Multivariate Changes SE: `seda'"				

cd $dofile		
grc1leg "$output/panel_a.gph" "$output/panel_b.gph", legendfrom("$output/panel_b.gph") xsize(5) ysize(2.75) col(2) imargin(medium) graphregion(fcolor(white) color(white) icolor(white) margin(zero)) plotregion(margin(tiny))
graph display, xsize(5) ysize(2.75)
	
graph export "$output/figure4.wmf", replace
	
erase "$output/amrhatadj.dta"
erase "$output/damrhatadj.dta"
erase "$output/panel_a.gph"
erase "$output/panel_b.gph"	
log close
exit


