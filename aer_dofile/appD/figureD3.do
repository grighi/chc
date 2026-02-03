/******************************************************
FIGURE D3: Propensity Score Distributions
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
log using "$output/log_figureD3", replace text	

do $dofile/pscore

#delimit ;
twoway kdensity pscore1 if ~treat1, 
yaxis(1) 
lpattern(dash) 
lwidth(thick)
xtitle("Propensity Score") 
ytitle("Untreated Density", axis(1)) || 
kdensity pscore1 if treat1, 
yaxis(2) 
lpattern(solid) 
lwidth(thick)
ytitle("Treated Density", axis(2)) 
title("{it:A. Full Sample}", size(medium) color(black))
legend(off)
graphregion(fcolor(white) color(white) icolor(white)) saving("$output/pscore1.gph", replace);
#delimit cr;

#delimit ;
twoway kdensity pscore1 if ~treat1 & pscore1>.1 & pscore1<.9, 
yaxis(1)  
lpattern(dash) 
lwidth(thick)
xtitle("Propensity Score") 
ytitle("Untreated Density", axis(1)) || 
kdensity pscore1 if treat1 & pscore1>.1 & pscore1<.9, 
yaxis(2) 
lpattern(solid) 
lwidth(thick)
ytitle("Treated Density", axis(2)) 
title("{it:B. Trimmed Sample, [.1,.9]}", size(medium) color(black))
legend(order(1 2) 
label(1 "Untreated") 
label(2 "Treated") 
region(style(none))  size(medium)) 
graphregion(fcolor(white) color(white) icolor(white)) saving("$output/pscore2.gph", replace);
#delimit cr;

graph combine "$output/pscore1.gph" "$output/pscore2.gph", imargin(tiny) col(1) xsize(8.5) ysize(11) xcommon graphregion(fcolor(white) color(white) icolor(white) margin(tiny))
graph export "$output/figureD3.wmf", replace

