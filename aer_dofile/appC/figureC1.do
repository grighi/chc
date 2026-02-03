/***********************************************
FIGURE C1. AMR by Cause Trends by Age
***********************************************/
clear
clear matrix
clear mata
set mat 1000
set more off, perm
pause on
capture log close
log using "$output/log_figureC1", replace text	

graph set window fontface "Times New Roman"							
local name2 	"A. Cardiovascular Disease"
local name3 	"B. Cerebrovascular Disease"
local name4 	"C. Cancer"		
local name5 	"D. Infectious Disease"
local name6 	"E. Diabetes"
local name7 	"F. Accidents"		


*collapse mean annual mortality rates for children
use fips stfips cofips year amr* copop* if year<1989 using "$data\aer_data",clear

collapse (mean) amr_ch* [aw=copop_ch], by(year)
forval c = 2/7{
	#delimit ;
	scatter amr_ch_`c' year,
	connect(l l l l l)										/*add lines*/
	cmissing(n n n n n)
	msymbol(i i)	
	mcolor(navy maroon )									/*marker color scheme*/ 
	lpattern(solid dash dash)
	lwidth(thick medthick medthick) 						/*line widths*/
	lcolor(navy maroon)										/*line color scheme*/ 
	ylabel(#4, labsize(small))
	xlabel(1959(5)1984 1988, labsize(small))
	xtitle("")
	ytitle("Deaths per 100,000 Residents")			
	title("{it: `name`c''}", size(medmsall) color(black))
	graphregion(fcolor(white) color(white) icolor(white)) saving(panel`c', replace);
	#delimit cr;	
}

graph combine panel2.gph panel3.gph panel4.gph panel5.gph panel6.gph panel7.gph, col(2) imargin(tiny) xcommon ycommon xsize(8.5) ysize(5.5) graphregion(fcolor(white) color(white) icolor(white))
graph display, xsize(7.5) ysize(10)

graph export "$output/figureC1A.wmf", replace

forval i = 2/7{
	erase panel`i'.gph
}


*collapse mean annual mortality rates for adults
use fips stfips cofips year amr* copop* if year<1989 using "$data\aer_data",clear

collapse (mean) amr_ad* [aw=copop_ad], by(year)
forval c = 2/7{
	#delimit ;
	scatter amr_ad_`c' year,
	connect(l l l l l)										/*add lines*/
	cmissing(n n n n n)
	msymbol(i i)	
	mcolor(navy maroon )									/*marker color scheme*/ 
	lpattern(solid dash dash)
	lwidth(thick medthick medthick) 						/*line widths*/
	lcolor(navy maroon)										/*line color scheme*/ 
	ylabel(#4, labsize(small))
	xlabel(1959(5)1984 1988, labsize(small))
	xtitle("")
	ytitle("Deaths per 100,000 Residents")			
	title("{it: `name`c''}", size(medmsall) color(black))
	graphregion(fcolor(white) color(white) icolor(white)) saving(panel`c', replace);
	#delimit cr;	
}

graph combine panel2.gph panel3.gph panel4.gph panel5.gph panel6.gph panel7.gph, col(2) imargin(tiny) xcommon ycommon xsize(8.5) ysize(5.5) graphregion(fcolor(white) color(white) icolor(white))
graph display, xsize(7.5) ysize(10)

graph export "$output/figureC1B.wmf", replace

forval i = 2/7{
	erase panel`i'.gph
}



*collapse mean annual mortality rates for older adults
use fips stfips cofips year amr* copop* if year<1989 using "$data\aer_data",clear

collapse (mean) amr_eld* [aw=copop_eld], by(year)
forval c = 2/7{
	#delimit ;
	scatter amr_eld_`c' year,
	connect(l l l l l)										/*add lines*/
	cmissing(n n n n n)
	msymbol(i i)	
	mcolor(navy maroon )									/*marker color scheme*/ 
	lpattern(solid dash dash)
	lwidth(thick medthick medthick) 						/*line widths*/
	lcolor(navy maroon)										/*line color scheme*/ 
	ylabel(#4, labsize(small))
	xlabel(1959(5)1984 1988, labsize(small))
	xtitle("")
	ytitle("Deaths per 100,000 Residents")			
	title("{it: `name`c''}", size(medmsall) color(black))
	graphregion(fcolor(white) color(white) icolor(white)) saving(panel`c', replace);
	#delimit cr;	
}

graph combine panel2.gph panel3.gph panel4.gph panel5.gph panel6.gph panel7.gph, col(2) imargin(tiny) xcommon ycommon xsize(8.5) ysize(5.5) graphregion(fcolor(white) color(white) icolor(white))
graph display, xsize(7.5) ysize(10)

graph export "$output/figureC1C.wmf", replace

forval i = 2/7{
	erase panel`i'.gph
}



