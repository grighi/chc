/***********************************************
FIGURE 2. AMR by Age Group, 1959-1988
***********************************************/
clear
clear matrix
clear mata
set mat 1000
set more off, perm
pause on
capture log close
log using "$output/log_figure2", replace text	

*collapse mean annual mortality rates by group
use fips stfips cofips year amr* imr nnmr pnmr copop* births if year<1989 using "$data\aer_data",clear

*make manual weights for each age group's mortality rate
cap drop testo
egen testo = total(copop), by(year)
replace amr  = amr*(copop/testo)

cap drop testo
egen testo = total(births), by(year)
replace imr  = imr*(births/testo)
replace nnmr  = nnmr*(births/testo)
replace pnmr  = pnmr*(births/testo)

cap drop testo
egen testo = total(copop_ch), by(year)
replace amr_ch  = amr_ch*(copop_ch/testo)

cap drop testo
egen testo = total(copop_ad), by(year)
replace amr_ad  = amr_ad*(copop_ad/testo)

cap drop testo
egen testo = total(copop_eld), by(year)
replace amr_eld  = amr_eld*(copop_eld/testo)

collapse (sum) amr imr nnmr pnmr amr_ch amr_ad amr_eld, by(year)

graph set window fontface "Times New Roman"							

*AMR
#delimit ;
scatter amr year,
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
title("{it: A. Age-Adjusted Mortality}", size(medmsall) color(black))
graphregion(fcolor(white) color(white) icolor(white)) saving(panel1, replace);
#delimit cr;	

*Infant Mortality
#delimit ;
scatter imr nnmr pnmr year,
connect(l l l l l)		
cmissing(n n n n n)
msymbol(i Oh S)	
msize(i medsmall medsmall)
mcolor(navy maroon forest_green)	
lpattern(solid dash dash)
lwidth(thick medium medium) 
lcolor(navy maroon forest_green)
ylabel(#4, labsize(small))
xlabel(1959(5)1984 1988, labsize(small))
legend(off)
text(27 1964 "Total", size(small) place(e) yaxis(1)) 
text(14 1959 "Neonatal", size(small) place(e) yaxis(1)) 
text(3 1959 "Post-Neonatal", size(small) place(e) yaxis(1)) 
xtitle("")
ytitle("Deaths per 1,000 Live Births")
title("{it: B. Infants}", size(medmsall) color(black))
graphregion(fcolor(white) color(white) icolor(white)) saving(panel2, replace);
#delimit cr;	

*Child and Adult Mortality
#delimit ;
scatter amr_ch amr_ad year,
connect(l l l l l)	
cmissing(n n n n n)
msymbol(i i)	
mcolor(navy maroon )
lpattern(solid dash dash)
lwidth(thick thick medthick)
lcolor(navy maroon)	
ylabel(#4, labsize(small))
xlabel(1959(5)1984 1988, labsize(small))
legend(off)
xtitle("Year")
ytitle("Deaths per 100,000 Residents")
title("{it: C. Children (1-19) and Adults (20-49)}", size(medmsall) color(black))
text(250 1980 "Adults", size(medsmall))
text(100 1965 "Children", size(medsmall))			
graphregion(fcolor(white) color(white) icolor(white)) saving(panel3, replace);
#delimit cr;	

*Older Adult Mortality
#delimit ;
scatter amr_eld year,
connect(l l l l l)	
cmissing(n n n n n)
msymbol(i i)	
mcolor(navy maroon )
lpattern(solid dash dash)
lwidth(thick medthick medthick)
lcolor(navy maroon)	
ylabel(#4, labsize(small))
xlabel(1959(5)1984 1988, labsize(small))
legend(off)
xtitle("Year")
ytitle("Deaths per 100,000 Residents")
title("{it: D. Older Adults (50+)}", size(medmsall) color(black))
graphregion(fcolor(white) color(white) icolor(white)) saving(panel4, replace);
#delimit cr;	


graph combine panel1.gph panel2.gph panel3.gph panel4.gph, col(2) imargin(tiny) xcommon xsize(8.5) ysize(5.5) graphregion(fcolor(white) color(white) icolor(white))
graph display, xsize(8.5) ysize(5.5)

graph export "$output/figure2.wmf", replace

forval i = 1/4{
	erase panel`i'.gph
}
