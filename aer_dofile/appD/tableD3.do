/**************************************************************
Table D3: SRH of more/less recent local migrants in CHC areas
**************************************************************/
clear
clear matrix
clear mata
set mat 1000
set more off, perm
pause on
capture log close
log using "$output/log_tableD3", replace text	

use "$data/aer_nhc.dta", clear
egen agecat = cut(age), at(0,1,15,50,100)
gen white = int_race_r==1
gen mig5 = hh_neighb_r<4 if hh_neighb_r>0
	
gen hpoor = h_subj==3|h_subj==4 if h_subj<=4 & h_subj>=1
gen knew = hh_knew_comphc==1 if hh_knew_comphc>-99 & hh_knew_comphc<.

*migration
char hh_neighb_r[omit] 1
xi i.hh_neighb_r 
keep if hh_neighb_r>-99 & hh_neighb_r<.
keep if agecat==50

*poor health t-tests
ttest hpoor if hh_neighb_r==1|hh_neighb_r==2, by(hh_neighb_r)
local hp2 = r(p)
ttest hpoor if hh_neighb_r==1|hh_neighb_r==3, by(hh_neighb_r)
local hp3 = r(p)
ttest hpoor if hh_neighb_r==1|hh_neighb_r==4, by(hh_neighb_r)
local hp4 = r(p)

ttest knew if hh_neighb_r==1|hh_neighb_r==2, by(hh_neighb_r)
local kp2 = r(p)
ttest knew if hh_neighb_r==1|hh_neighb_r==3, by(hh_neighb_r)
local kp3 = r(p)
ttest knew if hh_neighb_r==1|hh_neighb_r==4, by(hh_neighb_r)
local kp4 = r(p)

gen mig = 1
keep if hh_neighb_r>0 & hh_neighb_r<.
collapse (sum) mig (mean) hpoor knew, by(hh_neighb_r)
egen tm = total(mig)
replace mig = mig/tm
gen hp = .
replace hp = `hp2' in 2
replace hp = `hp3' in 3
replace hp = `hp4' in 4

gen kp = .
replace kp = `kp2' in 2
replace kp = `kp3' in 3
replace kp = `kp4' in 4

order hh_neighb_r mig hpoor hp knew kp tm

export excel "$output/tableD3.xls", replace firstr(var)


exit
