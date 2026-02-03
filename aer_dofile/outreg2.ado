*! outreg2 2.0.3 Jul2008 by roywada@hotmail.com
*! based on outreg 3.0.6/4.0.0 by john_gallup@alum.swarthmore.edu

program define outreg2Main
version 8.2

* write formatted regression output to file
syntax [anything] using [,			 	///
	drop(string)					///
	keep(string)					///
	APpend 						///
	REPLACE						///	
	SEEout						///
	LABel 						///
	LABelA(passthru)					///
	TItle(passthru) 					///
	CTitle(passthru) 					///
	Onecol 						///
	TEX							///
	TEX1(passthru)					///
	WORD							///
	EXCEL							///
	TEXT							///
	LONG							///
	SIDEway						///
	COMma 						///
	Quote							///
	noNOTes 						///
	ADDNote(passthru) 				///
	BDec(numlist int >=0 <=11) 			///
	BFmt(passthru) 					///
	AUTO(integer 3)					///
	LESS(integer 0)					///
	STats(passthru)					///
	ESTATS(passthru)					///
	noSE							///
	TSTAT							///
	Pvalue 						///
	CI 							///
	BEtaco 						///
	Level(integer $S_level)				///
	Tdec(numlist int >=0 <=11 max=1) 		///
	noPAren						///
	PARenthesis(string)				///
	BRacket						///
	BRacketA(string)					///
	ENClose(string)					///
	noASter 						///
	2aster 						///
	ALPHA(passthru)					///
	SYMbol(passthru)	 				///
	ASTERisk(passthru)				///
	EQuation(string)					///
	noCONs 						///
	noNI 							///
	noR2 							///
	ADJr2 						///
	E(string)						///
	RDec(numlist int >=0 <=11 max=1) 		///
	ADDStat(passthru) 				///
	ADDText(string) 					///
	ADEc(numlist int >=0 <=11) 			///
	EForm 						///
	MFX							///
	Margin1 						///
	Margin2(string)] 					///



***  a partial list of original macro names
* eqlist is the equation names within multi-equation models
* neq is the number of equation
* varlist is the variables requested by the user
* tdec specifies the decimal points for pvalue
* numi is e(N_g), the xt number of groups
* noNI is user request to not to report xt number of groups
* ivar is the e(ivar), the id for xt


*** ereturn matrix names
if "`mfx'"~="mfx" {
	cap confirm matrix e(b)
	if _rc {
		* it does not exist
		di in red "matrix e(b) not found; need to run regression first"
		exit 111
	}
}
local ebnames "e(b)"
local eVnames "e(V)"
if "`mfx'"=="mfx" {
	cap confirm matrix e(Xmfx_dydx)
	if _rc {
		* it does not exist
		di in red "matrix e(Xmfx_dydx) not found; need to run {cmd:mfx} first"
		exit 111
	}
	local ebnames "e(Xmfx_dydx)"
	local eVnames "e(Xmfx_se_dydx)"
	local eXnames "e(Xmfx_X)"
}


*** default warnings
if "`replace'"=="replace" & "`append'"=="append" {
	di in green "replaced when both {opt replace} and {opt append} chosen"
	local replace "replace"
	local append ""
}

*** set default options
if "`replace'"=="" & "`append'"=="" {
	local append "append"
}


* noSE: because se indicates stn.err, convert noSE into something else
if "`se'"=="nose" {
	local se_skip "se_skip"
}

* stats( ) is not compatible with two-column options
if "`stats'"~="" {
	if "`se'"=="nose" {
		di in red "cannot specify both {opt st:ats( )} and {opt nose} options"
		exit 198
	}
	if "`ci'"=="ci" {
		di in red "cannot specify both {opt st:ats( )} and {opt ci} options"
		exit 198
	}
	if "`tstat'"=="tstat" {
		di in red "cannot specify both {opt st:ats( )} and {opt tstat} options"
		exit 198
	}
	if "`pvalue'"=="pvalue" {
		di in red "cannot specify both {opt st:ats( )} and {opt p:value} options"
		exit 198
	}
	if "`beta'"=="beta" {
		di in red "cannot specify both {opt st:ats( )} and {opt be:ta} options"
		exit 198
	}
}



* always se instead of tstat
if "`tstat'"~="tstat" & "`pvalue'"~="pvalue" & "`ci'"~="ci" & "`betaco'"~="betaco" {
	local se "se"
}
else {
	local se ""
}



*** clean up file name, enclose .txt if no file type is specified
local rest "`using'"
* strip off "using"
gettoken part rest: rest, parse(" ")
* strip off quotes
gettoken first second: rest, parse(" ")
local rest: list clean local(rest)

local rabbit `"""'
if index(`"`using'"', ".")==0 {
	local file = `"`rabbit'`first'.txt`rabbit'"'
	local using = `"using `file'"'
}
else {
	local file = `"`rabbit'`first'`rabbit'"'
	local using = `"using `file'"'
	
	* put a warning on .xls extensions
/*
	local open=index(".xls")
	local temp=trim(substr("`using'",`open'+1,`close'-`open'-1))
	local temp1=trim(substr("`anything'",1,`open'-1))
	local temp2=trim(substr("`anything'",`close'+1,length("`anything'")))
	local varlist=trim("`temp1' `temp2'")
*/
}


*** confirm the output file existance, to be adjusted later
cap confirm file `file'
if !_rc {
	* it exists
	local fileExist 1
}
else {
	local fileExist 0
}






*** mainfile
* cleaning the user provided inputs

if "`long'"=="long" & "`onecol'"=="onecol" {
	di in yellow "{opt long} implies {opt o:necol} (no need to specify both)"
}

if "`long'"=="long" & "`onecol'"~="onecol" {
	local onecol "onecol"
}


if ("`tstat'"!="")+("`pvalue'"!="")+("`ci'"!="")+("`betaco'"!="")>1 {	
	di in red "choose only one of tstat, pvalue, ci, or beta"
	exit 198
}

if `level'<10 | `level'>99 {
	di in red "level() invalid"
	exit 198
}


if `"`paren'"'=="noparen" & `"`enclose'"'~="" {
	di in red "cannot choose both {opt nopa:ren} and {opt enc:lose} option"
	exit 198
}


if `"`paren'"'=="noparen" & `"`parenthesis'"'~="" {
	di in red "cannot choose both {opt nopa:ren} and {opt paren:thesis()} option"
	exit 198
}

if `"`paren'"'=="noparen" & `"`bracketA'"'~="" {
	di in red "cannot choose both {opt nopa:ren} and {opt br:acket()} option"
	exit 198
}

if  `"`bracket'"'~="" & `"`bracketA'"'~="" {
	di in red "cannot choose both {opt br:acket} and {opt br:acket()} option"
	exit 198
}


if "`aster'"=="noaster" & ("`asterisk'"~="" | "`symbol'"!="") {
	if "`asterisk'"~="" {
		di in red "cannot choose both {opt noaster} and {opt asterisk( )}"
	}
	else {
		di in red "cannot choose both {opt noaster} and {opt symbol( )}"
	}
	exit 198
}



if (`"`addnote'"'!="" & "`append'"=="append" & `fileExist'==1) {
	di in yellow "warning: addnote ignored in appended columns"
}


*** LaTeX options
local tex = ("`tex'"!="")
if "`tex1'"!="" {
	if `tex' {
		di in red "may not specify both {opt tex} and {opt tex()} options"
		exit 198
	}
	local tex 1
	gettoken part rest: tex1, parse(" (")
	gettoken texopts zilch: rest, parse(" (") match(parns) /* strip off "tex1()" */
}


*** label options
if "`label'"=="label" & "`labelA'"~="" {
	di in red "cannot specify both {opt lab:el} and {opt lab:el()} options"
	exit 198
}
if "`labelA'"~="" {
	gettoken part rest: labelA, parse(" (")
	gettoken labelOption zilch: rest, parse(" (") match(parns) /* strip off "label()" */
	local labelOption=trim("`labelOption'")
	if "`labelOption'"~="insert" {
		di in red "cannot specify any option other than {opt insert} for {opt lab:el( )}"
		exit 198
	}
	else if "`labelOption'"~="insert" {
		local label "label"
	}
}



*** setting the decimals
if `"`bfmt'"'~="" {
	local bdec = 3
}


* tdec (option) vs. dec2 (#)
if "`tdec'"~="" {
	local dec2 = "`tdec'"
}
else {
	if "`ci'"=="ci" {
		*local dec2 = `bdec'
		local dec2 = 3
	}
	else if "`pvalue'"=="pvalue" {
		local dec2 = 3
	}
	else {
		local dec2 = 3
	}
}


if "`rdec'"=="" {
	local rdec = `dec2'
}
if (`"`addstat'"'=="" & "`adec'"!="" & "`e'"=="" ) {
	di in red "cannot choose adec option without addstat option"
	exit 198
}
if "`adec'"=="" {
	* disabled
	* local adec = `dec2'
}


if "`quote'"!="quote" {
	local quote "noquote"
}


*** portion here snipped away to outside the bracket below

tempname df_r

if "`margin1'"~="" | "`margin2'"~="" {
	if "`mfx'"=="mfx" {
		di in red "cannot specify both {opt mfx} and {opt margin} options"
		exit 198
	}
	
	local margin = "margin"
	if "`margin2'"~="" {
		local margucp "margucp(_`margin2')"
		scalar `df_r' = .
		if "`margin1'"~="" {
			di in red "may not specify both margin and margin()"
			exit 198
		}
	}
	else {
		if "`cmd'"=="tobit" {
			di in red "dtobit requires margin({u|c|p}) after dtobit command"
			exit 198
		}
	}
}



*** separate the varist from the estimates names
local open=index("`anything'","[")
local close=index("`anything'","]")

if `open'~=0 & `close'~=0 {
	local estimates=trim(substr("`anything'",`open'+1,`close'-`open'-1))
	local temp1=trim(substr("`anything'",1,`open'-1))
	local temp2=trim(substr("`anything'",`close'+1,length("`anything'")))
	local varlist=trim("`temp1' `temp2'")
}
else {
	local varlist "`anything'"
}


*** varlist, keep, drop
if "`varlist'"~="" & "`keep'"~="" {
	di in yellow "{opt keep( )} supersedes {opt varlist} when both specified"
}
if "`drop'"~="" & "`keep'"~="" {
	di in red "cannot use both {opt keep( )} and {opt drop( )}"
	exit 198
}
if "`drop'"~="" & "`varlist'"~="" {
	di in red "cannot use both {it:varlist} and {opt drop( )}"
	exit 198
}

* unambiguate the names of stored estimates (wildcards)
if "`estimates'"~="" {
	local collect ""
	foreach var in `estimates' {
		local temp "_est_`var'"
		local collect "`collect' `temp'"
	}
	unab estimates : `collect'
	local collect ""
	foreach var in `estimates' {
		local temp=substr("`var'",6,length("`var'")-4)
		local collect "`collect'`temp' "
	}
	local estimates=trim("`collect'")
}

* or use est_expand



tempname estnameUnique
* a place holding name to the current estimates that has no name entered into the outreg

if "`estimates'"=="" {
	local estimates="`estnameUnique'"
}
else {
	local estimates: list uniq local(estimates)
}



*** titlefile needs set out here
tempfile titlefile


*** per Richard W., store the currently active estimates to be restored later
tempname coefActive
_estimates hold `coefActive', restore copy nullok



	*** stats( ) option cleanup : dealing with rows/stats to be reported per variable/coeff
	local statsValid "coef se tstat pval ci aster blank beta"
	* ci_low ci_hi level coef_eform se_eform coef_beta se_beta"
	
	local asterAsked 0
	local betaAsked ""
	
	if `"`estats'"'~="" {
		* the names of the available stats in e(matrices)
		local ematrices ""
		local var: e(matrices)
		
		*noi di in yellow "`var'"
		
		tokenize `var'
		local i=1
		while "``i''"~="" {
			*** di "e(``i'')" _col(25) "`e(``i'')'"
			local ematrices="`ematrices'``i'' "
			local i=`i'+1
		}
	}
	else if `"`stats'"'~="" {
		* take comma out
		local stats : subinstr local stats "stats(" " ", all
		local stats : subinstr local stats ")" " ", all
		local stats : subinstr local stats "," " ", all
		
		local statsPerCoef : word count `stats'
		local num=1
		local statsList ""
		
		while `num'<=`statsPerCoef' {
			local stats`num' : word `num' of `stats'
			
			* it must be one of the list
			local test 0
			foreach var in `statsValid' {
				if "`var'"=="`stats`num''" & `test'==0 {
					local test 1
				}
				
				* checking if aster/beta specified
				if "`stats`num''"=="aster" {
					local asterAsked 1
				}
				if "`stats`num''"=="beta" {
					local betaAsked "betaAsked"
				}
			}
			if `test'==0 {
				noi di in red "{opt `stats`num''} is not a valid option for {opt stats( )}"
				exit 198
			}
			local statsList "`statsList' `stats`num''"
			local num=`num'+1
		}
	}
	else if "`se_skip'"=="se_skip" {
		local statsPerCoef 1
		local statsList "coef"
	}
	else {
		local statsPerCoef 2
		
		if "`ci'"=="ci" {
			if "`eform'"=="eform" {
				local statsList "coefEform ciEform"
			}
			else {
				local statsList "coef ci"
			}
		}
		else if "`betaco'"=="betaco" {
			local statsList "coef betaco"
		}
		
		* regular: tstat, pval, or se
		else if "`eform'"=="eform" {
			local statsList "coefEform seEform"
			
			if "`tstat'"=="tstat" {
				local statsList "coefEform tstat"
			}
			else if "`pvalue'"=="pvalue" {
				local statsList "coefEform pval"
			}
		}
		else {
			local statsList "coef se"
			
			if "`tstat'"=="tstat" {
				local statsList "coef tstat"
			}
			else if "`pvalue'"=="pvalue" {
				local statsList "coef pval"
			}
		}
	}
	
	* when stats(aster) specified, aster( ) should not be attached to coef unless asked
	if `asterAsked'==1 & "`asterisk'"=="" {
		* the encased blank will trigger the parsing codes in coeftxt2
		local asterisk " "
	}
	
	* update stats( ) when eform specified
	if "`eform'"=="eform" {
		local statsList "`statsList' "
		local statsList : subinstr local statsList "coef " "coefEform ", all
		local statsList : subinstr local statsList "ci " "ciEform ", all
		local statsList : subinstr local statsList "se " "seEform ", all
	}
	
	*** run each estimates consecutively
	local estmax: word count `estimates'
	forval estnum=1/`estmax' {
		local estname: word `estnum' of `estimates'
		if "`estimates'"~="`estnameUnique'" {
			qui estimates restore `estname'
		}
		* to avoid overwriting after the first time, append from the second time around (1 of 3)
		if `estnum'==2 & "`replace'"=="replace" {
			local append "append"
			local replace ""
		}
		
		
		* the names of the available stats in e( )
		local result "scalars"
			* took out macros from the local result
		local elist=""
		foreach var in `result' {
			local var: e(`var')
			tokenize `var'
			local i=1
			while "``i''"~="" {
				*** di "e(``i'')" _col(25) "`e(``i'')'"
				local elist="`elist'``i'' "
				local i=`i'+1
			}
		}
		local elist: list uniq local(elist)
		
		* take out N (because it is always reported)
		local subtract "N"
		local elist : list elist - subtract		
		
		
		* r2 option
		* save the original for the first run and restore prior to each subsequent run
		if `estnum'==1 {
			local r2Save `"`r2'"'
		}
		else {
			local r2 `"`r2Save'"'
		}
		
		*** e(all) option
		* save the original for the first run and restore prior to each subsequent run
		if `estnum'==1 {
			local addstatSave `"`addstat'"'
		}
		else {
			local addstat `"`addstatSave'"'
		}
		
		
		*** dealing with e( ) option: put it through addstat( )
		* local= expression restricts the length		///
			requires a work-around to avoid subinstr/substr	functions
		
		* looking for "all" anywhere
		local position: list posof "all" in e
		
		if `"`addstat'"'~="" {
			if "`e'"~="" {
				local e: subinstr local e "," " ",all
				local e: list uniq local(e)
				
				if `position'~=0 {
					local count: word count `elist'
					local addstat=substr("`addstat'",1,length("`addstat'")-1)
					forval num=1/`count' {
						local wordtemp: word `num' of `elist'
						local addstat "`addstat',`wordtemp',e(`wordtemp')"
					}
				}
				else { /* other than all */
					local count: word count `e'
					local addstat=substr("`addstat'",1,length("`addstat'")-1)
					forval num=1/`count' {
						local wordtemp: word `num' of `e'
						local addstat "`addstat',`wordtemp',e(`wordtemp')"
					}
				}
				local addstat "`addstat')"
			}
		}
		
		
		* if addstat was previously empty
		else if "`addstat'"=="" {
			if "`e'"~="" {
				local e: subinstr	local e "," " ",all
				local e: list uniq local(e)
				
				if `position'~=0 {
					local count: word count `elist'
					local addstat "addstat("
					forval num=1/`count' {
						local wordtemp: word `num' of `elist'
						local addstat "`addstat'`wordtemp',e(`wordtemp')"
						if `num'<`count' {
							local addstat "`addstat',"
						}
					}
				}
				else {
					local count: word count `e'
					local addstat "addstat("
					forval num=1/`count' {
						local wordtemp: word `num' of `e'
						local addstat "`addstat'`wordtemp',e(`wordtemp')"
						if `num'<`count' {
							local addstat "`addstat',"
						}
					}
				}
				local addstat "`addstat')"
			}
		}
		
		
		
		
		*** dealing with single/multiple equations
		* getting equation names
		local eqlist: coleq `ebnames'
		local eqlist: list clean local(eqlist)
		local eqlist: list uniq local(eqlist)
		
		
		* counting the number of equation
		local eqnum: word count `eqlist'
		* local eqnum : list sizeof eqlist
		
		
		* 0 if it is multiple equations; 1 if it is a single
		if 1<`eqnum' & `eqnum'<. {
			local univar=0
		}
		else {
			local univar=1
		}
		
		
		tempname regN rsq numi r2mat
		tempname varname coefcol b vc b_alone convert
		
		**** snipped portion moved here from above
		* for svy commands with subpop(), N_sub is # of obs used for estimation
		local cmd = e(cmd)
		
		local svy = substr("`cmd'",1,3)
		if "`svy'"=="svy" & e(N_sub) != . {
			scalar `regN' = e(N_sub)
		}  
		else {
			scalar `regN' = e(N)
		}
		
		scalar `df_r' = e(df_r)
		local	depvar  = e(depvar)
		
		mat `b'=`ebnames'
		mat `vc'=`eVnames'
		
		if "`mfx'"=="mfx" {
			mat `vc' = `vc'' * `vc'
		}
		
		local bcols=colsof(`b')	/* cols of b */
		local bocols=`bcols'	/* cols of b only, w/o other stats */
		
		* the work around for xtmixed
		if "`e(N_g)'"=="matrix" {
			mat `convert'=e(N_g)
			scalar `numi'=`convert'[1,1]
		}
		else {
			scalar `numi'	= e(N_g)
		}
		
		local	robust  = e(vcetype)
		if "`robust'"=="." {
			local robust "none"
		}
		local	ivar	 = e(ivar)
		* equals one if true	
		capture local fracpol = (e(fp_cmd)=="fracpoly")
		**** snipped portion end
		
		
		*** parse addstat to convert possible r(), e(), and s() macros to numbers
		* clear newadd every time
		local newadd=""
		* (to avoid conflicts with r-class commands used in this program)
		if `"`addstat'"'!="" {
			gettoken part rest: addstat, parse(" (")
			gettoken part rest: rest, parse(" (") /* strip off "addstat(" */
			local i = 1
			while `"`rest'"' != "" {
				gettoken name rest : rest, parse(",") quote
				if `"`name'"'=="" {
					di in red "empty strings not allowed in addstat() option"
					exit 6
				}
				gettoken acomma rest : rest, parse(",")
				gettoken valstr rest : rest, parse(",")
				if `"`rest'"' == "" { /* strip off trailing parenthesis */
					local valstr = substr(`"`valstr'"',1,length(`"`valstr'"')-1)
					local comma2 ""
				}
				else {
					gettoken comma2 rest: rest, parse(",")
				}
				
				
				*local value = `valstr'
				*capture confirm number `value'
				*if _rc!=0 {
				* 	* di in red `"`valstr' found where number expected in addstat() option"'
				* 	* exit 7
				*}
				
				
				* creating e(p) if missing
				if ("`valstr'"=="e(p)" | trim("`valstr'")=="e(p)") & "`e(p)'"=="" {
					if "`e(df_m)'"~="" & "`e(df_r)'"~="" & "`e(F)'"~="" {
						local valstr = Ftail(`e(df_m)',`e(df_r)',`e(F)')
					}
					else if "`e(df_m)'"~="" & "`e(chi2)'"~="" {
						local valstr = chi2tail(`e(df_m)',`e(chi2)')
					}
				}
				
				local value=`valstr'
				capture confirm number `value'
				
				if _rc==0 {
					* it's a number
					
					local value = `valstr'
					
					local count: word count `adec'
					local aadec : word `i' of `adec'
					
					* coding check: di "adec `adec' i `i' count `count' name `name' value `value'"
					* runs only if the user defined adec is absent for that number
					if `i'>`count' & `i'<. {
							* auto-digits: auto( )
						autodigits2 `value' `auto'
							* needs to be less than 11
						local valstr = string(`value',"%12.`r(valstr)'")
						if "`valstr'"=="" {
							local valstr .
						}
						local newadd `"`newadd'`name'`acomma'`valstr'`comma2'"'
					}
					else {
						* using previous ones if no other option
						if "`aadec'"=="" {
							local aadec `prvadec'
						}
						local valstr = string(`value',"%12.`aadec'f")
						local newadd `"`newadd'`name'`acomma'`valstr'`comma2'"'
						local prvadec = `aadec'
					}
				}
				else {
					* it's a text
					
					local value `"`valstr'"'
					local newadd `"`newadd'`name'`acomma'`valstr'`comma2'"'
				}
				
				local i = `i'+1
			}
			local addstat `"`newadd'"'
		}
		
		*** logistic reports coeffients in exponentiated form (odds ratios)
		if "`cmd'"=="logistic" {
			local eform "eform"
		}
		
		* report the constant anyway
		*if "`eform'"=="eform" {
		*	local cons "nocons"
		*}
		
		
		
		
		
		*** multi-equation models
		if "`cmd'"=="mvreg" | "`cmd'"=="sureg" | "`cmd'"=="reg3" {
			local univar = 0 /* multivariate regression (multiple equations) */
			if "`onecol'" != "onecol" {
				mat `r2mat' = `ebnames' /* get column labels */
				local neq = e(k_eq)
			***D	local eqlist = e(eqnames)
				local depvar = "`eqlist'"
				if "`cmd'"=="mvreg" {
					local r2list = e(r2)
				}
				local eq = 1
				while `eq' <= `neq' {
					if "`cmd'"=="mvreg" {
						local r2str: word `eq' of `r2list'
						scalar `rsq' = real("`r2str'")
					}
					else {
						scalar `rsq' = e(r2_`eq')
					}
					mat `r2mat'[1,`eq'] = `rsq'
					local eq = `eq' + 1
				}
			}
			else {
				/* if onecol */
				local r2 = "nor2"	
				scalar `rsq' = .
			}
		} /* `rsq' after `r2list' to avoid type mismatch */
		
		else if "`adjr2'"=="adjr2" {
			scalar `rsq' = e(r2_a)
			if `rsq' == . {
				di in red "Adjusted R-squared (e(r2_a)) not defined; cannot use adjr2 option"
				exit 198
			}
		}
		else {
			scalar `rsq' = e(r2)
		}
		
		if ("`cmd'"=="intreg" | "`cmd'"=="svyintrg" | "`cmd'"=="xtintreg") {
			local depvar : word 1 of `depvar' /* 2 depvars listed */
		}
		
		* nolabels for anova and fracpoly
		if ("`cmd'"=="anova" | `fracpol' | "`cmd'"=="nl") {
			/* e(fp_cmd)!=. means fracpoly */
			local cons "nocons"
		}
			
		*** margin or dprobit: substitute marginal effects into b and vc
		else if ("`cmd'"=="dprobit" | "`margin'"=="margin") {
			if "`cmd'"=="dlogit2" | "`cmd'"=="dprobit2" | "`cmd'"=="dmlogit2" {
				di in yellow "warning: margin option not needed"
			}
			else {
				marginal2, b(`b') vc(`vc') `se' `margucp'
				local bcols = colsof(`b') /* cols of b */
				local bocols = `bcols' /* cols of b only, w/o other stats */
				if "`cmd'"=="dprobit" {
					local cons "nocons"
				}
			}
		}
		
		
		*** to handle single or multiple equations
		local neq = `eqnum'
		local eqlist "`eqlist'"
		if "`onecol'"=="onecol" | `univar'==1 {
			if "`depvar'"=="" {
				local depvar: rowname `ebnames'
				local depvar: word 1 of `depvar'
			}
		}
		
		*** the column title:
		* ctitle1: from ctitle, estimates name; or depvar
		* ctitle2: sometimes depvar
		* save the original ctitle for the first run and restore prior to each subsequent run
		if `estnum'==1 {
			local ctitleSave `"`ctitle'"'
		}
		else {
			local ctitle `"`ctitleSave'"'
		}
		
		* set ctitle1 with ctitle
		
		
		
		*** clean up column titles
		* parse title
		if `"`ctitle'"'~="" {
			partxtl3 `"`ctitle'"'
			local temp = `r(numtxt)'
			local t = 1
			while `t'<=`temp' {
				local ctitle`t' `r(txt`t')'
				local t = `t'+1
			}
			
			* temporary fix
			local ctitle `"`ctitle1'"'
		}
		
		
		
		if (`"`ctitle1'"'=="" ) & (`univar'==1|"`onecol'"=="onecol") {
			if "`estname'"== "`estnameUnique'" {
				* singles here
				local ctitle1=`"`depvar'"'
				*local ctitle2=""
				
				if `univar'~=1 & "`onecol'"=="onecol" {
					local temp=proper("`e(cmd)'")
					local ctitle=`"`temp'"'
					*local ctitle2=""
				}
			}
			else {
				local ctitle=`"ctitle(`estname')"'
				*local ctitle2=""
				if `univar'==1 & "`onecol'"~="onecol" {
					local ctitle1=`"`estname'"'
					*local ctitle2="`depvar'"
				}
			}
		}
		* ctitle2 is set inside sideway loop
		
		*** lots of codes removed from here
		* tested: dlogit2 and dprobit2
		* leaves the equation names: bivariate probit, heckman, heckprob
		* leaves the choice names: mlogit
		* not sure: dmlogit2
		* not tested: svymlog
		
		
		*** when `ebnames' includes extra statistics (which don't have variable labels)
		capture mat `b_alone' = `b'[1,"`depvar':"]
		
		if _rc==0 {
			local bocols = colsof(`b_alone')
		}
		else if ("`cmd'"=="ologit" | "`cmd'"=="oprobit") {
			local bocols = e(df_m)
			mat `b_alone' = `b'[1,1..`bocols']
		}
		else if ("`cmd'"=="cnreg" | ("`cmd'"=="tobit" & "`margin'"~="margin")) {
			local bocols = `bocols'-1 /* last element of `ebnames' is not est coef */
			mat `b_alone' = `b'[1,1..`bocols']
		}
		else if ("`cmd'"=="intreg" | "`cmd'"=="svyintrg") {
			mat `b_alone' = `b'[1,"model:"]
			local bocols = colsof(`b_alone')
		}
		else if ("`cmd'"=="truncreg") {
			mat `b_alone' = `b'[1,"eq1:"]
			local bocols = colsof(`b_alone')
		}
		*if `univar' & `bcols'>`bocols' & "`xstats'"!="xstats" {
		*	mat `b' = `b_alone'
		*	mat `vc' = `vc'[1..`bocols',1..`bocols']
		*}
		*if "`xstats'"=="xstats"& `bcols'==`bocols' {
		*	di in yellow "warning: no extra statistics - xstats ignored"
		*}
		
		
		* keep these here for sideway option
		local statsListKeep "`statsList'"
		local statsPerCoefKeep "`statsPerCoef'"
		
		
		*** create table with coeftxt2 and append to existing table
		* NOTE: coeftxt2 command is rclass
		qui {
			cap preserve
			
			*** make univariate regression table (single equation)
			if `univar'==1 | "`onecol'"=="onecol" {
				
				* changing the equation name of univariate case for housekeeping purposes
				if `univar'==1 & "`onecol'"=="onecol" {
					* attach equation marker for onecol output; it sorts better
					mat colnames `b'= "`depvar':"
				}
				
				
				*** sideway single equation
				
				if "`sideway'"=="sideway" {
					local sidewayRun "`statsPerCoefKeep'"
					local statsPerCoef 1
				}
				else {
					local sidewayRun 1
				}
				
				forval countingNum=1/`sidewayRun' {
					if "`sideway'"=="sideway" {
						local var: word `countingNum' of `statsListKeep'
						local statsList "`var'"
						local ctitle2 "`var'"
					}
					
					
					
					* to avoid overwriting after the first time, append from the second time around (2 of 3)
					if `countingNum'==2 & "`replace'"=="replace" {
						local append "append"
						local replace ""
					}
					
					coeftxt2 `varlist', keep(`keep') drop(`drop') eqlist(`eqlist') `betaAsked' statsPerCoef(`statsPerCoef') statsList(`statsList') `se_skip' `se' `pvalue' `ci' `betaco' `tstat' level(`level') bdec(`bdec') `bfmt' dec2(`dec2') tdec(`tdec') `paren' parenthesis(`parenthesis') `bracket' bracketA(`bracketA')  `aster' `symbol' enclose(`enclose') `cons' `eform' `nobs' `ni' `r2' `adjr2' rdec(`rdec') ctitle1(`ctitle1') ctitle2(`ctitle2') addstat(`addstat') addtext(`addtext') adec(`adec') `notes' `addnote' `append' regN(`regN') df_r(`df_r') rsq(`rsq') numi(`numi') ivar(`ivar') depvar(`depvar') robust(`robust') borows(`bocols') b(`b') vc(`vc') varname(`varname') coefcol(`coefcol') univar(`univar') `onecol' estname(`estname') auto(`auto') estnameUnique(`estnameUnique') fileExist(`fileExist') less(`less') alpha(`alpha') asterisk(`asterisk') `2aster' 
					
					*if "`append'"~="append" {
					if "`append'"~="append" & `countingNum'==1 {
						* replace
						noi outsheet `varname' `coefcol' `using', nonames `quote' `comma' replace
						local fileExist 1
					}
					
					else {
						*** appending
						* confirm the existence of the output file
						local rest "`using'"
						* strip off "using"
						gettoken part rest: rest, parse(" ")
						if `fileExist'==1 {
							appfile2 `using', varname(`varname') coefcol(`coefcol') titlefile(`titlefile') `sideway' `onecol'
							noi outsheet v* `coefcol' `using', nonames `quote' `comma' replace
							drop v*
						}
						else {
							* does not exist and therefore needs to be created
							noi outsheet `varname' `coefcol' `using', nonames `quote' `comma' replace
							local fileExist 1
						}
					}
					restore, preserve
				} /* sideway single equation */
			}
			
			*** make multiple equation regression table
			else {
				
*************** ought to be fixed to handle multiple ctitle/eq or update the help file

				*if `"`ctitle'"'!="" & `"`ctitle'"'!=`"ctitle("")"' {		/* parse ctitle */
				*	partxtl2 `"`ctitle'"'
				*	local nct = r(numtxt)	/* number of ctitles */
				*	local n = 1
				*	while `n'<=`nct' {
				*		local ctitl`n' `r(txt`n')'
				*		local n = `n'+1
				*	}
				*}
				*else {
				*	local nct=0
				*}
				local nct=0
				
				*if `"`ctitle2'"'~="" {
				*	local nct=1
				*}
				tempname b_eq vc_eq
				
				* getting the depvar list from eqlist
				local eq = 1
				while `eq' <= `neq' {
					*if `eq' <= `nct' {
					*	local ctitle1 `"`ctitl`eq'')"'
					*	local ctitle2 ""
					*}
					local eqname: word `eq' of `eqlist'
					local depvar: word `eq' of `eqlist'
					
					if `nct'==0 {
						if "`estname'"~="`estnameUnique'" & "`estname'"~="" {
							local ctitle ""
							if `eq'==1 {
								local ctitle "ctitle(`estname')"
							}
							local ctitle2 "`depvar'"
						}
						else {
							local ctitle1 "`depvar'"
							local ctitle2 ""
						}
					}
					
					
					*** r2mat doesn't exist for mlogit ="capture" 
					
					capture scalar `rsq' = `r2mat'[1,`eq']	
					mat `b_eq' = `b'[.,"`eqname':"]
					matrix colnames `b_eq' = _:				/* remove roweq from b_eq for explicit varlist */
					mat `vc_eq' = `vc'["`eqname':","`eqname':"]
					local bocols = colsof(`b_eq')
					
					
					
					
					*** sideway multiple equation
					
					if "`sideway'"=="sideway" {
						local sidewayRun "`statsPerCoefKeep'"
						local statsPerCoef 1
					}
					else {
						local sidewayRun 1
					}
					
					local doit 0
					if "`sideway'"=="sideway" & "`ctitle2'"=="" {
						local doit 1
					}
					forval countingNum=1/`sidewayRun' {
						if "`sideway'"=="sideway" & `doit'==1 {
							local var: word `countingNum' of `statsListKeep'
							local ctitle2 "`var'"
							local statsList "`var'"
						}
						
						
						* to avoid overwriting after the first time, append from the second time around (3 of 3)
						if `countingNum'==2 & "`replace'"=="replace" {
							local append "append"
							local replace ""
						}
						
						
						*if `eq'>1 {
						if `eq'>1 & `countingNum'>1 {
							local addstat ""
						}
						
						*if `eq' == 1 & "`append'"!="append" {
						if `eq' == 1 & "`append'"!="append" & `countingNum'==1 {
							local apptmp ""
						}
						else {
							local apptmp "append"
						}
						
						coeftxt2 `varlist', keep(`keep') drop(`drop') eqlist(`eqlist') `betaAsked' statsPerCoef(`statsPerCoef') statsList(`statsList') `se_skip' `se' `pvalue' `ci' `betaco' `tstat' level(`level') bdec(`bdec') `bfmt' dec2(`dec2') tdec(`tdec') `paren'  parenthesis(`parenthesis') `bracket' bracketA(`bracketA') `aster' `symbol' enclose(`enclose') `cons' `eform' `nobs' `ni' `r2' `adjr2' rdec(`rdec') ctitle1(`ctitle1') ctitle2(`ctitle2') addstat(`addstat') addtext(`addtext') adec(`adec') `notes' `addnote' `apptmp' regN(`regN') df_r(`df_r') rsq(`rsq') numi(`numi') ivar(`ivar') depvar(`depvar') robust(`robust') borows(`bocols') b(`b_eq') vc(`vc_eq') varname(`varname') coefcol(`coefcol') univar(`univar') `onecol' estname(`estname') auto(`auto') estnameUnique(`estnameUnique') fileExist(`fileExist') less(`less') alpha(`alpha') asterisk(`asterisk') `2aster'
						
						* create new file: replace and the first equation		
						*if `eq' == 1 & "`append'"!="append" {
						if `eq' == 1 & "`append'"!="append" & `countingNum'==1 {
							noi outsheet `varname' `coefcol' `using', nonames `quote' `comma' `replace'
							local fileExist 1
						}
						* appending here: another estimates or another equation
						else {
							* confirm the existence of the output file
							local rest "`using'"
						 	* strip off "using"
							gettoken part rest: rest, parse(" ")
							if `fileExist'==1 {
								* it exists: keep on appending even if it's the first equation
								appfile2 `using', varname(`varname') coefcol(`coefcol') titlefile(`titlefile') `sideway' `onecol'
								noi outsheet v* `coefcol' `using', nonames `quote' `comma' replace
								drop v*
							}
							else {
								* does not exist and specified append: need to be created for the first equation only
								*if `eq' == 1 & "`append'"=="append" {
								if `eq' == 1 & "`append'"=="append" & `countingNum'==1 {
									noi outsheet `varname' `coefcol' `using', nonames `quote' `comma' `replace'
									local fileExist 1
								}
							}
						}
						
						restore, preserve
					}  /* sideway multiple equation */
					
					local eq = `eq' + 1
					
					*restore, preserve /* to access var labels after first equation */
				}
			}	
		}		/* for quietly */
	}		/* run each estimates consecutively */
	
	
	
			
			* local file defined earlier, strip off quotes and extension
			gettoken first second: file, parse(" ")
			local beg_dot = index(`"`first'"',".")
			local strippedname = substr(`"`first'"',1,`=`beg_dot'-1')
			
	
	quietly {
		
		*** get the label names
		if "`label'"=="label" | "`labelOption'"=="insert" {
			tempfile labelfile
			
			gen str7 var1=""
			gen str7 labels=""
			unab varlist_all : *
			cap unab subtract: _est_*
			local varlist_only : list varlist_all - subtract
			local count=1
			foreach var in `varlist_only' {
				local lab: var label `var'
				local lab=trim("`lab'")
				if "`lab'"~="" {
					replace var1="`var'" in `count'
					replace labels="`lab'" in `count'
					local count=`count'+1
				}
			}
			
			keep var1 labels
			
			drop if var1==""
			
			* indicate no label contained
			if `=_N'==0 {
				local emptyLabel=1
			}
			else {
				local emptyLabel=0
			}
			
			* add constant
			set obs `=_N+1'
			
			replace labels="Constant" in `=_N'
			replace var1="Constant" in `=_N'
			
			save `"`labelfile'"'
		}
		
		
		tempvar id1 id2 id3 id4
		insheet `using', nonames clear
		
		*** clean up equation names, title, label
		gen id1=_n
		gen str7 equation=""
		gen str7 variable=""
		
		* take care if colon (:) that may appears in the notes by limiting the search to the above
		gen temp=1 if v1=="Observations"
		replace temp=1 if temp[_n-1]==1
		count if temp==1
		
		drop temp
		
		forval num=1/`=_N-`r(N)'' {
			local name=trim(v1[`num'])
			local column=index("`name'",":")
			if `column'~=0 {
				local equation=trim(substr("`name'",1,`column'-1))
				local variable=trim(substr("`name'",`column'+1,length("`name'")))
				replace equation="`equation'" in `num'
				replace variable="`variable'" in `num'
			}
		}
		
		
		replace equation=equation[_n-1] if equation=="" & equation[_n-1]~="" & v1~="Observations"
		
		gen top="1" if equation[_n]~=equation[_n-1] & equation[_n]~=""
		
		count if equation~=""
		if `r(N)'~=0 {
			* move equation names, instead of inserting them
			if v1[1]~="EQUATION" & v1[2]~="EQUATION" {
				gen v0=equation
				if v1[3]~="" {
					replace v0="EQUATION" in 1
				}
				else {
					replace v0="EQUATION" in 2
				}
				order v0
				replace v1=variable if variable~=""
			}
			else {
				replace v1=equation
				if v2[3]~="" {
					replace v1="EQUATION" in 1
				}
				else {
					replace v1="EQUATION" in 2
				}
			}
		}
		
		* strips the redundant equation names
		* must be undone at the insheet that recall this file in appfile2
		
		count if equation~=""
		if `r(N)'~=0 {
			*** for one column option
			replace v0="" if top=="" & _n>2
		}
		
		drop id1 equation variable        top
		
		outsheet `using', nonames `quote' `comma' replace
		
		
		*** clean up labels
		if "`label'"=="label" | "`labelOption'"=="insert" {
			/*
			local dotloc = index("`bname'", ".") 
			if `dotloc'!=0 {
				/* deal w/time series prefixes */
				local tspref = substr("`bname'",1,`dotloc')
				local bname = substr("`bname'",`dotloc'+1,.)
				local blabel : var label `bname'
				local blabel = `"`tspref' `blabel'"'
			}
			else {
				local blabel : var label `bname'
			}
			*/
			
			ren v1 var1
			gen `id2'=_n
			
			* skip merging process if no label was contained
			if `emptyLabel'==1 {
				gen str7 labels=""
			}
			else {
				joinby var1 using `"`labelfile'"', unmatched(master)
				drop _merge
			}
			
			sort `id2'
			drop `id2'
			order var1 labels
			cap order v0 var1 labels
			if var1[3]~="" {
				replace labels="LABELS" in 1
			}
			else {
				replace labels="LABELS" in 2
			}
			ren var1 v1
		}
		
		
		if `"`title'"'=="" {
			* NOTE: v0- saved here
			tempfile appending
			tempvar tomato potato
			gen `tomato' =_n+10000
			save `"`appending'"',replace
			
			*** Clean up titles
			* just coef, no label, no equation
			cap confirm file `titlefile'
			if !_rc {
				use `titlefile',clear
				
				*************d change this in the next update
				gen `id3'=1 if v1=="COEFFICIENT" | v1=="VARIABLES"
				replace `id3'=1 if `id3'[_n-1]==1
				drop if `id3'==1
				keep if v1~=""
				if `=_N'~=0 {
					keep v1
					gen `potato'=_n
					local titlrow=_N
					joinby v1 using `"`appending'"', unmatched(both)
					sort `potato' `tomato'
					drop _merge `potato' `tomato'
					aorder
				}
				else {
					use `"`appending'"',replace
					drop `tomato'
				}
			}
			cap drop `tomato'
		}
		
		else {
			* parse title
			partxtl2 `"`title'"'
			local titlrow = `r(numtxt)'
			local t = 1
			while `t'<=`titlrow' {
				local titl`t' `r(txt`t')'
				local t = `t'+1
			}
			
			local oldN=_N
			set obs `=`r(numtxt)'+_N'
			gen `id4'=_n+10000
			forval num=1/`r(numtxt)' {
				replace v1="`r(txt`num')'" in `=`oldN'+`num''
				replace `id4'=`num' in `=`oldN'+`num''
			}
			sort `id4'
			drop `id4'
		}
		
		if "`titlrow'"=="" {
			local titlrow=0
		}
				
		outsheet `using', nonames `quote' `comma' replace
		
		*** preparing for outputs and seeout
		ren v1 coef
		cap ren v0 eq
		
		unab vlist : v*
		local count: word count `vlist'
		forval num=1/`count' {
			local vname: word `num' of `vlist'
			ren `vname' v`num'
		}
		
		
		* number of columns
		local numcol = c(k)
		tempvar blanks rowmiss
		gen int `blanks' = (trim(v1)=="")
		
		foreach var of varlist v* {
			replace `blanks' = `blanks' & (trim(`var')=="")
		}
		
		* in case ctitle is missing: 1 of 2
		* colheadN

		*************d change this in the next update
		replace `blanks'=0 if coef=="COEFFICIENT" | coef[_n-1]=="COEFFICIENT" | coef=="VARIABLES" | coef[_n-1]=="VARIABLES"
		if `count'==1 {
			local colheadN=2+`titlrow' 
		}
		else {
			local colheadN=3+`titlrow'
		}
		
		* statistics rows
		count if `blanks'==0
		local strowN `=`titlrow'+`r(N)''
		
		*** making alternative output files
		if "`long'"=="long" | `tex'==1 | "`word'"=="word" | "`excel'"=="excel" | "`text'"=="text" {
			
			if "`text'"=="text" | ("`long'"=="long" & "`onecol'"=="onecol") {
				local dot=index(`"`using'"',".")
				if `dot'~=0 {
					local before=substr(`"`using'"',1,`dot'-1)
					local after=substr(`"`using'"',`dot'+1,length("`using'"))
					
					*local usingLong=`"`before'_long.`after'"'
					local usingLong=`"`before'_exact.`after'"'
				}
			}
			
			
			*** convert the data into long format (insert the equation names if they exist)
			if "`long'"=="long" & "`onecol'"=="onecol" {		
				* a routine to insert equation names into coefficient column
				count if `blanks'==0 & eq~="" & eq~="EQUATION"
				tempvar id5
				gen float `id5'=_n
				local _firstN=_N
				set obs `=_N+`r(N)''
				
				local times 1
				forval num=2/`_firstN' {
					if eq[`num']~="" & eq[`num']~="EQUATION" {
						replace `id5'=`num'-.5 in `=`_firstN'+`times''
						replace coef=eq[`num'] in `=`_firstN'+`times'' 
						local times=`times'+1
					}
				}
				
				sort `id5'
				drop eq `id5' `blanks'
				
				* change `strowN' by the number of equations inserted
				local strowN=`strowN'+`r(N)'
				
				
				* v names
				unab vlist : *
				local count: word count `vlist'
				forval num=1/`count' {
					local vname: word `num' of `vlist'
					ren `vname' c`num'
				}
				forval num=1/`count' {
					local vname: word `num' of `vlist'
					ren c`num' v`num'
				}
				
				
				if "`text'"=="text" {
					noi outsheet v* `usingLong', nonames `quote' `comma' replace
				}
				
			} /* long format */
			
			else {
				drop `blanks'
				
				* v names
				unab vlist : *
				local count: word count `vlist'
				forval num=1/`count' {
					local vname: word `num' of `vlist'
					ren `vname' c`num'
				}
				forval num=1/`count' {
					local vname: word `num' of `vlist'
					ren c`num' v`num'
				}
			}
			
			
			*** label replacement
			if "`label'"=="label" {
				if ("`long'"~="long" & "`onecol'"~="onecol") | ("`long'"=="long" & "`onecol'"=="onecol") {
					replace v2=v1 if v2==""
					drop v1
					forval num=1/`c(k)' {
						ren v`=`num'+1' v`num'
					}
					
					* change LABELS to VARIABLES in 1/3
					replace v1="VARIABLES" in 1/3 if v1=="LABELS"
				}
				else if "`long'"~="long" & "`onecol'"=="onecol" {
					replace v3=v2 if v3==""
					drop v2
					forval num=2/`c(k)' {
						ren v`=`num'+1' v`num'
					}
					
					* change LABELS to VARIABLES in 1/3
					replace v2="VARIABLES" in 1/3 if v2=="LABELS"
				}
				
				
				* create new text file
				* do it for _long file as well
				if "`text'"=="text" {
					
					
				}
			}
			else if "`labelOption'"=="insert" {
				* label inserted earlier
			}
			
			
			tempfile outing
			save `outing'
			
			
			******* strippedname creation moved up (erase)
			
			
			*** Text thing
			if "`text'"=="text" & "`label'"=="label" {
				* produce verbatim text
				noi outsheet v* `usingLong', nonames `quote' `comma' replace
			}
			
			
			*** LaTeX thing
			if `tex' {
				
				* make certain `1' is not `using' (another context)
				
				out2tex2 v* `using', titlrow(`titlrow') colheadN(`colheadN') strowN(`strowN') `texopts' replace
				local usingTerm `"`strippedname'.tex"'
				
				*local cl `"{stata "shellout using `usingTerm'":"`usingTerm'"}"'
				local cl `"{stata "shellout using `usingTerm'":`usingTerm'}"'
				
				noi di as txt `"`cl'"'
			}
			
			
			*** Word rtf file thing
			if "`word'"=="word" {
				use `outing',clear
				
				
				* there must be varlist to avoid error
				
			      out2rtf2 v* `using',  titlrow(`titlrow') colheadN(`colheadN') strowN(`strowN') replace nopretty
				local temp `r(documentname)'
				
				* strip off "using" and quotes
				gettoken part rest: temp, parse(" ")
				gettoken usingTerm second: rest, parse(" ")
				
				* from school
				*local cl `"{stata shell winexec cmd /c tommy.rtf & exit `usingTerm' & EXIT :`usingTerm' }"'
				* these work but leaves the window open
				*local cl `"{stata winexec cmd /c "`usingTerm'" & EXIT :`usingTerm'}"'
				*local cl `"{stata shell "`usingTerm'" & EXIT :`usingTerm'}"'
				*local cl `"{stata shell cmd /c "`usingTerm'" & EXIT :`usingTerm'}"'
				
				*local cl `"{stata "shellout using `usingTerm'":"`usingTerm'"}"'
				local cl `"{stata "shellout using `usingTerm'":`usingTerm'}"'
				
				noi di as txt `"`cl'"'
			}
			
			
			*** Excel xml file thing
			if "`excel'"=="excel" {
				use `outing',clear
				
				
				if c(stata_version)<9 & c(stata_version)~=. {
					noi di in yellow "{opt excel} requires Stata 9 or higher"
				}
				else {
					*tempfile fileXml
					*xmlsave `"`strippedname'.xml"',doctype(excel) replace legible
					
					xmlout using `"`strippedname'"'
					
					local usingTerm `"`strippedname'.xml"'
					
					*local cl `"{stata "shellout using `usingTerm'":"`usingTerm'"}"'
					local cl `"{stata "shellout using `usingTerm'":`usingTerm'}"'
					
					noi di as txt `"`cl'"'
				}
			}
		} /* output files */
	}  /* quietly */













*** see the output
*if "`label'"=="label" {
if "`label'"=="label" | "`labelOption'"=="insert" {
	if "`seeout'"=="seeout" {
		*seeing `using', `label' label(`labelOption')
		
		if "`label'"=="label" {
			seeing `using', label
		}
		else {
			seeing `using', label(`labelOption')
		}
	}
	
	if "`label'"=="label" {
		*local cl `"{stata seeout `using', label:seeout}"'
		local usingTerm1 `"`strippedname'.txt"'
		local cl `"{stata "seeout using `usingTerm1', label":seeout}"'
	}
	else {
		*local cl `"{stata seeout `using', label(`labelOption'):seeout}"'		
		local usingTerm1 `"`strippedname'.txt"'
		local cl `"{stata "seeout using `usingTerm1', label(`labelOption')":seeout}"'
	}
	di as txt `"`cl'"'
}
else {
	if "`seeout'"=="seeout" {
		seeing `using'
	}
	
	*local cl `"{stata seeout `using':seeout}"'
	local usingTerm1 `"`strippedname'.txt"'
	local cl `"{stata "seeout using `usingTerm1'":seeout}"'
	di as txt `"`cl'"'
}



*** saving the current preferences
gettoken first second : 0, parse(",:. ")

while "`first'"~="," & "`first'"~="" {
	gettoken first second : second, parse(",:. ")
}

* strip seeout, replace
local second: subinstr local second "replace" "",all
local second: subinstr local second "seeout" "",all
local second: subinstr local second "seeou" "",all
local second: subinstr local second "seeo" "",all
local second: subinstr local second "see" "",all

*local second: list uniq local(second)
*local second: list retokenize second

* retokenize is dangerous for quotes; do it manually for double space
local secondClean ""
while `"`second'"'~="" {
	gettoken second1 second : second, parse(" ") quotes
	local secondClean `"`secondClean'`second1'"'
	if `"`second'"'~="" {
		local secondClean `"`secondClean' "'
	}
}

local pref `"`using'"'
local options `"`secondClean'"'

* NOTE: `0' is now overwritten
quietly findfile outreg2_prf.ado
tempname myfile

* capture for write protected files
cap file open `myfile' using `"`r(fn)'"', write text replace
cap file write `myfile'  `"`c(current_date)'"' _n
cap file write `myfile'  `"`pref'"' _n
cap file write `myfile'  `"`options'"'
cap file close `myfile'

* restoring the currently active estimate here
_estimates unhold `coefActive'

end		/* end of outreg2Main */




***********************


program define appfile2
version 8.2
* append regression results to pre-existing file

syntax using/, varname(string) coefcol(string) titlefile(string) [sideway onecol]

*** take out COEFFICIENT as the column heading and restore later
replace `varname' = "" in 1

* r2 issue: convert dot into empty, 2 of 2
*if "`rsq'"=="." {
*	local rsq==""
*}



* these tempname not necessary because preserve & no original variable names
*tempname Vord1 Vord2 vartmp varsml v2plus Vorder merge2



* pre-create Vorder here
gen Vorder2=_n/100 in 1/3

* Constant is now done as eqOrder0 + .5
*replace Vorder2=2 if `varname'=="Constant" /* ok because equation names would still be attached if present */

replace Vorder2=3 if `varname'=="Observations"
replace Vorder2=3.5 if Vorder2[_n-1]==3 | Vorder2[_n-1]==3.5
replace Vorder2=1 if Vorder2==. & (Vorder2[_n-1]<1 | Vorder2[_n-1]==1)
replace Vorder2=2.5 if `varname'=="" & (Vorder2[_n-1]==2 | Vorder2[_n-1]==2.5)


* generating order within each coefficient
gen Vorder2_0 = _n if Vorder2==1 & `varname'~=""
replace Vorder2_0 = Vorder2_0[_n-1]     if Vorder2_0==. & Vorder2==1

gen Vorder2_1 = _n if Vorder2==1 & `varname'~=""
replace Vorder2_1 = Vorder2_1[_n-1]+.01 if Vorder2_1==. & Vorder2==1




tempfile tmpf1

*gen str80 vartmp = substr(`varname',1,79)	/* room for "!" at end */
gen str8 vartmp = ""
replace vartmp = `varname'				/* room for "!" at end */

replace vartmp = "0" if _n==1
gen varsml = trim(vartmp)



* fill the spaces between the names
*replace `varname'=`varname'[_n-1] in 4/`=_N' if `varname'=="" & `varname'[_n-1]~=""



replace vartmp = vartmp[_n-1]+"!" if varsml==""
* add "!" to variable name to make it sort after previous variable name
* will cause bug if both "varname" and "varname!" already exist


count if (varsml=="" | (varsml[_n+1]=="" & _n!=_N))
local ncoeff2 = r(N)				/* number of estimated coefficients in file 2 */
local N2 = _N					/* number of lines in file 2 */
gen Vord2 = _n					/* ordering variable for file 2 */



ren varName varName2
ren eqName eqName2



* genderate eq_order2 (handles Constant within each equation)
gen eq_order2=0 in 3/`=_N' if eqName2~=""
replace eq_order2=1 in 3/`=_N' if eqName2[_n]~=eqName2[_n-1] & eqName2~=""
replace eq_order2=eq_order2+eq_order2[_n-1] if eq_order2+eq_order2[_n-1]~=.

drop varsml
sort vartmp



*keep `varname' `coefcol' vartmp Vord2

* eqName vs eqName2
keep `varname' `coefcol' vartmp Vord2 Vorder2 Vorder2_0 Vorder2_1 varName2 eqName2 eq_order2

save `"`tmpf1'"', replace







*** prepare the original text for merging

insheet using `"`using'"', nonames clear

*** save equation column if it exists before dropping it
local exists_eq=0
count if v1=="EQUATION"
if `r(N)'~=0 {
	gen v0=v1
	local exists_eq=1
	drop v1
	* count v0 as well
	forval num=2/`=c(k)' {
		ren v`num' v`=`num'-1'
	}
}
*** strip labels columns
/* throw away after a while Aug 2006
count if v2=="LABELS"
if `r(N)'~=0 {
	drop v2
	* count v0 as well
	forval num=2/`=c(k)' {
		ren v`num' v`=`num'-1'
	}
}
*/

count if v2=="LABELS"
if `r(N)'~=0 {
	drop v2
	* count v0 as well
	* cap is added to avoid the last column v0 being misnamed
	forval num=2/`=c(k)' {
		cap ren v`=`num'+1' v`num'
	}
}

*** save title first one only, before stripping coef columns
cap save `titlefile'
* drop titles

*************d change this in the next update
while (v1[1]~="COEFFICIENT" & v1[1]~="VARIABLES") & v1[1]~="" {
	drop in 1
}


*** finish cleaning the equation columns
* NOTE: assuming Observation exists

gen str8 varName1=""
gen str8 eqName1=""



if "`exists_eq'"=="1" {

	order v0
	*** Strip the equation names and slap it back onto the variable column
	
	/* works when exactly two stats per coefficients
	forval num=5/`=_N' {
		replace v0=v0[`num'-2] in `num' if v0[`num']=="" & v1[`num']~="" & v1[`num']~="Observations"
	}
	forval num=3/`=_N' {
		if v0[`num']~="" {
			local temp1=v0[`num']
			local temp2=v1[`num']
			replace v1="`temp1':`temp2'" in `num'
		}
	}
	*/
	
	replace v0=v0[_n-1] in 3/`=_N' if v0=="" & v0[_n-1]~="" & v1~="Observations"
	
	* genderate eq_order1 (allows handling of Constant within each equation later)
	gen eq_order1=0 in 3/`=_N' if v0~="" & v0~="EQUATION"
	replace eq_order1=1 in 3/`=_N' if v0[_n]~=v0[_n-1] & v0~=""
	replace eq_order1=eq_order1+eq_order1[_n-1] if eq_order1+eq_order1[_n-1]~=.
	
	* location of Constant within each equation (moved down, after merged)
	*replace eq_order1=eq_order1+.5 if v1=="Constant"
	*replace eq_order1=eq_order1[_n-1] if eq_order1<eq_order1[_n-1] & v1=="" & v0~=""
	
	replace eqName1=v0 in 3/`=_N'
	replace varName1=v1 in 3/`=_N'
	replace varName1=varName1[_n-1] in 3/`=_N' if varName1=="" & varName1[_n-1]~="" & varName1~="Observations"
	
	replace v1=v0 + ":" + v1 in 3/`=_N' if v0~="" & v1~=""
	drop v0
}
else if "`sideway'"=="sideway" & "`onecol'"=="onecol" {
	* special case for sideway and onecol
	* because sideway loops internally, EQUATION and LABELS columns does not exist
	* eqName and varName are still joined, they need to separated
	* v1 is as it should be
	
	*** borrowed from:
		*** clean up equation names, title, label
	gen id1=_n
	gen str7 equation=""
	gen str7 variable=""
	forval num=1/`=_N' {
		local name=trim(v1[`num'])
		local column=index("`name'",":")
		if `column'~=0 {
			local equation=trim(substr("`name'",1,`column'-1))
			local variable=trim(substr("`name'",`column'+1,length("`name'")))
			replace equation="`equation'" in `num'
			replace variable="`variable'" in `num'
		}
	}
	replace equation=equation[_n-1] if equation=="" & equation[_n-1]~="" & v1~="Observations"
	
	
	
	replace eqName1=equation if equation~=""
	replace varName1=variable if variable~=""
	drop equation variable id1
	
	
	gen eq_order1=0 in 3/`=_N' if eqName1~=""
	replace eq_order1=1 in 3/`=_N' if eqName1[_n]~=eqName1[_n-1] & eqName1~=""
	replace eq_order1=eq_order1+eq_order1[_n-1] if eq_order1+eq_order1[_n-1]~=.
}
else {
	*** eq names not present
	
	gen eq_order1=1
	replace eq_order1=. if v1=="Observations"
}



*** take out COEFFICIENT as the column heading and restore later
local coeftitle1 v1[1]
local coeftitle2 v1[2]
replace v1 = "" in 1/2

/*
* the 0 makes it match
if "`coeftitle1'"~="" & "`coeftitle2'"~="" {
	replace v1="0" in 1
}
*/


* getting the characteristics
describe, short
*local numcol = r(k)				/* number of columns already in file 1 */

* subtract 3 to account for eq_order1, varName1, eqName1
local numcol = r(k)-3				/* number of columns already in file 1 */

*gen str80 vartmp = substr(v1,1,79)		/* room for "!" at end */
gen str8 vartmp = ""
replace vartmp=v1					/* room for "!" at end */
local titlrow = (v1[1]!="")

* `titlrow'	is assumed to be zero
local frstrow = 1 + `titlrow'			/* first non-title row */


replace vartmp = "0" if _n==`frstrow' & v2=="(1)"
replace vartmp = "0!" if _n==`frstrow' & v2!="(1)"

gen long Vord1 = _n
*gen str80 v2plus = trim(v2)
gen str8 v2plus = ""
replace v2plus=trim(v2)


local col = 3
if `col'<=`numcol' {
	replace v2plus = v2plus + trim(v`col')
	local col = `col'+1
}
*count if ((v1=="" & v2plus!="") | (v1[_n+1]=="" & (v2plus[_n+1]!=""|_n==1) & _n!=_N))
	* i.e. a t stat or column heading
	* i.e. a coefficient (next row is a t stat)
* make it count empty ctitle2 with this code:
*tempvar topoff
gen topoff=1 if v1~=""
replace topoff=1 if topoff[_n-1]==1
replace topoff=sum(topoff)
count if (topoff==0 | (v1=="" & v2plus!="") | (v1[_n+1]=="" & (v2plus[_n+1]!=""|_n==1) & _n!=_N))
drop topoff

local ncoeff1 = r(N)

gen varsml = vartmp
summ Vord1 if Vord1>`ncoeff1' & v2plus!=""	/* v2plus for addstat */
local endsta1 = r(max)						/* calc last row of statistics before notes */

if `endsta1'==. {
	local endsta1 = `ncoeff1'
}


* fill the spaces between the names
*replace v1=v1[_n-1] in 3/`=_N' if v1=="" & v1[_n-1]~="" & v1~="Observations"


replace vartmp = vartmp[_n-1]+"!" if varsml==""


* pre-create Vorder here
gen Vorder1 = _n/100 in 1/3 if v1==""

* Constant is now done as eqOrder0 + .5
*replace Vorder1=2 if v1=="Constant" /* ok because equation names would still be attached if present */

replace Vorder1=3 if v1=="Observations"
replace Vorder1=3.5 if Vorder1[_n-1]==3 | Vorder1[_n-1]==3.5
replace Vorder1=1 if Vorder1==. & (Vorder1[_n-1]<1 | Vorder1[_n-1]==1)
replace Vorder1=4 if v2=="" & Vorder1==3.5

* Constant is now done as eqOrder0 + .5
*replace Vorder1=2.5 if v1=="" & (Vorder1[_n-1]==2 | Vorder1[_n-1]==2.5)


* generating order within each coefficient
gen Vorder1_0 =_n if Vorder1==1 & varsml~=""
replace Vorder1_0=Vorder1_0[_n-1]     if Vorder1_0==. & Vorder1==1

gen Vorder1_1 =_n if Vorder1==1 & varsml~=""
replace Vorder1_1=Vorder1_1[_n-1]+.01 if Vorder1_1==. & Vorder1==1


drop varsml
sort vartmp






*** merging the two files
merge vartmp using `"`tmpf1'"'


/* old codes for establishing Vorder
gen varsml = vartmp
gen Vorder = 1 if (Vord1<=`ncoeff1' | Vord2<=`ncoeff2')			/* coefficients */
replace Vorder = 0 if ((Vord1<=`titlrow') | (vartmp=="0" & _merge==2))	/* has title or has no column numbers */
replace Vorder = 2 if (varsml=="Constant" | varsml=="Constant!")		/* constant */
replace Vorder = 3 if Vorder==. & (Vord1<=`endsta1' | Vord2<=`N2')	/* statistics */
replace Vorder = 4 if Vorder==.							/* notes below statistics */
*/


* Vorder2 has the information for the top 0.01-0.03; hence use Vorder2 first
gen Vorder=Vorder2
replace Vorder=Vorder1 if Vorder==.


/* ad hoc fixes not needed
* manually fix, especially for single-equations
replace Vorder = 3 if v1=="Observations"
replace Vorder = 3 if v1=="R-squared"
* manually fix if not multipe-equation
if `exists_eq'~=1 {
	replace Vorder = 3 if v1=="Constant"
}
* merge==2 from using file
sort Vord2
replace Vord1=Vord1[_n+1]-1 if _merge==2
* merge==1 from master file
sort Vord1
replace Vord2=Vord2[_n-1]+1 if _merge==1*/


gen byte merge2 = _merge==2

* Notes and defintions:
* Vorder2    _n for master file
* Vorder1    _n for using file

* Vorder2_0  identifier for each coefficient (using _n for the top most stats)
* Vorder2_1  added 0.01 consequtively to bysort Vorder2_0

* Vorder1_0  identifier for each coefficient (using _n for the top most stats)
* Vorder1_1  added 0.01 consequtively to bysort Vorder1_0


*** this fills up the potential gaps in Vord1 and Vord2 if the number of stats( ) per coefficient is different
*order Vorder Vord1 Vord2 Vord*

sort Vorder2_1
replace Vord1=Vord1[_n-1]+.01 if (Vorder2_0==Vorder2_0[_n-1] & Vorder2_0~=.) & (Vord1==. & Vord1[_n-1]~=.) & Vorder==1

sort Vorder1_1
replace Vord2=Vord2[_n-1]+.01 if (Vorder1_0==Vorder1_0[_n-1] & Vorder1_0~=.) & (Vord2==. & Vord2[_n-1]~=.) & Vorder==1




*** new sorting rules
*** June 2008 Version
gen str8 eqName0=""
replace eqName0=eqName2
replace eqName0=eqName1 if eqName0=="" & eqName1~=""


sort eqName0 Vorder1_1


gen eq_order0=.
gen eq_temp=1 if eqName0[_n]~=eqName0[_n-1] & Vorder1_1~=.


************ sort to the existing column?
sort eq_temp Vorder1_1

************* needs beter levelsof code?
if "`exists_eq'"=="1" | ("`sideway'"=="sideway" & "`onecol'"=="onecol") {
	count if eq_temp==1
	if r(N)~=0 {
		forval num=1/`=r(N)' {
			local temp=eqName0[`num']
			
			* collecting names
			*local eqOrderList "`temp' `eqOrderList'"
			
			replace eq_order0=`num' if eqName0=="`temp'"
		}
		replace eq_order0=eq_order0+.5 if varName1=="_cons" | varName2=="_cons" | varName1=="Constant" | varName2=="Constant"
	}
}
else {
	
	* just a hack that pushes _cons toward the bottom
	replace eq_order0=1
	replace eq_order0=eq_order0+.5 if varName1=="_cons" | varName2=="_cons" | varName1=="Constant" | varName2=="Constant"
}




* for viewing
*sort eq_order0 Vorder1_1
*order eqName0 Vorder1_1 eq_order*



count if Vorder2_0>=1 & Vorder2_0<.
local countV2=r(N)

count if Vorder1_0>=1 & Vorder1_0<.
local countV1=r(N)

if `countV2'>`countV1' {
************************** these sorting makes difference?
	sort Vorder2_1
	sort Vorder1_1
	sort Vorder eq_order0 Vorder2_1 Vorder1_1 merge2
}
else {
	sort Vorder1_1
	sort Vorder2_1
	sort Vorder eq_order0 Vorder1_1 Vorder2_1 merge2

*order Vorder eq_order0 Vorder1_1 Vorder2_1 merge2


}


/* March 2008 Version
*** actual sorting

count if Vorder2_0>=1 & Vorder2_0<.
local countV2=r(N)

count if Vorder1_0>=1 & Vorder1_0<.
local countV1=r(N)

if `countV2'>`countV1' {
	sort Vorder Vord2 Vord1 merge2
	
	* use eq_order1 to place multiple-eq constant in the correct place
	replace eq_order1=eq_order1[_n-1] if v1=="" & eq_order1==.
	replace Vorder=Vorder+eq_order1/1000 if eq_order1~=.
	sort Vorder Vord2 Vord1 merge2
}
else {
	sort Vorder Vord1 Vord2 merge2
	
	* use eq_order1 to place multiple-eq constant in the correct place
	replace eq_order1=eq_order1[_n-1] if v1=="" & eq_order1==.
	replace Vorder=Vorder+eq_order1/1000 if eq_order1~=.
	sort Vorder Vord1 Vord2 merge2
}
*/

replace v1 = `varname' if v1=="" & `varname'!=""
*drop `varname' vartmp varsml Vorder Vord1 Vord2 merge2 _merge v2plus
drop `varname' vartmp         Vorder Vord1 Vord2 merge2 _merge v2plus Vorder1* Vorder2* eq* *Name*




if (`numcol'==2) {
	replace v2 = "(1)" if _n==`frstrow'
	replace `coefcol' = "(2)" if _n==`frstrow'
}
else {
	replace `coefcol' = "(" + string(`numcol') + ")" if _n==`frstrow'
}


* remove filled in names
*replace v1="" if v1~="" & v1==v1[_n-1] in 3/`=_N'


*** restore COEFFICIENT and 0 head
replace v1 = "" in 1
replace v1 = "VARIABLES" in 2

/* appfile2 */

end


***********************


program define marginal2
version 8.2
* put marginal effects (dfdx) into b and vc matrices 

syntax , b(string) vc(string) [se margucp(string)]

tempname dfdx se_dfdx new_vc dfdx_b2		
capture mat `dfdx' = e(dfdx`margucp')
if _rc==0 {
	local cnam_b : colnames `dfdx'
	local cnam_1 : word 1 of `cnam_b'
}
if _rc!=0 {
	if "`cnam_1'"=="c1" {
		di in yellow `"Update dprobit ado file: type "help update" in Stata"'
	}
		else {
		di in yellow "{opt margin} option invalid: no marginal effects matrix e(dfdx`margucp') exists"
	}
	exit
}


/* create matrix of diagonals for vc */
if "`se'"=="se" {
	if e(cmd)=="dprobit" | e(cmd)=="tobit" {
		if e(cmd)=="dprobit" {
			local margucp "_dfdx"
		}
		mat `se_dfdx' = e(se`margucp')
		mat `vc' = diag(`se_dfdx')
		mat `vc' = `vc' * `vc'
	}
	else {
		mat `vc' = e(V_dfdx)
	}
	mat colnames `vc' = `cnam_b'
}
else {
	/* if t or p stats reported then trick `cv' into giving the right t stat */
	local coldfdx = colsof(`dfdx')
	mat `new_vc' = J(`coldfdx',`coldfdx',0)
	local i = 1
	while `i' <= `coldfdx' {
		scalar `dfdx_b2' = (el(`dfdx',1,`i')/el(`b',1,`i'))^2
		mat `new_vc'[`i',`i'] = `dfdx_b2'*`vc'[`i',`i']
		local i = `i'+1
	}
	mat colnames `new_vc' = `cnam_b'
	mat `vc' = `new_vc'
}  
mat `b' = `dfdx'
end


***********************


program define partxtl2, rclass

version 8.2
*** parse text list to find number of text elements and return them
	local ntxt = 0
	gettoken part rest: 1, parse(" (") 
	gettoken part rest: rest, parse(" (")		/* strip off "option(" */
	while `"`rest'"' != "" {
		local ntxt = `ntxt'+1
		gettoken part rest: rest, parse(",)") 
		return local txt`ntxt' `"`part'"'
		gettoken part rest: rest, parse(",)")	/* strip off "," or "(" */
	}
	return local numtxt `ntxt'
end



***********************


*** this one avoids stripping the wrong parenthesis
program define partxtl3, rclass
version 8.2

*** parse text list to find number of text elements and return them
	local ntxt = 0
	
	local begin = index(`"`1'"',`"("')
	local length : length local 1
	local rest=substr(`"`1'"',`begin'+1,`length'-`begin'-1)
	
	while `"`rest'"' != "" {
		local ntxt = `ntxt'+1
		gettoken part rest: rest, parse(",") 
		return local txt`ntxt' `"`part'"'
		gettoken part rest: rest, parse(",")
	}
	return local numtxt `ntxt'
end


***********************


program define coeftxt2
version 8.2
* getting the coefficient name, values, and t statistics

syntax [varlist(default=none ts)] [, keep(string) drop(string) eqlist(string) betaAsked statsPerCoef(integer 2) statsList(string) se_skip SE Pvalue CI BEtaco Tstat Level(integer $S_level) BDEC(numlist) BFmt(passthru) DEC2(numlist) TDEC(numlist) noPAren  parenthesis(passthru) BRacket BRacketA(passthru) noASter SYMbol(passthru) enclose(passthru) noCONs EForm noNOBs noNI noR2 ADJr2 RDec(numlist) ctitle1(string) ctitle2(string) ADDStat(passthru) ADDText(passthru) ADEC(numlist) noNOTes ADDNote(passthru) APpend regN(string) df_r(string) rsq(string) numi(string) ivar(string) depvar(string) robust(string) BOROWS(string) b(string) vc(string) varname(string) coefcol(string) univar(string) Onecol estname(string) AUTO(integer 3) estnameUnique(string) fileExist(integer 1) less(integer 0) ALPHA(string) asterisk(passthru) 2aster]

* r2 issue: convert dot into empty, 1 of 2
*if "`rsq'"=="." {
*	local rsq==""
*}




* varname already defined
tempvar beta st_err

* these tempvar taken out or added prefix
*mrgrow astrix

/* moved down
if "`betaco'"=="betaco" {
	tempname betcoef
}
*/


*tempfile bcopy
tempname b_alone vc_alon b_xtra vc_xtra
*t_alpha


* avoid re-transposing them later by giving names
tempname b_transpose vc_diag_transpose

mat `b_transpose' = `b''
mat `vc_diag_transpose' = vecdiag(`vc')
mat `vc_diag_transpose' = `vc_diag_transpose''


local brows = rowsof(`b_transpose')

*** setting ctitle1
local coltitl `"`ctitle1'"'

*** setting ctitle2
local coltit2=`"`ctitle2'"'

*** xt options
if (`numi'!=. & "`ni'"!="noni") {
	if `"`iname'"'=="" {
		local iname "`ivar'"
	}
	if `"`iname'"'=="." {
		local iname "groups"
	}
}








/******* disabled, taken out xstats

* fill in "beta" "st. err." & "tstat" variables from regression output
* in case varlist is specified:

mat `b_alone' = `b_transpose'[1..`borows',1] /* use to avoid _cons in xtra stats */

if `brows'>`borows' {
	/* allow for xtra stats */
	local borows1 = `borows'+1
	mat `b_xtra' = `b_transpose'[`borows1'...,1...]
	mat `vc_xtra' = `vc_diag_transpose'[`borows1'...,1...]
}

if "`varlist'"!="" {
	
	* slap equation names onto varlist to handle _cons correctly
	if `univar'==0 & "`onecol'"=="onecol"  {
		local newlist ""
		
		/* add the constant unless "nocons" is chosen */		
		if "`cons'"~="nocons" {
			local temp "_cons"
		}
		else {
			local temp ""
		}
		
		local eqnum: word count `eqlist'
		local varnum: word count `varlist'
		
		forval num1=1/`eqnum' {
			local var1: word `num1' of `eqlist'
			local eq_exist 0
			
			forval num2=1/`varnum' {
				local var2: word `num2' of `varlist'
				
				local newname "`var1':`var2'"
				local j = rownumb(`b_alone',"`newname'")
				if `j'~=. {
					*noi di in red "`j' `newname'"
					local eq_exist 1
					local newlist "`newlist' `newname'"
				}
			}
			* avoid reporting _cons when no other coefficients exists
			if `eq_exist'==1 & "`cons'"~="nocons" {
				local newname "`var1':_cons"
				local newlist "`newlist' `newname'"
			}
		}
		local varlist "`newlist'"
	}
	else {
		* add the constant unless "nocons" is chosen
		if "`cons'"!="nocons" {
			local varlist "`varlist' _cons"
		}
	}
	
	
	tempname arow testnew newb newvc
	local vname : word 1 of `varlist'
	local i=1
	while "`vname'"!="" {
		local j = rownumb(`b_alone',"`vname'")
 		if `j'!=. {
			matrix `arow' = `b_transpose'[`j',1...]			/* "..." needed to get rownames */
			matrix `newb' = nullmat(`newb')\ `arow'
			matrix `arow' = `vc_diag_transpose'[`j',1...]
			matrix `newvc' = nullmat(`newvc')\ `arow'
		}
		else if (`univar' & "`vname'"!="_cons") {
			di in red "`vname' not found in regression coefficients"
			exit 111
		}
		local i = `i'+1
		local vname : word `i' of `varlist'
	}
	mat `b_alone' = `newb'
	if `brows'>`borows' {
		* allow for xtra stats
		mat `newb' = `newb'\ `b_xtra'
		mat `newvc' = `newvc'\ `vc_xtra'
	}
	mat `b_transpose' = `newb'
	mat `vc_diag_transpose' = `newvc'
}
else if "`cons'"=="nocons" {
	******************* nocons needs to be fixed when the location of constant is no longer at end when multiple equation
	******************* nocons also crashes when it cases empty matrix
	* delete the constant if "nocons" is chosen
	local j_1 = rownumb(`b_alone',"_cons")-1
	if `j_1'!=. {
		/* in case there is no constant in b */
		mat `b_alone' = `b_alone'[1..`j_1',1...]
		mat `vc_alon' = `vc_diag_transpose'[1..`j_1',1...]
		if `brows'==`borows' {
			mat `b_transpose' = `b_alone'
			mat `vc_diag_transpose' = `vc_alon'
		}
		else {
			* allow for xtra stats
			mat `b_transpose' = `b_alone' \ `b_xtra'
			mat `vc_diag_transpose' = `vc_alon' \ `vc_xtra'
		}
	}
}


local borows = rowsof(`b_alone')
local brows = rowsof(`b_transpose') /* reset brows */

gen double b`beta' = matrix(`b_transpose'[_n, 1]) in 1/`brows'
gen double s`st_err' = matrix(`vc_diag_transpose'[_n, 1]) in 1/`brows'
replace s`st_err' = sqrt(s`st_err')

/* moved down
if "`eform'"=="eform" {
	* exponentiate beta and st_err
	replace b`beta' = exp(b`beta')
	replace s`st_err' = b`beta'*s`st_err'
}
*/


* create "beta" coefficients regardless
if "`betaco'"=="betaco" {
	sum `depvar' if e(sample)
	gen `betcoef' = b`beta'/r(sd) in 1/`brows'
}

* fill in variables names column
gen str31 `varname' = ""
local bnames : rowfullnames(`b_alone')

local bname : word 1 of `bnames'
local i 1

*** _cons is replaced by Constant
* to make the replacement in presence of equation name: take the equation name off for constant only
while "`bname'"!="" {
	tokenize "`bname'", parse(":")
	local temp="`3'"
	
	if "`bname'"!="_cons" & "`3'"~="_cons" {
		* codes cut out: labels with time-series put into blabel if nolabel not specified
		local blabel ""
		if "`betaco'"=="betaco" & "`onecol'"=="onecol" {
			/* create "beta" coefficients */
			sum `3' if e(sample)
			replace `betcoef' = r(sd)*`betcoef' if `i'==_n
		}
		else if "`betaco'"=="betaco" {
			/* create "beta" coefficients */
			sum `bname' if e(sample)
			replace `betcoef' = r(sd)*`betcoef' if `i'==_n
		}
	}
	else {
		local blabel "Constant"
	}
	
	if "`temp'"=="_cons" {
		local blabel "`1':Constant"
	}
	
	if `"`blabel'"'=="" { 
		local blabel "`bname'" 
	}
	replace `varname' = trim(`"`blabel'"') if `i'==_n
	local i = `i'+1
	local bname : word `i' of `bnames'
}


if `brows'>`borows' {
	* allow for xtra stats
	local borows1 = `borows'+1
	mat `b_xtra' = `b_transpose'[`borows1'...,1...]
	local bnames : rownames(`b_xtra')
	local beqs : roweq(`b_xtra')
	local bname : word 1 of `bnames'
	local beq : word 1 of `beqs'
	local i 1
	while "`bname'"!="" {
		if "`bname'"!="_cons" {
			if "`beq'"=="_" {
				local blabel "`bname'"
			}
			else {
				local blabel "`beq':`bname'"
			}
		}
		else {
			local blabel "`beq':Constant"
		}
		replace `varname' = `"`blabel'"' if `i'+`borows'==_n
		local i = `i'+1
		local bname : word `i' of `bnames'
		local beq : word `i' of `beqs'
	}
}
**************** taken out xstats **********/






****** replacement codes

tempvar `beta' `st_err'
tempvar first second varKeepDrop


*** fill in variables names column
gen str5 `varname' = ""
gen str5 `first' = ""
gen str5 `second' = ""

local Names : rowfullnames(`b_transpose')
local Rows = rowsof(`b_transpose')

forval num=1/`Rows' {
	local temp : word `num' of `Names'
	
	tokenize "`temp'", parse(":")
	
	if "`2'"==":" {
		replace `first' = "`1'" in `num'
		replace `second' = "`3'" in `num'
	}
	else {
		replace `second' = "`temp'" in `num'
		replace `varname' = "`temp'" in `num'
	}
}
replace `varname' = "Constant" if `first'=="" & `second'=="_cons"
replace `varname' = `first' + ":" + `second' if `first'~=""
replace `varname' = `first' + ":Constant" if `first'~="" & `second'=="_cons"

gen double b`beta' = matrix(`b_transpose'[_n, 1]) in 1/`brows'
gen double s`st_err' = matrix(`vc_diag_transpose'[_n, 1]) in 1/`brows'
replace s`st_err' = sqrt(s`st_err')


*** beta coefficient here
if "`betaco'"=="betaco" | "`betaAsked'"=="betaAsked" {
	tempname betcoef
	
	sum `depvar' if e(sample)
	local betaSD `r(sd)'
	gen `betcoef' =.
	
	forval num=1/`Rows' {
		local temp=`second'[`num']
		cap sum `temp' if e(sample)
		replace `betcoef' = r(sd)/`betaSD' * b`beta' if `num'==_n & `second'~="_cons"
	}
	
	* set nocons
	local cons "nocons"
}



*** starting to kee/drop here (data changes)

* varlist/keep
if "`keep'"~="" {
	tsunab keep : `keep'
	local varlist "`keep'"
}


* varlist
if "`varlist'"~="" {
	gen str5 `varKeepDrop'=""
	* add the constant unless "nocons" is chosen
	if "`cons'"~="nocons" {
		local varlist "`varlist' _cons"
	}
	
	local count: word count `varlist'
	forval num=1/`count' {
		local temp : word `num' of `varlist'
		replace `varKeepDrop'="`temp'" if "`temp'"==`second'
	}
	
	count if `varKeepDrop'=="" & `second'~=""
	local brows=`brows'-r(N)
	local borows=`borows'-r(N)
	drop if `varKeepDrop'=="" & `second'~=""
}


* drop
if "`drop'"~="" {
	tsunab drop : `drop'
	gen str5 `varKeepDrop'=""
	
	local count: word count `drop'
	forval num=1/`count' {
		local temp : word `num' of `drop'
		replace `varKeepDrop'="`temp'" if "`temp'"==`second'
	}
	
	count if `varKeepDrop'~=""
	local brows=`brows'-r(N)
	local borows=`borows'-r(N)
	drop if `varKeepDrop'~=""
}

*noi di in red "`brows'"
*noi di in red "`borows'"

if "`cons'"=="nocons" {
	gen count=1 if `second'=="_cons"
	count if count==1
	local brows=`brows'-r(N)
	local borows=`borows'-r(N)
	drop if count==1
}



* get rid of original data since labels already accessed
*keep if b`beta'!=.
keep if `varname'~=""

* get rid of original data since labels already accessed
* also dropped: `varKeepDrop'

ren `first' eqName
ren `second' varName

keep `varname' b`beta' s`st_err' `betcoef' eqName varName




*** rename them because the original data now gone
ren b`beta' coefVal
ren s`st_err' seVal

if "`betaco'"=="betaco" | "`betaAsked'"=="betaAsked" {
	ren `betcoef' betacoVal
}

*** obtain the statistics of interest

* tstatVal
gen double tstatVal = (coefVal/seVal)


* T_alpha for the Ci
if `df_r'==. {
	gen double T_alpha = invnorm( 1-(1-`level' /100)/2 )
}
else {
	* replacement for invt( ) function under version 6
	* note the absolute sign: invttail is flipped from invnorm
	gen double T_alpha = abs(invttail(`df_r', (1-`level' /100)/2))
}

* ci
gen double ciLowVal=coefVal-T_alpha*seVal
gen double ciHighVal=coefVal+T_alpha*seVal

	
	* exponentiate beta and st_err
	gen double coefEformVal = exp(coefVal)
	gen double seEformVal = coefEformVal * seVal
	gen double ciLowEformVal = exp(coefVal - seEformVal * T_alpha / coefEformVal)
	gen double ciHighEformVal = exp(coefVal + seEformVal * T_alpha / coefEformVal)
	

* pvalVal
if `df_r'==. {
	gen double pvalVal = 2*(1-normprob(abs(tstatVal)))
}
else {
	gen double pvalVal = tprob(`df_r', abs(tstatVal))
}


* calculate asterisks for t-stats (or standard errors)
local titlrow=0
if "`append'"=="append" & `fileExist'==1 {
	local appottl = 1
}
else {
	local appottl = `titlrow'
}

* either an appended column (not the first regression) or has a title
* i.e. need an extra line above the coefficients
* added a second extra line above the coefficients: place 1 of 2
gen mrgrow = 2*_n + 1 + `appottl' + 1


*** dealing with the asterisks
if "`aster'"!="noaster" {
	
	if "`alpha'"~="" {
		* parse ALPHA
		partxtl2 `"`alpha'"'
		local alphaCount = r(numtxt)
		local num=1
		while `num'<=`alphaCount' {
			local alpha`num' `r(txt`num')'
			capture confirm number `alpha`num''
			if _rc!=0 {
				noi di in red `"`alpha`num'' found where number expected in {opt alpha()} option"'
				exit 7
			}
		local num = `num'+1
		}
	}
	else {
		if "`2aster'"=="2aster" {
			local alpha1=.01
			local alpha2=.05
			local alphaCount=2
		}
		else {
			local alpha1=.01
			local alpha2=.05
			local alpha3=.10
			local alphaCount=3
		}
	}
	
	
	if `"`symbol'"'!="" {
		* parse SYMBOL
		partxtl2 `"`symbol'"'
		local symbolCount = r(numtxt)
		local num=1
		while `num'<=`symbolCount' {
			local symbol`num' `r(txt`num')'
			capture confirm number `symbol`num''
			if _rc==0{
				noi di in red `"`symbol`num'' found where non-number expected in {opt sym:bol()}"'
				exit 7
			}
		local num = `num'+1
		}
	}
	else {
		*** assume 2aster when only two alpha was given
		if "`2aster'"=="2aster" | `alphaCount'==2 {
			* 1 and 5 %
			local symbol1 "**"
			local symbol2 "*"
			local symbolCount=2
		}
		else {
			* 1, 5, and 10%
			local symbol1 "***"
			local symbol2 "**"
			local symbol3 "*"
			local symbolCount=3
		}
		* when only SYMBOL was given
		if "`alpha'"=="" {
			
			
		}
	}
	
	if "`alpha'"~="" & `"`symbol'"'~="" {
		if `symbolCount'~=`alphaCount' {
			di in red "{opt alpha()} and {opt sym:bol()} must have the same number of elements"
			exit 198
		}
	}
	
	if "`alpha'"=="" & `"`symbol'"'~="" {
		if `symbolCount'>=4 {
			di in red "{opt alpha()} must be specified when more than 3 symbols are specified with {opt sym:bol()}"
			exit 198
		}
	}
	
	if "`alpha'"~="" & `"`symbol'"'=="" {
		local symbolCount=`alphaCount'
		if `alphaCount'>=4 {
			di in red "{opt sym:bol()} must be specified when more than 3 levels are specified with {opt alpha()}"
			exit 198
		}
	}
	
	* fix the leading zero
	local num=1
	while `num'<=`alphaCount' {
		if index(trim("`alpha`num''"),".")==1 {
			local alpha`num'="0`alpha`num''"
		}
		local num=`num'+1
	}
	
	* creating the notes for the alpha significance
	local astrtxt `"`symbol1' p<`alpha1'"'
	local num=2
	while `num'<=`symbolCount' {
		local astrtxt `"`astrtxt', `symbol`num'' p<`alpha`num''"'
		local num=`num'+1
	}
	
	* assign the SYMBOL
	gen str12 astrix = `"`symbol1'"' if (abs(pvalVal)<`alpha1' & abs(pvalVal)!=.)
	
	local num=2
	while `num'<=`symbolCount' {
		replace astrix = `"`symbol`num''"' if astrix=="" & (abs(pvalVal)<`alpha`num'' & abs(pvalVal)!=.)
		local num=`num'+1
	}
}
else {
	gen str2 astrix = ""
}


/*
if "`enclose'"~="" {
	* parse enclose
	partxtl2 `"`enclose'"'
	if `r(numtxt)'==1 {
		local lparen `r(txt1)'
	}
	else {
		local lparen `r(txt1)'
		local rparen `r(txt2)'
	}
}
else if "`paren'"!="noparen" {
	local lparen "("
	local rparen ")"
}
else {
	local lparen ""
	local rparen ""
}
*/



* the fixed or floating specification
if `"`bfmt'"'=="" {
	local bfmt1 "fc"
	local bfmtcnt=1
}
else {
	* parse bfmt
	local fmttxt "e f g fc gc"
	partxtl2 `"`bfmt'"'
	local bfmtcnt = r(numtxt)
	local b = 1
	while `b'<=`bfmtcnt' {
		local bfmt`b' `r(txt`b')'
		if index("`fmttxt'","`bfmt`b''")==0 {
			di in red `"bfmt element "`bfmt`b''" is not a valid number format (f,fc,e,g or gc)"'
			exit 198
		}
	local b = `b'+1
	}
}

local bdeccnt : word count `bdec'

* this condition is always satisfied: `bfmtcnt'>=1
if `bdeccnt'>=1 {
	*** fill in bdec(#) & bfmt(txt)
	local b = 1
	while `b'<=_N {
		local bdec`b' : word `b' of `bdec'
		if "`bdec`b''"=="" {
			local bdec`b' = `prvbdec'
		}
		local prvbdec "`bdec`b''"
		local b = `b'+1
	}
	* bfmt1 is already set above
	local b = `bfmtcnt'+1
	while `b'<=_N {
		local b_1 = `b'-1
		local bfmt`b' "`bfmt`b_1''"
		local b = `b'+1
	}
}


*** putting together

* list of current column names:
* coefVal seVal `varname' (betacoVal) tstatVal T_alpha ciLowVal ciHighVal pvalVal mrgrow astrix

gen str12 `coefcol' = ""

* first prepare ancillary stats (tstat | se | ci| pvalue | beta)

gen str12 coefString = ""
gen str12 coefEformString = ""
gen str12 betacoString = ""

gen str12 pvalString = ""
gen str12 tstatString = ""

gen str12 ciString = ""
gen str12 ciEformString = ""
gen str12 seString = ""
gen str12 seEformString = ""

*gen str12 asterString = astrix
gen str12 asterString = ""
replace asterString = astrix if astrix~=""

gen str12 blankString =""
gen str12 betaString = ""

*** for the (parenthesis) numbers
* pvalue is here; t-stat is here as well

	if "`tdec'"=="" {
		* use autodigits
		forval num=1/`=_N' {
			autodigits2 tstatVal[`num'] `auto' `less'
			replace tstatString = string(tstatVal,"%12.`r(valstr)'") in `num'
			
			autodigits2 pvalVal[`num'] `auto' `less'
			replace pvalString = string(pvalVal,"%12.`r(valstr)'") in `num'
		}
	}
	else {
		* use tdec values
		forval num=1/`=_N' {
			replace tstatString = string(tstatVal,"%12.`tdec'f") in `num'
			replace pvalString = string(pvalVal,"%12.`tdec'f") in `num'
		}
	}
	
	
	if `bdeccnt'==0 & `bfmtcnt'==1 & "`tdec'"=="" {
		* use autodigits because bdec AND tdec was NOT given
		forval num=1/`=_N' {
			autodigits2 ciLowVal[`num'] `auto' `less'
			replace ciString = string(ciLowVal[`num'],"%12.`r(valstr)'") + " - " + string(ciHighVal[`num'],"%12.`r(valstr)'") in `num'
			
			autodigits2 ciLowEformVal[`num'] `auto' `less'
			replace ciEformString = string(ciLowEformVal,"%12.`r(valstr)'") + " - " + string(ciHighEformVal,"%12.`r(valstr)'") in `num'
			
			if "`betaco'"=="betaco" | "`betaAsked'"=="betaAsked" {
				autodigits2 betacoVal[`num'] `auto' `less'
				replace betacoString = string(betacoVal,"%12.`r(valstr)'") in `num'
			}
			
			autodigits2 seVal[`num'] `auto' `less'
			replace seString = string(seVal,"%12.`r(valstr)'") in `num'
				
			autodigits2 seEformVal[`num'] `auto' `less'
			replace seEformString = string(seEformVal,"%12.`r(valstr)'") in `num'
		}
	}
	else {
		if "`tdec'"=="" {
		* because bdec was given but not tdec, use the fixed format according to bdec
			local i 1
			while `i'<=_N {
				replace ciString = string(ciLowVal,"%12.`bdec`i''`bfmt`i''") + " - " + string(ciHighVal,"%12.`bdec`i''`bfmt`i''") if `i'==_n
				replace ciEformString = string(ciLowEformVal,"%12.`bdec`i''`bfmt`i''") + " - " + string(ciHighEformVal,"%12.`bdec`i''`bfmt`i''") if `i'==_n
				
				if "`betaco'"=="betaco" | "`betaAsked'"=="betaAsked" {
					replace betacoString = string(betacoVal,"%12.`bdec`i''`bfmt`i''") if `i'==_n
				}
				
				replace seString = string(seVal,"%12.`bdec`i''`bfmt`i''") if `i'==_n
				replace seEformString = string(seEformVal,"%12.`bdec`i''`bfmt`i''") if `i'==_n
				local i = `i'+1
			}
		}
		else {
		* because only tdec was given, use the fixed format according to tdec
			local i 1
			while `i'<=_N {
				replace ciString = string(ciLowVal,"%12.`tdec'f") + " - " + string(ciHighVal,"%12.`tdec'f") if `i'==_n
				replace ciEformString = string(ciLowEformVal,"%12.`tdec'f") + " - " + string(ciHighEformVal,"%12.`tdec'f") if `i'==_n
				
				if "`betaco'"=="betaco" | "`betaAsked'"=="betaAsked" {
					replace betacoString= string(betacoVal,"%12.`tdec'f") if `i'==_n
				}
				
				replace seString = string(seVal,"%12.`tdec'f") if `i'==_n
				replace seEformString = string(seEformVal,"%12.`tdec'f") if `i'==_n
				local i = `i'+1
			}
		}
	}
	
	
	
	*** transfer (betacoString for beta option) to (betaString for stats(beta))
	replace betaString=betacoString
	
	
	*** prepare coefSring
	
	if `bdeccnt'==0 & `bfmtcnt'==1 {
		forval num=1/`=_N' {
			autodigits2 coefVal[`num'] `auto'
			replace coefString = string(coefVal,"%12.`r(valstr)'") in `num'
			
			autodigits2 coefEformVal[`num'] `auto'
			replace coefEformString = string(coefEformVal,"%12.`r(valstr)'") in `num'
		}
	}
	else if `bdeccnt'==1 & `bfmtcnt'==1 {
		replace coefString = string(coefVal,"%12.`bdec'`bfmt1'")
		replace coefEformString = string(coefEformVal,"%12.`bdec'`bfmt1'")
	}
	else if `bdeccnt'>1 | `bfmtcnt'>1 {
		local i 1
		while `i'<=_N {
			replace coefString = string(coefVal,"%12.`bdec`i''`bfmt`i''") if `i'==_n
			replace coefEformString = string(coefEformVal,"%12.`bdec`i''`bfmt`i''") if `i'==_n
			local i = `i'+1
		}
	}
	
	
	
	
if `"`paren'"'~="noparen" {
	
	if `"`parenthesis'"'~="" {
		*** parenthesis( ) option cleanup
		local parenValid "coef se tstat pval ci aster blank beta"
		* ci_low ci_hi level coef_eform se_eform coef_beta se_beta"
		
		* take comma out
		local parenthesis : subinstr local parenthesis "parenthesis(" " ", all
		local parenthesis : subinstr local parenthesis ")" " ", all
		local parenthesis : subinstr local parenthesis "," " ", all
		
		local parenPerCoef : word count `parenthesis'
		local num=1
		local parenList ""
		
		while `num'<=`parenPerCoef' {
			local paren`num' : word `num' of `parenthesis'
			
			* it must be one of the list
			local test 0
			foreach var in `parenValid' {
				if "`var'"=="`paren`num''" & `test'==0 {
					local test 1
				}
			}
			if `test'==0 {
				noi di in red "{opt `paren`num''} is not a valid option for {opt paren:thesis( )}"
				exit 198
			}
			local parenList "`parenList' `paren`num''"
			local num=`num'+1
		}
	}
	
	if `"`bracketA'"'~="" {
		
		*** bracketA( ) option cleanup
		local bracketValid "coef se tstat pval ci aster blank beta"
		* ci_low ci_hi level coef_eform se_eform coef_beta se_beta"
		
		* take comma out
		local bracketA : subinstr local bracketA "bracketA(" " ", all
		local bracketA : subinstr local bracketA ")" " ", all
		local bracketA : subinstr local bracketA "," " ", all
		
		local bracketPerCoef : word count `bracketA'
		local num=1
		local bracketList ""
		
		while `num'<=`bracketPerCoef' {
			local bracket`num' : word `num' of `bracketA '
			
			* it must be one of the list
			local test 0
			foreach var in `bracketValid' {
				if "`var'"=="`bracket`num''" & `test'==0 {
					local test 1
				}
			}
			if `test'==0 {
				noi di in red "{opt `bracket`num''} is not a valid option for {opt br:acket( )}"
				exit 198
			}
			local bracketList "`bracketList' `bracket`num''"
			local num=`num'+1
		}
	}
	
	if "`bracket'"=="" & "`bracketA'"=="" & "`parenthesis'"=="" {
		
		replace tstatString = "(" + tstatString + ")"
		replace pvalString = "(" + pvalString + ")"
		replace ciString = "(" + ciString + ")"
		replace ciEformString = "(" + ciEformString + ")"
		replace betacoString= "(" + betacoString+ ")"
		replace seString = "(" + seString + ")"
		replace seEformString = "(" + seEformString + ")"
		replace betaString= "(" + betaString+ ")"
	}
	else if "`bracket'"=="bracket" & "`parenthesis'"==""{
		replace tstatString = "[" + tstatString + "]"
		replace pvalString = "[" + pvalString + "]"
		replace ciString = "[" + ciString + "]"
		replace ciEformString = "[" + ciEformString + "]"
		replace betacoString= "[" + betacoString+ "]"
		replace seString = "[" + seString + "]"
		replace seEformString = "[" + seEformString + "]"
		replace betaString= "[" + betaString+ "]"
	}
	else if "`bracket'"=="bracket" & "`parenthesis'"~="" {
		local num 1
		while `num'<=`parenPerCoef' {
			local temp : word `num' of `parenList'
			replace `temp'String = "[" + `temp'String + "]"
			local num=`num'+1
		}
	}	
	else {
		if "`parenthesis'"~="" {
			local num 1
			while `num'<=`parenPerCoef' {
				local temp : word `num' of `parenList'
				replace `temp'String = "(" + `temp'String + ")"
				local num=`num'+1
			}
		}
		if "`bracketA'"~="" {
			local num 1
			while `num'<=`bracketPerCoef' {
				local temp : word `num' of `bracketList'
				replace `temp'String = "[" + `temp'String + "]"
				local num=`num'+1
			}
		}
	}
} /* if `"`paren'"'~="noparen" */





/* old sort and merge codes:
sort mrgrow
save "`bcopy'", replace /* double quotes needed for Macintosh */
/* problems with spaces in file names, fixed Sep 2005 */
*** then offset the row number and put the coefficients in coefcol
replace mrgrow = mrgrow-1
*/



*sort mrgrow
* interweave the coefficients with the t statistics
*merge mrgrow using "`bcopy'"
*replace `varname' = " " if _merge==2 /* no variable names next to tstats */
*drop coefVal seVal tstatVal pvalVal astrix T_alpha ciLowVal ciHighVal betacoVal _merge


* when no coefficient/cons are present (prevent subid from going undefined)
if `=_N'==0 {
	set obs 1
}

gen id=_n

expand `statsPerCoef'
bys id: gen subid=_n

*replace `varname' = " " if subid~=1 /* no variable names next to tstats */
replace `varname' = "" if subid~=1 /* no variable names next to tstats */



	if `"`asterisk'"'~="" {
		
		*** asterisk( ) option cleanup
		local asterValid "coef se tstat pval ci        blank beta"
		* no aster here
		* ci_low ci_hi level coef_eform se_eform coef_beta se_beta"
		
		* take comma out
		local asterisk : subinstr local asterisk "asterisk(" " ", all
		local asterisk : subinstr local asterisk ")" " ", all
		local asterisk : subinstr local asterisk "," " ", all
		
		local asterPerCoef : word count `asterisk'
		local num=1
		local asterList ""
		
		while `num'<=`asterPerCoef' {
			local aster`num' : word `num' of `asterisk'
			
			* it must be one of the list
			local test 0
			foreach var in `asterValid' {
				if "`var'"=="`aster`num''" & `test'==0 {
					local test 1
				}
			}
			if `test'==0 {
				noi di in red "{opt `aster`num''} is not a valid option for {opt aster:isk( )}"
				exit 198
			}
			local asterList "`asterList' `aster`num''"
			local num=`num'+1
		}
	}
	
	
	
	*** combining them into one column
	
	if "`asterisk'" == "" {
		forval num=1/`statsPerCoef' {
			local var : word `num' of `statsList'
			replace `coefcol'=`var'String if subid==`num'
			
			* attach asterString
			replace `coefcol'=`var'String + asterString if subid==`num' & ("`var'"=="coef" | "`var'"=="coefEform")
			
			*replace coefString = coefString + asterString
			*replace coefEformString = coefEformString + asterString
		}
	}
	else {
		forval num=1/`statsPerCoef' {
			local var : word `num' of `statsList'
			replace `coefcol'=`var'String if subid==`num'
			
			* attach asterString
			forval nn=1/`asterPerCoef' {
				replace `coefcol'=`var'String + asterString if subid==`num' & ("`var'"=="`aster`nn''")
				*noi di in red "`var' & `temp'"
			}
			
			*replace coefString = coefString + asterString
			*replace coefEformString = coefEformString + asterString
		}
		
	}
	
	
	
	
	
	
	
	
	
	
	
**************************d check ci interval digits for Eforms







cap drop *Eform*
cap drop betacoVal
drop coefVal seVal tstatVal pvalVal astrix T_alpha ciLowVal ciHighVal *String id subid


local num=mrgrow[1]-2
replace mrgrow=`num'+_n


* sort mrgrow
* the coefficient and parenthesis combined
*save "`bcopy'", replace
*drop `varname' `coefcol'
* offset row numbers again to add header and other statistics


* first find number of new rows for addstat()
if `"`addstat'"'!="" {
	partxtl3 `"`addstat'"'
	local naddst = int((real(r(numtxt))+1)/2)
	
	local n = 1
	while `n'<=`naddst' {
		local t = (`n'-1)*2+1
		local astnam`n' `r(txt`t')'
		local t = `t'+1
		local astval`n' `r(txt`t')' /* pair: stat name & value */
		local n = `n'+1
	}
}
else {
	local naddst=0
}


* find number of new rows for addnote()
if (`"`addnote'"'!="" & "`append'"!="append") | (`"`addnote'"'!="" & `fileExist'==0) {
	partxtl2 `"`addnote'"'
	local naddnt = r(numtxt)
	local n = 1
	while `n'<=`naddnt' {
		local anote`n' `r(txt`n')'
			local n = `n'+1
	}
}
else {
	local naddnt=0
}



* calculate total number of rows in table
* added a second extra line above the coefficients: place 2 of 2
*local coefrow = 2*`brows'+1+`appottl' + 1
local coefrow = `statsPerCoef'*`brows'+1+`appottl' + 1

* for ivreg2 type per Kit B.
*local totrows = `coefrow' + ("`nobs'"!="nonobs") + (`numi'!=.) + ("`r2'"!="nor2"&`rsq'!=.&`df_r'!=.) + `naddst' + ("`notes'"!="nonotes"&"`append'"!="append")*(1+("`aster'"!="noaster")) + `naddnt'
local totrows  = `coefrow' + ("`nobs'"!="nonobs") + (`numi'!=.) + ("`r2'"!="nor2")                    + `naddst' + ("`notes'"!="nonotes"&"`append'"!="append")*(1+("`aster'"!="noaster")) + `naddnt' + ("`notes'"!="nonotes" & `fileExist'==0)*(1+("`aster'"!="noaster"))

* totrows calculation is no longer accurate when no file exists; merely drop the extra row at the end

* cap here because could be lower due to drop/nocons
cap set obs `totrows'

replace mrgrow = 1 in `=_N'
replace mrgrow = 2 in `=_N-1'

if "`append'"=="append" & `fileExist'==1 {
	replace mrgrow = 2.5 in `=_N-2'
}

sort mrgrow
replace mrgrow = _n

*sort mrgrow
*merge mrgrow using "`bcopy'"
*sort mrgrow
*drop _merge




* inserting column titles
if "`append'"=="append" & `fileExist'==1 {
	replace `coefcol' = `"`coltitl'"' if _n==2
	replace `coefcol' = `"`coltit2'"' if _n==3
}
else {
	replace `coefcol' = `"`coltitl'"' if _n==1
	replace `coefcol' = `"`coltit2'"' if _n==2
}

if "`nobs'"!="nonobs" {
	local coefrow = `coefrow'+1
	replace `varname' = "Observations" if _n==`coefrow'
	replace `coefcol' = string(`regN') if _n==`coefrow'
}
if (`numi'!=. & "`ni'"!="noni") {
	local coefrow = `coefrow'+1
	replace `varname' = "Number of " + rtrim(`"`iname'"') if _n==`coefrow'
	replace `coefcol' = string(`numi') if _n==`coefrow'
}


* the r2 is reported for ivreg2, per Kit B.
*if "`r2'"!="nor2" & `rsq'!=. & `df_r'!=. {
if "`r2'"!="nor2" {
	/* if df_r=., not true r2 */
	local coefrow = `coefrow'+1
	replace `coefcol' = string(`rsq',"%12.`rdec'f") if _n==`coefrow'
	replace `varname' = "R-squared" if _n==`coefrow'
	if "`adjr2'"=="adjr2" {
		replace `varname' = "Adjusted " + `varname' if _n==`coefrow'
	}
}



*** addtext here
if `"`addtext'"'!="" {
	partxtl2 `"`addtext'"'
	local temp = int((real(r(numtxt))+1)/2)
	
	local n = 1
	while `n'<=`temp' {
		local t = (`n'-1)*2+1
		local textName`n' `r(txt`t')'
		local t = `t'+1
		local textValue`n' `r(txt`t')' /* pair: stat name & value */
		local n = `n'+1
	}
	
	local i 1
	while `i'<=`temp' {
		* increase
		local coefrow = `coefrow'+1
		set obs `=`=_N'+1'
		
		if `"`textValue`i''"'!="" {
			replace `coefcol' = "`textValue`i''" if _n==`coefrow'
		}
		replace `varname' = trim(`"`textName`i''"') if _n==`coefrow'
		local i = `i'+1
	}
	
	* cleanup counting
	replace mrgrow=_n
}



*** addstat here

if `"`addstat'"'!="" {
	local i 1
	local adeccnt : word count `adec'
	while `i'<=`naddst' {
		local coefrow = `coefrow'+1
		local aadec : word `i' of `adec'
		if "`aadec'"=="" {
			local aadec `prvadec'
		}
		if `"`astval`i''"'!="" {
			replace `coefcol' = "`astval`i''" if _n==`coefrow'
		}
		replace `varname' = trim(`"`astnam`i''"') if _n==`coefrow'
		local i = `i'+1
		local prvadec `aadec'
	}
}





if ("`notes'"!="nonotes" & "`append'"!="append") | ("`notes'"!="nonotes" & `fileExist'==0) {
	local coefrow = `coefrow'+1
	*if "`bracket'"=="bracket" {
	if "`bracket'"=="bracket" | "`bracketA'" ~= "" {
		local par_bra "brackets"
	}
	else {
		local par_bra "parentheses"
	}
	if "`pvalue'"=="pvalue" {
		local statxt "p values"
	}
	else if "`se'"=="se" {
		local statxt "Standard errors"
	}
	else if "`ci'"=="ci" {
		local statxt "`level'% confidence intervals"
	}
	else if "`betaco'"=="betaco" {
		local statxt "Normalized beta coefficients"
	}
	else {
		if `df_r'!=. {
			local t_or_z "t"
		}
		else {
			local t_or_z "z"
		}
		local statxt "`t_or_z' statistics"
		if "`robust'"=="none" {
			local statxt "`statxt'"
		}
	}
	if "`robust'"=="Robust" {
		local statxt = "Robust " + lower("`statxt'")
	}
	replace `varname' = "`statxt' in `par_bra'" if _n==`coefrow'
	if "`aster'"!="noaster" {
		local coefrow = `coefrow'+1
		replace `varname' = "`astrtxt'" if _n==`coefrow'
	}
}
if (`"`addnote'"'!="" & "`append'"!="append") | (`"`addnote'"'!="" & `fileExist'==0) {
	local i 1
	while `i'<=`naddnt' {
		local coefrow = `coefrow'+1
		replace `varname' = `"`anote`i''"' if _n==`coefrow'
		local i = `i'+1
	}
}

* attach the column name
replace `varname'="VARIABLES" in 1



*** drop the extra rows at the end, if still exist
local temp=`varname'[`=_N']
while "`temp'"=="" {
	drop in `=_N'
	local temp=`varname'[`=_N']	
}

end		/* coeftxt2 */







***********************



program define seeing
version 8.2

quietly{
	* syntax using/[, Clear]
	syntax using [, LABel LABelA(string) ]
	
	preserve
	
	insheet `using', nonames clear
	describe, short
	
	
	* number of columns
	local numcol = r(k)	
	
	tempvar blanks rowmiss	
	count if v1=="EQUATION"
	if `r(N)'~=0 {
		count if v3=="LABELS"
		if `r(N)'~=0 {
			local num=4
		}
		else {
			local num=3
		}
	}
	else {
		count if v2=="LABELS"
		if `r(N)'~=0 {
			local num=3
		}
		else {
			local num=2
		}
	}
	
	gen int `blanks' = (trim(v`num')=="")
	
	forvalues col = `num'/`numcol' {
		replace `blanks' = `blanks' & (trim(v`col')=="")
	}
	
	
	* in case ctitle is missing: 2 of 2
	* colheadN
	
	*************d change this in the next update
	replace `blanks'=0 if v1=="COEFFICIENT" | v1[_n-1]=="COEFFICIENT" | v2=="COEFFICIENT" | v2[_n-1]=="COEFFICIENT" ///
				| v1=="VARIABLES" | v1[_n-1]=="VARIABLES" | v2=="VARIABLES" | v2[_n-1]=="VARIABLES"
	
	* title rows
      local titlrow = 0 
      while `blanks'[`titlrow'+1] {
		local titlrow = `titlrow'+1
	}
	
	
	if `numcol'==2 {
		local colheadN=2+`titlrow' 
	}
	else {
		local colheadN=3+`titlrow'
	}
	
	
	* avoid counting space within each statistics row as missing
	replace `blanks'=0 if `blanks'[_n+1]==0 & `blanks'==1 & _n >`titlrow'
	
	
	* statistics rows
	count if `blanks'==0
	local strowN = `r(N)'+`titlrow'
	
	
	* move the notes and titles to the top of a new column
	gen str5 Notes_Titles=""
	format Notes_Titles %-20s 
	count if v1=="EQUATION"
	if `r(N)'==0 {
		* EQUATION column does not exist
		if `titlrow'>0 {
			forval num=1/`titlrow' {
				replace Notes_Titles=v1[`num'] in `num'
				replace v1="" in `num'
			}
		}
		
		local one = 1
		local legend = v1[`strowN'+`one']
		
		
		local place 1
		*while "`legend'"~="" {
		while `place' <= `=_N' {
			local place=`strowN'+`one'
			local legend = v1[`place']
			replace Notes_Titles="`legend'" in `=`one'+`titlrow'+1'
			if "`legend'"~="" {
				replace v1="" in `place'
			}
			local one = `one'+1
		}
		
		* insert label changes here, minus 2 from c(k) for `blanks' & Notes_Titles column
		if "`label'"=="label" {
				*if ("`long'"~="long" & "`onecol'"~="onecol") | ("`long'"=="long" & "`onecol'"=="onecol") {
					replace v2=v1 if v2==""
					drop v1
					forval num=1/`=`c(k)'-2' {
						ren v`=`num'+1' v`num'
					}
					
					* change LABELS to VARIABLES in 1/3
					replace v1="VARIABLES" in 1/3 if v1=="LABELS"
				*}
				local label_adjust "-1"
		}
		
		* change the string length
		gen str5 temp=""
		replace temp=v1
		drop v1
		ren temp v1
		order v1
		* format
		foreach var of varlist v1 {
			local _format= "`: format `var''"
			local _widths=substr("`_format'",2,length(trim("`_format'"))-2)
			format `var' %-`_widths's
		}
	}
	else {
		* equation column exists
		if `titlrow'>0 {
			forval num=1/`titlrow' {
				replace Notes_Titles=v2[`num'] in `num'
				replace v2="" in `num'
			}
		}
		
		local one = 1
		local legend = v2[`strowN'+`one']
		while "`legend'"~="" {
			local place=`strowN'+`one'
			local legend = v2[`place']
			replace Notes_Titles="`legend'" in `=`one'+`titlrow'+1'
			if "`legend'"~="" {
				replace v2="" in `place'
			}
			local one = `one'+1
		}
		
		* insert label changes here, minus 2 from c(k) for `blanks' & Notes_Titles column
		if "`label'"=="label" {
				*else if "`long'"~="long" & "`onecol'"=="onecol" {
					replace v3=v2 if v3==""
					drop v2
					forval num=2/`=`c(k)'-2' {
						ren v`=`num'+1' v`num'
					}
					
					* change LABELS to VARIABLES in 1/3
					replace v2="VARIABLES" in 1/3 if v2=="LABELS"
				*}
				local label_adjust "-1"
		}
		
		
		* change the string length
		gen str5 temp=""
		replace temp=v2
		drop v2
		ren temp v2
		order v1 v2
		* format
		foreach var of varlist v1 v2 {
			local _format= "`: format `var''"
			local _widths=substr("`_format'",2,length(trim("`_format'"))-2)
			format `var' %-`_widths's
		}
	}
	
	* clean up
	*egen `rowmiss'=rowmiss(_all)
	* rowmiss option not available in 8.2, do it by hand
	
	gen `rowmiss'=0
	foreach var of varlist _all {
		if "`var'"~="`rowmiss'" & "`var'"~="`blanks'" {
			replace `rowmiss'=1+`rowmiss' if `var'==""
		}
	}
	
	*drop if `rowmiss'==`numcol'+1
	
	* adjust to handle label column droppings
	*drop if `rowmiss'==`numcol'+1 & `blanks'==1
	drop if `rowmiss'==`numcol'+1 `label_adjust' & `blanks'==1
	drop `blanks' `rowmiss'
	
	browse
	
	*restore, preserve
}

end  /* end of seeing */



***********************


program define autodigits2, rclass
version 8.2
* getting the significant digits
args input auto less

if `input'~=. {
	local times=0
	local left=0
	
	* integer checked by modified mod function
	if round((`input' - int(`input')),0.0000000001)==0 {
		local whole=1
	}
	else {
		local whole=0
		* non-interger
		 if `input'<. {
			
			* digits that need to be moved if it were only decimals: take the ceiling of log 10 of absolute value of decimals
			local times=abs(int(ln(abs(`input'-int(`input')))/ln(10)-1))	
			
			* the whole number: take the ceiling of log 10 of absolute value
			local left=int(ln(abs(`input'))/ln(10)+1)
		}
	}
	
	
	* assign the fixed decimal values into aadec
	if `whole'==1 {
		local aadec=0
	}
	else if .>`left' & `left'>0 {
		* reduce the left by one if more than zero to accept one extra digit
		if `left'<=`auto' {
			local aadec=`auto'-`left'+1
		}
		else {
			local aadec=0
		}
	}
	else {
		local aadec=`times'+`auto'-1
	}
	
	if "`less'"=="" {
		* needs to between 0 and 11
		if `aadec'<0 {
			local aadec=0
		}
		*if `aadec'<11 {
		if `aadec'<7 {
			* use fixed
			local valstr "`aadec'f"
		}
		else {
			* use exponential
			local valstr "`=`auto'-1'e"
		}
	}
	else {
		* needs to between 0 and 11
		local aadec=`aadec'-`less'
		if `aadec'<0 {
			local aadec=0
		}
		*if `aadec'<10 {
		if `aadec'<7 {
			* use fixed
			local valstr "`aadec'f"
		}
		else {
			* use exponential
			local valstr "`=`auto'-1'e"
		}
	}
	
	* make it exponential if too big
	if `input'>1000000 & `input'<. {
		local valstr "`=`auto'-1'e"		
	}
	
	return scalar value=`aadec'
	return local valstr="`valstr'"
}
else {
	* it is a missing value
	return scalar value=.
	return local valstr="missing"
}
end


****************


program define outreg2
version 8.2
	
	
	
	* separate the possible regression command from the outreg2 precommand
	
	tokenize `"`0'"', parse(" ,")
	
	* avoid the column in the file name by encasing it in quotes
	local 0 ""
	local count 1
	local countUsing=0
	
	while `"``count''"'~="" {
		if `"``count''"'=="using" {
			local countUsing=1
			
			*** clean up file name, attach .txt if no file type is specified
			*local rest "`using'"
			* strip off "using"
			*gettoken part rest: rest, parse(" ")
			
			local rest `"``=`count'+1''"'
			* strip off quotes
			gettoken first second: rest, parse(" ")
			local rest: list clean local(rest)
			
			* take off comma at the end
			*if index(`"`rest'"',",")==length(`"`rest'"',",") {
			*	local rest=substr(`"`rest'"',1,`=length(`"`rest'"')-1')
			*}
			
			* has no comma at the end
			local rabbit `"""'
			if index(`"`rest'"', ".")==0 {
				local `=`count'+1' `"`rabbit'`rest'.txt`rabbit'"'
			}
			else {
				local `=`count'+1' `"`rabbit'`rest'`rabbit'"'
			}
		}
		local 0 `"`0' ``count''"'
		local count=`count'+1
	}
	
	
	* check for the column location
	/* old codes:
	gettoken first second : 0, parse(": ")
	while `"`first'"'~=":" & `"`first'"'~="" {
		local options "`options' `first'"
		gettoken first second : second, parse(":")
	}
	local command `"`second'"'
	*/
	
	* from _on_colon_parse:
	gettoken first second : 0, parse(":") bind match(par) quotes
	while `"`first'"'~=":" & `"`first'"'~="" {
		local options "`options' `first'"
		gettoken first second : second, parse(":") bind match(par) quotes
	}
	local command `"`second'"'
	
	
	* need to handle very long expression, did it the long way above with countUsing
	*if `"`second'"'=="" & index(`"`0'"', "using")~=0 {
	
	if `"`second'"'=="" & `countUsing'==1 {
		local second: subinstr local 0 "replace" "",all word count (local replace1)
		local second: subinstr local second "seeout" "",all word count (local seeout1)
		*local second: list uniq local(second)
		*local second: list retokenize second
		
		if `replace1' {
			local extraCmd "replace"
		}
		if `seeout1' {
			local extraCmd "seeout"
		}
		if `seeout1' & `replace1' {
			local extraCmd "replace seeout"
		}
		
		outreg2Main `0'
	}
	else {
		
		* need to handle very long expression, did it the long way above with countUsing
		*if index(`"`0'"', "using")~=0 {
		if `countUsing'==1 {
			di "{yellow}using not allowed in the shorthand syntax"
			exit 198
		}
		else {
			
			local 0 `"`options'"'
			syntax [anything] [,				 	///
				REPLACE						///
				SEEout						///
				APpend]
			
			`command'
			
			*** read the set preference if not out of date
			
			* NOTE: `0' is written over below
			quietly findfile outreg2_prf.ado
			tempname myfile
			file open `myfile' using `"`r(fn)'"', read text
			file read `myfile' date
			file read `myfile' pref
			file read `myfile' options
			file close `myfile'
			
			* fix comma
			local comma ""
			if `"`macval(options)'"'~="" | "`replace'"~="" | "`seeout'"~="" {
				local comma ","
			}
			
			if "`date'"== "`c(current_date)'" {
				local seecommand "outreg2"
				local precommand "outreg2Main"
				foreach var in anything  macval(pref) comma macval(options) replace seeout {
					if `"``var''"'~="" {
						if `"``var''"'=="," {
							local seecommand `"`seecommand'``var''"'
							local precommand `"`precommand'``var''"'
						}
						else {
							local seecommand `"`seecommand' ``var''"'
							local precommand `"`precommand' ``var''"'
						}
					}
				}
				local cl `"{stata `"`seecommand'"':  `seecommand'}"'
				di as txt `"`cl'"'
				`precommand'
			}
			else {
				di in red "must specify the full syntax (the last preference has expired)"
				exit 100
			}
		}
	}


end /* end of outreg2 */



*******************


program define out2tex2, sortpreserve
* based on version 0.9 4oct01 by john_gallup@alum.swarthmore.edu
version 8.2

if "`1'" == "using" {
	syntax using/ [, Landscape Fragment noPRetty			///
		Fontsize(numlist integer max=1 >=10 <=12) noBorder Cellborder ///
		Appendpage noPAgenum a4 a5 b5 LETter LEGal EXecutive replace  ///
		Fast]
		
	if "`fast'" == "" {
		preserve
	}
	
	loadout using "`using'", clear
	local numcol	= `r(numcol)'
	local titlrow  = `r(titlrow)'
	local colheadN = `r(colheadN)'
	local strowN	= `r(strowN)'
	local totrows	= _N
	
	local varname "v1"
	unab statvars : v2-v`numcol'
	}
	
	else {
		syntax varlist using/, TItlrow(int) ColheadN(int) StrowN(int)		///
			[TOtrows(int 0) Landscape Fragment noPRetty				///
			Fontsize(numlist integer max=1 >=10 <=12) noBorder Cellborder	///
			Appendpage noPAgenum a4 a5 b5 LETter LEGal EXecutive replace]
		if `totrows'==0 {
			local totrows = _N
		}
		local numcols : word count `varlist'
		gettoken varname statvars : varlist
		local fast 1
	}
	
	local colhead1 = `titlrow' + 1
	local strow1 = `colheadN' + 1
	
	* insert $<$ to be handled in LaTeX conversion
	forval num=`strowN'/`=_N' {
		local temp=v1[`num']
		tokenize `"`temp'"', parse (" <")
		local count 1
		local newTex ""
		local noSpace 0
		while `"``count''"'~="" {
			if `"``count''"'=="<" {
				local `count' "$<$"
				local newTex `"`newTex'``count''"'
				local noSpace 1
			}
			else {
				if `noSpace'~=1 {
					local newTex `"`newTex' ``count''"'
				}
				else {
					local newTex `"`newTex'``count''"'					
					local noSpace 0
				}
			}
			local count=`count'+1
		}
		replace v1=`"`newTex'"' in `num'
	}
	
	*** replace if equation column present
	count if v1=="EQUATION"
	if `r(N)'~=0 {
		tempvar myvar
		* use v2 instead
		replace v1 = v2 in `=`strowN'+1'/`totrows'
		replace v2 = "" in `=`strowN'+1'/`totrows'
		
		* change the string length
		gen str5 `myvar' =""
		replace `myvar' =v2
		drop v2
		ren `myvar' v2
		order v1 v2
	}
	
	* if file extension specified in `using', replace it with ".tex" for output
	local beg_dot = index(`"`using'"', ".")
	if `beg_dot' {
		local using = substr("`using'",1,`=`beg_dot'-1')
	}
	
	local using `"using "`using'.tex""'
	local fsize = ("`fontsize'" != "")
	if `fsize' {
		local fontsize "`fontsize'pt"
	}
	local lscp = ("`landscape'" != "") 
	if (`lscp' & `fsize') {
		local landscape ",landscape"
	}
	local pretty	= ("`pretty'" == "")
	local cborder  = ("`cellborder'" != "")
	local noborder = ("`border'" != "")
	local nopagen  = ("`pagenum'" != "")
	local nofrag	= ("`fragment'" == "")
	
	if `cborder' & `noborder' {
		di in red "may not specify both cellborder and noborder options"
		exit 198
	}
	
	local nopt : word count `a4' `a5' `b5' `letter' `legal' `executive'
	if `nopt' > 1 {
		di in red "choose only one of a4, a5, b5, letter, legal, executive"
		exit 198 
	}
	local pagesize "`a4'`a5'`b5'`letter'`legal'`executive'"
	if "`pagesize'"=="" | "`letter'"!="" {
		local pwidth  "8.5in"
		local pheight "11in"
	}
	else if "`legal'"!="" {
		local pwidth  "8.5in"
		local pheight "14in"
	}
	else if "`executive'"!="" {
		local pwidth  "7.25in"
		local pheight "10.5in"
	}
	else if "`a4'"!="" {
		local pwidth  "210mm"
		local pheight "297mm"
	}
	else if "`a5'"!="" {
		local pwidth  "148mm"
		local pheight "210mm"
	}
	else if "`b5'"!="" {
		local pwidth  "176mm"
		local pheight "250mm"
	}
	if `lscp' {
		local temp	 "`pwidth'"
		local pwidth  "`pheight'"
		local pheight "`temp'"
	}
	if "`pagesize'"!="" {
		local pagesize "`pagesize'paper"
		if (`lscp' | `fsize') {
			local pagesize ",`pagesize'"
		}
	}
	if `cborder' & `noborder' {
		di in red "may not specify both cellborder and noborder options"
		exit 198
	}
	
	quietly {
		tempvar has_eqn st2_row last_st pad0 pad1 pad2_n padN order
		
		* replace % with \%, and _ with \_ if <2 $'s (i.e. not an inline equation: $...$
		* has_eqn indicates that varname has 2+ $'s
		
		gen byte `has_eqn' = index(`varname',"$")
		
		* make sure there are 2+ "$" in varname
		replace `has_eqn' = index(substr(`varname',`has_eqn'+1,.),"$")>0 if `has_eqn'>0
		replace `varname'= subinstr(`varname',"_", "\_", .) if !`has_eqn'
		replace `varname'= subinstr(`varname',"%", "\%", .)
		
		if `pretty' {
			replace `varname'= subinword(`varname',"R-squared", "\$R^2$", 1) in `strow1'/`strowN'
			replace `varname'= subinstr(`varname'," t stat", " \em t \em stat", 1) in `strowN'/`totrows'
			replace `varname'= subinstr(`varname'," z stat", " \em z \em stat", 1) in `strowN'/`totrows'
		}
		
		foreach svar of local statvars { /* make replacements for column headings rows of statvars */
			replace `has_eqn' = index(`svar',"$") in `colhead1'/`colheadN'
			replace `has_eqn' = index(substr(`svar',`has_eqn'+1,.),"$")>0 in `colhead1'/`colheadN' if `has_eqn'>0
			replace `svar'= subinstr(`svar',"_", "\_", .) in `colhead1'/`colheadN' if !`has_eqn'
			replace `svar'= subinstr(`svar',"%", "\%", .) in `colhead1'/`colheadN'
			
			/* replace <, >, {, }, | with $<$, $>$, \{, \}, and $|$ in stats rows */
			/* which can be used as brackets by outstat */
			replace `svar'= subinstr(`svar',"<", "$<$", .) in `strow1'/`strowN'
			replace `svar'= subinstr(`svar',">", "$>$", .) in `strow1'/`strowN'
			replace `svar'= subinstr(`svar',"{", "\{", .)  in `strow1'/`strowN'
			replace `svar'= subinstr(`svar',"}", "\}", .)  in `strow1'/`strowN'
			replace `svar'= subinstr(`svar',"|", "$|$", .) in `strow1'/`strowN'
		}
		
		if `pretty' {  /* make title fonts large; notes & t stats small */
			local blarge "\begin{large}"
			local elarge "\end{large}"
			local bfnsize "\begin{footnotesize}"
			local efnsize "\end{footnotesize}"
		}
		if `cborder' {
			local vline "|"
		} 
		gen str20 `pad0' = ""
		gen str20 `padN' = ""
		if `titlrow' {
			replace `pad0' = "\multicolumn{`numcols'}{`vline'c`vline'}{`blarge'" in 1 / `titlrow'
			replace `padN' = "`elarge'} \\\" in 1 / `titlrow'
		}
		if `strowN' < `totrows' {
			local noterow1 = `strowN' + 1
			replace `pad0' = "\multicolumn{`numcols'}{`vline'c`vline'}{`bfnsize'" in `noterow1' / l
			replace `padN' = "`efnsize'} \\\" in `noterow1' / l
		}
		
		gen str3 `pad1' = " & " in `colhead1' / `strowN'
		if `numcols' > 2 {
			gen str3 `pad2_n' = `pad1'
		}
		if `pretty' { /* make stats 2-N small font */
			local strow1 = `colheadN' + 1
			gen byte `st2_row' = 0
			replace `st2_row' = (trim(`varname') == "") in `strow1' / `strowN'	 /* only stats 2+ */
			gen byte `last_st' = (`st2_row' & `varname'[_n+1] != "")			 /* last stats row */
			if !`cborder' {
				replace `pad0'	= "\vspace{4pt}" if `last_st'
			}
				replace `pad1'	= `pad1' + "`bfnsize'" if `st2_row'
				if `numcols' > 2 {
					replace `pad2_n' = "`efnsize'" + `pad2_n' + "`bfnsize'" if `st2_row'
				}
				replace `padN'	= "`efnsize'" if `st2_row'
			}
		
			replace `padN' = `padN' + " \\\" in `colhead1' / `strowN'
			if `cborder' {
				replace `padN' = `padN' + " \hline"
			}
			else {
			if !`noborder' {
				if `colheadN' {
					if `titlrow' {
						replace `padN' = `padN' + " \hline" in `titlrow'
					}
					replace `padN' = `padN' + " \hline" in `colheadN'
				}
				replace `padN' = `padN' + " \hline" in `strowN'
			}
		}
		
		local vlist "`pad0' `varname' `pad1'"
		tokenize `statvars'
		local ncols_1 = `numcols' - 1
		local ncols_2 = `ncols_1' - 1
		forvalues v = 1/`ncols_2' {
			local vlist "`vlist' ``v'' `pad2_n'"
		}
		local vlist "`vlist' ``ncols_1'' `padN'"
		
		local texheadfootrows = `nofrag' + `pretty' + 1	/* in both headers and footers */ 
		local texheadrow = 2 * `nofrag' + `nopagen' + `texheadfootrows'
		local texfootrow = `texheadfootrows'
		local newtotrows = `totrows' + `texheadrow' + `texfootrow'
		if `newtotrows' > _N {
			local oldN = _N
			set obs `newtotrows'
		}
		else {
			local oldN = 0
		}
		gen long `order' = _n + `texheadrow' in 1 / `totrows'
		local newtexhrow1 = `totrows' + 1
		local newtexhrowN = `totrows' + `texheadrow'
		replace `order' = _n - `totrows' in `newtexhrow1' / `newtexhrowN'
		sort `order'
		
		
		* insert TeX header lines
		local ccc : display _dup(`ncols_1') "`vline'c"
		if `nofrag' {
			replace `pad0' = "\documentclass[`fontsize'`landscape'`pagesize']{article}" in 1
			replace `pad0' = "\setlength{\pdfpagewidth}{`pwidth'} \setlength{\pdfpageheight}{`pheight'}" in 2
			replace `pad0' = "\begin{document}" in 3
			replace `pad0' = "\end{document}" in `newtotrows'  
		}
		if `nopagen' {
			local row = `texheadrow' - 1 - `pretty'
			replace `pad0' = "\thispagestyle{empty}" in `row'
		}
		if `pretty' {
			local row = `texheadrow' - 1
			replace `pad0' = "\begin{center}" in `row'
			local row = `newtotrows' - `texfootrow' + 2
			replace `pad0' = "\end{center}"	in `row'
		}
		local row = `texheadrow'
		replace `pad0' = "\begin{tabular}{`vline'l`ccc'`vline'}" in `row'
		if (!`titlrow' | `cborder') & !`noborder' {
			replace `pad0' = `pad0' + " \hline" in `row'
		}
		local row = `newtotrows' - `texfootrow' + 1
		replace `pad0' = "\end{tabular}" in `row'

		noi outfile `vlist' `using' in 1/`newtotrows', `replace' runtogether

		* delete new rows created for TeX table, if any
		if `oldN' {
			keep in 1/`totrows'
		}
	} /* quietly */
end  /* end out2tex2 */



*******************


program define out2rtf2, sortpreserve rclass
* based on version 0.9 4oct01 by john_gallup@alum.swarthmore.edu
	if "`1'" == "using" {
		syntax using/ [, Landscape Fragment noPRetty				///
			Fontsize(numlist max=1 >0) noBorder Cellborder			///
			Appendpage PAgesize(string)						///
			Lmargin(numlist max=1 >=0.5) Rmargin(numlist max=1 >=0.5) 	///
			Tmargin(numlist max=1 >=0.5) Bmargin(numlist max=1 >=0.5) 	///
			replace Fast]
		if "`fast'" == "" {preserve}
		loadout using "`using'", clear
		local numcol	= `r(numcol)'
		local titlrow  = `r(titlrow)'
		local colheadN = `r(colheadN)'
		local strowN	= `r(strowN)'
		local totrows	= _N
		local varname "v1"
		unab statvars : v2-v`numcol'
	}
	else {
		syntax varlist using/, TItlrow(int) ColheadN(int) StrowN(int)	///
			[TOtrows(int 0) Landscape Fragment noPRetty			///
			Fontsize(numlist max=1 >0) noBorder Cellborder			///
			Appendpage PAgesize(string)						///
			Lmargin(numlist max=1 >=0.5) Rmargin(numlist max=1 >=0.5)	///
			Tmargin(numlist max=1 >=0.5) Bmargin(numlist max=1 >=0.5)	///
			replace]
		if `totrows'==0 {
			local totrows = _N
		}
		local numcols : word count `varlist'
		gettoken varname statvars : varlist
		local fast 1
	}
	
	local colhead1 = `titlrow' + 1
	local strow1 = `colheadN' + 1
	
	
	
	*** replace if equation column present
	local hack 0
	count if v1=="EQUATION"
	if `r(N)'~=0 {
		* use v2 instead
		replace v1 = v2 in `=`strowN'+1'/`totrows'
		replace v2 = "" in `=`strowN'+1'/`totrows'
		
		* change the string length
		gen str5 myvar =""
		replace myvar =v2
		drop v2
		ren myvar v2
		order v1 v2
		
		local hack 1
	}
	
	* if file extension specified in `using', replace it with ".rtf" for output
	local beg_dot = index("`using'", ".")
	if `beg_dot' {
		local using = substr("`using'",1,`=`beg_dot'-1')
	}
	local using `"using "`using'.rtf""'
	return local documentname `"`using'"'
	
	if "`fontsize'" == "" {
		local fontsize "11"
	}
	
	local lscp = ("`landscape'" != "") 
	local pretty	= ("`pretty'" == "")
	local cborder  = ("`cellborder'" != "")
	local noborder = ("`border'" != "")
	local stdborder = (!`noborder' & !`cborder')
	local nopagen  = ("`pagenum'" != "")
	local nofrag	= ("`fragment'" == "")
	
	
	if `cborder' & !`noborder' {
		di in red "may not specify both cellborder and noborder options"
		exit 198
	}
	
	* reformat "R-squared" and italicize "t" or "z"
	if `pretty' {
		quietly {
			replace `varname'= subinword(`varname',"R-squared", "{\i R{\super 2}}", 1) in `strow1'/`strowN'
			replace `varname'= subinstr(`varname'," t stat", " {\i t} stat", 1) in `strowN'/`totrows'
			replace `varname'= subinstr(`varname'," z stat", " {\i z} stat", 1) in `strowN'/`totrows'
		}
	}
	
	* font sizes in points*2
	local font2 = int(`fontsize'*2)
	if `pretty' {
		/* make title fonts large; notes & t stats small */
		local fslarge = "\fs" + string(int(`font2' * 1.2))
		local fsmed	= "\fs" + string(`font2')
		local fssmall = "\fs" + string(int(`font2' * 0.8))
		local sa0 "\sa0"	/* put space after t stats rows */
		local gapsize = int(`fontsize'*0.4*20)  /* 40% of point size converted to twips */
		local sa_gap "\sa`gapsize'"
	}
	else {
		local fs0 = "\fs" + string(`font2')
	}
	
	local onecolhead = (`colheadN' - `titlrow' == 1)
			/* onecolhead = true if only one row of column headings */
	if `stdborder' {
		if !`onecolhead' {
			* runs here
			*local trbrdrt "\clbrdrt\brdrs"	/* table top is overlined */
			*local trbrdrt "\trbrdrt\brdrs"	/* table top is overlined */
			local clbrdr_ul "\clbrdrb\brdrs"	/* cells are underlined */
		}
		else {
			/* cells are over- and underlined */
			local clbrdr_ul "\clbrdrt\brdrs\clbrdrb\brdrs"
		
		}
		local trbrdrb "\trbrdrb\brdrs"
	}
	if `cborder' {
		/* if !cborder then clbrdr is blank */
		local clbrdr "\clbrdrt\brdrs\clbrdrb\brdrs\clbrdrl\brdrs\clbrdrr\brdrs"
	}
	
	* figure out max str widths to make cell boundaries
	* cell width in twips = (max str width) * (pt size) * 12
	* (12 found by trial and error)
	local twipconst = int(`fontsize' * 12 )
	tempvar newvarname
	qui gen str80 `newvarname' = `varname' in `strow1'/`strowN'
	
	local newvarlist "`newvarname' `statvars'"
	qui compress `newvarlist'
	local cellpos = 0
	foreach avar of local newvarlist {
		local strwidth : type `avar'
		local strwidth = subinstr("`strwidth'", "str", "", .)
		local strwidth = `strwidth' + 1  /* add buffer */
		local cellpos = `cellpos' + `strwidth'*`twipconst'

		* hacking
		if `hack'==1 & "`avar'"=="`newvarname'" & `cellpos'<1270 {
			local cellpos=1270
		}
		local clwidths "`clwidths'`clbrdr'\cellx`cellpos'"
		* put in underline at bottom of header in clwidth_ul
		local clwidth_ul "`clwidth_ul'`clbrdr_ul'\cellx`cellpos'"
	}
	
	if `stdborder' {
		if `onecolhead' {
			local clwidth1 "`clwidth_ul'"
		}
		else {
			local clwidth1 "`clwidths'"
			local clwidth2 "`clwidth_ul'"
		}
		local clwidth3 "`clwidths'"
	}
	else{
		local clwidth1 "`clwidths'"
	}
	
	* statistics row formatting
	tempvar prettyfmt
	qui gen str12 `prettyfmt' = ""  /* empty unless `pretty' */
	if `pretty' {
		* make stats 2-N small font
		tempvar st2_row last_st
		quietly {
			gen byte `st2_row' = 0
			replace `st2_row' = (trim(`varname') == "") in `strow1' / `strowN'	 /* only stats 2+ */
			gen byte `last_st' = (`st2_row' & `varname'[_n+1] != "")			 /* last stats row */
			replace `prettyfmt' = "`sa0'" in `strow1' / `strowN'
			replace `prettyfmt' = "`sa_gap'"  if `last_st' in `strow1' / `strowN'
			replace `prettyfmt' = `prettyfmt' + "`fsmed'" if !`st2_row' in `strow1' / `strowN'
			replace `prettyfmt' = `prettyfmt' + "`fssmall'"  if `st2_row' in `strow1' / `strowN'
		}
	}
	
	* create macros with file write contents
	
	forvalues row = `colhead1'/`strowN' { 
		local svarfmt`row' `"(`prettyfmt'[`row']) "\ql " (`varname'[`row']) "\cell""'
		foreach avar of local statvars {
			local svarfmt`row' `"`svarfmt`row''"\qc " (`avar'[`row']) "\cell""' 
		}
		local svarfmt`row' `"`svarfmt`row''"\row" _n"'
	}
	
	* write file
	tempname rtfile
	file open `rtfile' `using', write `replace'
	file write `rtfile' "{\rtf1`fs0'" _n  /* change if not roman: \deff0{\fonttbl{\f0\froman}} */
	
	if `titlrow' {
		file write `rtfile' "\pard\qc`fslarge'" _n
		forvalues row = 1/`titlrow' {
			file write `rtfile' (`varname'[`row']) "\par" _n
		}
	}
	
	
	file write `rtfile' "\trowd\intbl\trqc`fsmed'`trbrdrt'`clwidth1'" _n
	
	if !`onecolhead' {
		* here
		*added:
		*file write `rtfile' "\trowd\trqc`clwidth2'" _n
		
		local colheadN_1 = `colheadN' - 1
		* write header rows 1 to N-1
		forvalues row = `colhead1'/`colheadN_1' {
			file write `rtfile' `svarfmt`row''
		}
		if `stdborder' {
			file write `rtfile' "\trowd\trqc`clwidth2'" _n
		}
	}
	* write last header row
	file write `rtfile' `svarfmt`colheadN''

	* write stat rows 1 to N-1 if stborder, else write all stat rows
	if `stdborder' {
		local strowNN_1 = `strowN' - 1
		/* turn off cell underlining */
		file write `rtfile' "\trowd\trqc`clwidth3'" _n
	}
	else {
		local strowNN_1 = `strowN'
	}
	
	forvalues row = `strow1'/`strowNN_1' {
		file write `rtfile' `svarfmt`row''
	}
	if `stdborder' {
		/* write last row */
		file write `rtfile' "\trowd\trqc`trbrdrb'`clwidths'" _n
		file write `rtfile' `svarfmt`strowN''
	}
	
	/* write notes rows */
	if `strowN' < `totrows' {
		local noterow1 = `strowN' + 1
		file write `rtfile' "\pard\qc`fssmall'" _n
		forvalues row = `noterow1'/`totrows' {
			file write `rtfile' (`varname'[`row']) "\par" _n
		}
	}
	
	* write closing curly bracket
	file write `rtfile' "}"
end  /* end out2rtf2 */







program define xmlout

* based on xmlsave
* xmlsave myfile, replace doctype(excel) legible


syntax using/ [, LEGible]

* LEGible not yet implemented
* assumes all columns are string; if numbers, then the format needs to be checked

tempname source saving

local save `"`using'.xml"'


*file open `source' using `"`using'"', read
file open `saving' using `save', write text replace

*file write `saving' `"`macval(line)'"'
file write `saving' `"<?xml version="1.0" encoding="US-ASCII" standalone="yes"?>"'
file write `saving' `"<?mso-application progid="Excel.Sheet"?>"'
file write `saving' `"<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet""'
file write `saving' `" xmlns:o="urn:schemas-microsoft-com:office:office""'
file write `saving' `" xmlns:x="urn:schemas-microsoft-com:office:excel""'
file write `saving' `" xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet""'
file write `saving' `" xmlns:html="http://www.w3.org/TR/REC-html40">"'
file write `saving' `"<DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">"'
file write `saving' `"<Author></Author>"'
file write `saving' `"<LastAuthor></LastAuthor>"'
file write `saving' `"<Created></Created>"'
file write `saving' `"<LastSaved></LastSaved>"'
file write `saving' `"<Company></Company>"'
file write `saving' `"<Version></Version>"'
file write `saving' `"</DocumentProperties>"'
file write `saving' `"<ExcelWorkbook  xmlns="urn:schemas-microsoft-com:office:excel">"'
file write `saving' `"<ProtectStructure>False</ProtectStructure>"'
file write `saving' `"<ProtectWindows>False</ProtectWindows>"'
file write `saving' `"</ExcelWorkbook>"'
file write `saving' `"<Styles>"'
file write `saving' `"<Style ss:ID="s1">"'
file write `saving' `"<Alignment ss:Horizontal="Center" ss:Vertical="Bottom"/>"'
file write `saving' `"<Font ss:Bold="1"/>"'
file write `saving' `"<NumberFormat/>"'
file write `saving' `"</Style>"'
file write `saving' `"</Styles>"'
file write `saving' `"<Names>"'
file write `saving' `"</Names>"'
file write `saving' `"<Worksheet ss:Name="Sheet1">"'

* set up file size
file write `saving' `"<Table ss:ExpandedColumnCount="`=c(k)'" ss:ExpandedRowCount="`=_N'"  x:FullColumns="1" x:FullRows="1">"' 
	
qui ds
* should be tostring and format here if dealing with numbers

* for bold-face
*<Cell ss:StyleID="s1"><Data ss:Type="String">make</Data></Cell>

forval num=1/`=_N' {
	file write `saving' `"<Row>"'
	
	foreach var in  `=r(varlist)' {
		local stuff `=`var'[`num']'
		local stuff : subinstr local stuff "<" "&lt;", all
		local stuff : subinstr local stuff ">" "&gt;", all

		file write `saving' `"<Cell><Data ss:Type="String">`macval(stuff)'</Data></Cell>"'
	}
	
	file write `saving' `"</Row>"'
}

file write `saving' `"</Table>"'
file write `saving' `"<WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">"'
file write `saving' `"<ProtectedObjects>False</ProtectedObjects>"'
file write `saving' `"<ProtectedScenarios>False</ProtectedScenarios>"'
file write `saving' `"</WorksheetOptions>"'
file write `saving' `"</Worksheet>"'
file write `saving' `"</Workbook>"'



* close out with the last line
*file write `saving' _n
*file close `source'

file close `saving'

end



* version history
* 0.9   Oct2005	beta
* 1.0   Oct2005	fixed the glitch with column (":") in the filename
*			the error in column title location due to converting to append default (which requires replace like behavior)
* 1.1   Nov2005	took seeout and seeing back to 8.2 by eliminating egen=rowmiss( ) option
*			alpha( ) option with user-defined levels of significance
*			took out 10pct option
*			fixed the error with label option (something with putting cap on _est_*)
* 1.2   Dec2005	handle very long outreg2 command (more than 244 characters)
*			handle if label was absent with label option
* 1.2.1 Aug2006	label: fixed when appending with only one column
* 1.3   Jun2007   inserted version control 8.2 for master program, changed strpos to index
*                 anova: local cons "nocons" fixed
*                 restoring the original active estimates per Richard W
* 1.4   Jan2008   compound quotes for tempfile: tmpf1, labelfile, appending; per Martin H, Austin N, Brian F
*                 made variables: tempname Vord1 Vord2 vartmp varsml v2plus (needed to be dropped in one place) Vorder merge2
*			tempvar tstat is now tstatVal to distinguish from tstat option; pvalVal by itself; Stat2 introduced
* 1.4.1 Feb2008	fixed itarogenic error: eform now uses the same pval for asterisks
*			introduced: coefVal seVal T_alpha (from t_alpha) ciLowVal ciHighVal betacoVal (from betacoef)
*			reported conflict between onecol and beta resolved
* 2.0.0 Mar2008	Unreleased (miscount rows in coeftxt2 when without replace/append but no file existed)
* 2.0.1 Mar2008   stat( ) option
*(2.1.0)		mfx option
*			noSE option
*                 tdec (rdec/dec2): defaults to 3 instead of 2; tdec apart from bdec
*			less: defaults to 0
*                 fixed: local minobs = max(1000,`=`brows*2'')
*			shorthand: colon fixed; APpend undocumented; seeout properly stripped
*			onecol: report _cons in multiple equations correctly
*			label: with multiple equation, "cap is added to avoid the last column v0 being misnamed" for last column
*			fixed: took out unnecessary refreshing the tempnames, which also dropped single b* variables
*			two instances of save/merge "`bcopy'" avoided in coeftxt2 by using expand and set obs
*			introduced: dec2 (from tdec option)
*			removed: Stat2, tempvar topoff
*			merging: sort according to the one with more specification
*			undocumented estats( ) option
* 2.0.2 Jun2008   sideway option: needed codes replaced for merging
*(2.2.0)		seeout: compatible with blank space in stats(aster)
*			seeing/seeout: took out -restore, preserve- from -seeing-
*			_estimates unhold: moved to bottom of outreg2Main
*			b_transpose, vc_diag_transpose: avoid overwriting b and vc
*			bracket( ): user-specified
*			paren( ): user-specified, noparen is subsumed under it
*			aster( ): user-specified
*			coefastr: taken out
*			Constant appears for label option
*			drop( ): enabled
*			keep( ): enabled
*			xstats/coefficients extraction: codes replaced, also fixes drop/keep/nocons
*			dtobit: typo fixed from "dtobit" to e(cmd)=="tobit"
*			addtext( ): enabled
*			eq_order2 renamed eq_order1; eq_order0, eq_order1, eq_order2
*			merging: fixed again, cons location & nocons handled correctly when multiple equation
*			COEFFICIENT: renamed VARIABLES
*			text: replacement .txt file
*			_long.text: replaced by text option
*			label: replaces, instead of label(insert)
*			seeout: accomodates label, no longer produces a new seeout blue hypertext
*			autodigits2: changed from 11 to 7 digits ==> e-06; 7 digits ==> e+06
*			long implies onecol
*			ctitle: accepts up to 2 column titles, but disconnected from multiple equation expression
*			partxtl3: avoids the closing parenthesis from being stripped by partxtl2
*			stats(aster) will disconnect aster from coef unless aster( ) specified
*			stats(beta): enabled
*			stats( ): works with eform
* 2.3.0 Jul2008	stats( ): previously dropped aster needlessly; fixed
*			xmlout instead of xmlsave
*			colon (:) in notes are ignored below "Observations"
*			tex & tex( ): column numbers fixed when ctitle( ) invoked; input into partxtl3 used the same local titlrow
*			seeout cmd: previously did not work with default label replacement
*			seeout option: previously hyperlink did not work if (:) was present in the file name (c:/, etc)
*			varName2 & eqName2 is dropped instead of varName & eqName: previously did not work if unabbrev was turned off
*			capture when writing outreg2_prf.ado to avoid read-only drives


*			bylist/byable
*			type in using is fatal: outreg2 usimg test, replace eform beta
*			stats(blank): hook up to paren( ) and bracket( ) and seeout/seeing (addstats & one gets the bottom moved as title/notes)
*			fix: ctitle disconnected from multiple equation expression
*			fix rest of stats( ) options
***************** Wish list and grumbles:
*			eq( ): matching
*			eq name and column name should be converted to labels as well
*			indicate in EQUATION column the beginning of extra statistics at the bottom
*			text suggested when long or label options are invoked
*			sideway causes pval tstat etc to appear in ctitle1

*			adjust autodigits so that 3333.333 shows up as 3333
*			insert rows between equations
*			vertical legends for pval tstat etc
*			pval should not be 0 (keep very small decimals)
*			remove rabbit ears from blue hypertexts
*			tex: nopretty as the default
*			Word rtf should have horizontal lines at top and bottom
*			omit r2 when r2 is missing
*			pseudo r2: e(r2_p) when logit
*			multiple r2's
*			takes MATRIX (to be used with sumout or estats)
*			label(replace) has LABELS re-marked as VARIABLES
*			label(replace) for equation names
*			xml from scratch
*			more do list: sumout; Vord1 Vord2 not needed?, eq_order2 should be inserted
*			tex: renamed latex
*			accept various matrices

*			stop going up and down between (1) and (2)
*			paren and bracket automatically specifies stats() list
*			eqdrop/eqkeep
*			merging: sort according to the one with more specification (THIS should be an option)


