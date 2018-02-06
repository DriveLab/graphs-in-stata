/*******************************************************************************
Overview:	
	This file reads cleaned and merged baseline, SMS, and endline 1 data; conducts
	treatment analysis specified in the PAP; and writes the estimated effects in
	.csv format.

Inputs:
	${input}data_endline_clean.dta
	${auxilliary}pstar.do
	${auxilliary}index_create_subroutine.do
	
Outputs:
	${output}results_spec*.csv

Created:
	2017/10/10 by LW from analysis_main_estout_v9_tex.do, but converts format
	to .csv

Updated:
	2017/10/19 by LW (v10)
		added spec1_plac and spec3_plac
	2017/10/23 by RG (v11):
		fixed specification 3 estimation code to avoid multicolinearity problem
		completely revised estimation code for spec1, spec1_plac, spec2, spec3,
			spec3_plac to support estimating both index and single-outcome models
		revised definition of outcome families to support estimating both index 
			and single-outcome models
		renamed vars in hypothesis testing for spec1, spec1_plac, spec2, spec3, 
			spec_plac to be consistent with restructured estimation code
		introduced $sampledef macro to allow placebo group to be dropped
		dropped attriters
		changed name convention for output files
		fixed some code bugs
		some formatting changes
	2017/11/14 by KH, LW, RG (v12):
		added & tested spec2_plac and spec4 code
		added risk & time measures as controls
		revised endline var names with _w instead of _w to avoid >32 chars
		removed 'coeflabels(...)' from the display specifications to avoid errors
	2017/11/16 by LW, RG (v13):
		added spec1 heterogeneity test (controlled by macro het_var)
		changed list of tests used by spec4

Notes:
	Before executing file, change macro $user
	
Outstanding tasks as of 2017/10/23:
	Update control names so we can run specs with controls
	Finish testing q-value code
	Finalize which test statistics should be displayed in tables - PARTDONE
	Decide whether to report interaction effects or subgroup means in main tables
*******************************************************************************/



clear all
capture log close
set more off
set matsize 2000
set seed 48762483
macro drop _all
program drop _all
matrix drop _all



/******************************************************************************
Set user path ( 1=LW 2=RG 3=LH 4=SP )
******************************************************************************/

//Set date to distinguish versions of output files
global date: display %tdCCYYNNDD date(c(current_date), "DMY")
global user = 3
	
if $user==1 {
	*global root =  "C:\Users\Laurel\Desktop\Box Sync\research_labor_harambee\"
	*global input = "${root}7.1. Data-PII_encrypted\dta_files\WS Baseline and Endline Merged\bl_endline_all.dta"
	*global output = "${root}12_analysis\ws_treatment_analysis\main_analysis\"
	*global clean = "${root}12_analysis\ws_data_prep\Master_cleaning\Clean Data\bl_endline_all_clean.dta"
	*global logs = "${output}"
	*log using "${logs}bl_endline_all_clean_v1.log", text replace
	
	global root =  "C:\Users\Laurel\OneDrive\Harambee_Labour_Market_Project\Analysis\"
	global output = "${root}14_analysis_results\ws_treatment_analysis_el1\results${date}\"
	global clean = "${root}Data\bl_endline_all_clean.dta"
	global auxilliary = "${root}12_programmes\auxilliary_files\" // edit LH
	global logs = "${root}12_programmes\ws_treatment_analysis\main_analysis\log\"  // edit LH
	log using "${logs}analysis_main_${date}.log", text replace
}

if $user==2 {
	global root = "/home/robgarlick/Dropbox/research_labour_harambee/"
	global input = "${root}7.2. Data-nonPII/dta_files/WS Baseline and Endline Merged/"
	global clean = "${input}bl_endline_all_clean.dta"
	global auxilliary = "${root}12_programmes/auxilliary_files/"
	global output = "${root}14_analysis_results/ws_treatment_analysis_el1/results${date}/"
	global logs = "${root}12_programmes/logs/"
	capture !mkdir "${output}"
	log using "${logs}analysis_main_${date}.log", text replace
}

if $user==3 {
	global root = "C:\Users\Lukas\Dropbox\research_labour_harambee_jpal\"
	global input = "${root}7.2. Data-nonPII\dta_files\WS Baseline and Endline Merged\"
	global output = "${root}14_analysis_results/ws_treatment_analysis_el1/graphs/"
	global auxilliary = "${root}12_programmes\auxilliary_files\"
	global clean = "${root}7.2. Data-nonPII\dta_files\WS Baseline and Endline Merged\bl_endline_all_clean.dta" //LW Edit (we changed where we save clean data)
	global logs = "${root}12_programmes\logs\"
	log using "${logs}treatment_effect_graphs_v1.log", text replace
}

if $user==4 {
if c(os)=="Windows" & c(username)=="pimkinas" gl dir "C:\Users\pimkinas" 
if c(os)=="Windows" & c(username)=="WB475160" gl dir "C:\Users\WB475160" 
	global root = "$dir\Box Sync\research_labor_harambee\"
	global input = "${root}12_analysis\ws_data_prep\Master_cleaning\Clean Data\"
	global output ="${root}12_analysis\ws_treatment_analysis\main_analysis\" 
	global clean= "${input}bl_endline_all_clean.dta"
	global logs = "${output}\"
	log using "${logs}analysis_main_v4.log", text replace
}



/******************************************************************************
Declare programme parameters
******************************************************************************/

/** Use only random subsample to test code quickly. RG 2017/10/23 **/

global testcode = 1


/** Define condition for flagging attriters, whom we drop. RG 2017/10/23 **/

global attritcondition = "el_date == ."


/** Select main treatment specifications to run with individual outcomes. **/

global spec1_nocont = 0
global spec1_cont = 0

global spec2_nocont = 0
global spec2_cont = 0

global spec3_nocont = 0
global spec3_cont = 0

global spec4_nocont = 0
global spec4_cont = 0


/** Select main treatment specifications to run with indices. **/

global spec1_ind_nocont = 0
global spec1_ind_cont = 0

global spec2_ind_nocont = 0
global spec2_ind_cont = 0

global spec3_ind_nocont = 0
global spec3_ind_cont = 0

global spec4_ind_nocont = 0
global spec4_ind_cont = 0


/** Select placebo treatment specifications to run with individual outcomes. **/

global spec1_plac_nocont = 1
global spec1_plac_cont = 0

global spec2_plac_nocont = 0
global spec2_plac_cont = 0

global spec3_plac_nocont = 0
global spec3_plac_cont = 0


/** Select placebo treatment specifications to run with indices. **/

global spec1_ind_plac_nocont = 0
global spec1_ind_plac_cont = 0

global spec2_ind_plac_nocont = 0
global spec2_ind_plac_cont = 0

global spec3_ind_plac_nocont = 0
global spec3_ind_plac_cont = 0

/** Select heterogeneity specification to run with individual outcomes. **/		//Added by LW: 2017/11/16

global spec1_het_nocont = 1
global spec1_het_cont = 0



/** Select whether to include placebo group in main analysis. **/

global includeplacebo = 0
if $includeplacebo == 0 {
	global sampledef = "if placebo == 0"
	}
else {
	global sampledef = ""
	}


/** Define treatment groups, control variables, and heterogeneity markers. **/

global public 		public
global private 		private
global placebo 		placebo
global control 		Edn_Matric bl_score_num bl_score_lit bl_score_cft bl_score_grit noncog_score ///
					bl_belsco_num bl_belsco_lit_ter bl_belsco_cft_ter bl_belest_index age bl_male /// 
					bl_emp_7d bl_time_med_nomiss bl_time_presentbias_nomiss  bl_risk_high_nomiss
global controllabels 	"At least Matric" "Baseline numeracy score" "Baseline literacy score" "Baseline cft score" ///
						"Baseline grit score" "Baseline noncog score" "Baseline numeracy belief" "Baseline literacy belief" ///
						"Baseline cft belief" "Baseline self-esteem" "Age" "Male dummy" "Worked in last 7 days" "Above median patience" "Present bias dummy" "Above median risk aversion"
global het_var 		bl_male  Edn_Degree Edn_Matric Edn_Cert ///
					bl_emp_7d bl_emp_hist_high1 bl_selfest_demean /// //Added next three lines by LW 2017/11/17 to coincide with PAP
					skill_pos skill_neg bl_risk_high_nomiss ///
					bl_time_med bl_time_presentbias
					
global rand_block 	i.bl_block
global ability 		skill
global shock 		shock


/** Define additional regression options. **/

global reg_opt cluster(bl_date_baseline)


/** Define outcomes by family **/
global fam_main_treat  emp_7d  emp_ever emp_hour_all_w emp_hour_all_uncd_wins emp_earn_all_w ///
 emp_earn_all_uncd_w emp_wage_all_w emp_wage_all_uncd_w  emp_m1 emp_m2 emp_m3 


		
							
													
/*******************************************************************************
Define regression specifications
*******************************************************************************/

global spec1 $public $private

global spec1_plac	$public $private $placebo

foreach het in $het_var {		//Added by LW: 2017/11/16. NB: Different specification name for each het_var
global spec1_`het' public private public_`het' private_`het' `het' 
}

global spec2   ///
	public public_shock_neg public_shock_pos   ///
	private private_shock_neg private_shock_pos  ///
	shock_neg shock_pos

global spec2_plac   ///
	public public_shock_neg public_shock_pos   ///
	private private_shock_neg private_shock_pos  ///
	placebo placebo_shock_neg placebo_shock_pos  ///
	shock_neg shock_pos

global spec3 	  ///
	public public_skill_neg public_skill_pos   ///Change
	private private_skill_neg private_skill_pos  ///
	skill_neg skill_pos

global spec3_plac 	  ///
	public public_skill_neg public_skill_pos   ///
	private private_skill_neg private_skill_pos  ///
	placebo placebo_skill_neg placebo_skill_pos  ///
	skill_neg skill_pos

// RG 2017/10/23 This must be updated to work with revised spec4 estimation code
global spec4 	///
	public public_skill_neg public_skill_pos   ///
		public_shock_neg public_shock_pos   ///
	private private_skill_neg private_skill_pos   ///
		private_shock_neg private_shock_pos   ///
	shock_neg shock_pos   ///
	skill_neg skill_pos   ///
	shock_neg_skill_neg shock_neg_skill_pos   ///
	shock_pos_skill_neg shock_pos_skill_pos   ///
	public_shock_pos_skill_pos public_shock_pos_skill_neg ///
	public_shock_neg_skill_pos public_shock_neg_skill_neg ///
	private_shock_pos_skill_pos private_shock_pos_skill_neg ///
	private_shock_neg_skill_pos private_shock_neg_skill_neg

/*
global spec4 	$public $private ///
				c.${public}#c.${shock}_pos c.${private}#c.${shock}_pos ///
				c.${public}#c.${shock}_neg c.${private}#c.${shock}_neg ///
				${shock}_pos ${shock}_neg ///
				c.${public}#c.${ability}_pos c.${private}#c.${ability}_pos ///
				c.${public}#c.${ability}_neg c.${private}#c.${ability}_neg  ///
				${ability}_neg ${ability}_pos ///
				c.${shock}_pos#c.${ability}_pos c.${shock}_neg#c.${ability}_pos ///
				c.${shock}_pos#c.${ability}_neg c.${shock}_neg#c.${ability}_neg ///
				c.${public}#c.${shock}_pos#c.${ability}_pos  c.${public}#c.${shock}_pos#c.${ability}_neg ///
				c.${public}#c.${shock}_neg#c.${ability}_pos  c.${public}#c.${shock}_neg#c.${ability}_neg ///
				c.${private}#c.${shock}_pos#c.${ability}_pos c.${private}#c.${shock}_pos#c.${ability}_neg ///
				c.${private}#c.${shock}_neg#c.${ability}_pos c.${private}#c.${shock}_neg#c.${ability}_neg 
*/
 
 
/*******************************************************************************
*** Subroutines for post-estimation hypothesis testing ***
*******************************************************************************/

/** Call separate programme to control display format in specifications 2-4. **/

do ${auxilliary}pstar.do
do ${auxilliary}minq.do


************************************************
*** Specification 1, updated 2017/10/23 by RG ***
************************************************/

*** Q-values and hypothesis testing ***

cap prog drop spec1_test
prog define  spec1_test 

loc regvars "${y$k}"  // needs k as family counter intput
loc length: list sizeof regvars
global length=`length'
/* Create results vectors */

forval i = 1/4 {

	mat def B`i' = J(`length', 1, .)
	mat rownames B`i' = ${y$k}

	mat def SE`i' = J(`length', 1, .)
	mat rownames SE`i' = ${y$k}

	mat def P`i' = J(`length', 1, .)
	mat rownames P`i' = ${y$k}

	}

/* Hypothesis tests */

foreach yvar in ${y$k} { 

	est restore e${k}_`yvar'
		
	loc H1 = "_b[public]"
		lincom `H1'
		
		mat def B1[rownumb(B1, "`yvar'"), 1] = `r(estimate)'
		mat def SE1[rownumb(SE1, "`yvar'"), 1] = `r(se)'
		test
		mat def P1[rownumb(P1, "`yvar'"), 1] = `r(p)'
		
	loc H2 = "_b[private]"
	
		lincom `H2'

		mat def B2[rownumb(B2, "`yvar'"), 1] = `r(estimate)'
		mat def SE2[rownumb(SE2, "`yvar'"), 1] = `r(se)'
		test
		mat def P2[rownumb(P2, "`yvar'"), 1] = `r(p)'	
	
	loc H3 = "_b[public]-_b[private]"
	
		lincom `H3'

		mat def B3[rownumb(B3, "`yvar'"), 1] = `r(estimate)'
		mat def SE3[rownumb(SE3, "`yvar'"), 1] = `r(se)'
		test
		mat def P3[rownumb(P3, "`yvar'"), 1] = `r(p)'
		
	loc H4 = "_b[public]=_b[private]=0"
	
	
		mat def B4[rownumb(B4, "`yvar'"), 1] = .
		mat def SE4[rownumb(SE4, "`yvar'"), 1] = .
		test `H4'
		mat def P4[rownumb(P4, "`yvar'"), 1] = `r(p)'	
}

forval i = 1/4 {
minq P`i', q("Q`i'") step(0.001)
	mat rownames Q`i' =  ${y$k}
	}

/* gen locals for output */
foreach yvar in ${y$k} { 
est restore e${k}_`yvar'

** Generate vector of q-values to be used by esttab **
		matrix temp = colsof(e(b))  // get length of coefficient vector
		local lengths_spec = temp[1,1]
		local names : colnames e(b) // get names of coefficient vector (neessary for esttab to find values)
		matrix  q=J(1,`lengths_spec',.)
		matrix colnames q = `names'
		matrix  q[1,colnumb(q, "public")] = el("Q1", rownumb(Q1, "`yvar'"), 1) //input q-values
		matrix  q[1,colnumb(q, "private")] =  el("Q2", rownumb(Q2, "`yvar'"), 1) //input q-values

		estadd matrix q , replace
		estadd scalar pub = el("P1", rownumb(P1, "`yvar'"),1), replace		//Added by LW: 10/10
		estadd scalar priv=el("P2", rownumb(P2, "`yvar'"),1), replace		//Added by LW: 10/10
		estadd scalar pubq=el("Q1", rownumb(Q1, "`yvar'"), 1), replace		//Added by LW: 10/10
		estadd scalar privq=el("Q2", rownumb(Q2, "`yvar'"), 1), replace		//Added by LW: 10/10
		estadd scalar ppq = el("Q3", rownumb(Q3, "`yvar'"), 1), replace	
		estadd scalar pp = el("P3", rownumb(P3, "`yvar'"), 1), replace	
		estadd scalar pp0q = el("Q4", rownumb(Q4, "`yvar'"), 1), replace
		estadd scalar pp0 = el("P4", rownumb(P4, "`yvar'"), 1), replace	
	
		qui	summarize el_`yvar' if treatment==0
		estadd scalar ymean=r(mean), replace

eststo e${k}_`yvar'

}

end


use "${clean}", clear


/** Temporary variable renaming from 2017/11/14.
		Delete once we've rerun the data creation process. **/

capture rename el_emp_earn_all_unif el_emp_earn_all
capture rename el_emp_earn_all_unif_wins el_emp_earn_all_w
capture rename el_emp_earn_all_unif_uncd el_emp_earn_all_uncd
capture rename el_emp_earn_all_unif_uncd_wins el_emp_earn_all_uncd_wins

capture rename el_emp_ever_earn_unif el_emp_earn_ever
capture rename el_emp_ever_earn_unif_wins el_emp_earn_ever_w
capture rename el_emp_ever_earn_unif_uncd el_emp_earn_ever_uncd
capture rename el_emp_ever_earn_unif_uncd_wins el_emp_earn_ever_uncd_w




capture drop private public placebo
gen private = ( treatment == 1 )
	label var private "Private treatment"
gen public = ( treatment == 2 )
	label var public "Public treatment"
gen placebo = ( treatment == 3 )
	label var placebo "Placebo treatment"
	
drop if placebo==1	

** Gen baseline variables for ancova **
gen bl_emp_hour_all_uncd_wins=bl_emp_hour_all_wins if bl_emp_hour_all_wins!=.
replace bl_emp_hour_all_uncd_wins=0 if	bl_emp_7d==0


** Recode missing values	
gen bl_emp_ever=0
gen bl_emp_m1=0
gen bl_emp_m2=0
gen bl_emp_m3=0


	foreach var in $fam_main_treat {
	sum bl_`var'
	replace bl_`var'=`r(mean)' if missing(bl_`var')
	}
	
	foreach var in  $control {
	sum `var'
	replace `var'=`r(mean)' if missing(`var')
	}
	
	

	

 
  ** Generate employement and hours results **

cap gen bpub=. in 1/12
cap gen bpriv=. in 1/12
cap gen cmean=. in 1/12
cap gen uc=. in 1/12
cap gen lc=. in 1/12
cap gen id=(_n-1)*1.5 in 1/12
cap gen bl_emp_ever=0 // quick fix
**
global fam_main_treat  emp_7d  emp_ever emp_hour_all_w emp_hour_all_uncd_wins

local k=1
foreach var in $fam_main_treat {
// Get control group mean
sum el_`var' if treatment==0 
global el_`var'_mean=`r(mean)'

reg  el_`var' $spec1_plac $control $rand_block bl_`var' if !missing(el_date), $reg_opt
di "`k'"
replace bpub=_b[public]+${el_`var'_mean} in `k'
replace uc=_b[public]+${el_`var'_mean}+1.96*_se[public] in `k'
replace lc=_b[public]+${el_`var'_mean}-1.96*_se[public] in `k'
local k=1+`k'
replace cmean = ${el_`var'_mean} in `k'
local k=1+`k'
replace bpriv=_b[private]+${el_`var'_mean} in `k'
replace uc=_b[private]+${el_`var'_mean}+1.96*_se[private] in `k'
replace lc=_b[private]+${el_`var'_mean}-1.96*_se[private] in `k'
local k=1+`k'
}

replace id= 0.25 in 1
replace id=0.5 in 2
replace id=0.75 in 3

replace id= 1.5 in 4
replace id=1.75 in 5
replace id=2 in 6

replace id= 2.75 in 7
replace id=3 in 8
replace id=3.25 in 9

replace id= 4 in 10
replace id=4.25 in 11
replace id=4.5 in 12


*** Generate graph ***

*Continue here *

twoway bar cmean id in 1/6,  yaxis(1) color(gs14) || bar cmean id in 7/12,  yaxis(2) color(gs8)  ///
||  scatter bpub id in 1/6, color(eltblue) msymbol(T)  yaxis(1)  ///
||  scatter bpriv id in 1/6, color(erose)  msymbol(S)  yaxis(1) || rcap uc lc id in 1/6, color (gs12)  yaxis(1)  ///
 || scatter bpub id in 7/12, color(eltblue) msymbol(T) yline(0)  yaxis(2)  ///
||  scatter bpriv id in 7/12, color(erose)  msymbol(S)  yaxis(2) || rcap uc lc id in 7/12, color (gs12)  yaxis(2)  ///
title("Treatment effects on employment") xtitle("") xscale(range(0 5))  xlab(  0.5 `""Worked last " "week""' ///
   1.75 `"Worked ever"'   3 `" "Hours" "(employed)""'  ///
   4.25 `" "Hours" "(all)""' , noticks)  ///
legend(order(2 "Public" 3 "Private" 4 "95% CI" 1 "       &" 5 "Control group mean" ) cols(3)) ///
plotregion(fcolor(white)) graphregion(fcolor(white)) bgcolor(white) yscale(range(0 1) axis(1)) ///
ytitle("Proportion of individuals", axis(1)) ytitle("Working hours per week", axis(2)) ///
yscale(range(0 40)  axis(2)) ylabel(0(0.2)1, axis(1))  yline(0, axis(2) lstyle(foreground))


graph export "${output}treatment_effect_employment.png", as(png) replace  
  
  
  ** Generate earnings and wage results **

cap gen bpub=. in 1/12
cap gen bpriv=. in 1/12
cap gen cmean=. in 1/12
cap gen uc=. in 1/12
cap gen lc=. in 1/12
cap gen id=(_n-1)*1.5 in 1/12



global fam_main_treat  emp_earn_all_w  emp_earn_all_uncd_w emp_wage_all_w emp_wage_all_uncd_w

local k=1
foreach var in $fam_main_treat {
// Get control group mean
sum el_`var' if treatment==0 
global el_`var'_mean=`r(mean)'

reg  el_`var' $spec1_plac $control $rand_block bl_`var' if !missing(el_date), $reg_opt
di "`k'"
replace bpub=_b[public]+${el_`var'_mean} in `k'
replace uc=_b[public]+${el_`var'_mean}+1.96*_se[public] in `k'
replace lc=_b[public]+${el_`var'_mean}-1.96*_se[public] in `k'
local k=1+`k'
replace cmean = ${el_`var'_mean} in `k'
local k=1+`k'
replace bpriv=_b[private]+${el_`var'_mean} in `k'
replace uc=_b[private]+${el_`var'_mean}+1.96*_se[private] in `k'
replace lc=_b[private]+${el_`var'_mean}-1.96*_se[private] in `k'
local k=1+`k'
}

replace id= 0.25 in 1
replace id=0.5 in 2
replace id=0.75 in 3

replace id= 1.5 in 4
replace id=1.75 in 5
replace id=2 in 6

replace id= 2.75 in 7
replace id=3 in 8
replace id=3.25 in 9

replace id= 4 in 10
replace id=4.25 in 11
replace id=4.5 in 12


*** Generate graph ***

*Continue here *

twoway bar cmean id in 1/6,  yaxis(1) color(gs14) || bar cmean id in 7/12,  yaxis(2) color(gs8)  ///
||  scatter bpub id in 1/6, color(eltblue) msymbol(T)  yaxis(1)  ///
||  scatter bpriv id in 1/6, color(erose)  msymbol(S)  yaxis(1) || rcap uc lc id in 1/6, color (gs12)  yaxis(1)  ///
 || scatter bpub id in 7/12, color(eltblue) msymbol(T) yline(0)  yaxis(2)  ///
||  scatter bpriv id in 7/12, color(erose)  msymbol(S)  yaxis(2) || rcap uc lc id in 7/12, color (gs12)  yaxis(2)  ///
title("Treatment effects on earnings") xtitle("") xscale(range(0 5))  xlab(  0.5 `""Earnings" "(employed)""' ///
   1.75 `""Earnings" "(all)""'   3 `" "Wages" "(employed)""'  ///
   4.25 `" "Wages" "(all)""' , noticks)  ///
legend(order(2 "Public" 3 "Private" 4 "95% CI" 1 "       &" 5 "Control group mean" ) cols(3)) ///
plotregion(fcolor(white)) graphregion(fcolor(white)) bgcolor(white) yscale(range(0 850) axis(1)) ///
ylab( 0 200 400 600 800, axis(1))   ytitle("Rand per week", axis(1)) ytitle("Rand per hour", axis(2)) ///
 yline(0, axis(2) lstyle(foreground))


graph export "${output}treatment_effect_earnings.png", as(png) replace 

*** Treatment effect over time **

cap gen ucpriv=. in 1/12
cap gen lcpriv=. in 1/12

replace id = 1 in 1
replace id = 2 in 2
replace id = 3 in 3

local k=1
foreach var in emp_m1 emp_m2 emp_m3 {

// Get control group mean
sum el_`var' if treatment==0 
global el_`var'_mean=`r(mean)'

reg  el_`var' $spec1_plac $control $rand_block bl_`var' if !missing(el_date), $reg_opt

replace bpub=_b[public]+${el_`var'_mean} in `k'
replace uc=_b[public]+${el_`var'_mean}+1.96*_se[public] in `k'
replace lc=_b[public]+${el_`var'_mean}-1.96*_se[public] in `k'
replace cmean = ${el_`var'_mean} in `k'
replace bpriv=_b[private]+${el_`var'_mean} in `k'
replace ucpriv=_b[private]+${el_`var'_mean}+1.96*_se[private] in `k'
replace lcpriv=_b[private]+${el_`var'_mean}-1.96*_se[private] in `k'
local k=1+`k'
}

twoway line cmean id in 1/3,  color(gs4)  ///
||  line bpub id in 1/3, color(eltblue) msymbol(T)  yaxis(1)  ///
||  line bpriv id in 1/3, color(erose)  msymbol(S)  yaxis(1) /// 
|| rcap uc lc id in 1/3, color(eltblue%20)   || rcap ucpriv lcpriv id in 1/3, color(erose%20) ///
 title("Treatment effects on employment") xtitle("") xscale(range(0.5 3.5))  xlab(  1 "Month 1" 2"Month 2" 3 "Month 3" , noticks)  ///
legend(order(2 "Public" 3 "Private"  1 "Control group" 4 "    &" 5 "95% CI" ) cols(3)) ///
plotregion(fcolor(white)) graphregion(fcolor(white)) bgcolor(white) yscale(range(0.35 0.55)) ///
ylab( 0.35(0.05)0.55 )   ytitle("Proportion employed")  yline(0, lstyle(foreground))


graph export "${output}treatment_effect_employment_months.png", as(png) replace 


/*
local k=1
foreach var in $fam_main_treat {
sum el_`var' if treatment==0 // Get control group mean

replace el_`var'=el_`var'/`r(mean)' //Recode as percentage chage of mean

local mean 

global el_`var'_mean=`r(mean)'

reg  el_`var' $spec1_plac $control $rand_block bl_`var' if !missing(el_date), $reg_opt

replace bpub=_b[public] in `k'
replace uc=_b[public]+1.96*_se[public] in `k'
replace lc=_b[public]-1.96*_se[public] in `k'
local k=1+`k'
replace bpriv=_b[private] in `k'
replace uc=_b[private]+1.96*_se[private] in `k'
replace lc=_b[private]-1.96*_se[private] in `k'
local k=1+`k'
}


*** Generate graph ***
global el_emp_7d_mean=100*$el_emp_7d_mean //Display in percent

twoway scatter bpub id, color(eltblue) msymbol(T) yline(0) ||  scatter bpriv id, color(erose)  msymbol(S) || rcap uc lc id, color (gs12) ///
title("Treatment effects") subtitle("as percentage of control group means") ///
legend(order(1 "Public" 2 "Private" 3 "95% CI")) ///
 xlab(  0.75 `""Employed last week" "(mean: `: di %3.2f `=$el_emp_7d_mean''%)""' ///
   3.75 `" "Working hours" "(mean: `: di %3.2f `=$el_emp_hour_all_uncd_wins_mean'')""'  ///
   6.75 `" "Weekly earnings" "(mean: R `: di %3.2f `=$el_emp_earn_all_uncd_wins_mean'')""'  ///
   9.75 `" "Hourly wage" "(mean: R `: di %3.2f `=$el_emp_wage_all_uncd_wins_mean'')""'  ///
  , noticks) xtitle("") xscale(range(-0.5 11)) ///
  plotregion(fcolor(white)) graphregion(fcolor(white)) bgcolor(white)
 */






