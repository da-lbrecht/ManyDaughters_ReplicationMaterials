********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup *
* ============================

* clear all
version 16
set maxvar 10000
 
* INSTALL PACKAGES * 
* install all packages you need here, for example:
* ssc install outreg2

// cd "/Users/[REDACTED]/Desktop/#ManyDaughters_RT_AnalysisPackage" // commented out by David Albrecht

* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global IN "$ROOT/data/raw" 
global DATA "$ROOT/data/derived"
global OUT "$ROOT/out" 
*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA *
use "$IN/pl.dta", clear

keep (pid syear hid cid pld0025 pld0026 plh0258_h)  

merge m:1 hid cid syear using "$IN/hl.dta", nogen keepusing(hlc0005_h)
merge m:1 pid using "$IN/biobirth.dta", nogen keepusing(sumkids kidsex01 kidsex02 kidsex03 kidsex04 kidsex05 kidsex06 kidsex07 kidsex08 kidsex09 kidsex10 kidsex11 kidsex12 kidsex13 kidsex14 kidsex15)
merge m:1 pid syear using "$IN/ppathl.dta", nogen keepusing(gebjahr sex)
merge m:1 pid syear using "$IN/pgen.dta", nogen keepusing(pgfamstd pgemplst pgbilzeit pglabgro pgnation)
merge m:1 hid syear using "$IN/hgen.dta", nogen keepusing(hghinc)
merge m:1 pid cid using "$IN/bioparen.dta", nogen keepusing(fprofedu msedu fnat mnat)

label language EN

save "$DATA/h3_dataset.dta", replace

* ============================
* Analysis
* ============================

* FILTERS * 
 
*We keep individuals who answered hlc0005_h
tab hlc0005_h
replace hlc0005_h = . if hlc0005_h < 0

keep if hlc0005_h != .

*We will use data from 2015 to 2021 for hlc0005_h, corresponding to the most recent political period available (Angela Merkel last mandate)

keep if syear >= 2015 & syear <= 2021
bysort hid (syear pid): keep if _n == _N

*We keep individuals who have kids
keep if sumkids >= 1 

* GENERATE VARIABLES * 

*We use the dataset Biobirth to have the number of children and their gender from participants of interest.

gen num_daughter = 0
foreach var of varlist kidsex01-kidsex15 {
    replace num_daughter = num_daughter + (`var' == 2)
}
tab num_daughter

gen has_daughter = 0
replace has_daughter = 1 if num_daughter >= 1

* HYPOTHESIS TEST FOR H3 *

*The dependent variables are four political attitudes items administered in 2016 (plh0004, plj0615, plj0619, plj0620).
*The independent variables are whether the individual has daughters, the number of daughters, and his/her gender.

reg hlc0005_h has_daughter num_daughter gebjahr sex pgfamstd pgemplst pgbilzeit pglabgro plh0258 fprofedu msedu fnat mnat pgnation

* EXTRACT AND EXPORT RESULTS *

* Define the dependent variable
local dv hlc0005_h

* Run regression once — use the already filtered dataset above (years 2015–2021, kids only, last obs per household)
regress `dv' has_daughter num_daughter gebjahr sex pgfamstd pgemplst pgbilzeit pglabgro plh0258 fprofedu msedu fnat mnat pgnation

* Extract model info
local df_residual = e(df_r)
local df_model = e(df_m)
local num_obs = e(N)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'
local test_stat_family = "t"

* Extract coefficient and standard error for num_daughter
local coef_num = _b[num_daughter]
local se_num = _se[num_daughter]
local t_num = `coef_num' / `se_num'
local pval_num = 2 * ttail(`df_residual', abs(`t_num'))
local interpretation_num =  "positive coefficient is in line with hypothesis"

* Open a CSV file to store the regression results
file open myfile using "$OUT/h3_results.csv", write replace
file write myfile "dv,variable,coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`dv',num_daughter,`coef_num',`se_num',`test_stat_family',`df_residual',`df_model',`pval_num',`num_obs',`num_participants',`interpretation_num'" _n
file close myfile

* Display results in console
di "----------------------------------------"
di "Dependent variable: `dv' (2015–2021)"
di "num_daughter: coef=`coef_num', se=`se_num', p=`pval_num', interp=`interpretation_num'"
********************************************************************************

exit
