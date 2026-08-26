********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup *
* ============================

*clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:
* ssc install outreg2
/*
* CONFIGURE FILE PATHS * (Please do not change)
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

keep (pid syear hid cid plh0004 pld0025 pld0026)  

merge m:1 pid using "$IN/biobirth.dta", nogen keepusing(sumkids kidsex01 kidsex02 kidsex03 kidsex04 kidsex05 kidsex06 kidsex07 kidsex08 kidsex09 kidsex10 kidsex11 kidsex12 kidsex13 kidsex14 kidsex15)
merge m:1 pid syear using "$IN/ppathl.dta", nogen keepusing(gebjahr sex)
merge m:1 pid syear using "$IN/pgen.dta", nogen keepusing(pgfamstd pgemplst pgbilzeit pglabgro pgnation)
merge m:1 hid syear using "$IN/hgen.dta", nogen keepusing(hghinc)
merge m:1 pid syear using "$IN/pl.dta", nogen keepusing(plh0258_h)
merge m:1 pid cid using "$IN/bioparen.dta", nogen keepusing(fprofedu msedu fnat mnat)

label language EN

save "$DATA/h2_dataset.dta", replace

* ============================
* Analysis
* ============================

* FILTERS * 
 
*We keep individuals who answered plh0004, plj0615, plj0619, plj0620
replace plh0004= . if plh0004 < 0

keep if plh0004 != .

*We will use data from 2015 to 2021 for plh0004, corresponding to the most recent political period available (Angela Merkel last mandate)

keep if syear >= 2015 & syear <= 2021
bysort pid: egen maxyear = max(syear)
replace plh0004 = . if syear != maxyear
drop maxyear

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

* HYPOTHESIS TEST FOR H2 *

*The dependent variables are four political attitudse items administered in 2016 (plh0004, plj0615, plj0619, plj0620).
*The independent variables are whether the individual as daughters, the number of daughters, and his/her gender.

reg plh0004 has_daughter num_daughter gebjahr sex pgfamstd pgemplst pgbilzeit pglabgro hghinc plh0258 fprofedu msedu fnat mnat pgnation

* EXTRACT AND EXPORT RESULTS *

* Open a CSV file to store the regression results
file open myfile using "$OUT/h2_results.csv", write replace
file write myfile "dv,variable,coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n

* Define the dependent variable and restrict to Merkel’s term
local dv plh0004

* Run the regression for 2015–2021 only (removed has_daughter)
regress `dv' num_daughter gebjahr sex pgfamstd pgemplst pgbilzeit pglabgro hghinc plh0258 fprofedu msedu fnat mnat pgnation if inrange(syear, 2015, 2021)

* Extract degrees of freedom and sample info
local df_residual = e(df_r)
local df_model = e(df_m)
local num_obs = e(N)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'
local test_stat_family = "t"

* Extract stats for num_daughter only
local coef_num = _b[num_daughter]
local se_num = _se[num_daughter]
local t_num = `coef_num' / `se_num'
local pval_num = 2 * ttail(`df_residual', abs(`t_num'))
local interpretation_num = "positive coefficient is in line with hypothesis"

* Display results
di "----------------------------------------"
di "Dependent variable: `dv' (2015–2021)"
di "num_daughter: coef=`coef_num', se=`se_num', p=`pval_num', interp=`interpretation_num'"

* Write to CSV: num_daughter only
file write myfile "`dv',num_daughter,`coef_num',`se_num',`test_stat_family',`df_residual',`df_model',`pval_num',`num_obs',`num_participants',`interpretation_num'" _n

* Close the file
file close myfile
********************************************************************************

exit
