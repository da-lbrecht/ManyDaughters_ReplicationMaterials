********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup *
* ============================

*clear all
version 16
set maxvar 10000

* INSTALL PACKAGES *
*ssc install outreg2

* Set the command directory in my computer
// cd "/Users/[REDACTED]/Desktop/#ManyDaughters_RT_AnalysisPackage" // commented out by David Albrecht on 07.07.2025
/*
* CONFIGURE FILE PATHS *
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

keep (pid syear hid cid plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 pld0025 pld0026 pld0028)  

merge m:1 pid using "$IN/biobirth.dta", nogen keepusing(sumkids kidsex01 kidsex02 kidsex03 kidsex04 kidsex05 kidsex06 kidsex07 kidsex08 kidsex09 kidsex10 kidsex11 kidsex12 kidsex13 kidsex14 kidsex15)
merge m:1 pid syear using "$IN/ppathl.dta", nogen keepusing(gebjahr sex)
merge m:1 pid syear using "$IN/pgen.dta", nogen keepusing(pgfamstd pgemplst pgbilzeit pglabgro pgnation)
merge m:1 hid syear using "$IN/hgen.dta", nogen keepusing(hghinc)
merge m:1 pid syear using "$IN/pl.dta", nogen keepusing(plh0258_h)
merge m:1 pid cid using "$IN/bioparen.dta", nogen keepusing(fprofedu msedu fnat mnat)

label language EN

save "$DATA/h1_dataset.dta", replace

* ============================
* Analysis
* ============================

* FILTERS *

*We keep individuals who answered plj0620, plj0621, plj0622, plj0623, plj0624, plj0625
replace plj0620= . if plj0620 < 0
replace plj0621= . if plj0621 < 0
replace plj0622= . if plj0622 < 0
replace plj0623= . if plj0623 < 0
replace plj0624= . if plj0624 < 0
replace plj0625= . if plj0625 < 0

keep if plj0620 != . & plj0621 != . & plj0622 != . & plj0623 != . & plj0624 != . & plj0625 != .

*Data regading these variables are also available for 2017 and 2018, thus we also use these years to increase the sample size.

*Reverse code plj0623, plj0624, plj0625 
gen r_plj0623 = 8 - plj0623
gen r_plj0624 = 8 - plj0624
gen r_plj0625 = 8 - plj0625

*Generate the standardized sum of plj0620, plj0621, plj0622, plj0623, plj0624 and plj0625
gen plj_sum = (plj0620 + plj0621 + plj0622 + r_plj0623 + r_plj0624 + r_plj0625)/6

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

* HYPOTHESIS TEST FOR H1 *

*The dependent variables are six SOEP attitude items administered in 2016 (plj0620, plj0621, plj0622, plj0623, plj0624, plj0625).
*The independent variables are whether the individual as daughters, the number of daughters, and his/her gender.

reg plj_sum has_daughter num_daughter gebjahr sex pgfamstd pgemplst pgbilzeit pglabgro hghinc plh0258 fprofedu msedu fnat mnat pgnation

* EXTRACT AND EXPORT RESULTS * 

* Open CSV file for writing results
file open myfile using "$OUT/h1_results.csv", write replace
file write myfile "dv,variable,coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n

* Run regression
regress plj_sum has_daughter num_daughter gebjahr sex pgfamstd pgemplst pgbilzeit pglabgro hghinc plh0258 fprofedu msedu fnat mnat pgnation

* Extract model statistics
local df_residual = e(df_r)
local df_model = e(df_m)
local num_obs = e(N)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'
local test_stat_family = "t"

* Extract coefficients and SE for num_daughter
local coef_num = _b[num_daughter]
local se_num = _se[num_daughter]
local t_num = `coef_num' / `se_num'
local pval_num = 2 * ttail(`df_residual', abs(`t_num'))
local interpretation_num = "positive coefficient is in line with hypothesis"
* Export results to csv - num_daughter
file write myfile "pls_sum,num_daughter,`coef_num',`se_num',`test_stat_family',`df_residual',`df_model',`pval_num',`num_obs',`num_participants',`interpretation_num'" _n

file close myfile

********************************************************************************

exit
