********************************************************************************
******************** H4 Do-File: Daughter Effect Moderated by Gender ***********
********************************************************************************

* Author: [REDACTED]

* ============================
* Setup
* ============================

* clear all
version 15.1
set maxvar 10000

* ============================
* Load and Merge Data
* ============================
/*
// File path inserted by David Albrecht for reproducibility check
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for datasets
global OUT "$ROOT/out" // folder for output (results, tables)

*/
use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)
/*
* Start with ppathl.dta
use "D:\Ashish\Desktop\Daughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\data\raw\ppathl.dta", clear
* Merge additional datasets
merge m:1 pid using "D:\Ashish\Desktop\Daughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\data\raw\biobirth.dta", nogen keep(master match)
merge 1:1 pid syear using "D:\Ashish\Desktop\Daughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\data\raw\pgen.dta", nogen keep(master match)
merge 1:1 pid syear using "D:\Ashish\Desktop\Daughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\data\raw\pl.dta", nogen keep(master match)
merge m:1 hid syear using "D:\Ashish\Desktop\Daughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\data\raw\hgen.dta", ///
    nogen keep(master match) keepusing(hid syear hghinc)
*/
* Set label language
label language EN

//save "D:\Ashish\Desktop\Daughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\data\derived\h4_dataset.dta", replace
save "$DATA/h4_dataset.dta", replace

* ============================
* Clean and Construct Variables
* ============================

* Clean gender equality questions
foreach var in plj0620 plj0621 plj0622 {
    replace `var' = . if `var' < 0
}

* Reverse-code where applicable
gen plj0621_r = (plj0621 - 1) * (10/6)
gen plj0622_r = (plj0622 - 1) * (10/6)

* Construct index
gen gender_eq_index = (plj0620 + plj0621_r + plj0622_r) / 3

* Generate daughter dummy
gen daughter = 0
foreach var of varlist kidsex01-kidsex19 {
    replace daughter = 1 if `var' == 2
}

* Clean sex variable
replace sex = . if sex < 0

* Interaction term: daughter × sex
gen dx_sex = daughter * sex

* ============================
* Run GLM Model with Interaction
* ============================

glm gender_eq_index daughter sex dx_sex, family(gaussian) link(identity)

* ============================
* Extract and Interpret Results
* ============================

local coefficient = _b[dx_sex]
local se = _se[dx_sex]
local family = "Wald"
local z = abs(`coefficient'/`se')
local p_value = 2 * (1 - normal(`z'))
local pval_str = string(`p_value', "%9.4f")

* Interpretation logic for interaction
local interpretation = "negative coefficient is in line with hypothesis"

* ============================
* Export Results
* ============================

file open myfile using "$OUT/h3_results.csv", write replace
//file write myfile "Variable,Estimate,SE,familiy,p-value,Interpretation" _n
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n //Adapted by DA to ensure consistent export of results.
file write myfile "`coefficient',`se',`familiy',,,`pval_str',,,`interpretation'" _n
file close myfile
