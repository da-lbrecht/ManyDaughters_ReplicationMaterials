********************************************************************************
****************** H2 Do-File: Effect of Daughters on Political Preferences ***
********************************************************************************

* Author: [REDACTED]

* ============================
* Setup
* ============================

*clear all
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

* Save the merged dataset
//save "D:\Ashish\Desktop\Daughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\data\derived\h2_dataset.dta", replace
save "$DATA/h2_dataset.dta", replace

* ============================
* Sample Restriction
* ============================

* Keep only respondents with at least one child
* keep if pld0172 >= 1
* TODO: Reintroduce the above line after validating complete data

* ============================
* Clean and Prepare Variables
* ============================

* Clean political preference variables
foreach var in plj0615 plj0619 {
    replace `var' = . if `var' < 0
}

* Create Political Preferences Index
gen pol_pref_index = (plj0615 + plj0619) / 2

* Generate daughter dummy
gen daughter = 0
foreach var of varlist kidsex01-kidsex19 {
    replace daughter = 1 if `var' == 2
}

* ============================
* Clean Control Variables
* ============================

foreach var in ple0001 pgemplst pld0026 pld0028 pla0010 plh0175 plh0176 plh0151 {
    replace `var' = . if `var' < 0
}

* ============================
* Hypothesis Test for H2
* ============================

* Model: GLM with Gaussian family and identity link
glm pol_pref_index daughter sex, ///
    family(gaussian) link(identity)

* ============================
* Extract and Export Results
* ============================

local coefficient = _b[daughter]
local se = _se[daughter]
local test_stat_family = "t"
local df_residual = e(df_r)
local df_model = e(df_m)
local num_obs = e(N)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'

* Compute z-statistic and p-value
local z = abs(`coefficient'/`se')
local p_value = 2 * (1 - normal(`z'))
local pval_str = string(`p_value', "%9.4f")

* Interpret based on direction and significance

local interpretation "positive coefficient is in line with hypothesis"


* Display Results
di "Coefficient: `coefficient'"
di "Standard Error: `se'"
di "Test Statistic Family: `test_stat_family'"
di "Residual Degrees of Freedom: `df_residual'"
di "Model Degrees of Freedom: `df_model'"
di "p-value: " `p_value'
di "Number of observations: " `num_obs'
di "Number of participants: " `num_participants'
di "Interpretation of directionality: " "`interpretation'"

* Save to CSV
file open myfile using "$OUT/h2_results.csv", write replace
//file write myfile "Variable,Estimate,SE,Family,p-value,Interpretation" _n
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n //Adapted by DA to ensure consistent export of results.
file write myfile "`coefficient',`se',`test_stat_family',,,`p_value',,,`interpretation'" _n
file close myfile

exit
