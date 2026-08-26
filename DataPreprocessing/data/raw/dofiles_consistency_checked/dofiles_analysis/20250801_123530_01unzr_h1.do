********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * * ============================

*clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2
/*
* CONFIGURE FILE PATHS * (Please do not change)
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
/* Adapted by David Albrecht for reproducibility check
global IN "F:\ManyDaughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\data\raw" // folder, in which the original datasets are stored
global DATA "F:\ManyDaughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\data\derived" // folder for datasets
global OUT "F:\ManyDaughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage\out" // folder for output (results, tables)
*/
global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for datasets
global OUT "$ROOT/out" // folder for output (results, tables)
*/
* ================================================================
* Data Preparation for H1
* ================================================================

* This script merges the necessary datasets to create the analysis file for H1.
* It starts with ppathl and merges biobirth for fertility history,
* and pl and pgen for dependent and control variables.

* LOAD AND MERGE DATA *

clear
use "$IN/ppathl.dta", clear // Use ppathl as the base file, which contains person-level tracking info
keep pid syear gebjahr sex sampreg corigin // Keep IDs and controls

* Merge with pl to get the dependent variables
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
    keepusing(plh0395i01-plh0395i06) // Keeping DVs for H1 

* Merge with pgen to get the education control variable
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) ///
    keepusing(pgbilzeit) // Keeping education variable 

* Merge with biobirth for IV construction
sort pid
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) ///
    keepusing(sumkids kidsex*) // Keeping total children and sex of each child 

* Set label language to English for consistency
label language EN

* Save the intermediate merged dataset
save "$DATA/h1_new_merged.dta", replace
di "Data preparation complete. Merged dataset saved as h1_new_merged.dta"

* ================================================================
* Filtering and Variable Generation 
* ================================================================

* This script takes the merged dataset and creates the final analysis-ready file.

* Start with the merged dataset
use "$DATA/h1_new_merged.dta", clear

* --- Step 1: Filtering the sample ---
keep if syear == 2019
gen age = 2019 - gebjahr // Calculate age
keep if age >= 18 & age < . // Keep adults 18+ 
keep if sumkids >= 1 & sumkids < . // 

* --- Step 2: Cleaning all variables ---
* Clean Dependent Variables
foreach var of varlist plh* {
    replace `var' = . if `var' < 1
}
* Clean Control Variables
foreach var of varlist sex sampreg pgbilzeit {
    replace `var' = . if `var' < 1
}
replace corigin = . if corigin < 1
gen is_german = (corigin == 1)
replace is_german = . if corigin == .

* --- Step 3: Generate analysis variables ---
gen num_daughters = 0
forval i = 1/19 {
    local child_num : di %02.0f `i'   
    replace num_daughters = num_daughters + 1 if kidsex`child_num' == 2
}
gen share_daughters = num_daughters / sumkids
gen age_sq = age^2

* --- Step 4: Save the final dataset ---
keep pid syear plh* share_daughters age age_sq sex sampreg is_german pgbilzeit
save "$DATA/h1_new_final.dta", replace
di "Final analysis-ready dataset for H1 created as h1_new_final.dta"

* ================================================================
* H1 Analysis: Final Single-Test Version (Based on Revised PAP)
* ================================================================
* NOTE: Per project requirements, the analysis was restricted to a single
* regression-based test. We selected 'plh0395i05' as the primary dependent
* variable and the 'same-gender children only' specification as the
* primary model, as outlined in our revised PAP.
* ================================================================

use "$DATA/h1_new_final.dta", clear

* --- Step 1: Create the sample and variables for the chosen test ---
* Restrict sample to parents with only same-gender children
keep if share_daughters == 0 | share_daughters == 1
* Create the primary independent variable
gen has_only_daughters = (share_daughters == 1)

* --- Step 2: Define variables for the single chosen test ---
local dv "plh0395i05" // The single DV selected in the revised PAP
local ind_vars "has_only_daughters age age_sq sex sampreg is_german pgbilzeit"

* --- Step 3: Prepare the single CSV file ---
file open myfile using "$OUT/h1_results.csv", write replace
//file write myfile "dependent_var,coefficient,se,test_stat_family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n //Adapted by DA to ensure consistent export of results.

* --- Step 4: Run the single, chosen model ---
di ""
di "********************************************************************************"
di "Processing Final H1 Model for: `dv' (Same-Gender Specification)"
di "********************************************************************************"

oprobit `dv' `ind_vars'

* --- Step 5: Extract and export results ---
local coefficient = _b[has_only_daughters]
local se = _se[has_only_daughters]
local test_stat_family "z"
local df_residual .
local df_model .
local z_stat = `coefficient' / `se'
local p_value = 2 * (1 - normal(abs(`z_stat')))
local num_obs = e(N)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'


local interpretation "positive coefficient is in line with hypothesis"

file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n

* --- Step 6: Close the file ---
file close myfile

di ""
di "Final analysis for H1 complete. Results saved in 'h1_final_results.csv'."


/*
================================================================================
================== SUPERSEDED ANALYSIS (COMMENTED OUT) =========================
================================================================================
*
* The following code block contains the previous analysis strategy, which
* involved multiple dependent variables and model specifications. It has been
* commented out to adhere to the project's single-test requirement.
*

* ================================================================
* H1 Analysis: Combined Output (NOW SUPERSEDED)
* ================================================================

use "$DATA/h1_new_final.dta", clear

file open myfile using "$OUT/h1_results_combined.csv", write replace
file write myfile "dependent_var," ///
    "coef_share_daughters,se_share_daughters,p_value_share_daughters,n_obs_share_daughters,interpretation_share," ///
    "coef_same_gender,se_same_gender,p_value_same_gender,n_obs_same_gender,interpretation_same_gender" _n

local dep_vars "plh0395i01 plh0395i02 plh0395i03 plh0395i04 plh0395i05 plh0395i06"

foreach dv of local dep_vars {
    
    di ""
    di "********************************************************************************"
    di "Processing Combined H1 Models for: `dv'"
    di "********************************************************************************"

    * --- Analysis A: Share of Daughters ---
    di "--- Running Model A: Share of Daughters ---"
    local ind_vars_A "share_daughters age age_sq sex sampreg is_german pgbilzeit"
    capture oprobit `dv' `ind_vars_A'
    
    if _rc == 0 {
        local coef_A = _b[share_daughters]
        local se_A = _se[share_daughters]
        local p_A = 2 * (1 - normal(abs(`coef_A'/`se_A')))
        local n_A = e(N)
        local interp_A "not in line with hypothesis"
        if `coef_A' > 0 {
            local interp_A "in line with hypothesis"
        }
    }
    else {
        local coef_A .
        local se_A .
        local p_A .
        local n_A .
        local interp_A "not estimated"
    }

    * --- Analysis B: Same-Gender Children Only ---
    di "--- Running Model B: Same-Gender Children ---"
    preserve 
    
    keep if share_daughters == 0 | share_daughters == 1
    gen has_only_daughters = (share_daughters == 1)
    
    local ind_vars_B "has_only_daughters age age_sq sex sampreg is_german pgbilzeit"
    capture oprobit `dv' `ind_vars_B'

    if _rc == 0 {
        local coef_B = _b[has_only_daughters]
        local se_B = _se[has_only_daughters]
        local p_B = 2 * (1 - normal(abs(`coef_B'/`se_B')))
        local n_B = e(N)
        local interp_B "not in line with hypothesis"
        if `coef_B' > 0 {
            local interp_B "in line with hypothesis"
        }
    }
    else {
        local coef_B .
        local se_B .
        local p_B .
        local n_B .
        local interp_B "not estimated"
    }
    
    restore

    * --- Write the combined results for this DV to the CSV file ---
    file write myfile "`dv'," ///
        "`coef_A',`se_A',`p_A',`n_A',`interp_A'," ///
        "`coef_B',`se_B',`p_B',`n_B',`interp_B'" _n
}

* --- Final Step: Close the file ---
file close myfile

di ""
di "Combined analysis for H1 complete. Results saved in 'h1_results_combined.csv'."

================================================================================
*/
