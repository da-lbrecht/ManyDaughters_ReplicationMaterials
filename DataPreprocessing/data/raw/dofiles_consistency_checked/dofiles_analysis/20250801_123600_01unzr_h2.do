********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * * ============================

*clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
//ssc install outreg2
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
* Data Preparation for H2 (No Changes Needed)
* ================================================================

* This script creates the pooled dataset for the H2 analysis,
* including all required variables from 2005, 2009, 2014, and 2019.

* --- Step 1: Merge the raw data ---
use "$IN/ppathl.dta", clear
keep pid syear gebjahr sex sampreg corigin // Keep IDs and controls specified in PAP

* Merge with pl to get the dependent variable for H2
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
    keepusing(plh0004)

* Merge with pgen to get the education control variable
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) ///
    keepusing(pgbilzeit)

* Merge with biobirth for IV construction
sort pid
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) ///
    keepusing(sumkids kidsex*)

* --- Step 2: Clean and Filter the sample for H2 ---
* Keep only the waves where the political preference question was asked
keep if inlist(syear, 2005, 2009, 2014, 2019)

* Keep only valid responses for the DV (0-10) as per the PAP
keep if plh0004 >= 0 & plh0004 <= 10

* Clean all control variables using our standard logic
foreach var of varlist sex sampreg pgbilzeit {
    replace `var' = . if `var' < 1
}
replace corigin = . if corigin < 1
gen is_german = (corigin == 1)
replace is_german = . if corigin == .

* Apply other standard filters
gen age_h2 = syear - gebjahr
keep if age_h2 >= 18 & age_h2 < .
keep if sumkids >= 1 & sumkids < .

* --- Step 3: Generate analysis variables ---
gen num_daughters = 0
forval i = 1/19 {
    local child_num : di %02.0f `i'
    replace num_daughters = num_daughters + 1 if kidsex`child_num' == 2
}
gen share_daughters = num_daughters / sumkids
gen age_sq_h2 = age_h2^2

* --- Step 4: Save the final dataset ---
keep pid syear plh0004 share_daughters age_h2 age_sq_h2 sex sampreg is_german pgbilzeit
save "$DATA/h2_data_final.dta", replace
di "Data preparation for H2 complete. Pooled dataset saved as h2_data_final.dta"

* ================================================================
* H2 Analysis: Final Single-Test Version (Based on Revised PAP)
* ================================================================
* NOTE: Per project requirements, the analysis was restricted to a single
* regression-based test. We use the 'same-gender children only' 
* specification as the primary model, as outlined in our revised PAP.
* ================================================================

use "$DATA/h2_data_final.dta", clear

* --- Step 1: Create the sample and variables for the chosen test ---
* Restrict sample to parents with only same-gender children
keep if share_daughters == 0 | share_daughters == 1
* Create the primary independent variable
gen has_only_daughters = (share_daughters == 1)

* --- Step 2: Define variables for the single chosen test ---
local dv "plh0004" // The single DV for H2
local ind_vars "has_only_daughters age_h2 age_sq_h2 sex sampreg is_german pgbilzeit i.syear"

* --- Step 3: Prepare the single CSV file ---
file open myfile using "$OUT/h2_results.csv", write replace
//file write myfile "dependent_var,coefficient,se,test_stat_family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n //Adapted by DA to ensure consistent export of results.

* --- Step 4: Run the single, chosen model ---
di ""
di "********************************************************************************"
di "Processing Final H2 Model for: `dv' (Same-Gender Specification)"
di "********************************************************************************"

* We use oprobit on the pooled data with standard errors clustered by person
oprobit `dv' `ind_vars', vce(cluster pid)

* --- Step 5: Extract and export results ---
local coefficient = _b[has_only_daughters]
local se = _se[has_only_daughters]
local test_stat_family "z"
local df_residual .
local df_model .
local z_stat = `coefficient' / `se'
local p_value = 2 * (1 - normal(abs(`z_stat')))
local num_obs = e(N)
local num_participants = e(N_clust) // For clustered SE, N_clust is the number of participants

* For H2, a NEGATIVE coefficient is in line with the hypothesis


local interpretation "negative coefficient is in line with hypothesis"

file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n

* --- Step 6: Close the file ---
file close myfile

di ""
di "Final analysis for H2 complete. Results saved in 'h2_final_results.csv'."


/*
================================================================================
================== SUPERSEDED ANALYSIS (COMMENTED OUT) =========================
================================================================================
*
* The following code block contains the previous analysis strategy, which
* involved two model specifications. It has been commented out to adhere 
* to the project's single-test requirement.
*

* ================================================================
* H2 Analysis: Combined Output (NOW SUPERSEDED)
* ================================================================

use "$DATA/h2_data_final.dta", clear

file open myfile using "$OUT/h2_results_combined.csv", write replace
file write myfile "dependent_var," ///
    "coef_share_daughters,se_share_daughters,p_value_share_daughters,n_obs_share_daughters,n_participants_share,interpretation_share," ///
    "coef_same_gender,se_same_gender,p_value_same_gender,n_obs_same_gender,n_participants_same,interpretation_same_gender" _n

local dep_vars "plh0004"

foreach dv of local dep_vars {
    
    di ""
    di "********************************************************************************"
    di "Processing Combined H2 Models for: `dv'"
    di "********************************************************************************"

    * --- Analysis A: Share of Daughters ---
    di "--- Running Model A: Share of Daughters ---"
    local ind_vars_A "share_daughters age_h2 age_sq_h2 sex sampreg is_german pgbilzeit i.syear"
    capture oprobit `dv' `ind_vars_A', vce(cluster pid)
    
    if _rc == 0 {
        local coef_A = _b[share_daughters]
        local se_A = _se[share_daughters]
        local p_A = 2 * (1 - normal(abs(`coef_A'/`se_A')))
        local n_A = e(N)
        local n_clust_A = e(N_clust)
        local interp_A "not in line with hypothesis"
        if `coef_A' < 0 {
            local interp_A "in line with hypothesis"
        }
    }
    else {
        local coef_A .
        local se_A .
        local p_A .
        local n_A .
        local n_clust_A .
        local interp_A "not estimated"
    }

    * --- Analysis B: Same-Gender Children Only ---
    di "--- Running Model B: Same-Gender Children ---"
    preserve 
    
    keep if share_daughters == 0 | share_daughters == 1
    gen has_only_daughters = (share_daughters == 1)
    
    local ind_vars_B "has_only_daughters age_h2 age_sq_h2 sex sampreg is_german pgbilzeit i.syear"
    capture oprobit `dv' `ind_vars_B', vce(cluster pid)

    if _rc == 0 {
        local coef_B = _b[has_only_daughters]
        local se_B = _se[has_only_daughters]
        local p_B = 2 * (1 - normal(abs(`coef_B'/`se_B')))
        local n_B = e(N)
        local n_clust_B = e(N_clust)
        local interp_B "not in line with hypothesis"
        if `coef_B' < 0 {
            local interp_B "in line with hypothesis"
        }
    }
    else {
        local coef_B .
        local se_B .
        local p_B .
        local n_B .
        local n_clust_B .
        local interp_B "not estimated"
    }
    
    restore

    * --- Write the combined results for this DV to the CSV file ---
    file write myfile "`dv'," ///
        "`coef_A',`se_A',`p_A',`n_A',`n_clust_A',`interp_A'," ///
        "`coef_B',`se_B',`p_B',`n_B',`n_clust_B',`interp_B'" _n
}

* --- Final Step: Close the file ---
file close myfile

di ""
di "Combined analysis for H2 complete. Results saved in 'h2_results_combined.csv'."

================================================================================
*/
