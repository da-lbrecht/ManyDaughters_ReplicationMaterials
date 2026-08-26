********************************************************************************
****************** Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * * ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2

* CONFIGURE FILE PATHS * (Please do not change)
/*
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
* Data Preparation for H4 (No Changes Needed)
* ================================================================

* This script performs the complete data preparation for H4.

* --- Step 1: Merge the raw data ---
use "$IN/ppathl.dta", clear
keep pid syear gebjahr sex sampreg corigin

* Merge with pl to get the dependent variables for H4
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
    keepusing(plh0395i01-plh0395i06)

* Merge with pgen to get the education control variable
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) ///
    keepusing(pgbilzeit)

* Merge with biobirth for IV construction
sort pid
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) ///
    keepusing(sumkids kidsex*)

* --- Step 2: Clean and Filter the sample for H4 ---
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

* Filter the sample (for 2019, adults, parents)
keep if syear == 2019
gen age = 2019 - gebjahr
keep if age >= 18 & age < .
keep if sumkids >= 1 & sumkids < .

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
save "$DATA/h4_data_final.dta", replace
di "Data preparation for H4 complete. Derived dataset 'h4_data_final.dta' is saved."

* ================================================================
* H4 Analysis: Final Single-Test Version (Based on Revised PAP)
* ================================================================
* NOTE: Per project requirements, the analysis was restricted to a single
* regression-based test. We selected 'plh0395i05' as the primary dependent
* variable and the 'same-gender children only' specification with an
* interaction term as the primary model, as outlined in our revised PAP.
* ================================================================

use "$DATA/h4_data_final.dta", clear

* --- Step 1: Create the sample and variables for the chosen test ---
* Restrict sample to parents with only same-gender children
keep if share_daughters == 0 | share_daughters == 1
* Create the primary independent variable and the moderator
gen has_only_daughters = (share_daughters == 1)
gen male = (sex == 1)

* --- Step 2: Define variables for the single chosen test ---
local dv "plh0395i05" // The single DV selected in the revised PAP
local ind_vars "i.has_only_daughters##i.male age age_sq sampreg is_german pgbilzeit"

* --- Step 3: Prepare the single CSV file ---
file open myfile using "$OUT/h4_results.csv", write replace
file write myfile "dependent_var,coef,se,family,residual_df,p_value,num_obs,num_participants,interpretation" _n

* --- Step 4: Run the single, chosen model ---
di ""
di "********************************************************************************"
di "Processing Final H4 Model for: `dv' (Same-Gender Interaction)"
di "********************************************************************************"

* We use OLS with robust SE to test the interaction term
reg `dv' `ind_vars', vce(robust)

* --- Step 5: Extract and export results for the INTERACTION TERM ---
if _rc == 0 {
    capture {
        local coefficient = _b[1.has_only_daughters#1.male]
        local se = _se[1.has_only_daughters#1.male]
    }
    if _rc == 0 {
        local test_stat_family "t"
        local df_residual = e(df_r)
        local t_stat = `coefficient' / `se'
        local p_value = 2 * ttail(`df_residual', abs(`t_stat'))
        local num_obs = e(N)
        quietly levelsof pid if e(sample), local(participants)
        local num_participants : word count `participants'
        
        * For H4, a POSITIVE interaction coefficient is in line with the hypothesis
		local interpretation "positive coefficient is in line with hypothesis"

        file write myfile "`dv',`coefficient',`se',`test_stat_family',`df_residual',`p_value',`num_obs',`num_participants',`interpretation'" _n
    }
    else {
        di "Model for `dv' ran, but interaction term was dropped. Result not saved."
    }
}
else {
    di "Model for `dv' failed to run (e.g. no observations). Result not saved."
}

* --- Step 6: Close the file ---
file close myfile

di ""
di "Final analysis for H4 complete. Results saved in 'h4_final_results.csv'."


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
* H4 Analysis: Combined Output (NOW SUPERSEDED)
* ================================================================

use "$DATA/h4_data_final.dta", clear

file open myfile using "$OUT/h4_results_combined.csv", write replace
file write myfile "dependent_var," ///
    "interact_coef_share,se_share,p_val_share,n_obs_share,n_part_share,df_r_share,interpretation_share," ///
    "interact_coef_same,se_same,p_val_same,n_obs_same,n_part_same,df_r_same,interpretation_same" _n

local dep_vars "plh0395i01 plh0395i02 plh0395i03 plh0395i04 plh0395i05 plh0395i06"
gen male = (sex == 1)

foreach dv of local dep_vars {
    
    di ""
    di "********************************************************************************"
    di "Processing Combined H4 Models for: `dv'"
    di "********************************************************************************"

    * --- Analysis A: Interaction with share_daughters ---
    di "--- Running Model A: share_daughters*male Interaction ---"
    local ind_vars_A "c.share_daughters##i.male age age_sq sampreg is_german pgbilzeit"
    capture reg `dv' `ind_vars_A', vce(robust)
    
    if _rc == 0 {
        capture {
            local coef_A = _b[1.male#c.share_daughters]
            local se_A = _se[1.male#c.share_daughters]
        }
        if _rc == 0 {
            local df_r_A = e(df_r)
            local p_A = 2 * ttail(`df_r_A', abs(`coef_A'/`se_A'))
            local n_A = e(N)
            quietly levelsof pid if e(sample), local(pids_A)
            local n_part_A : word count `pids_A'
            local interp_A "not in line with H4"
            if `coef_A' > 0 { local interp_A "in line with H4" }
        }
        else {
            local coef_A . ; local se_A . ; local df_r_A . ; local p_A .
            local n_A = e(N) ; local n_part_A . ; local interp_A "term dropped"
        }
    }
    else {
        local coef_A . ; local se_A . ; local df_r_A . ; local p_A .
        local n_A . ; local n_part_A . ; local interp_A "not estimated"
    }

    * --- Analysis B: Interaction with has_only_daughters ---
    di "--- Running Model B: has_only_daughters*male Interaction ---"
    preserve 
    
    keep if share_daughters == 0 | share_daughters == 1
    gen has_only_daughters = (share_daughters == 1)
    
    local ind_vars_B "i.has_only_daughters##i.male age age_sq sampreg is_german pgbilzeit"
    capture reg `dv' `ind_vars_B', vce(robust)

    if _rc == 0 {
        capture {
            local coef_B = _b[1.has_only_daughters#1.male]
            local se_B = _se[1.has_only_daughters#1.male]
        }
        if _rc == 0 {
            local df_r_B = e(df_r)
            local p_B = 2 * ttail(`df_r_B', abs(`coef_B'/`se_B'))
            local n_B = e(N)
            quietly levelsof pid if e(sample), local(pids_B)
            local n_part_B : word count `pids_B'
            local interp_B "not in line with H4"
            if `coef_B' > 0 { local interp_B "in line with H4" }
        }
        else {
             local coef_B . ; local se_B . ; local df_r_B . ; local p_B .
             local n_B = e(N) ; local n_part_B . ; local interp_B "term dropped"
        }
    }
    else {
        local coef_B . ; local se_B . ; local df_r_B . ; local p_B .
        local n_B . ; local n_part_B . ; local interp_B "not estimated"
    }
    
    restore

    * --- Write the combined results for this DV to the CSV file ---
    file write myfile "`dv'," ///
        "`coef_A',`se_A',`p_A',`n_A',`n_part_A',`df_r_A',`interp_A'," ///
        "`coef_B',`se_B',`p_B',`n_B',`n_part_B',`df_r_B',`interp_B'" _n

}

* --- Final Step: Close the file ---
file close myfile

di ""
di "Combined analysis for H4 complete. Results saved in 'h4_results_combined.csv'."

================================================================================
*/
