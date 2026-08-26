********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
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
* Data Preparation for H3 (Revised for Single DV)
* ================================================================

* This script creates the dataset for the H3 analysis (time use).

* --- Step 1: Merge the raw data ---
use "$IN/ppathl.dta", clear

keep pid syear gebjahr sex sampreg corigin

* Merge with pl to get the single DV for H3
* NOTE: We now merge only the selected dependent variable, pli0044_h.
* If pli0044_h is not the correct variable name, please adjust it here.
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
    keepusing(pli0044_h)

* Merge with pgen to get education and the CORRECT marital status variable
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) ///
    keepusing(pgbilzeit pgfamstd) 

* Merge with biobirth for IV construction
sort pid
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) ///
    keepusing(sumkids kidsex*)

* --- Step 2: Clean and Filter the sample for H3 ---
* Clean the DV for H3 - Negative values are invalid
replace pli0044_h = . if pli0044_h < 0

* Clean Marital status variable (pgfamstd)
replace pgfamstd = . if pgfamstd < 1

* Clean all control variables using our standard logic
foreach var of varlist sex sampreg pgbilzeit {
    replace `var' = . if `var' < 1
}
replace corigin = . if corigin < 1
gen is_german = (corigin == 1)
replace is_german = . if corigin == .

* Filter sample based on the CORRECT marital status variable
keep if inlist(pgfamstd, 1, 7)

* Apply other standard filters
gen age_h3 = syear - gebjahr
keep if age_h3 >= 18 & age_h3 < .
keep if sumkids >= 1 & sumkids < .

* --- Step 3: Generate analysis variables ---
gen num_daughters = 0
forval i = 1/19 {
    local child_num : di %02.0f `i'
    replace num_daughters = num_daughters + 1 if kidsex`child_num' == 2
}
gen share_daughters = num_daughters / sumkids
gen age_sq_h3 = age_h3^2

* --- Step 4: Save the final dataset ---
keep pid syear pli0044_h share_daughters age_h3 age_sq_h3 sex sampreg is_german pgbilzeit
save "$DATA/h3_data_final.dta", replace
di "Data preparation for H3 complete. Dataset saved as h3_data_final.dta"

* ================================================================
* H3 Analysis: Final Single-Test Version (Based on Revised PAP)
* ================================================================
* NOTE: Per project requirements, the analysis was restricted to a single
* regression-based test. We selected 'pli0044_h' as the primary dependent
* variable and the 'same-gender children only' specification as the
* primary model, as outlined in our revised PAP.
* ================================================================

use "$DATA/h3_data_final.dta", clear

* --- Step 1: Create the sample and variables for the chosen test ---
* Restrict sample to parents with only same-gender children
keep if share_daughters == 0 | share_daughters == 1
* Create the primary independent variable
gen has_only_daughters = (share_daughters == 1)

* --- Step 2: Define variables for the single chosen test ---
local dv "pli0044_h" // The single DV selected in the revised PAP
local ind_vars "has_only_daughters age_h3 age_sq_h3 sex sampreg is_german pgbilzeit i.syear"

* --- Step 3: Prepare the single CSV file ---
file open myfile using "$OUT/h3_results.csv", write replace
//file write myfile "dependent_var,coefficient,se,test_stat_family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n //Adapted by DA to ensure consistent export of results.

* --- Step 4: Run the single, chosen model ---
di ""
di "********************************************************************************"
di "Processing Final H3 Model for: `dv' (Same-Gender Specification)"
di "********************************************************************************"

* We use OLS on the pooled data with standard errors clustered by person
reg `dv' `ind_vars', vce(cluster pid)

* --- Step 5: Extract and export results ---
local coefficient = _b[has_only_daughters]
local se = _se[has_only_daughters]
local test_stat_family "t"
local df_residual = e(df_r)
local df_model = e(df_m)
local t_stat = `coefficient' / `se'
local p_value = 2 * ttail(`df_residual', abs(`t_stat'))
local num_obs = e(N)
local num_participants = e(N_clust)

* For H3, a POSITIVE coefficient is in line with the hypothesis
local interpretation "positive coefficient is in line with hypothesis"

file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n

* --- Step 6: Close the file ---
file close myfile

di ""
di "Final analysis for H3 complete. Results saved in 'h3_final_results.csv'."


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
* H3 Analysis: Combined Output (NOW SUPERSEDED)
* ================================================================

use "$DATA/h3_data_final.dta", clear // Note: this would need to load a dataset with all DVs

file open myfile using "$OUT/h3_results_combined.csv", write replace
file write myfile "dependent_var," ///
    "coef_share,se_share,p_val_share,n_obs_share,n_part_share,df_r_share,interpretation_share," ///
    "coef_same,se_same,p_val_same,n_obs_same,n_part_same,df_r_same,interpretation_same" _n

local dep_vars "pli0010 pli0036 pli0051 pli0019_h pli0022_h"

foreach dv of local dep_vars {
    
    di ""
    di "********************************************************************************"
    di "Processing Combined H3 Models for: `dv'"
    di "********************************************************************************"

    * --- Analysis A: Share of Daughters ---
    di "--- Running Model A: Share of Daughters ---"
    local ind_vars_A "share_daughters age_h3 age_sq_h3 sex sampreg is_german pgbilzeit i.syear"
    capture reg `dv' `ind_vars_A', vce(cluster pid)
    
    if _rc == 0 {
        local coef_A = _b[share_daughters]
        local se_A = _se[share_daughters]
        local df_r_A = e(df_r)
        local p_A = 2 * ttail(`df_r_A', abs(`coef_A'/`se_A'))
        local n_A = e(N)
        local n_part_A = e(N_clust)
        
        local interp_A "direction unclear"
        if inlist("`dv'", "pli0019_h", "pli0022_h") & `coef_A' > 0 { 
            local interp_A "in line with hypothesis"
        }
        if inlist("`dv'", "pli0010", "pli0036", "pli0051") & `coef_A' < 0 {
            local interp_A "in line with hypothesis"
        }
    }
    else {
        local coef_A . ; local se_A . ; local df_r_A . ; local p_A .
        local n_A . ; local n_part_A . ; local interp_A "not estimated"
    }

    * --- Analysis B: Same-Gender Children Only ---
    di "--- Running Model B: Same-Gender Children ---"
    preserve 
    
    keep if share_daughters == 0 | share_daughters == 1
    gen has_only_daughters = (share_daughters == 1)
    
    local ind_vars_B "has_only_daughters age_h3 age_sq_h3 sex sampreg is_german pgbilzeit i.syear"
    capture reg `dv' `ind_vars_B', vce(cluster pid)

    if _rc == 0 {
        local coef_B = _b[has_only_daughters]
        local se_B = _se[has_only_daughters]
        local df_r_B = e(df_r)
        local p_B = 2 * ttail(`df_r_B', abs(`coef_B'/`se_B'))
        local n_B = e(N)
        local n_part_B = e(N_clust)
        
        local interp_B "direction unclear"
        if inlist("`dv'", "pli0019_h", "pli0022_h") & `coef_B' > 0 { 
            local interp_B "in line with hypothesis"
        }
        if inlist("`dv'", "pli0010", "pli0036", "pli0051") & `coef_B' < 0 {
            local interp_B "in line with hypothesis"
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
di "Combined analysis for H3 complete. Results saved in 'h3_results_combined.csv'."

================================================================================
*/
