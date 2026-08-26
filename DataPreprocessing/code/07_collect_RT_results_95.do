********************************************************************************
*** #ManyDaughters Project: Data Preprocessing - 07_collect_RT_results_95.do ***
********************************************************************************

* Authors: David Albrecht

* =========================================
* GATHER RT ANALYSIS RESULTS FROM 95% DATA
* =========================================

* Create empty variables for each hypothesis and result type
foreach h in RT_h1_ RT_h2_ RT_h3_ RT_h4_ {
    gen double `h'coefficient_95 = .
    gen double `h'se_95 = .
    gen str20 `h'family_95 = ""
    gen double `h'residual_df_95 = .
    gen double `h'model_df_95 = .
    gen double `h'p_value_95 = .
    gen double `h'p_value_95_manual = .
    gen double `h'num_obs_95 = .
    gen double `h'num_participants_95 = .
    gen str100 `h'interpretation_95 = ""
}

* Loop through all result files and update variables accordingly
local results_dir "data/derived/95_results"
local endings h1_results h2_results h3_results h4_results

* Get list of all relevant result files
local flist : dir "`results_dir'" files "*.csv"

foreach f of local flist {
    preserve

    * Extract rt_id and hypothesis ending from filename
    local fname = "`f'"
    * Parse filename: repro_YYYYMMDD_HHMMSS_XXXXXX_hX_results.csv
    * Find the rt_id (6 characters before _hX_results.csv)
    * Strategy: find position of _h1_, _h2_, _h3_, or _h4_ and extract 6 chars before it
    local rt_id = ""
    local hyp = ""
    
    foreach ending of local endings {
        if strpos("`fname'", "_`ending'.csv") {
            local hyp = substr("`ending'", 1, 3) // e.g. h1_, h2_, h3_, h4_
            local pos = strpos("`fname'", "_`ending'.csv")
            * Extract 6 characters before the _hX_results.csv part
            local rt_id = substr("`fname'", `pos' - 6, 6)
            continue, break
        }
    }

    * Display all locals for debugging
    di "DEBUG: Processing file `fname', extracted rt_id=`rt_id', hyp=`hyp'"

    * Try to import the result file and catch errors
    capture noisily import delimited using "`results_dir'/`f'", clear stringcols(1)
    local import_rc = _rc

    // If import succeeded, display the actual rt_id value from the CSV for debugging
    if `import_rc' == 0 {
        capture {
            if _N > 0 {
                local csv_rt_id = rt_id[1]
                di "  CSV rt_id value: `csv_rt_id' (type: " + cond(missing(real("`csv_rt_id'")), "string", "could be numeric") + ")"
            }
        }
    }

    // If import failed, set manual entry flag and skip to next file
    if `import_rc' != 0 {
        restore
        * Use case-insensitive comparison for rt_id match
        qui count if upper(trim(rt_id)) == upper(trim("`rt_id'"))
        di "Import failed. Attempted to flag `r(N)' observation(s) with rt_id matching `rt_id'"
        continue
    }

    // Check for missing expected columns after successful import
    local missingcol 0
    foreach var in coefficient se family residual_df model_df p_value num_obs num_participants interpretation {
        capture confirm variable `var'
        if _rc != 0 local missingcol 1
    }

    if `missingcol' {
        restore
        qui count if upper(trim(rt_id)) == upper(trim("`rt_id'"))
        di "Missing columns. Attempted to flag `r(N)' observation(s) with rt_id matching `rt_id'"
        continue
    }

    * Store values from the result file
    foreach var in coefficient se family residual_df model_df p_value num_obs num_participants interpretation {
        capture {
            * Check if it's a string variable first
            capture confirm string variable `var'
            if _rc == 0 {
                * It's a string variable - use directly
                local `var'_val = trim(`var'[1])
            }
            else {
                * It's numeric - convert to string with high precision
                if "`var'" == "p_value" {
                    * Special handling for p-values - use maximum precision
                    * If the value is exactly 0, keep it as 0
                    if `var'[1] == 0 {
                        local `var'_val = "0"
                    }
                    else {
                        local `var'_val = string(`var'[1], "%24.16e")
                    }
                }
                else if "`var'" == "coefficient" | "`var'" == "se" {
                    local `var'_val = string(`var'[1], "%24.16e")
                }
                else {
                    local `var'_val = trim(string(`var'[1]))
                }
            }
        }
    }
    restore
    
    * Check how many observations match before updating
    qui count if upper(trim(rt_id)) == upper(trim("`rt_id'"))
    local match_count = r(N)
    di "Found `match_count' observation(s) matching rt_id=`rt_id' (case-insensitive)"
    
    foreach var in coefficient se family residual_df model_df p_value num_obs num_participants interpretation {
        local mvar = "RT_`hyp'`var'_95"
        local val = trim("``var'_val'")
        di "Updating `mvar' for rt_id=`rt_id' with value=`val'"
        capture confirm string variable `mvar'
        if _rc == 0 {
            * Target is string variable
            qui replace `mvar' = "`val'" if upper(trim(rt_id)) == upper(trim("`rt_id'"))
            di "  -> `r(N)' row(s) updated"
        }
        else {
            * Target is numeric variable - try to convert
            capture {
                local num_val = real("`val'")
                if missing(`num_val') {
                    * Value is not numeric, set to missing
                    qui replace `mvar' = . if upper(trim(rt_id)) == upper(trim("`rt_id'"))
                    di "  -> Set to missing (non-numeric value: `val')"
                }
                else {
                    qui replace `mvar' = `num_val' if upper(trim(rt_id)) == upper(trim("`rt_id'"))
                    di "  -> `r(N)' row(s) updated"
                }
            }
        }
    }
}

* =========================================
* CHECK FOR INCONSISTENCIES AND TYPOS MAIN ANALYSIS RESULTS FROM 95% DATA
* =========================================

* Trim spaces from RT variables for hypotheses h1-h4 and suffixes _95
foreach suffix in _95 {
    foreach h in h1 h2 h3 h4 {
        * Trim string variables
        capture replace RT_`h'_family`suffix' = trim(RT_`h'_family`suffix')
        capture replace RT_`h'_interpretation`suffix' = trim(RT_`h'_interpretation`suffix')
        
        * Convert numeric variables stored as strings back to proper format after trimming
        foreach var in coefficient se residual_df model_df p_value num_obs num_participants {
            capture confirm string variable RT_`h'_`var'`suffix'
            if _rc == 0 {
                replace RT_`h'_`var'`suffix' = trim(RT_`h'_`var'`suffix')
                * Convert back to numeric if it contains only numeric values
                capture destring RT_`h'_`var'`suffix', replace
            }
        }
    }
}

* Harmonize impermissible values (typos) in family and interpretation variables

* Fix typos in interpretation variables for 95% data
foreach h in h1 h2 h3 h4 {
    * Replace various typos with correct interpretation text
    replace RT_`h'_interpretation_95 = "positive coefficient is in line with hypothesis" ///
        if inlist(RT_`h'_interpretation_95, ///
            "positive coeffficient is in line with hypothesis", ///
            "positive coefficient is in line with hypothesis positive coefficient is in line with hypothesis", ///
            "posititve coefficient is in line with hypothesis", ///
            "positive coefficient in line with hypothesis", ///
            "positive coeffiecient is in line with hypothesis")
}

* Fix typos in family variables for 95% data
foreach h in h1 h2 h3 h4 {
    * Replace one-sided t-test variations with "one-sided t"
    replace RT_`h'_family_95 = "one-sided t" ///
        if inlist(RT_`h'_family_95, ///
            "one sided t-test (H1: \beta > 0)", ///
            "one sided t: > 0", ///
            "one sided t-test (H1: \beta < 0)")
    
    * Replace "robust t" with "t"
    replace RT_`h'_family_95 = "t" ///
        if RT_`h'_family_95 == "robust t"
    
    * Recode filler values to proper missing values
    replace RT_`h'_family_95 = "" if RT_`h'_family_95 == "."
    replace RT_`h'_interpretation_95 = "" if RT_`h'_interpretation_95 == "."
}


* Check for impermissible values in test family and interpretation variables
* Table RT family values for each hypothesis (95% data)
foreach h in h1 h2 h3 h4 {
    di as text _n "RT_`h'_family_95 frequency table:"
    tab RT_`h'_family_95, missing
}

* Table RT interpretation values for each hypothesis (95% data)
foreach h in h1 h2 h3 h4 {
    di as text _n "RT_`h'_interpretation_95 frequency table:"
    tab RT_`h'_interpretation_95, missing
}

* Harmonize reporting of degrees of freedom
foreach h in h1 h2 h3 h4 {
    * For z-tests: set both residual_df and model_df to missing
    replace RT_`h'_residual_df_95 = . if RT_`h'_family_95 == "z"
    replace RT_`h'_model_df_95 = . if RT_`h'_family_95 == "z"
    
    * For t-tests: set model_df to missing
    replace RT_`h'_model_df_95 = . if inlist(RT_`h'_family_95, "t", "one-sided t")
    
    * For Chi² tests: set residual_df to missing
    replace RT_`h'_residual_df_95 = . if RT_`h'_family_95 == "Chi²"
}

* Manually populate p-values for cases with extremely small values that failed automatic extraction
* These are cases where p-value is 0 or in extreme scientific notation

* RT 01mmkn H1 - p-value: 2.6368338586e-258
replace RT_h1_p_value_95_manual = 2.6368338586e-258 if rt_id == "01mmkn"

* RT 01mmkn H2 - p-value: 8.16195553477e-69
replace RT_h2_p_value_95_manual = 8.16195553477e-69 if rt_id == "01mmkn"

* RT 01v3eq H3 - p-value: 9.0646909854e-169
replace RT_h3_p_value_95_manual = 9.0646909854e-169 if rt_id == "01v3eq"

* RT 01toXX H3 - p-value: 1.4876298532e-146
replace RT_h3_p_value_95_manual = 1.4876298532e-146 if rt_id == "01toXX"

* RT 01p8xt H3 - p-value: 4.88156180436e-48
replace RT_h3_p_value_95_manual = 4.88156180436e-48 if rt_id == "01p8xt"

* RT 0119XX H3 - p-value: 1.5454016736e-120
replace RT_h3_p_value_95_manual = 1.5454016736e-120 if rt_id == "0119XX"

* RT 011q3i H3 - p-value: 0 (extremely small, below precision)
replace RT_h3_p_value_95_manual = 0 if rt_id == "011q3i"

* RT 01mmkn H3 - p-value: 0 (extremely small, below precision)
replace RT_h3_p_value_95_manual = 0 if rt_id == "01mmkn"

* RT 01ufoq  H4- p-value: 0 (1.97681084583e-53)
replace RT_h4_p_value_95_manual = 1.97681084583e-53 if rt_id == "01ufoq"

* RT 01p8xt H4 - p-value: 0 (extremely small, below precision)
replace RT_h4_p_value_95_manual = 0 if rt_id == "01p8xt"









* Flag RTs whose analysis revealed inconsistencies during checking
gen byte RT_results_inconsistent = 0
replace RT_results_inconsistent = 1 if inlist(rt_id, "01iw87", "015w1n", "01vtfa", "016nbt", "01xrXX", "01f8uy", "01ygl2")

/* List of inconsistencies found during checking of 95% results:
  
    RT 01iw87 H1 - standard errors and p-value cannot be calculated for the coefficient of interest (ydaughters)
    RT 015w1n H1 - coefficient, standard error and p-value reported as 0, as regression fails due to perfect collinearity
    RT 01vtfa H2 - latest submission is .do-file for H3!
    RT 016nbt H2 - standard errors and p-value cannot be calculated for the coefficient of interest (coefficient_2017)
    RT 01xrXX H3 - standard errors and p-value cannot be calculated for the coefficient of interest (d_daughter)
    RT 016nbt H4 - run two regressions (male + female) and do not provide complete code or instructions how to proceed to results of interest for the hypothesis test
    RT 01f8uy H4 - do-file does produces error on 95% sample (unexpected values <0, i.e. in SOEP coding some kind of missing, for control variable sex in 95% sample)
    RT 01ygl2 H4 - standard errors and p-value cannot be calculated for the coefficient of interest 
*/
