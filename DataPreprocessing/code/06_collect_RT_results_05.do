********************************************************************************
*** #ManyDaughters Project: Data Preprocessing - 06_collect_RT_results_05.do **
********************************************************************************

* Authors: David Albrecht

* =========================================
* GATHER RT ANALYSIS RESULTS FROM 5% DATA
* =========================================

* Create empty variables for each hypothesis and result type
foreach h in RT_h1_ RT_h2_ RT_h3_ RT_h4_ {
    // gen double `h'coefficient_5 = .
    // gen double `h'se_5 = .
    // gen str20 `h'family_5 = ""
    // gen double `h'residual_df_5 = .
    // gen double `h'model_df_5 = .
    gen double `h'p_value_5 = .
    // gen double `h'num_obs_5 = .
    // gen double `h'num_participants_5 = .
    // gen str100 `h'interpretation_5 = ""
}


* Loop through all result files and update variables accordingly
local results_dir "data/derived/5_results"
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
    foreach var in p_value /*coefficient  se family residual_df model_df num_obs num_participants interpretation*/ {
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
    foreach var in p_value /*coefficient  se family residual_df model_df num_obs num_participants interpretation*/ {
        capture {
            * Check if it's a string variable first
            capture confirm string variable `var'
            if _rc == 0 {
                * It's a string variable - use directly
                local `var'_val = trim(`var'[1])
            }
            else {
                * It's numeric - convert to string with high precision
                if "`var'" == "p_value" | "`var'" == "coefficient" | "`var'" == "se" {
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
    
    foreach var in p_value /*coefficient  se family residual_df model_df num_obs num_participants interpretation*/ {
        local mvar = "RT_`hyp'`var'_5"
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
* CHECK FOR REMAINING GAPS IN P-VALUES FROM 5% DATA
* =========================================

gen byte RT_pvalue_5_incomplete = (missing(RT_h1_p_value_5) | missing(RT_h2_p_value_5) | missing(RT_h3_p_value_5) | missing(RT_h4_p_value_5))
