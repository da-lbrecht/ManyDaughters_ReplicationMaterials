/*=============================================================================
COLLECT AND COMBINE IDIOSYNCRASY ANALYSIS RESULTS
=============================================================================
Purpose: Collect all idiosyncrasy analysis results from individual CSV files
         (one per rt_id, hypothesis, iteration combination) and combine them
         into a single long-format Stata dataset.

Structure: Each CSV file is named:
           idiosyncrasy_YYYYMMDD_HHMMSS_[rt_id]_h[hyp]_results_iter[N].csv
           
Output:    Long format dataset with variables:
           - rt_id: Research team identifier
           - hypothesis: Hypothesis number (1-4)
           - iteration: Sampling iteration number
           - coefficient, se, family, residual_df, model_df, p_value
           - num_obs, num_participants, interpretation

=============================================================================*/

version 17.0

set more off
set type double

* Runtime tracking
local run_start_date = "`c(current_date)'"
local run_start_time = "`c(current_time)'"
timer clear 1
timer on 1

* Use root directory from main script, or set it if running standalone
if "`ROOT'" == "" {
    if strpos("`c(pwd)'", "code") {
        global ROOT "`c(pwd)'/.."
    }
    else {
        global ROOT "`c(pwd)'"
    }
}

* Define idiosyncrasy-specific paths
global results "$ROOT/data/derived/5_idiosyncrasy_results"
global output "$ROOT/data/derived"

disp ""
disp "=========================================="
disp "COLLECTING IDIOSYNCRASY RESULTS"
disp "=========================================="
disp "Started: `run_start_date' `run_start_time'"
disp "Source directory: $results"
disp ""

* Generate file list using dir command and save to temporary text file
local templist "$results/_csvfilelist.txt"
shell dir "$results" /s /b | find "_results_iter" > "`templist'" 2>nul

* Track provenance to diagnose duplicate keys and misplaced files
local track_provenance 1

* Initialize postfiles (faster than append/save per file)
tempfile master_results source_results
postfile master_handle str32 rt_id byte hypothesis int iteration ///
    double coefficient double se str32 family double residual_df ///
    double model_df double p_value double num_obs double num_participants ///
    str244 interpretation using `master_results', replace

if `track_provenance' {
    postfile source_handle str32 src_rt_id byte src_hypothesis int src_iteration ///
        str244 source_filename str244 source_filepath using `source_results', replace
}

* Read file list and process each file
local file_count = 0
local error_count = 0

* Close any previously open file handles
capture file close infile

file open infile using "`templist'", read

local keep_reading = 1
while `keep_reading' {
    file read infile filepath
    if r(eof) {
        local keep_reading = 0
        continue
    }
    * Skip empty lines
    if length("`filepath'") == 0 continue
    
    * Only process CSV files
    if !strmatch("`filepath'", "*.csv") continue
    
    * Extract just the filename
    local filename = substr("`filepath'", strrpos("`filepath'", "\") + 1, .)
    
    * Parse filename: idiosyncrasy_YYYYMMDD_HHMMSS_[rt_id]_h[hyp]_results_iter[N].csv
    * Extract hypothesis digit (position after _h)
    local hyp_pos = strpos("`filename'", "_h")
    if `hyp_pos' == 0 {
        local error_count = `error_count' + 1
        continue
    }
    local hyp_digit = substr("`filename'", `hyp_pos' + 2, 1)
    
    * Extract iteration number (after _iter)
    local iter_pos = strpos("`filename'", "_iter")
    if `iter_pos' == 0 {
        local error_count = `error_count' + 1
        continue
    }
    local iter_str = substr("`filename'", `iter_pos' + 5, .)
    local iteration = subinstr("`iter_str'", ".csv", "", .)
    
    * Convert to numeric, skip if invalid
    capture local iteration_num = real("`iteration'")
    if _rc > 0 | missing(`iteration_num') {
        local error_count = `error_count' + 1
        continue
    }
    
    * Extract rt_id: everything between last _ before _h and _h
    local before_h = substr("`filename'", 1, `hyp_pos' - 1)
    local last_under = strrpos("`before_h'", "_")
    if `last_under' == 0 {
        local error_count = `error_count' + 1
        continue
    }
    local rt_id = substr("`before_h'", `last_under' + 1, .)
    
    * Verify valid hypothesis digit
    if !inlist("`hyp_digit'", "1", "2", "3", "4") {
        local error_count = `error_count' + 1
        continue
    }
    
    * Import CSV file
    capture noisily import delimited "`filepath'", ///
        clear varnames(1) stringcols(_all)
    
    if _rc > 0 {
        local error_count = `error_count' + 1
        disp "ERROR reading: `filename'"
        continue
    }
    
    * Verify expected columns exist
    capture confirm var coefficient se family residual_df model_df p_value ///
        num_obs num_participants interpretation
    if _rc > 0 {
        local error_count = `error_count' + 1
        disp "ERROR: Missing expected columns in `filename'"
        continue
    }
    
    * Convert numeric columns
    foreach var in coefficient se residual_df model_df p_value num_obs num_participants {
        capture destring `var', replace force
    }
    
    * Keep only the core result columns, dropping anything else
    keep coefficient se family residual_df model_df p_value num_obs num_participants interpretation

    * Each results CSV should contain exactly one row
    if _N == 0 {
        local error_count = `error_count' + 1
        disp "ERROR: No data rows in `filename'"
        continue
    }
    if _N > 1 {
        disp "WARNING: Multiple rows in `filename' (using first row only)"
    }
    
    * Post first row to master results
    post master_handle ("`rt_id'") (`hyp_digit') (`iteration_num') ///
        (coefficient[1]) (se[1]) (family[1]) (residual_df[1]) (model_df[1]) ///
        (p_value[1]) (num_obs[1]) (num_participants[1]) (interpretation[1])

    if `track_provenance' {
        post source_handle ("`rt_id'") (`hyp_digit') (`iteration_num') ///
            ("`filename'") ("`filepath'")
    }
    
    local file_count = `file_count' + 1
    if mod(`file_count', 50) == 0 {
        disp "  Processed: `file_count' files"
    }
}

file close infile

* Close postfiles and load final combined dataset
postclose master_handle
if `track_provenance' {
    postclose source_handle
}

use `master_results', clear

disp ""
disp "=========================================="
disp "IMPORT SUMMARY"
disp "=========================================="
disp "Total CSV files processed: `file_count'"
if `error_count' > 0 {
    disp "Errors encountered: `error_count'"
}
disp ""

count
local total_rows = r(N)
disp "Total observations: `total_rows'"

* Standardize interpretation variable: fix typos and unify levels
disp ""
disp "=========================================="
disp "STANDARDIZING INTERPRETATION VARIABLE"
disp "=========================================="
disp ""
disp "Before standardization:"
tab interpretation

* Fix typos and standardize to common phrasing
replace interpretation = "positive coefficient is in line with hypothesis" ///
    if strpos(interpretation, "posititve") > 0
replace interpretation = "positive coefficient is in line with hypothesis" ///
    if strpos(interpretation, "coeffficient") > 0
replace interpretation = "positive coefficient is in line with hypothesis" ///
    if strpos(interpretation, "coeffiecient") > 0
replace interpretation = "positive coefficient is in line with hypothesis" ///
    if strmatch(interpretation, "positive coefficient in line with*")
replace interpretation = "positive coefficient is in line with hypothesis" ///
    if strmatch(interpretation, " positive coefficient is in line with*")

* Standardize negative coefficient phrasing (keep as is but ensure consistency)
replace interpretation = "negative coefficient is in line with hypothesis" ///
    if strmatch(interpretation, "negative coefficient*")

disp ""
disp "After standardization:"
tab interpretation
disp ""

* Duplicate key diagnostics with optional source attribution
quietly duplicates tag rt_id hypothesis iteration, gen(dup_rhi)
quietly count if dup_rhi > 0

* Sort by rt_id, hypothesis, iteration
sort rt_id hypothesis iteration

* Display summary
disp ""
disp "Data summary by RT_ID and Hypothesis:"
disp ""
tabulate rt_id hypothesis

* Save final dataset
disp ""
disp "Saving combined dataset..."
save "$output/idiosyncrasy_results_combined.dta", replace

disp ""
disp "=========================================="
disp "SUCCESS: Combined dataset saved"
disp "=========================================="
disp "File: idiosyncrasy_results_combined.dta"
disp "Location: $output"
disp "Total observations: `total_rows'"
disp "Variables: rt_id hypothesis iteration coefficient se family"
disp "           residual_df model_df p_value num_obs num_participants"
disp "           interpretation"
disp "=========================================="

* Stop timer and report total runtime
timer off 1
quietly timer list 1
local elapsed_seconds = round(r(t1), 0.01)
local elapsed_whole = floor(r(t1))
local elapsed_hours = floor(`elapsed_whole' / 3600)
local elapsed_minutes = floor(mod(`elapsed_whole', 3600) / 60)
local elapsed_secs = mod(`elapsed_whole', 60)

local hh = string(`elapsed_hours', "%02.0f")
local mm = string(`elapsed_minutes', "%02.0f")
local ss = string(`elapsed_secs', "%02.0f")

disp "Started:  `run_start_date' `run_start_time'"
disp "Finished: `c(current_date)' `c(current_time)'"
disp "Runtime:  `elapsed_seconds' seconds (`hh':`mm':`ss')"

* Display descriptive statistics
disp ""
disp "Descriptive Statistics:"
describe
summarize

* Clean up
capture erase "`templist'"
