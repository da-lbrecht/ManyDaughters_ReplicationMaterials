********************************************************************************
***************************** General Stata Setup ******************************
********************************************************************************

* Setup
clear all
capture log close // close log session, if one is still runnning
set maxvar 10000 // extend the maximum number of variables to 10,000

* TRACK SETUP EXECUTION TIME
local setup_start_time = clock(c(current_date) + " " + c(current_time), "DMY hms")
display as text "Setup file execution started at: `c(current_date)' `c(current_time)'"

* CONFIGURE FILE PATHS FOR SAMPLING *
* the following code automatically sets paths based on the project location on your system *
local curdir = c(pwd)
if substr("`curdir'", -36, .) == "data\raw\dofiles_consistency_checked" | substr("`curdir'", -36, .) == "data/raw/dofiles_consistency_checked" {
    // Move three directories up - handle both separators
    local root : subinstr local curdir "\data\raw\dofiles_consistency_checked" "", all
    local root : subinstr local root "/data/raw/dofiles_consistency_checked" "", all
    global ROOT "`root'"
}
else {
    global ROOT "`curdir'"
}
display as text "Resolved ROOT path: $ROOT"

* CHECK CORRECT PASSING ON OF rt_id_iter, iteration, AND hypothesis VARIABLES *
display as text "rt_id_iter: $rt_id_iter"
display as text "iteration: $iteration"
display as text "hypothesis: $hypothesis"

* FILE PATH FOR SAMPLING - SOURCE PATHS (network location with pre-sampled data) *
global NETWORK_DATA_RAW "M:/user/dalbrecht/Research Projects/ManyDaughters_IdiosyncrasySampleData/data_raw" // network source for sampled raw data
display as text "Resolved NETWORK_DATA_RAW path: $NETWORK_DATA_RAW"
global NETWORK_DATA_DERIVED "M:/user/dalbrecht/Research Projects/ManyDaughters_IdiosyncrasySampleData/data_derived" // network source for sampled derived data
display as text "Resolved NETWORK_DATA_DERIVED path: $NETWORK_DATA_DERIVED"

* DESTINATION PATHS (local working directory) *
* NOTE: Paths include rt_id_iter and hypothesis for job-specific isolation
global SAMPLING_OUT "$ROOT/data/raw_confidential" // folder for output (datasets, tables)

* Ensure sampling output directory exist
cap mkdir "$SAMPLING_OUT"

*FILE PATH AS SETUP FOR ANALYSIS DOFILES *
global IN "$ROOT\data\raw_confidential\5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder, in which the sampled datasets are stored
display as text "Resolved IN path: $IN"
global DATA "$ROOT\data\derived_confidential\5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder for datasets
display as text "Resolved DATA path: $DATA"
global OUT "$ROOT/data/derived/5_idiosyncrasy_results/${rt_id_iter}/.job_${rt_id_iter}_${iteration}_h${hypothesis}" // folder for output (results, tables) - per-job subdir prevents race conditions
display as text "Resolved OUT path: $OUT"

* Verify output directories exist (Python creates them with retries before launching Stata)
* Setup only does quick verification - no creation to avoid race conditions
cap mkdir "$IN"
cap mkdir "$DATA"
cap mkdir "$OUT"


********** COPY PRE-SAMPLED DATASETS FROM NETWORK LOCATIONS ************

display as text "Beginning copy of pre-sampled datasets from network..."

* Define source directories in network location (generic, without rt_id_iter or hypothesis)
* Source data is shared across all rt_ids and hypotheses - will be copied to job-specific destinations
* Convert forward slashes to backslashes for Windows compatibility
local network_5 "$NETWORK_DATA_RAW/5%_iter_${iteration}"
local network_5 : subinstr local network_5 "/" "\" , all
local network_derived "$NETWORK_DATA_DERIVED/5%_iter_${iteration}"
local network_derived : subinstr local network_derived "/" "\" , all

display as text "Source network paths:"
display as text "  5%: `network_5'"
display as text "  Derived: `network_derived'"

display as text "Destination local paths:"
display as text "  5%: $IN"
display as text "  Derived: $DATA"

display as text ""
display as text "Verifying source files exist on network..."

* List files in 5% network source directory
display as text "Files in 5% source directory:"
shell dir "`network_5'" /B

* List files in derived network source directory
display as text "Files in derived source directory:"
shell dir "`network_derived'" /B

display as text ""

* Copy 5% sample datasets
display as text "Copying 5% sample datasets..."
foreach file in ppathl biobirth bioparen biol pgen pl hl hgen {
    local source_file "`network_5'\\`file'.dta"
    shell xcopy "`source_file'" "$IN\" /Y /Q >nul
    display as text "  ✓ `file'.dta"
}

display as text "✓ Completed copying sampled raw datasets from network"
display as text ""
********** COPY PRE-DERIVED DATASETS FROM NETWORK LOCATION ************

display as text "Copying pre-derived analysis datasets..."

* Copy derived datasets from network
foreach file in h1_dataset h2_dataset h3_dataset h4_dataset {
    local source_file "`network_derived'\\`file'.dta"
    shell xcopy "`source_file'" "$DATA\" /Y /Q >nul
    display as text "  ✓ `file'.dta"
}

display as text "✓ Completed copying derived datasets from network"


* TRACK SETUP COMPLETION TIME
local setup_end_time = clock(c(current_date) + " " + c(current_time), "DMY hms")
local setup_duration_ms = `setup_end_time' - `setup_start_time'
local setup_duration_sec = `setup_duration_ms' / 1000
local setup_duration_min = `setup_duration_sec' / 60

display as text ""
display as text "=========================================================="
display as text "SETUP TIME TRACKING"
display as text "=========================================================="
display as text "Setup file execution completed at: `c(current_date)' `c(current_time)'"
display as text "Total setup time: `setup_duration_min' minutes (`setup_duration_sec' seconds)"
display as text "=========================================================="
display as text ""


* *RENEW SETUP FOR ANALYSIS *

* Preserve the iteration variables before clearing
local rt_id_iter_temp "$rt_id_iter"
local iteration_temp "$iteration"
local hypothesis_temp "$hypothesis"

* Setup
clear all

* Restore the iteration variables
global rt_id_iter "`rt_id_iter_temp'"
global iteration "`iteration_temp'"
global hypothesis "`hypothesis_temp'"

* the following code automatically sets paths based on the project location on your system *
local curdir = c(pwd)
if substr("`curdir'", -36, .) == "data\raw\dofiles_consistency_checked" | substr("`curdir'", -36, .) == "data/raw/dofiles_consistency_checked" {
    // Move three directories up - handle both separators
    local root : subinstr local curdir "\data\raw\dofiles_consistency_checked" "", all
    local root : subinstr local root "/data/raw/dofiles_consistency_checked" "", all
    global ROOT "`root'"
}
else {
    global ROOT "`curdir'"
}
display as text "Resolved ROOT path: $ROOT"

*FILE PATH AS SETUP FOR ANALYSIS DOFILES *
global IN "$ROOT\data\raw_confidential\5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder, in which the sampled datasets are stored
display as text "Resolved IN path: $IN"
global DATA "$ROOT\data\derived_confidential\5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder for datasets
display as text "Resolved DATA path: $DATA"
global OUT "$ROOT/data/derived/5_idiosyncrasy_results/${rt_id_iter}/.job_${rt_id_iter}_${iteration}_h${hypothesis}" // folder for output (results, tables) - per-job subdir prevents race conditions
display as text "Resolved OUT path: $OUT"
