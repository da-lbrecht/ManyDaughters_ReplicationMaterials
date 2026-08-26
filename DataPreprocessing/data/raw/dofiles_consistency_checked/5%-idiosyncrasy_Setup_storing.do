********************************************************************************
***************************** General Stata Setup ******************************
********************************************************************************

* Setup
clear all
capture log close 
set maxvar 10000 // extend the maximum number of variables to 10,000

* CONFIGURE FILE PATHS FOR SAMPLING *
if "$ROOT" == "" {
    local curdir = c(pwd)
    global ROOT "`curdir'"
    }

log using "$ROOT/logs/5%_bootstrap_sampling.log", replace

global NETWORK_ROOT "M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData"
display as text "Resolved ROOT path: $ROOT"

* Define target directory for storing bootstrap samples 
display as text "Resolved NETWORK_ROOT path: $NETWORK_ROOT"

* SET idiosyncrasy metadata globals *
global start_iteration = 251
global end_iteration = 255
global total_iterations = ${end_iteration} - ${start_iteration} + 1

* SET iteration independent data directories
global SOEP_IN "$ROOT/data/raw_confidential/SOEP_v37" // folder, in which the original datasets are stored
global SAMPLING_OUT "$NETWORK_ROOT/data_raw/" // folder for bootstrap samples
global DERIVED_OUT "$NETWORK_ROOT/data_derived" // folder for derived datasets provided to RTs
global TEMP "$NETWORK_ROOT/temp" // folder for temporary datasets

* Ensure sampling output directories exist
cap mkdir "$SAMPLING_OUT"
cap mkdir "$DERIVED_OUT"
cap mkdir "$TEMP"

* Initialize timer variables
local loop_start_time = clock("$S_DATE $S_TIME", "DMYhms")
local total_iterations = ${total_iterations}
display as text ""
display as text "============================================================"
display as text "LOOP TIMER INITIALIZED"
display as text "Total iterations expected: ${total_iterations}"
display as text "============================================================"

local start_iteration = ${start_iteration}
local end_iteration = ${end_iteration}

forvalues i = `start_iteration'/`end_iteration' {
	global iteration `i' 
    local iteration `i'
    
    * Initialize timer for this iteration
    local iteration_start_time = clock("$S_DATE $S_TIME", "DMYhms")

    * SET iteration dependent data directories
    global HUNDRED "$SAMPLING_OUT/100%_iter_${iteration}" // folder for 100% datasets
    display as text "Resolved HUNDRED path: $HUNDRED"
    global FIVE "$SAMPLING_OUT/5%_iter_${iteration}" // folder for 5% datasets
    display as text "Resolved FIVE path: $FIVE"
    global DERIVED "$DERIVED_OUT/5%_iter_${iteration}" // folder for provided 5% datasets
    display as text "Resolved DERIVED path: $DERIVED"

    cap mkdir "$HUNDRED"
    cap mkdir "$FIVE"
    cap mkdir "$DERIVED"


    ********** SAMPLING BASED ON PPATHL ************

    * Sample from all original households (based on cid). For each person in the 5% sample, all original household members that fulfill the sample restrictions (-> at least one realized interview 2000-2020) will also be in the 5% sample.

    *** 100% samples ***

    use "$SOEP_IN/ppathl.dta", clear

    * set relevant filters
    keep if inrange(syear, 2000, 2020) // only keep survey years 2000 to 2020
    keep if (inrange(netto,10,16) | netto==19) // only keep those with a realized individual interview (adults and 17-year olds)
    duplicates drop pid, force

    * merge with ppathl again, to add all observations of the selected participants:
    keep pid
    merge 1:m pid using "$SOEP_IN/ppathl.dta", nogen keep (master matches)
    * again only keep survey years 2000-2020
    keep if inrange(syear, 2000, 2020)

    * save 100% sample:
    save "$HUNDRED/ppathl.dta", replace

    * create dataset where hid and syear uniquely identify an observation for creating 100% household datasets:
    preserve
    duplicates drop hid syear, force
    save "$TEMP/ppathl_100_hid.dta", replace
    restore

    * save for creating wide 100% datasets:
    preserve
    duplicates drop pid, force
    save "$TEMP/ppathl_100_wide.dta", replace
    restore


    *** 5% samples ***

    * draw random 5% sample of all households based on the original household numnber from survey entry (cid):
    duplicates drop cid, force
    * Set seed based on base seed (123) plus iteration number to ensure different samples
    local seed_value = 123 + ${iteration}
    set seed `seed_value'
    display as text "Using seed: `seed_value' (base 123 + iteration ${iteration})"
    sample 5
    * Now we have a 5% sample of all original households containing observations of individuals completed an individual questionnaire 2000 - 2020.

    * merge by cid with 100% ppathl again, to observations of all members of the selected households, that fulfill the sample restriction:
    keep cid
    merge 1:m cid using "$HUNDRED/ppathl.dta", nogen keep (master matches)

    * save 5% sample:
    save "$FIVE/ppathl.dta", replace

    * for creating wide 5% datasets:
    preserve
    duplicates drop pid, force
    save "$TEMP/ppathl_5_wide.dta", replace
    restore

    * create dataset where hid and syear uniquely identify an observation for creating 5% household datasets:
    preserve
    duplicates drop hid syear, force
    save "$TEMP/ppathl_5_hid.dta", replace
    restore

    **************** WIDE DATASETS *******************

    ********** BIOBIRTH ************

    * create 5% sample:
    use "$TEMP/ppathl_5_wide.dta", clear
    keep pid
    merge m:1 pid using "$SOEP_IN/biobirth.dta", nogen keep(master matches)
    save "$FIVE/biobirth.dta", replace

    ********** BIOPAREN ************

    * create 5% sample:
    use "$TEMP/ppathl_5_wide.dta", clear
    keep pid
    merge m:1 pid using "$SOEP_IN/bioparen.dta", nogen keep(master matches)
    save "$FIVE/bioparen.dta", replace

    **************** LONG DATASETS *******************

    ********** BIOL ************

    * create 5% sample:
    use "$FIVE/ppathl.dta", clear
    keep pid syear
    merge 1:1 pid syear using "$SOEP_IN/biol.dta", nogen keep(master matches)
    save "$FIVE/biol.dta", replace

    ********** PGEN ************

    * create 5% sample:
    use "$FIVE/ppathl.dta", clear
    keep pid syear
    merge 1:1 pid syear using "$SOEP_IN/pgen.dta", nogen keep(master matches)
    //compress
    save "$FIVE/pgen.dta", replace

    ********** PL ************

    * create 5% sample:
    use "$FIVE/ppathl.dta", clear
    keep pid syear
    merge 1:1 pid syear using "$SOEP_IN/pl.dta", nogen keep(master matches)
    //compress
    save "$FIVE/pl.dta", replace

    **************** HOUSEHOLD LEVEL DATASETS *******************

    ********** HL ************

    * create 5% sample:
    use "$TEMP/ppathl_5_hid.dta", clear
    keep hid syear
    merge 1:1 hid syear using "$SOEP_IN/hl.dta", nogen keep(master matches)
    save "$FIVE/hl.dta", replace

    ********** HGEN ************

    * create 5% sample:
    use "$TEMP/ppathl_5_hid.dta", clear
    keep hid syear
    merge 1:1 hid syear using "$SOEP_IN/hgen.dta", nogen keep(master matches)
    save "$FIVE/hgen.dta", replace

   * Create provided 5% sample datasets for each iteration
    use "$FIVE/ppathl.dta", clear // use ppathl as a starting dataset

    * Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

    * biobirth contains information on fertility history
    merge m:1 pid using "$FIVE/biobirth.dta", nogen keep(master matches)
    * pgen contains user-friendly data on the individual level that are consolidated from different sources
    merge 1:1 pid syear using "$FIVE/pgen.dta", nogen keep(master matches)
    * pl contains all variables from the individual questionnaire
    merge 1:1 pid syear using "$FIVE/pl.dta", nogen keep(master matches)
    * hgen contains generated variables on household level from the household interview; use for household net income
    merge m:1 hid syear using "$FIVE/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

    label language EN // change language of the labels to English

    save "$DERIVED/h1_dataset.dta", replace // save merged dataset
    save "$DERIVED/h2_dataset.dta", replace // save merged dataset
    save "$DERIVED/h3_dataset.dta", replace // save merged dataset
    save "$DERIVED/h4_dataset.dta", replace // save merged dataset






    ********************************************************************************

    * Calculate elapsed time for this iteration
    local iteration_end_time = clock("$S_DATE $S_TIME", "DMYhms")
    local iteration_elapsed_seconds = (`iteration_end_time' - `iteration_start_time') / 1000
    local iteration_minutes = floor(`iteration_elapsed_seconds' / 60)
    local iteration_seconds = mod(`iteration_elapsed_seconds', 60)

    display as text ""
    display as text "Iteration ${iteration} completed in: `iteration_minutes' min `iteration_seconds' sec"

    * Clear temporary datasets
    cap rm "$TEMP/ppathl_5_hid.dta"
    cap rm "$TEMP/ppathl_5_wide.dta"
    cap rm "$TEMP/ppathl_100_hid.dta"
    cap rm "$TEMP/ppathl_100_wide.dta"
    cap rm "$SAMPLING_OUT/100%_iter_${iteration}/ppathl.dta"
    cap rmdir "$SAMPLING_OUT/100%_iter_${iteration}"

    /* If directly saving files to a network drive is slow, you may consider to first work into a local drive, and 
    then copy files to the network drive through the code block below*/
    * Move files from local drive to network drive
    /*local net_ready 0
    while `net_ready' == 0 {
        capture confirm dir "M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData"
        if _rc == 0 {
            local net_ready 1
        }
        else {
            display as text "Network drive not available. Waiting 1 minute before retry..."
            sleep 6000
        }
    }

    display as text "Moving data_derived from local C: to network M:..."
    shell robocopy "C:\Users\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_derived" "M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_derived" /E /MOVE
    display as text "Moving data_raw from local C: to network M:..."
    shell robocopy "C:\Users\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_raw" "M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_raw" /E /MOVE
    cap mkdir "$SAMPLING_OUT"
    cap mkdir "$DERIVED_OUT"
    display as text "File transfer complete."
    */
    
}  /* End of forvalues loop */

* Calculate total elapsed time and display summary
local loop_end_time = clock("$S_DATE $S_TIME", "DMYhms")
local total_elapsed_seconds = (`loop_end_time' - `loop_start_time') / 1000
local total_minutes = floor(`total_elapsed_seconds' / 60)
local total_seconds = mod(`total_elapsed_seconds', 60)
local average_seconds_per_iteration = `total_elapsed_seconds' / ${total_iterations}
local average_minutes_per_iteration = floor(`average_seconds_per_iteration' / 60)
local average_seconds_remainder = mod(`average_seconds_per_iteration', 60)

display as text ""
display as text "============================================================"
display as text "LOOP TIMER SUMMARY"
display as text "============================================================"
display as text "Total iterations completed: ${total_iterations}"
display as text "Total elapsed time: `total_minutes' min `total_seconds' sec"
display as text "Average time per iteration: `average_minutes_per_iteration' min `average_seconds_remainder' sec"
display as text "============================================================"
display as text ""

exit
