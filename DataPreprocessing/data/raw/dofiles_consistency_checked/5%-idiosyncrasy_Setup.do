********************************************************************************
***************************** General Stata Setup ******************************
********************************************************************************

* Setup
clear all
capture log close // close log session, if one is still runnning
set maxvar 10000 // extend the maximum number of variables to 10,000

* Define retry save program for network I/O resilience
capture program drop retry_save
program define retry_save
    syntax anything, [replace]
    local filename `anything'
    local max_attempts 5
    local attempt 1
    local success 0
    
    while `attempt' <= `max_attempts' & `success' == 0 {
        capture {
            if "`replace'" == "replace" {
                //compress
                save "`filename'", replace
            }
            else {
                //compress
                save "`filename'"
            }
        }
        if _rc == 0 {
            local success 1
            display as text "Successfully saved: `filename'"
        }
        else if _rc == 693 {
            display as error "I/O error on attempt `attempt' of `max_attempts' for: `filename'"
            if `attempt' < `max_attempts' {
                display as text "Waiting 2 seconds before retry..."
                sleep 2000
                local attempt = `attempt' + 1
            }
            else {
                display as error "Failed to save after `max_attempts' attempts: `filename'"
                error 693
            }
        }
        else {
            display as error "Unexpected error (code: " _rc ") saving: `filename'"
            error _rc
        }
    }
end

 

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

* FILE PATH FOR SAMPLING *
global SOEP_IN "$ROOT/data/raw_confidential/SOEP_v37" // folder, in which the original datasets are stored
global SAMPLING_OUT "$ROOT/data/raw_confidential" // folder for output (datasets, tables)
global TEMP "$SAMPLING_OUT/temp_${rt_id_iter}_h${hypothesis}_${iteration}" // folder for temporary datasets (unique per rt_id, hypothesis, and iteration)
global HUNDRED "$SAMPLING_OUT/100%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder for 100% datasets
global IN "$ROOT/data/raw_confidential/5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder, in which the original datasets are stored
display as text "Resolved IN path: $IN"
global NINETYFIVE "$SAMPLING_OUT/95%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder for 95% datasets
display as text "Resolved NINETYFIVE path: $NINETYFIVE"
global FIVE "$SAMPLING_OUT/5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder for 5% datasets
display as text "Resolved FIVE path: $FIVE"

* Ensure sampling output directories exist
cap mkdir "$HUNDRED"
cap mkdir "$NINETYFIVE"
cap mkdir "$FIVE"
cap mkdir "$TEMP"

*FILE PATH AS SETUP FOR ANALYSIS DOFILES *
global IN "$ROOT\data\raw_confidential\5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder, in which the original datasets are stored
display as text "Resolved IN path: $IN"
global DATA "$ROOT\data\derived_confidential\5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder for datasets
display as text "Resolved DATA path: $DATA"
global OUT "$ROOT/data/derived/5_idiosyncrasy_results/${rt_id_iter}/.job_${rt_id_iter}_${iteration}_h${hypothesis}" // folder for output (results, tables) - per-job subdir prevents race conditions
display as text "Resolved OUT path: $OUT"

* Verify output directories exist (Python creates them with retries before launching Stata)
* Setup only does quick verification - no creation to avoid race conditions
cap mkdir "$DATA"
* Ensure OUT parent directories exist (Stata does not create nested dirs)
local out_base "$ROOT/data/derived/5_idiosyncrasy_results"
cap mkdir "`out_base'"
cap mkdir "`out_base'/${rt_id_iter}"
cap mkdir "$OUT"

display as text "✓ Directory verification: $DATA and $OUT ready for analysis"

* Quick write-test to verify OUT path is writable (no braces block)
cap file close __out_test
capture file open __out_test using "$OUT/test_write.txt", write replace
local __rc = _rc
if `__rc' == 0 {
    file write __out_test "ok" _n
    file close __out_test
    display as text "✓ OUT write-test succeeded"
    cap erase "$OUT/test_write.txt"
}
else {
    display as error "✗ OUT write-test failed (rc=`__rc') at: $OUT"
}


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
retry_save "$HUNDRED/ppathl.dta", replace

* create dataset where hid and syear uniquely identify an observation for creating 100% household datasets:
preserve
duplicates drop hid syear, force
retry_save "$TEMP/ppathl_100_hid.dta", replace
restore

* save for creating wide 100% datasets:
preserve
duplicates drop pid, force
retry_save "$TEMP/ppathl_100_wide.dta", replace
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
retry_save "$FIVE/ppathl.dta", replace

* for creating wide 5% datasets:
preserve
duplicates drop pid, force
retry_save "$TEMP/ppathl_5_wide.dta", replace
restore

* create dataset where hid and syear uniquely identify an observation for creating 5% household datasets:
preserve
duplicates drop hid syear, force
retry_save "$TEMP/ppathl_5_hid.dta", replace
restore


*** 95% samples ***

use "$TEMP/ppathl_100_wide.dta", clear
merge 1:1 pid using "$TEMP/ppathl_5_wide.dta", keep(master matches)
* only keep the observations, that were not matched (i.e., those pid that were not in the 5% sample):
keep if _merge==1
drop _merge

* save for creating wide 95% datasets:
retry_save "$TEMP/ppathl_95_wide.dta", replace

* merge with ppathl again, to add all observations of the selected participants:
keep pid
merge 1:m pid using "$HUNDRED/ppathl.dta", nogen keep (master matches)

* save 95% sample:
retry_save "$NINETYFIVE/ppathl.dta", replace

* create dataset where hid and syear uniquely identify an observation for creating 95% household datasets:
duplicates drop hid syear, force
retry_save "$TEMP/ppathl_95_hid.dta", replace


**************** WIDE DATASETS *******************

********** BIOBIRTH ************

* create 100% sample:
use "$TEMP/ppathl_100_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_IN/biobirth.dta", nogen keep(master matches)
retry_save "$HUNDRED/biobirth.dta", replace

* create 5% sample:
use "$TEMP/ppathl_5_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_IN/biobirth.dta", nogen keep(master matches)
retry_save "$FIVE/biobirth.dta", replace

* create 95% sample:
use "$TEMP/ppathl_95_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_IN/biobirth.dta", nogen keep(master matches)
retry_save "$NINETYFIVE/biobirth.dta", replace

********** BIOPAREN ************

* create 100% sample:
use "$TEMP/ppathl_100_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_IN/bioparen.dta", nogen keep(master matches)
retry_save "$HUNDRED/bioparen.dta", replace

* create 5% sample:
use "$TEMP/ppathl_5_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_IN/bioparen.dta", nogen keep(master matches)
retry_save "$FIVE/bioparen.dta", replace

* create 95% sample:
use "$TEMP/ppathl_95_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_IN/bioparen.dta", nogen keep(master matches)
retry_save "$NINETYFIVE/bioparen.dta", replace

**************** LONG DATASETS *******************

********** BIOL ************

* create 100% sample:
use "$HUNDRED/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_IN/biol.dta", nogen keep(master matches)
retry_save "$HUNDRED/biol.dta", replace

* create 5% sample:
use "$FIVE/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_IN/biol.dta", nogen keep(master matches)
retry_save "$FIVE/biol.dta", replace

* create 95% sample:
use "$NINETYFIVE/ppathl.dta", clear
keep pid syear
merge m:1 pid syear using "$SOEP_IN/biol.dta", nogen keep(master matches)
retry_save "$NINETYFIVE/biol.dta", replace

********** PGEN ************

* create 100% sample:
use "$HUNDRED/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_IN/pgen.dta", nogen keep(master matches)
//compress
retry_save "$HUNDRED/pgen.dta", replace

* create 5% sample:
use "$FIVE/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_IN/pgen.dta", nogen keep(master matches)
//compress
retry_save "$FIVE/pgen.dta", replace

* create 95% sample:
use "$NINETYFIVE/ppathl.dta", clear
keep pid syear
merge m:1 pid syear using "$SOEP_IN/pgen.dta", nogen keep(master matches)
//compress
retry_save "$NINETYFIVE/pgen.dta", replace

********** PL ************

* create 100% sample:
use "$HUNDRED/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_IN/pl.dta", nogen keep(master matches)
//compress
retry_save "$HUNDRED/pl.dta", replace

* create 5% sample:
use "$FIVE/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_IN/pl.dta", nogen keep(master matches)
//compress
retry_save "$FIVE/pl.dta", replace

* create 95% sample:
use "$NINETYFIVE/ppathl.dta", clear
keep pid syear
merge m:1 pid syear using "$SOEP_IN/pl.dta", nogen keep(master matches)
//compress
retry_save "$NINETYFIVE/pl.dta", replace


**************** HOUSEHOLD LEVEL DATASETS *******************

********** HL ************

* create 100% sample:
use "$TEMP/ppathl_100_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_IN/hl.dta", nogen keep(master matches)
retry_save "$HUNDRED/hl.dta", replace

* create 5% sample:
use "$TEMP/ppathl_5_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_IN/hl.dta", nogen keep(master matches)
retry_save "$FIVE/hl.dta", replace

* create 95% sample:
use "$TEMP/ppathl_95_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_IN/hl.dta", nogen keep(master matches)
retry_save "$NINETYFIVE/hl.dta", replace

********** HGEN ************

* create 100% sample:
use "$TEMP/ppathl_100_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_IN/hgen.dta", nogen keep(master matches)
retry_save "$HUNDRED/hgen.dta", replace

* create 5% sample:
use "$TEMP/ppathl_5_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_IN/hgen.dta", nogen keep(master matches)
retry_save "$FIVE/hgen.dta", replace

* create 95% sample:
use "$TEMP/ppathl_95_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_IN/hgen.dta", nogen keep(master matches)
retry_save "$NINETYFIVE/hgen.dta", replace




***** Create provided example derived datasets

* Ensure DATA directory exists before saving
cap mkdir "$DATA"

* 5% sample
use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	retry_save "$DATA/h1_dataset.dta", replace // save merged dataset
    retry_save "$DATA/h2_dataset.dta", replace // save merged dataset
    retry_save "$DATA/h3_dataset.dta", replace // save merged dataset
    retry_save "$DATA/h4_dataset.dta", replace // save merged dataset


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
global IN "$ROOT\data\raw_confidential\5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder, in which the original datasets are stored
display as text "Resolved IN path: $IN"
global DATA "$ROOT\data\derived_confidential\5%_iter_${rt_id_iter}_h${hypothesis}_${iteration}" // folder for datasets
display as text "Resolved DATA path: $DATA"
global OUT "$ROOT/data/derived/5_idiosyncrasy_results/${rt_id_iter}/.job_${rt_id_iter}_${iteration}_h${hypothesis}" // folder for output (results, tables) - per-job subdir prevents race conditions
display as text "Resolved OUT path: $OUT"

* Ensure all output directories exist before analysis (also after global reset)
cap mkdir "$DATA"
* Ensure OUT parent directories exist after renew
local out_base2 "$ROOT/data/derived/5_idiosyncrasy_results"
cap mkdir "`out_base2'"
cap mkdir "`out_base2'/${rt_id_iter}"
cap mkdir "$OUT"

* Quick write-test to verify OUT path still writable after renew
cap file close __out_test2
capture file open __out_test2 using "$OUT/test_write_renew.txt", write replace
local __rc2 = _rc
if `__rc2' == 0 {
    file write __out_test2 "ok" _n
    file close __out_test2
    display as text "✓ OUT write-test (renew) succeeded"
    cap erase "$OUT/test_write_renew.txt"
}
else {
    display as error "✗ OUT write-test (renew) failed (rc=`__rc2') at: $OUT"
}