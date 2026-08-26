********************************************************************************
************ #ManyDaughters Project: Data Preprocessing - 0. Main ***************
********************************************************************************

* Authors: David Albrecht

* ============================
* SETUP
* ============================

clear all
version 19

* FILE PATHS *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for generated datasets
global OUT "$ROOT/out" // folder for output
global CODE "$ROOT/code"

* Create logs directory if it doesn't exist
capture mkdir "$ROOT/logs"


* Create one timestamp shared by the main and child log files
local timestamp "`c(current_date)'_`c(current_time)'"
local timestamp = subinstr("`timestamp'", ":", "-", .)
local timestamp = subinstr("`timestamp'", " ", "_", .)

* Close any stale log before opening the new ones
capture log close _all

* Start the main log.
local main_logfile "$ROOT/logs/0_main_`timestamp'.log"
log using "`main_logfile'", name(main_log) replace text

********************************************************************************

* do "$CODE/01_SOEP-dataset_creation.do" // requires SOEP data, which is not publicly available as part of the replication kit
do "$CODE/05_data_import.do"
do "$CODE/06_collect_RT_results_05.do"
do "$CODE/07_collect_RT_results_95.do"
save "$DATA/ManyDaughters_preprocessed.dta", replace

********************************************************************************

do "$CODE/08_collect_RT_results_idiosyncrasy.do" // gather RT analysis results for idiosycnrasy index calculations



log close main_log
