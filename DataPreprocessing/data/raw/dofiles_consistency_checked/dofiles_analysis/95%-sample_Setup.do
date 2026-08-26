********************************************************************************
***************************** General Stata Setup ******************************
********************************************************************************

clear all

* CONFIGURE FILE PATHS * (Please do not change)
* Resolve the project root whether Stata starts in the consistency-checked folder
* or its dofiles_analysis subfolder. Normalize Windows separators first.
local curdir = subinstr(c(pwd), "\", "/", .)
local marker "/data/raw/dofiles_consistency_checked"
local marker_pos = strpos("`curdir'", "`marker'")

if `marker_pos' > 0 {
    local root = substr("`curdir'", 1, `marker_pos' - 1)
    global ROOT "`root'"
}
else {
    global ROOT "`curdir'"
}
display as text "Resolved ROOT path: $ROOT"

global IN "$ROOT/data/raw_confidential/95%-sample" // folder, in which the original datasets are stored
display as text "Resolved IN path: $IN"
global DATA "$ROOT/data/derived_confidential/95%-sample" // folder for datasets
display as text "Resolved DATA path: $DATA"
global OUT "$ROOT/data/derived/95_results" // folder for output (results, tables)
display as text "Resolved OUT path: $OUT"
