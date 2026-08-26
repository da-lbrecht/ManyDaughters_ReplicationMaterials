********************************************************************************
*********** Create SOEP data datasets for the #ManyDaughters Project ***********
********************************************************************************
***************** Realized Individual Interview in 2000 - 2020 *****************
************************ Randomization on cid level ****************************
********************************************************************************

* Authors: Tina Wang, David Albrecht

clear all
version 19 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* Start timer
timer clear
timer on 1

* GLOBALS *
* Preserve the canonical main-script globals (IN, DATA, OUT, CODE). We derive the
* SOEP-specific source path and sample output folders from those values instead of
* overwriting them.

global BASEDATA "$ROOT/data/raw_confidential" // folder for SOEP datasets

global SOEP_RAW "$BASEDATA/SOEP_v37" // folder with original SOEP files

global TEMP "$BASEDATA/temp" // folder for temporary datasets

global HUNDRED "$BASEDATA/100%-sample" // folder for 100% datasets
global NINETYFIVE "$BASEDATA/95%-sample" // folder for 95% datasets
global FIVE "$BASEDATA/5%-sample" // folder for 5% datasets

capture mkdir "$TEMP"
capture mkdir "$HUNDRED"
capture mkdir "$NINETYFIVE"
capture mkdir "$FIVE"

********** PPATHL ************

* The resulting 100% sample is identitical to the one in option 4. However, the 5% sample will not be drawn from all individuals but from all original households (based on cid). This means, that for each person in the 5% sample, all original household members that fulfill the sample restrictions (-> at least one realized interview 2000-2020) will also be in the 5% sample.

*** 100% samples ***

use "$SOEP_RAW/ppathl.dta", clear

* set relevant filters
keep if inrange(syear, 2000, 2020) // only keep survey years 2000 to 2020
keep if (inrange(netto,10,16) | netto==19) // only keep those with a realized individual interview (adults and 17-year olds)
duplicates drop pid, force

* merge with ppathl again, to add all observations of the selected participants:
keep pid
merge 1:m pid using "$SOEP_RAW/ppathl.dta", nogen keep (master matches)
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
set seed 24082026 // altered from original analysis, to ensure anonymity of RTs with respect to their dofiles and results
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


*** 95% samples ***

use "$TEMP/ppathl_100_wide.dta", clear
merge 1:1 pid using "$TEMP/ppathl_5_wide.dta", keep(master matches)
* only keep the observations, that were not matched (i.e., those pid that were not in the 5% sample):
keep if _merge==1
drop _merge

* save for creating wide 95% datasets:
save "$TEMP/ppathl_95_wide.dta", replace

* merge with ppathl again, to add all observations of the selected participants:
keep pid
merge 1:m pid using "$HUNDRED/ppathl.dta", nogen keep (master matches)

* save 95% sample:
save "$NINETYFIVE/ppathl.dta", replace

* quick checks for 95% ppathl sample size:
count
local n_obs = r(N)

preserve
duplicates drop pid, force
count
local n_pid = r(N)
restore

preserve
duplicates drop hid, force
count
local n_hid = r(N)
restore

display as text "95% ppathl.dta checks"
display as result "Individuals (unique pid): `n_pid'"
display as result "Households (unique hid): `n_hid'"
display as result "Observations: `n_obs'"



* create dataset where hid and syear uniquely identify an observation for creating 95% household datasets:
duplicates drop hid syear, force
save "$TEMP/ppathl_95_hid.dta", replace


**************** WIDE DATASETS *******************

********** BIOBIRTH ************

* create 100% sample:
use "$TEMP/ppathl_100_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_RAW/biobirth.dta", nogen keep(master matches)
save "$HUNDRED/biobirth.dta", replace

* create 5% sample:
use "$TEMP/ppathl_5_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_RAW/biobirth.dta", nogen keep(master matches)
save "$FIVE/biobirth.dta", replace

* create 95% sample:
use "$TEMP/ppathl_95_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_RAW/biobirth.dta", nogen keep(master matches)
save "$NINETYFIVE/biobirth.dta", replace

********** BIOPAREN ************

* create 100% sample:
use "$TEMP/ppathl_100_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_RAW/bioparen.dta", nogen keep(master matches)
save "$HUNDRED/bioparen.dta", replace

* create 5% sample:
use "$TEMP/ppathl_5_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_RAW/bioparen.dta", nogen keep(master matches)
save "$FIVE/bioparen.dta", replace

* create 95% sample:
use "$TEMP/ppathl_95_wide.dta", clear
keep pid
merge m:1 pid using "$SOEP_RAW/bioparen.dta", nogen keep(master matches)
save "$NINETYFIVE/bioparen.dta", replace

**************** LONG DATASETS *******************

********** BIOL ************

* create 100% sample:
use "$HUNDRED/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_RAW/biol.dta", nogen keep(master matches)
save "$HUNDRED/biol.dta", replace

* create 5% sample:
use "$FIVE/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_RAW/biol.dta", nogen keep(master matches)
save "$FIVE/biol.dta", replace

* create 95% sample:
use "$NINETYFIVE/ppathl.dta", clear
keep pid syear
merge m:1 pid syear using "$SOEP_RAW/biol.dta", nogen keep(master matches)
save "$NINETYFIVE/biol.dta", replace

********** PGEN ************

* create 100% sample:
use "$HUNDRED/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_RAW/pgen.dta", nogen keep(master matches)
save "$HUNDRED/pgen.dta", replace

* create 5% sample:
use "$FIVE/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_RAW/pgen.dta", nogen keep(master matches)
save "$FIVE/pgen.dta", replace

* create 95% sample:
use "$NINETYFIVE/ppathl.dta", clear
keep pid syear
merge m:1 pid syear using "$SOEP_RAW/pgen.dta", nogen keep(master matches)
save "$NINETYFIVE/pgen.dta", replace

********** PL ************

* create 100% sample:
use "$HUNDRED/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_RAW/pl.dta", nogen keep(master matches)
save "$HUNDRED/pl.dta", replace

* create 5% sample:
use "$FIVE/ppathl.dta", clear
keep pid syear
merge 1:1 pid syear using "$SOEP_RAW/pl.dta", nogen keep(master matches)
save "$FIVE/pl.dta", replace

* create 95% sample:
use "$NINETYFIVE/ppathl.dta", clear
keep pid syear
merge m:1 pid syear using "$SOEP_RAW/pl.dta", nogen keep(master matches)
save "$NINETYFIVE/pl.dta", replace


**************** HOUSEHOLD LEVEL DATASETS *******************

********** HL ************

* create 100% sample:
use "$TEMP/ppathl_100_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_RAW/hl.dta", nogen keep(master matches)
save "$HUNDRED/hl.dta", replace

* create 5% sample:
use "$TEMP/ppathl_5_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_RAW/hl.dta", nogen keep(master matches)
save "$FIVE/hl.dta", replace

* create 95% sample:
use "$TEMP/ppathl_95_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_RAW/hl.dta", nogen keep(master matches)
save "$NINETYFIVE/hl.dta", replace

********** HGEN ************

* create 100% sample:
use "$TEMP/ppathl_100_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_RAW/hgen.dta", nogen keep(master matches)
save "$HUNDRED/hgen.dta", replace

* create 5% sample:
use "$TEMP/ppathl_5_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_RAW/hgen.dta", nogen keep(master matches)
save "$FIVE/hgen.dta", replace

* create 95% sample:
use "$TEMP/ppathl_95_hid.dta", clear
keep hid syear
merge 1:1 hid syear using "$SOEP_RAW/hgen.dta", nogen keep(master matches)
save "$NINETYFIVE/hgen.dta", replace


**************** PROVIDED EXAMPLE DERIVED DATASETS *******************

* Create example derived datasets for the 5% and 95% samples in the confidential derived folder.
* This runs at the end so all raw sample datasets are already available.
local provided_5_dir "$ROOT/data/derived_confidential/5%-sample_provided"
local provided_95_dir "$ROOT/data/derived_confidential/95%-sample_provided"
local sample_5_dir "$ROOT/data/derived_confidential/5%-sample"
local sample_95_dir "$ROOT/data/derived_confidential/95%-sample"
cap mkdir "$ROOT/data/derived_confidential"
cap mkdir "`provided_5_dir'"
cap mkdir "`provided_95_dir'"
cap mkdir "`sample_5_dir'"
cap mkdir "`sample_95_dir'"

* 5% sample example datasets
use "$FIVE/ppathl.dta", clear
merge m:1 pid using "$FIVE/biobirth.dta", nogen keep(master matches)
merge 1:1 pid syear using "$FIVE/pgen.dta", nogen keep(master matches)
merge 1:1 pid syear using "$FIVE/pl.dta", nogen keep(master matches)
merge m:1 hid syear using "$FIVE/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)
label language EN
save "`provided_5_dir'/h1_dataset.dta", replace
save "`sample_5_dir'/h1_dataset.dta", replace
save "`provided_5_dir'/h2_dataset.dta", replace
save "`sample_5_dir'/h2_dataset.dta", replace
save "`provided_5_dir'/h3_dataset.dta", replace
save "`sample_5_dir'/h3_dataset.dta", replace
save "`provided_5_dir'/h4_dataset.dta", replace
save "`sample_5_dir'/h4_dataset.dta", replace

* 95% sample example datasets
use "$NINETYFIVE/ppathl.dta", clear
merge m:1 pid using "$NINETYFIVE/biobirth.dta", nogen keep(master matches)
merge 1:1 pid syear using "$NINETYFIVE/pgen.dta", nogen keep(master matches)
merge 1:1 pid syear using "$NINETYFIVE/pl.dta", nogen keep(master matches)
merge m:1 hid syear using "$NINETYFIVE/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)
label language EN
save "`provided_95_dir'/h1_dataset.dta", replace
save "`sample_95_dir'/h1_dataset.dta", replace
save "`provided_95_dir'/h2_dataset.dta", replace
save "`sample_95_dir'/h2_dataset.dta", replace
save "`provided_95_dir'/h3_dataset.dta", replace
save "`sample_95_dir'/h3_dataset.dta", replace
save "`provided_95_dir'/h4_dataset.dta", replace
save "`sample_95_dir'/h4_dataset.dta", replace

* Stop timer and display elapsed time
timer off 1
timer list 1

exit

























