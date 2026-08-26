********************************************************************************
****************** Do-File H2: ManyDaughters Project ******************
********************************************************************************

* Changes from PAP
* The variable "hgnrpers" denoting number of persons living in a household from the dataset "pgen" that was present in the documentation was unfortunately not featured in the datasets made available to us, therefore we could not calculate the Nr of Adults control variable and consequently we dropped it from the analyses. 
* Recoded negative values for all control variables as missing values 
* When constructing HavingDaughter variable per Wave, we have counted with kids-related variables from the year prior (so HavingDaughters for 2005 runs our pre-defined construction formula with kids from before 2005)
* We have dropped the following individual-difference control variables from the final model because they were static: Education, Immigration Status, Sex
* EastWest: will we code East as 0 and West as 1


* Authors: [REDACTED]

* ============================
* Setup *
* ============================

*clear all
version 19.5
set maxvar 10000

* Install PACKAGES
* ssc install reghdfe, replace
* ssc install ftools, replace
/*
* CONFIGURE FILE PATHS * (Please do not change)
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for datasets
global OUT "$ROOT/out" // folder for output (results, tables)
*/

* ============================
* Load and Merge Data *
* ============================

use "$IN/pl.dta", clear	//
merge 1:1 pid syear using "$IN/ppathl.dta", keep(master match) nogen
merge 1:1 pid syear using "$IN/pgen.dta", keep(match) nogen keepusing(pgfamstd pgpartz pgbilzeit pgnation)
merge m:1 pid using "$IN/biobirth.dta", keep(match) nogen keepusing(kidsex01-kidsex19 sumkids kidgeb01-kidgeb19)

* ============================
* Data Preparation *
* ============================

* Filter waves of interest
keep if inlist(syear, 2005, 2009, 2014, 2019)

* Exclude respondents <18
gen AgeP = syear - gebjahr
keep if AgeP > 18

* Create Wave variable
gen Wave = syear

* Recode political scale: LeftRight = 10 – plh0004, 0–10
gen LeftRight = .
replace LeftRight = 10 - plh0004 if inrange(plh0004,0,10)

* Replace all negative values with missing in kidgeb01 to kidgeb19
foreach v of varlist kidgeb01-kidgeb19 {
    replace `v' = . if `v' < 0
}

foreach v of varlist kidsex01-kidsex19 {
    replace `v' = . if `v' < 0
}

* 5) Generate HavingDaughter for each respondent‐wave
*    flag any daughter born before the survey year
*    first, build 19 flags
forvalues i = 1/19 {
    local id = string(`i', "%02.0f")
    gen has_daughter`id' = (kidsex`id'==2 & kidgeb`id' < syear)
}
* then collapse to a single indicator
* collapse 19 flags into one indicator
egen HavingDaughter = rowmax(has_daughter01-has_daughter19)
drop has_daughter01-has_daughter19

* Count participants with at least one daughter
tab HavingDaughter

* AgeD (age of oldest daughter)
forvalues i = 1/19 {
    local idx = string(`i', "%02.0f")
    * Create temporary age variable for each daughter
    gen age_temp`idx' = . 
    replace age_temp`idx' = Wave - kidgeb`idx' ///
        if kidsex`idx' == 2 & !missing(kidgeb`idx') & kidgeb`idx' < Wave
}

* Find MAXIMUM age (oldest daughter)
egen max_age = rowmax(age_temp01-age_temp19)  // CHANGED: rowmin → rowmax

* Set AgeD to oldest daughter's age
gen AgeD = max_age if !missing(max_age)

* Remove variables
drop age_temp01-age_temp19 max_age

* 0 for no daughters
replace AgeD = 0 if missing(AgeD)  // No daughters

*BioKids: count children born before Wave 
forvalues i = 1/19 {
    local idx = string(`i', "%02.0f")
    gen kid`idx'_before = (kidgeb`idx' >= 0 & kidgeb`idx' < Wave)
}

egen BioKids = rowtotal(kid01_before-kid19_before)

drop kid01_before-kid19_before

* Clean control variables

* MaritalStatus
gen MaritalStatus = pgfamstd
replace MaritalStatus = . if pgfamstd < 0

* PartnerStatus
gen PartnerStatus = pgpartz
replace PartnerStatus = . if pgpartz < 0

* Education
gen Edu = pgbilzeit
replace Edu = . if pgbilzeit < 0

* East/West
gen EastWest = sampreg
replace EastWest = . if sampreg < 0

* Immigration status
gen Imm = .
replace Imm = 0 if pgnation == 1
replace Imm = 1 if pgnation > 1
replace Imm = . if pgnation < 0

* Save the derived dataset
save "${DATA}/h2_dataset.dta", replace

* ============================
* Analysis: Model H2 *
* ============================

* run two‐way FE: pid & Wave, cluster by pid
reghdfe LeftRight HavingDaughter AgeD BioKids MaritalStatus ///
        PartnerStatus Edu Imm EastWest, absorb(pid Wave) vce(cluster pid)

* ============================
* Export Results
* ============================

* Extract statistics
local coefficient = _b[HavingDaughter]
local se = _se[HavingDaughter]
local df_model        = e(df_m)
local test_stat_family = "t"
local test_statistic = `coefficient'/`se'
local p_value = 2*normal(-abs(`test_statistic'))
local num_obs = e(N)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'
local df_residual = `num_obs' - `df_model' - 1

local interpretation "positive coefficient is in line with hypothesis"



di "Coefficient: `coefficient'"
di "Std. Error: `se'"
di "family: `test_stat_family'"
di "Residual DF: `df_residual'"
di "Model DF: `df_model'"
di "p_value: `p_value'"
di "Num Obs: `num_obs'"
di "Num Participants: `num_participants'"
di "Direction: `interpretation'"

* Export results to CSV
file open res using "${OUT}/h2_results.csv", write replace
file write res "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write res "`coefficient',`se', `test_stat_family', `df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close res

exit
