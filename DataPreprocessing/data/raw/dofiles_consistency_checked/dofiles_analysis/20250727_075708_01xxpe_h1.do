********************************************************************************
****************** Do-File H1: ManyDaughters Project ******************
********************************************************************************

* Changes from PAP
* The variable "hgnrpers" denoting number of persons living in a household from the dataset "pgen" that was present in the documentation was unfortunately not featured in the datasets made available to us, therefore we could not calculate the Nr of Adults control variable and consequently we dropped it from the analyses. 
* The variable pl:plh0308_v2 used for construction of GenderAttitude did not have 0-7 response options as we originally outlined, but 1-7, so the code was adjusted to reflect that. 
* In addition to excluding respondents who report having their first child in 2018, we excluded also those having their first child any year after.
* Recoded negative values for all control variables as missing values 
* EastWest: will we code East as 0 and West as 1, Immigration status: 0 = German, 1 = Immigrant

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

*clear all
version 19.5
set maxvar 10000
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

* Filter to survey year 2018
keep if syear == 2018

* Exclude respondents <18
gen AgeP = syear - gebjahr
keep if AgeP > 18

* Replace all negative values with missing in kidgeb01 to kidgeb19
foreach v of varlist kidgeb01-kidgeb19 {
    replace `v' = . if `v' < 0
}

foreach v of varlist kidsex01-kidsex19 {
    replace `v' = . if `v' < 0
}

* Exclude if first child in 2018 or any child born after 2018
drop if kidgeb01 == 2018

* Flag anyone with any child birth-year > 2018
gen has_future_birth = 0
foreach v of varlist kidgeb01-kidgeb19 {
    replace has_future_birth = 1 if `v' > 2018 & !missing(`v')
}

* Now drop all those flagged
drop if has_future_birth == 1
drop has_future_birth

* Recode GenderAttitude
gen GenderAttitude = .
replace GenderAttitude = plh0308_v2 if inrange(plh0308_v2,1,7)

* Generate HavingDaughter: 1 if any kidsex## == 2
gen HavingDaughter = (kidsex01==2 | kidsex02==2 | kidsex03==2 | kidsex04==2 | ///
                      kidsex05==2 | kidsex06==2 | kidsex07==2 | kidsex08==2 | ///
                      kidsex09==2 | kidsex10==2 | kidsex11==2 | kidsex12==2 | ///
                      kidsex13==2 | kidsex14==2 | kidsex15==2 | kidsex16==2 | ///
                      kidsex17==2 | kidsex18==2 | kidsex19==2)


* Clean control variables

* BioKids
gen BioKids = sumkids
replace BioKids = 0 if sumkids == -2

* Age of first daughter
gen AgeD = 0
gen min_daughter_year = .

forvalues i = 1/19 {
    local kidnum = string(`i', "%02.0f")  // use backticks here
    replace min_daughter_year = kidgeb`kidnum' if kidsex`kidnum' == 2 & (missing(min_daughter_year) | kidgeb`kidnum' < min_daughter_year)
}

replace AgeD = syear - min_daughter_year if !missing(min_daughter_year)

drop min_daughter_year

* ParentSex: recode sex (1=male → 0, 2=female → 1)
gen ParentSex = .
replace ParentSex = 0 if sex == 1
replace ParentSex = 1 if sex == 2
label define ParentSex_lbl 0 "Father" 1 "Mother"
label values ParentSex ParentSex_lbl

* MaritalStatus
gen MaritalStatus = pgfamstd
replace MaritalStatus = . if pgfamstd < 0

* PartnerStatus
gen PartnerStatus = pgpartz
replace PartnerStatus = . if pgpartz < 0

* Education: impute from 2017 if missing
preserve
use "$IN/pgen.dta", clear
keep if syear == 2017
keep pid pgbilzeit
rename pgbilzeit Edu2017
duplicates drop pid, force
tempfile edu2017
save `edu2017'
restore

merge 1:1 pid using `edu2017', keep(master match) nogen
replace pgbilzeit = . if pgbilzeit < 0
gen Edu = pgbilzeit
replace Edu = Edu2017 if missing(Edu)
drop Edu2017

* EastWest: impute from 2017 if missing
preserve
use "$IN/ppathl.dta", clear
keep if syear == 2017
keep pid sampreg
rename sampreg EastWest2017
duplicates drop pid, force
tempfile ew2017
save `ew2017'
restore

merge 1:1 pid using `ew2017', keep(master match) nogen
gen EastWest = sampreg
replace EastWest = EastWest2017 if missing(EastWest)
drop EastWest2017

* Immigration status
gen Imm = .
replace Imm = 0 if pgnation == 1
replace Imm = 1 if pgnation > 1
replace Imm = . if pgnation < 0

* Save derived dataset
save "${DATA}/h1_dataset.dta", replace

* ============================
* Analysis: Model H1 *
* ============================

glm GenderAttitude HavingDaughter BioKids AgeD AgeP ParentSex MaritalStatus PartnerStatus Edu Imm EastWest ///
    , family(gamma) link(log)

* ============================
* Export Results
* ============================

* Extract statistics
local coefficient = _b[HavingDaughter]
local se = _se[HavingDaughter]
local df_model        = e(df_m)
local test_stat_family = "z"
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
file open res using "${OUT}/h1_results.csv", write replace
file write res "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write res "`coefficient',`se', `test_stat_family', `df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close res


exit
