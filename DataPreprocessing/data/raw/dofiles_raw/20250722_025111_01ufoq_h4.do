********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]


* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
// Re-insertion of global definitions for input, data, and output directories by David Albrecht for reproducibility check
global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for datasets
global OUT "$ROOT/out" // folder for output (results, tables)

*/

* ============================
* Data Preparation * 
* ============================
* Load data (biobirth.dta, bioparen.dta, pl.dta only)
// use "../data/ManyDaughters/data/raw/biobirth.dta", clear // Adapted by David Albrecht for reproducibility check
use "$IN/biobirth.dta", clear

* Create child, daughter, son flags
gen IsChild = (biokids > 0)
gen NumDaughter = 0
gen NumSon = 0
forvalues i = 1/19 {
    local vname kidsex`=string(`i', "%02.0f")'
    quietly replace NumDaughter = NumDaughter + (`vname' == 2)
    quietly replace NumSon = NumSon + (`vname' == 1)
}
gen IsDaughter = (NumDaughter > 0)
gen IsSon = (NumSon > 0)

sum IsChild NumDaughter IsDaughter IsSon

// save "../data/ManyDaughters/data/processed/d1.dta", replace // Adapted by David Albrecht for reproducibility check
save "$DATA/d1.dta", replace


// use "../data/ManyDaughters/data/raw/bioparen.dta", clear // Adapted by David Albrecht for reproducibility check
use "$IN/bioparen.dta", clear
gen IsDadGerman = (fnat == 1) if !missing(fnat)
gen IsMomGerman = (mnat == 1) if !missing(mnat)

drop cid
// save "../data/ManyDaughters/data/processed/d3.dta", replace // Adapted by David Albrecht for reproducibility check
save "$DATA/d3.dta", replace

// use "../data/ManyDaughters/data/processed/d1.dta", clear // Adapted by David Albrecht for reproducibility check
use "$DATA/d1.dta", clear
// merge 1:1 pid using "../data/ManyDaughters/data/processed/d3.dta" // Adapted by David Albrecht for reproducibility check
merge 1:1 pid using "$DATA/d3.dta"
drop if _merge == 2
drop _merge
// save "../data/ManyDaughters/data/processed/d13.dta", replace // Adapted by David Albrecht for reproducibility check
save "$DATA/d13.dta", replace

// use "../data/ManyDaughters/data/raw/pl.dta", clear // Adapted by David Albrecht for reproducibility check 
use "$IN/pl.dta", clear
// merge m:1 pid using "../data/ManyDaughters/data/processed/d13.dta" // Adapted by David Albrecht for reproducibility check
merge m:1 pid using "$DATA/d13.dta"
keep if inlist(_merge, 1, 3)
drop _merge

// save "../data/ManyDaughters/data/processed/d713.dta", replace // Adapted by David Albrecht for reproducibility check
save "$DATA/d713.dta", replace

sum pid syear pld0025 plh0004 pli0043_h pli0016_h pli0012_h

* Fill missing daughter/son dummy from periodic questions
// use "../data/ManyDaughters/data/processed/d713.dta", clear // Adapted by David Albrecht for reproducibility check
use "$DATA/d713.dta", clear
sort pid syear
gen Dau_Dummy = -100
gen Son_Dummy = -100

* Assign values in survey years where data is available
replace Dau_Dummy = pld0025 if inlist(syear, 2001, 2006, 2011, 2016)
replace Son_Dummy = pld0027 if inlist(syear, 2001, 2006, 2011, 2016)

* Fill values in years between known survey years
* Loop over all observations
quietly {
    gen index = _n
    gen prev_pld0025 = pld0025
    gen prev_pld0027 = pld0027

    forvalues i = 1/`=_N' {
        quietly {
            local year = syear[`i']

            if inrange(`year', 2002, 2005) {
                local t = `year' - 2001
                replace Dau_Dummy = pld0025[`i' - `t'] in `i'
                replace Son_Dummy = pld0027[`i' - `t'] in `i'
            }
            else if inrange(`year', 2007, 2010) {
                local t = `year' - 2006
                replace Dau_Dummy = pld0025[`i' - `t'] in `i'
                replace Son_Dummy = pld0027[`i' - `t'] in `i'
            }
            else if inrange(`year', 2012, 2015) {
                local t = `year' - 2011
                replace Dau_Dummy = pld0025[`i' - `t'] in `i'
                replace Son_Dummy = pld0027[`i' - `t'] in `i'
            }
            else if inrange(`year', 2017, 2020) {
                local t = `year' - 2016
                replace Dau_Dummy = pld0025[`i' - `t'] in `i'
                replace Son_Dummy = pld0027[`i' - `t'] in `i'
            }
        }
    }
}

* Gender equality support variable (standardized)
gen gender_equality_support = -100
replace gender_equality_support = plh0308_v2 if syear == 2018
replace gender_equality_support = (5 - plh0308_v1) * 7 / 4 if syear == 2012 & plh0308_v1 > 0
replace gender_equality_support = plh0308_v1 if syear == 2012 & plh0308_v1 <= 0

gen Dau_Dummy2 = (Dau_Dummy == 1) if Dau_Dummy != .
gen Son_Dummy2 = (Son_Dummy == 1) if Son_Dummy != .

/*Create reversed political support score (H2)*/
gen Support_Left = 10 - plh0004 if plh0004 > -1

/* Construct cleaned housework time variables (missing if < 0) (H3)*/
gen h1 = . 
replace h1 = pli0012_h if pli0012_h >= 0

gen h2 = . 
replace h2 = pli0016_h if pli0016_h >= 0

gen h3 = . 
replace h3 = pli0043_h if pli0043_h >= 0

* Total housework time across all three time points
gen total = h1 + h2 + h3

gen weekend = h1 + h2

/* Create Male dummy if not done yet (H4)*/
gen Male = (sex == 1) if !missing(sex)

* ============================
* Analysis
* ============================
  
* Regression with clustered SE
regress weekend i.Dau_Dummy2##i.Male IsDadGerman IsMomGerman if inlist(Dau_Dummy, 1, -2) & weekend >= 0, vce(cluster pid)

* Extract statistics for interaction term: 1.Dau_Dummy2#1.Male
local coef = _b[1.Dau_Dummy2#1.Male]
local se = _se[1.Dau_Dummy2#1.Male]
local test_stat_family = "t"
local df_residual = e(df_r)
local df_model = e(df_m)

local t_stat = `coef' / `se'
local p_value = 2 * ttail(`df_residual', abs(`t_stat'))

local num_obs = e(N)

* Count number of unique participants
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'

* Interpret directionality
scalar coef_int = _b[1.Dau_Dummy2#1.Male]
local interpretation "positive coefficient is in line with hypothesis"


* Display for confirmation
di "Coefficient: `coef'"
di "Standard Error: `se'"
di "Test Statistic Family: `test_stat_family'"
di "Residual Degrees of Freedom: `df_residual'"
di "Model Degrees of Freedom: `df_model'"
di "p-value: `p_value'"
di "Number of observations: `num_obs'"
di "Number of participants: `num_participants'"
di "Interpretation of directionality: `interpretation'"

* Export results to CSV
//file open myfile using "../data/ManyDaughters/out/h4_clustered_results.csv", write replace // Original file name convention restored by David Albrecht for reproducibility check
file open myfile using "$OUT/h4_results.csv", write replace 
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coef',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile
