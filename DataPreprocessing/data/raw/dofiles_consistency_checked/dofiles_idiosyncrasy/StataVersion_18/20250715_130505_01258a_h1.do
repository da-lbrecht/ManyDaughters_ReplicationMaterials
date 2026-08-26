********************************************************************************
********************** Do-File H1: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

/* Deviations from PAP:

WE had to adjust for imbalances in the covariates. However, "teffects aipw" did not appear to be
very robust since it involves a lot of going back and forth when it comes to establishing common support. 
Therefore, we decided to keep with the spirit of the idea but calculate the propensity score by hand and weight
our regression (IPW).

*/


* ============================
* Setup
* ============================

*clear all
version 18                        // Specify Stata version
set maxvar 10000                  // Increase maximum number of variables

* Install required packages
foreach pack in outreg2 stddiff {
    ssc install `pack', replace
}
/*
* Set working directory manually (adjust as needed)
// cd "N:/MA/iweyland/#ManyDaughters_RT_AnalysisPackage/#ManyDaughters_RT_AnalysisPackage/code" // Commented out by David Albrecht on 15. July 2025

* Set global paths for data and outputs
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}

global IN "$ROOT/data/raw"
global DATA "$ROOT/data/derived"
global OUT "$ROOT/out"
*/
* ============================
* Data Preparation
* ============================

* Load base dataset
use "$IN/ppathl.dta", clear

* Merge additional relevant datasets
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master match)
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master match)
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master match)
merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master match) keepusing(hghinc)
merge m:1 pid using "$IN/bioparen.dta", nogen keep(master match)

* Preprocess biol.dta (ensure one observation per pid)
preserve
use "$IN/biol.dta", clear
keep if !mi(hid)
bysort pid: egen min_syear = min(syear)
keep if syear == min_syear
drop min_syear
tempfile biol_edit
save "`biol_edit'"
restore

merge m:1 pid using "`biol_edit'", nogen keep(master match)

* Translate labels to English
label language EN

* Standardize missing values
mvdecode _all, mv(-1=. \ -2=.t \ -3=.x \ -5=.y \ -8=.z)

* Save merged dataset
save "$DATA/h1_dataset.dta", replace
use "$DATA/h1_dataset.dta", clear

* ============================
* Analysis
* ============================

xtset pid syear

* Apply filters
keep if (netto >= 10 & netto < 20)            // Private households
keep if (pop == 1 | pop == 2)                // West/East Germans

* Generate dummy for daughter per year
forval x = 1/9 {
    gen daughter0`x' = (kidsex0`x' == 2 & syear >= kidgeb0`x') if !missing(kidsex0`x') & !missing(kidgeb0`x')
    replace daughter0`x' = 0 if missing(daughter0`x')
}
forval x = 10/19 {
    gen daughter`x' = (kidsex`x' == 2 & syear >= kidgeb`x') if !missing(kidsex`x') & !missing(kidgeb`x')
    replace daughter`x' = 0 if missing(daughter`x')
}

* Create indicator for at least one daughter
gen has_daughter_by_year = 0
forval x = 1/9 {
    replace has_daughter_by_year = 1 if daughter0`x' == 1
}

* Generate number of children observed each year
forval x = 1/9 {
    gen kid0`x' = (syear >= kidgeb0`x') if !missing(kidgeb0`x')
    replace kid0`x' = 0 if missing(kid0`x')
}
forval x = 10/19 {
    gen kid`x' = (syear >= kidgeb`x') if !missing(kidgeb`x')
    replace kid`x' = 0 if missing(kid`x')
}
egen numkids = rowtotal(kid01-kid19)

* ============================
* Control Variables
* ============================

gen male = (sex == 1) if inlist(sex, 1, 2)
gen yob = gebjahr
gen born_in_east = (birthregion_ew == 21) if !missing(birthregion_ew)
replace born_in_east = 0 if birthregion_ew == .t
gen mig_background = migback if inlist(migback, 1, 2, 3)
gen f_ed = fsedu if !missing(fsedu)
gen m_ed = msedu if !missing(msedu)
gen single_parent_upbringing = (lb0066 < 15) if !missing(lb0066)

* Respondent's education
gen educ_resp = pgisced11 if !missing(pgisced11)
replace educ_resp = -7 if pgisced11 == .t
bysort pid: egen max_educ = max(pgisced11)
replace educ_resp = max_educ

* Age at first interview
bysort pid: egen syear_first = min(syear)
gen age_at_first_interview = syear_first - gebjahr if !missing(syear_first) & !missing(gebjahr)

* Save and reload dataset
save "$DATA/h1_dataset_prep.dta", replace
use "$DATA/h1_dataset_prep.dta", clear

* ============================
* Partner's Education
* ============================


* Separate out individuals without partner info
preserve
keep if mi(pgpartnr)
tempfile data_intm
save "`data_intm'"
restore

* Generate partner education from pgen
use "$IN/pgen.dta", clear
keep pid pgisced11 syear
rename pid pgpartnr
rename pgisced11 pgisced11_partner
mvdecode _all, mv(-1=. \ -2=.t \ -3=.x \ -5=.y \ -8=.z)
save "$DATA/pgen_partner.dta", replace

* Merge back partner education
use "$DATA/h1_dataset_prep.dta", clear
keep if !mi(pgpartnr)
merge 1:1 pgpartnr syear using "$DATA/pgen_partner.dta", keep(1 3) nogen
append using "`data_intm'"

gen educ_partner = pgisced11_partner if !missing(pgisced11_partner)
replace educ_partner = -7 if pgisced11_partner == .t
replace educ_partner = 98 if pgpartnr == .t

* Keep only observations with children
*keep if numkids > 0

* ============================
* Sample selection
* ============================

* Keep only observations with children & 2018
keep if numkids > 0 & syear == 2018



* ============================
* Descriptive Balance Test
* ============================

stddiff i.male yob i.born_in_east i.mig_background i.f_ed i.m_ed ///
        i.single_parent_upbringing i.educ_resp i.age_at_first_interview pgisced11_partner ///
		if syear == 2018, by(has_daughter_by_year)

* ============================
* Regression Model
* ============================

* Standardize outcome
su plh0308_v2
replace plh0308_v2 = (plh0308_v2 - r(mean)) / r(sd)

* Step 1: Estimate Logit
logit has_daughter_by_year yob i.born_in_east i.mig_background i.f_ed i.m_ed ///
        i.single_parent_upbringing i.educ_resp ///
		age_at_first_interview  i.educ_partner numkids if syear == 2018


* Step2: Predict treatment probabilities		
cap drop pscore	
predict pscore if e(sample) == 1, pr
gen double weight = 1/pscore

*****************************
* Common support
*****************************

* Step 3: Get min and max pscore in treated and control groups
summarize pscore if has_daughter_by_year == 1
local min_treated = r(min)
local max_treated = r(max)

summarize pscore if has_daughter_by_year == 0
local min_control = r(min)
local max_control = r(max)

* Step 4: Define common support bounds
local lower_bound = max(`min_treated', `min_control')
local upper_bound = min(`max_treated', `max_control')

di `lower_bound'
di `upper_bound'

* Step 5: Keep only observations within common support
gen in_support = pscore >= `lower_bound' & pscore <= `upper_bound' if !mi(pscore)
keep if in_support


* OLS regression
reg plh0308_v2 i.has_daughter_by_year i.male yob i.born_in_east i.mig_background ///
              i.f_ed i.m_ed i.single_parent_upbringing i.educ_resp ///
              i.age_at_first_interview numkids pgisced11_partner if syear == 2018 [aweight=weight] , robust

* Extract relevant statistics
local coefficient       = _b[1.has_daughter_by_year]
local se                = _se[1.has_daughter_by_year]
local test_stat_family  = "t" // Adjusted by DA from "one-sided t" in code consistency check
local df_residual       = e(df_r)
local df_model          = e(df_m)
local t                 = _b[1.has_daughter_by_year] / _se[1.has_daughter_by_year]
local p_value           = 2 * ttail(`df_residual', abs(`t'))
local num_obs           = e(N)
quietly levelsof pid if e(sample), local(participants)
local num_participants  : word count `participants'


local interpretation "positive coefficient is in line with hypothesis"

* ============================
* Export Results
* ============================

* Display results
di "Coefficient: `coefficient'"
di "Standard Error: `se'"
di "Test Statistic Family: `test_stat_family'"
di "Residual Degrees of Freedom: `df_residual'"
di "Model Degrees of Freedom: `df_model'"
di "p-value: `p_value'"
di "Number of observations: `num_obs'"
di "Number of participants: `num_participants'"
di "Interpretation of directionality: `interpretation'"

* Export to CSV
file open myfile using "$OUT/h1_results.csv", write replace
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile

********************************************************************************
exit
