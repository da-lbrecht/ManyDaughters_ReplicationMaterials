********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
*** Having daughters leads to more left-leaning political preferences
********************************************************************************

* Authors: 

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 15.1 
set maxvar 10000 

* INSTALL PACKAGES * 
* install all packages you need here, for example:
/*
ssc install drdid
ssc install csdid
ssc install outreg2

// cd "/Users/[REDACTED]/Documents/#ManyDaughters_RT_AnalysisPackage/code" //commented by David Albrecht on 07.07.2025

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
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

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

save "$DATA/h2_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* use "$DATA/h2_dataset.dta", replace
*--------------------------------------------
* GENERATE DEPENDENT VARIABLES * 
*--------------------------------------------

** plh0004: political attidue (available in years 2009, 2014, 2019)

gen left_leaning = .
replace left_leaning = 10 - plh0004 if inrange(plh0004, 0, 10)

*--------------------------------------------
* GENERATE INDEPENDENT VARIABLES: Having Daughters
*--------------------------------------------
* 1. Find the year of first daughter for each individual
gen first_daughter_year = .

foreach i of numlist 1/19 {
    local j : display %02.0f `i'
    quietly replace first_daughter_year = kidgeb`j' if ///
        kidsex`j' == 2 & ///
        inrange(kidgeb`j', 1900, 2025) & ///
        (missing(first_daughter_year) | kidgeb`j' < first_daughter_year)
}

* 2. Treatment indicator: 1 if ever had a daughter, 0 otherwise
gen treated = !missing(first_daughter_year)

* 3. years at or after first daughter
gen post = syear >= first_daughter_year if treated == 1
replace post = 0 if treated == 0

* 4. interaction term treat_post
gen treat_post = treated * post

*--------------------------------------------
* GENERATE ADDITIONAL CONTROLS
*--------------------------------------------

* Education: Replace negative codes with missing 
gen edu_years = pgbilzeit
replace edu_years = . if inlist(edu_years, -1, -2)

* Employment status

gen employment_status = .

* Assign values
replace employment_status = 1 if pgemplst == 1   // Full-time
replace employment_status = 2 if pgemplst == 2   // Regular part-time
replace employment_status = 3 if pgemplst == 3   // Vocational training
replace employment_status = 4 if pgemplst == 4   // Irregular part-time
replace employment_status = 5 if pgemplst == 5   // Not employed
replace employment_status = 6 if pgemplst == 6   // Sheltered workshop

* Handle invalid/missing codes
replace employment_status = . if inlist(pgemplst, -2)

* Add labels
label define empstat_lbl ///
    1 "Full-time" ///
    2 "Part-time (regular)" ///
    3 "Vocational training" ///
    4 "Part-time (irregular)" ///
    5 "Not employed" ///
    6 "Sheltered workshop"
	
*--------------------------------------------
* I estimate a two-way fixed effects linear regression:

* plh0004 political attidue is used to measure left-leaning political preferences, which is the dependent variable in hypothesis 2 (H2).

* Independent variables: 
* A dummy variable indicating if an indivdiual has had a daugther at or before the survey year (syear)
* I control for time trends (via i.syear) and individual covariates (edu_years and employment_status).
*--------------------------------------------

xtset pid syear
xtreg left_leaning treat_post i.syear edu_years employment_status, fe cluster(pid)

	
* EXTRACT AND EXPORT RESULTS * 

* (i) Coefficient estimate
local coefficient = _b[treat_post]

* (ii) Standard error
local se = _se[treat_post]

* (iii) Family of the test statistic
local test_stat_family = "t"

* (iv) Residual and model degrees of freedom
local df_residual = e(df_r)
local df_model = e(df_m)

* (v) p-value
local t_stat = _b[treat_post] / _se[treat_post]
local p_value = 2 * ttail(`df_residual', abs(`t_stat'))

* (vi) Number of observations
local num_obs = e(N)

* (vii) Number of participants (clusters)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'

* (viii) Interpretation of significance level and directionality
local interpretation "positive coefficient is in line with hypothesis"


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

* (ix) Export results to CSV
file open myfile using "$OUT/h2_results.csv", write replace
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile

exit
