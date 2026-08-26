********************************************************************************
********************** Do-File H3: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

/* Deviations from PAP:

We recognized that the method specified in our pre-analysis plan (PAP) was not sufficiently robust for replication.
Therefore, we chose to rely on two-way fixed effects (TWFE) models estimated via standard OLS regressions using "reghdfe".

According to our PAP specification, there was no remaining variation in treatment.
To address this, we adjusted the number of pre- and post-treatment periods. 
To avoid concerns of cherry-picking, we did not impose a fixed number of pre- or post-treatment periods.

*/


* ============================
* Setup
* ============================

//clear all
version 18                          // Specify Stata version
set maxvar 10000                    // Allow for many variables

* Install required packages
foreach pack in outreg2 stddiff csdid {
    ssc install `pack', replace
}

* Set working directory manually (adjust as needed)
// cd "N:/MA/iweyland/#ManyDaughters_RT_AnalysisPackage/#ManyDaughters_RT_AnalysisPackage/code" // Commented out by David Albrecht on 15. July 2025

/*
* Set global paths based on file structure
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

* Merge in relevant SOEP datasets
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master match)
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master match)
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master match)
merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master match) keepusing(hghinc)
merge m:1 pid using "$IN/bioparen.dta", nogen keep(master match)

* Preprocess biol.dta to ensure one obs per pid
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

* Set label language to English
label language EN

* Recode missing values
mvdecode _all, mv(-1=. \ -2=.t \ -3=.x \ -5=.y \ -8=.z)

* Save cleaned dataset
save "$DATA/h3_dataset.dta", replace
use "$DATA/h3_dataset.dta", clear

* ============================
* Analysis
* ============================

xtset pid syear

* Apply filtering: private households only
keep if inrange(netto, 10, 19)
keep if inlist(pop, 1, 2)

* Create dummy variables for having a daughter by year
forval x = 1/9 {
    gen daughter0`x' = (kidsex0`x' == 2 & syear >= kidgeb0`x') if !missing(kidsex0`x') & !missing(kidgeb0`x')
    replace daughter0`x' = 0 if missing(daughter0`x')
}
forval x = 10/19 {
    gen daughter`x' = (kidsex`x' == 2 & syear >= kidgeb`x') if !missing(kidsex`x') & !missing(kidgeb`x')
    replace daughter`x' = 0 if missing(daughter`x')
}

* Create indicator for at least one daughter in household
gen has_daughter_by_year = 0
forval x = 1/9 {
    replace has_daughter_by_year = 1 if daughter0`x' == 1
}

* Count number of children present
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

gen male                    = (sex == 1) if inlist(sex, 1, 2)
gen yob                     = gebjahr
gen born_in_east           = (birthregion_ew == 21) if !missing(birthregion_ew)
replace born_in_east       = 0 if birthregion_ew == .t
gen mig_background         = migback if inlist(migback, 1, 2, 3)
gen f_ed                   = fsedu if !missing(fsedu)
gen m_ed                   = msedu if !missing(msedu)
gen single_parent_upbringing = (lb0066 < 15) if !missing(lb0066)

gen educ_resp              = pgisced11 if !missing(pgisced11)
replace educ_resp          = -7 if pgisced11 == .t
bysort pid: egen max_educ = max(pgisced11)
replace educ_resp          = max_educ

bysort pid: egen syear_first = min(syear)
gen age_at_first_interview = syear_first - gebjahr if !missing(syear_first) & !missing(gebjahr)

* Save prepared data
save "$DATA/h3_dataset_prep.dta", replace
use "$DATA/h3_dataset_prep.dta", clear

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
use "$DATA/h3_dataset_prep.dta", clear
keep if !mi(pgpartnr)
merge 1:1 pgpartnr syear using "$DATA/pgen_partner.dta", keep(1 3) nogen
append using "`data_intm'"

gen educ_partner = pgisced11_partner if !missing(pgisced11_partner)
replace educ_partner = -7 if pgisced11_partner == .t
replace educ_partner = 98 if pgpartnr == .t

* ============================
* housework inequality
* ============================

* Separate out individuals without partner info
preserve
keep if mi(pgpartnr)
tempfile data_intm
save "`data_intm'"
restore

* Generate partner education from pgen
preserve
use pid syear pli0043_h using "$IN/pl.dta", clear
rename pid pgpartnr
rename pli0043_h pli0043_h_partner
mvdecode _all, mv(-1=. \ -2=.t \ -3=.x \ -5=.y \ -8=.z)
save "$DATA/hw_partner.dta", replace
restore

* Merge back partner education
keep if !mi(pgpartnr)
merge 1:1 pgpartnr syear using "$DATA/hw_partner.dta", keep(1 3) nogen
append using "`data_intm'"

gen share = pli0043_h/(pli0043_h_partner + pli0043_h)
gen hw_ineq = abs(0.5-share)

* Keep only observations with children
*keep if numkids > 0

* ============================
* Descriptive Comparison
* ============================

stddiff  yob i.born_in_east i.mig_background i.f_ed i.m_ed ///
        i.single_parent_upbringing i.educ_resp i.age_at_first_interview ///
        if syear == 2018 & male == 0, by(has_daughter_by_year)
		
stddiff  yob i.born_in_east i.mig_background i.f_ed i.m_ed ///
        i.single_parent_upbringing i.educ_resp i.age_at_first_interview ///
        if syear == 2018 & male == 1, by(has_daughter_by_year)
		
		


* ============================
* Sample selecton
* ============================
drop if missing(hw_ineq) | missing(numkids) | mi(has_daughter_by_year)
keep if inrange(syear, 2000, 2019)

save "$DATA/h3_dataset_fin.dta", replace

* ============================
* Main Regression
* ============================

use "$DATA/h3_dataset_fin.dta", clear


egen gvar = min(cond(has_daughter_by_year==1 , syear , .)), by(pid)


g time_to_treat = syear - gvar
replace time_to_treat = 0 if missing(gvar)
* this will determine the difference
* btw controls and treated states
g treat = !missing(gvar)

* Stata won't allow factors with negative values, so let's shift
* time-to-treat to start at 0, keeping track of where the true -1 is
summ time_to_treat
g shifted_ttt = time_to_treat - r(min)
*drop if (shifted_ttt < 15 | shifted_ttt > 22) & !mi(shifted_ttt)
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* Enforce restritciton on # of pre- and post-treatment
sum shifted_ttt if time_to_treat == 0
local up = r(mean) + 3
local low = r(mean) -3

keep if inrange(shifted_ttt, `low', `up')
* Regress on our interaction terms with FEs for group and year,
* clustering at the group (state) level
* use ib# to specify our reference group
reghdfe hw_ineq ib`true_neg1'.shifted_ttt numkids, a(pid syear) vce(cluster pid)


/*
Identification: Pre-periods (15 to 17) are each not different from zero, therefore,
all three will be also jointly equal to zero.
*/


reghdfe hw_ineq i.has_daughter_by_year numkids, absorb(pid syear) vce(cluster pid)


* One sided t-test
test _b[1.has_daughter_by_year] = 0
local dfree= r(df)
local t = sqrt(r(F))
local sign_wgt = sign(_b[1.has_daughter_by_year])
local p_value =1-ttail(r(df_r),`sign_wgt'*sqrt(r(F)))*2  // Adjusted by DA to two-sided p-value

reghdfe hw_ineq i.has_daughter_by_year numkids, absorb(pid syear) vce(cluster pid)


	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.has_daughter_by_year]

		* (ii) standard error
		local se = _se[1.has_daughter_by_year]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		//local test_stat_family = "one sided t-test (H1: \beta < 0)"
		local test_stat_family  = "t" // Adjusted by DA from "one-sided t" in code consistency check

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value

		*Calculated above
		
		* (vi) number of observations
		local num_obs           = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local interpretation "negative coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile