********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************
*----------------------------------------------------------
* H3: Effect of Having a Daughter on Household Behavior
*----------------------------------------------------------

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2
*ssc install esttab
*ssc install coefplot
* CONFIGURE FILE PATHS * (Please do not change)
/*
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

	save "$DATA/h3_dataset.dta", replace // save merged dataset

*-----------------------------------------------
* Step 1: Prepare individual-level totals
*-----------------------------------------------

gen age = syear - gebjahr
keep if age >= 18


* Drop invalid values (<0 are missing codes in SOEP data)
foreach var in pli0044_h pli0019_h pli0022_h pli0043_h pli0012_h pli0016_h pli0049_h pli0034_v4 pli0031_h age pgbilzeit hghinc pgemplst pgfamstd migback {
    replace `var' = . if `var' < 0
}


* Drop if any household behavior variable is missing
drop if missing(pli0044_h, pli0019_h, pli0022_h, pli0012_h, pli0043_h, pli0016_h, pli0031_h, pli0034_v4, pli0049_h, age, pgbilzeit, hghinc, pgemplst, pgfamstd, migback)


* household with available partner information
keep if parid >= 0




* Total weekly hours by category
gen childcare = 5*pli0044_h + pli0019_h + pli0022_h //compute weekly hours for childcare
gen chores    = 5*pli0043_h + pli0012_h + pli0016_h //compute weekly hours for household chores
gen repair    = 5*pli0049_h + pli0034_v4 + pli0031_h //compute weekly hours for repair work

gen total_hh_labor = childcare + chores + repair

*-----------------------------------------------
* Step 2: Create treatment variable has_daughter
*-----------------------------------------------

gen has_daughter = 0
foreach var of varlist kidsex01-kidsex15 {
    replace has_daughter = 1 if `var' == 2
}

drop if missing(has_daughter)
//replace has_daughter = 0 if missing(has_daughter)

*-----------------------------------------------
* Step 3: Merge respondent with partner's total_hh_labor
*-----------------------------------------------

* Step 1: Keep only needed person-level variables
preserve
keep cid syear pid sex age pgbilzeit pgemplst migback
tempfile person_vars
save "$DATA/person_vars.dta", replace
restore

* Step 2: Create dataset with partner's total household labor
preserve
keep cid syear pid total_hh_labor
rename pid partner_id
rename total_hh_labor partner_total_hh_labor
tempfile partner_data
save "$DATA/partner_data.dta", replace
restore

* Step 3: Prepare main dataset to join
keep cid syear pid parid sex total_hh_labor hghinc has_daughter
rename parid partner_id


* Step 4: Merge with partner's total_hh_labor
merge m:1 cid syear partner_id using "$DATA/partner_data.dta"

* Step 5: Keep only matches (drop if no partner info)
keep if _merge == 3
drop _merge
tempfile data_joined
save "$DATA/data_joined.dta", replace


* Step 6: Prepare partner attributes to merge *
* Keep only partner attributes from person_vars
use "$DATA/person_vars.dta", clear
rename age partner_age
rename pgbilzeit partner_pgbilzeit
rename pgemplst partner_pgemplst
rename migback partner_migback
rename pid partner_id
tempfile partner_attrs
save "$DATA/partner_attrs.dta", replace

* Now go back to joined dataset
use "$DATA/data_joined.dta", clear

* Merge partner attributes
merge m:1 cid syear partner_id using "$DATA/partner_attrs.dta"
drop if _merge != 3
drop _merge
tempfile data_joined
save "$DATA/data_joined.dta", replace

* Step 7: Merge in respondent (self) attributes   *
* Reload person_vars and rename respondent variables
use "$DATA/person_vars.dta", clear
rename age respondent_age
rename pgbilzeit respondent_pgbilzeit
rename pgemplst respondent_pgemplst
rename migback respondent_migback
* Keep pid for merging (no rename)
tempfile respondent_attrs
save "$DATA/respondent_attrs.dta", replace

* Reload current joined dataset
use "$DATA/data_joined.dta", clear

* Merge respondent attributes by cid syear pid
merge m:1 cid syear pid using "$DATA/respondent_attrs.dta"
drop if _merge != 3
drop _merge
tempfile data_joined
save "$DATA/data_joined.dta", replace

* Fix sex variable

tab sex
tab sex, nolabel
rename sex gender
generate sex=0
replace sex=1 if gender==2



*Step 8: Assign mother and father household labor hours
gen mother_total = .
gen father_total = .

replace mother_total = total_hh_labor if sex == 1
replace mother_total = partner_total_hh_labor if sex == 0

replace father_total = total_hh_labor if sex == 0
replace father_total = partner_total_hh_labor if sex == 1

* Labor gap
gen labor_gap = mother_total - father_total

* Assign mother and father age


gen mother_age = .
gen father_age = .

replace mother_age = respondent_age if sex == 1
replace mother_age = partner_age     if sex == 0

replace father_age = respondent_age if sex == 0
replace father_age = partner_age     if sex == 1

* Assign education
gen mother_edu = .
gen father_edu = .

replace mother_edu = respondent_pgbilzeit if sex == 1
replace mother_edu = partner_pgbilzeit    if sex == 0

replace father_edu = respondent_pgbilzeit if sex == 0
replace father_edu = partner_pgbilzeit    if sex == 1

* Assign employment status
gen mother_empl = .
gen father_empl = .

replace mother_empl = respondent_pgemplst if sex == 1
replace mother_empl = partner_pgemplst    if sex == 0

replace father_empl = respondent_pgemplst if sex == 0
replace father_empl = partner_pgemplst    if sex == 1

* Assign migration background
gen mother_migback = .
gen father_migback = .

replace mother_migback = respondent_migback if sex == 1
replace mother_migback = partner_migback    if sex == 0

replace father_migback = respondent_migback if sex == 0
replace father_migback = partner_migback    if sex == 1

collapse (firstnm) pid partner_id  hghinc  has_daughter partner_total_hh_labor partner_migback partner_pgemplst partner_pgbilzeit partner_age respondent_migback respondent_pgemplst respondent_pgbilzeit respondent_age sex mother_total father_total labor_gap mother_age father_age mother_edu father_edu mother_empl father_empl mother_migback father_migback gender total_hh_labor , by(cid syear)
*-----------------------------------------------
* Step 4: Estimate main model
*-----------------------------------------------
* Main model: year fixed effects, clustered at household (cid) level
reghdfe labor_gap has_daughter ///
    mother_age mother_edu i.mother_empl i.mother_migback ///
    father_age father_edu i.father_empl i.father_migback ///
    hghinc, absorb(syear) cluster(cid) //noconstant

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[has_daughter]

		* (ii) standard error
		local se = _se[has_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[has_daughter] / _se[has_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_has_daughter = _b[has_daughter]
		local interpretation "positive coefficient is in line with hypothesis"

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


********************************************************************************

exit
