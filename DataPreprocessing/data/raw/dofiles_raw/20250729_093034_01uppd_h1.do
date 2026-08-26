********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************
*-----------------------------------------------
* H1: Effect of Having a Daughter on Gender Equality Attitudes
*-----------------------------------------------

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2
*ssc install esttab
*ssc install coefplot

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
* Dataset Preparation * 
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

	save "$DATA/h1_dataset.dta", replace // save merged dataset

*-----------------------------------------------
* Prepare data: filter and generate age
*-----------------------------------------------

* Calculate age
gen age = syear - gebjahr

* Keep only 2018 respondents age 18+
keep if age >= 18 & syear == 2018

* Drop observations with missing values on key attitude items and family status
drop if missing(plh0357, plh0298_v2, plh0302_v2, plh0308_v2, plh0359, plh0360, age, pgbilzeit, hghinc, pgemplst, pgfamstd, migback)

* Drop invalid values (e.g., -1, -2, etc. from SOEP data)
foreach var in plh0357 plh0298_v2 plh0302_v2 plh0308_v2 plh0359 plh0360 age pgbilzeit hghinc pgemplst pgfamstd migback {
    drop if `var' < 0
}


* Reverse code items
gen rc_plh0298 = 8 - plh0298_v2 //invert variable Children under 6 statement
gen rc_plh0302 = 8 - plh0302_v2 //invert variable Children under 3 statement

*-----------------------------------------------
* Compute Cronbach's alpha and create scale
*-----------------------------------------------
//This part of the code is for internal consistency of our analysis. You can comment out this part if necessary.
//Support for gender equality attitudes
alpha plh0357 rc_plh0298 rc_plh0302 plh0308_v2 plh0359 plh0360
// It turned out that Cronbach's Alpha value for our six-item scale is 0.75.
// We, therefore, retain all the items for our mean attitude score. 

*compute the respondent's mean gender attitude scores
gen attitude_mean = (plh0357 + rc_plh0298 + rc_plh0302 + plh0308_v2 + plh0359 + plh0360) / 6


*-----------------------------------------------
* Create treatment and controls
*-----------------------------------------------
* Indicator for having at least one daughter
gen has_daughter = 0
foreach var of varlist kidsex01-kidsex15 {
    replace has_daughter = 1 if `var' == 2  // 2 = female
}

* Handle missing values in controls
gen age_imp = age
replace age_imp = . if missing(age)

gen edu = pgbilzeit
gen hh_income = hghinc
gen employment = pgemplst
gen marital = pgfamstd
gen migration = migback

*-----------------------------------------------
* Estimate model (OLS with robust SEs)
*-----------------------------------------------
///regress attitude_mean has_daughter, vce(robust)

*model with controls
regress attitude_mean has_daughter age_imp edu hh_income i.employment i.marital i.migration, vce(robust)


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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
