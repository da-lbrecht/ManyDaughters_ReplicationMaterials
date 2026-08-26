********************************************************************************
********************** Do-File H4: #ManyDaughters Project **********************
********************************************************************************

* Authors: Tina Wang, David Albrecht, RT 01klXX

* ============================
* Setup *
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *

* CONFIGURE FILE PATHS *
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

* LOAD AND MERGE DATA *

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets *

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches)
	* biol contains data from biography questionnaires
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches)
	* hl contains data from household questionnaires
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches)
	* bioparen contains generated biographical information
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches)

	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS *
 
	* only keep participants with kids
	keep if sumkids > 0 & !missing(sumkids)
	* only keep participants with kids in survey year
	keep if kidgeb01 < syear & kidgeb01 > 0
	* only keep participants with information on sex of first child
	keep if kidsex01 > 0


* GENERATE VARIABLES *

	* generate variable for number of kids in survey year
	gen n_kids = 1
	replace n_kids = 2 if kidgeb02 < syear & kidgeb02 > 0
	replace n_kids = 3 if kidgeb03 < syear & kidgeb03 > 0
	replace n_kids = 4 if kidgeb04 < syear & kidgeb04 > 0
	replace n_kids = 5 if kidgeb05 < syear & kidgeb05 > 0
	replace n_kids = 6 if kidgeb06 < syear & kidgeb06 > 0
	replace n_kids = 7 if kidgeb07 < syear & kidgeb07 > 0
	replace n_kids = 8 if kidgeb08 < syear & kidgeb08 > 0
	replace n_kids = 9 if kidgeb09 < syear & kidgeb09 > 0
	replace n_kids = 10 if kidgeb10 < syear & kidgeb10 > 0
	replace n_kids = 11 if kidgeb11 < syear & kidgeb11 > 0
	replace n_kids = 12 if kidgeb12 < syear & kidgeb12 > 0
	replace n_kids = 13 if kidgeb13 < syear & kidgeb13 > 0
	replace n_kids = 14 if kidgeb14 < syear & kidgeb14 > 0
	replace n_kids = 15 if kidgeb15 < syear & kidgeb15 > 0
	replace n_kids = 16 if kidgeb16 < syear & kidgeb16 > 0
	replace n_kids = 17 if kidgeb17 < syear & kidgeb17 > 0
	replace n_kids = 18 if kidgeb18 < syear & kidgeb18 > 0
	replace n_kids = 19 if kidgeb19 < syear & kidgeb19 > 0
	label variable n_kids "Number of kids in survey year"

	* generate dummy variable for whether participant has at least one daughter (1) or not (0)
	gen d_daughter = 0
	replace d_daughter = 1 if (kidsex01 == 2 & n_kids >= 1) | (kidsex02 == 2 & n_kids >= 2) | (kidsex03 == 2 & n_kids >= 3) | (kidsex04 == 2 & n_kids >= 4) | (kidsex05 == 2 & n_kids >= 5) | (kidsex06 == 2 & n_kids >= 6) | (kidsex07 == 2 & n_kids >= 7) | (kidsex08 == 2 & n_kids >= 8) | (kidsex09 == 2 & n_kids >= 9) | (kidsex10 == 2 & n_kids >= 10) | (kidsex11 == 2 & n_kids >= 11) | (kidsex12 == 2 & n_kids >= 12) | (kidsex13 == 2 & n_kids >= 13) | (kidsex14 == 2 & n_kids >= 14) | (kidsex15 == 2 & n_kids >= 15) | (kidsex16 == 2 & n_kids >= 16) | (kidsex17 == 2 & n_kids >= 17) | (kidsex18 == 2 & n_kids >= 18) | (kidsex19 == 2 & n_kids >= 19)
	label variable d_daughter "Dummy variable for whether participant has at least one daughter (1) or not (0)"

	* generate dummy variable for female (0) or male (1) gender
	gen d_sex = 0 if sex == 2
	replace d_sex = 1 if sex == 1
	label variable d_sex "Dummy variable for female (0) or male (1) gender"

	* generate variable for age in survey year
	gen age = syear - gebjahr
	label variable age "Age in survey year"

	* generate variable for support for gender equality (as measured by attitudes)
	gen support_gender_equality = 1 if lb1120 == 1
	replace support_gender_equality = 2/3 if lb1120 == 2
	replace support_gender_equality = 1/3 if lb1120 == 3
	replace support_gender_equality = 0 if lb1120 == 4
	label variable support_gender_equality "Support for gender equality"


* HYPOTHESIS TEST FOR H4 *

	* linear OLS regression with interaction term

	* as measure of support for gender equality, we use the question "Fam: Man/Women Equal in Household/Family and Job" (lb1120); if agreeing to this statement, we assume the respondent is more likely to support gender equality
	* we run a linear OLS regression analysis with lb1120, recoded to range between 0 (completely disagree) and 1 (agree completely), as dependent variable, and the dummy variable d_daughter interacted with gender (d_sex) as independent variable (hypothesis 4), controlling for d_daughter, d_sex, number of kids (n_kids), age (age), and survey year (syear) fixed effects, with robust standard errors clustered at the household (hid) level

	reg support_gender_equality i.d_daughter##i.d_sex n_kids age i.syear, cluster(hid)


* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.d_daughter#1.d_sex]

		* (ii) standard error
		local se = _se[1.d_daughter#1.d_sex]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.d_daughter#1.d_sex] / _se[1.d_daughter#1.d_sex]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter_d_sex = _b[1.d_daughter#1.d_sex]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
