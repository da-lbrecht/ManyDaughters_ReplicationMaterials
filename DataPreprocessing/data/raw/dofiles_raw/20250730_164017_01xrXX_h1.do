********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Author: RT 01xrXX
* (adapted from the framework provided by the organisers and written by Tina Wang, David Albrecht)

* ============================
* Setup * 
* ============================
* clear all 
version 19 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES 
ssc install outreg2
ssc install ftools, replace
ssc install moremata, replace
ssc install reghdfe


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

* LOAD AND MERGE DATA 

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets

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



* ============================
* Analysis
* ============================

* FILTERS * 
 
	* Keep only participants who have children
	keep if sumkids>0


* GENERATE VARIABLES * 

	* generate dummy variable for whether participant has at least one daughter (1) or not (0)
	egen num_daughters = anycount(kidsex01-kidsex19), values(2)
	gen d_daughter = (num_daughters > 0) if num_daughters!=.
	
	* recode the sex variable for ease of interpretation
	gen male = 1- (sex - 1)

	* clean & standardize variable measuring support for gender equality 
	* check the variable definition
	des plj0625
	* missing values should be coded as such
	replace plj0625 = . if plj0625 < 0
	* standardize for better interpretation, and reverse sign (so that higher values means more support for gender equality)
	egen std_plj0625 = std(plj0625)
	gen gender_equality_support = - std_plj0625
	
	
* HYPOTHESIS TEST FOR H1 * 

	* I run a OLS regression with a variable measuring support for gender equality as the dependent variable and an indicator for having at least one daughter as the dependent variable. The sample is restricted to individuals who have children, because I think they are a better comparison (people with and without children tend to be different, while sex at birth conditional on having a child is randomly assigned)
	
	reghdfe gender_equality_support d_daughter, absorb(sumkids gebjahr syear corigin male) vce(cluster pid syear)


* EXTRACT AND EXPORT RESULTS 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = _se[d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter] / _se[d_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
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
