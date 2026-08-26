********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
*Set working directory
*cd 
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000


* INSTALL PACKAGES * 
*ssc install outreg2

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
	merge m:m pid using "$IN/biol.dta", nogen keep(master matches)
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches)
	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)
	
merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) 
	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* CHILDREN VARIABLES * 
	
	*Generate the dummy variable for children
	gen d_child=0
	replace d_child=1 if sumkids>0
	
	*Generate dummy variables for childrens.
	forvalues i = 2/19 {
    gen child_`i' = (sumkids == `i')
}
	*Generate the dummy variable for at least one daughter
	gen d_daughter = 0

	foreach var of varlist kidsex01-kidsex19 {
		replace d_daughter = 1 if `var' == 2
		}
	
	*Keep only the adults with at least one children
	keep if d_child==1

* GENERATE CONTROL VARIABLES * 
	*Gender variables
	gen father=0
	replace father=1 if sex==1
	
	*Age
	gen age=syear-gebjahr
	
	*Migration background
	gen direct_mig=0
	replace direct_mig=1 if migback==2
	
	gen indirect_mig=0
	replace indirect_mig=1 if migback==3
	
	*Region of birth
	gen west_region=.
	replace west_region=1 if sampreg==1
	replace west_region=0 if sampreg==2
	
	*Education
	gen education=.
	replace education=pgbilzeit if pgbilzeit>=0
	
* POLITICAL PREFERENCES *

	*Generate political_preferences variable (0 ext. left - 10 ext. right)
	gen political_preferences = .
	replace political_preferences = plh0004 if inlist(syear, 2005, 2009, 2014, 2019) ///
    & inrange(plh0004, 0, 10)
	
* HYPOTHESIS TEST FOR H2 *

	*We will be tested using OLS regressions, with standard errors clustered at the individual level. We will interpret a two-sided p-value <0.05 in the significance test of the coefficient of interest as statistically significant evidence.
	*If having daughters promotes more leftist political preferences, we should observe that the d_daughter regressor is negative.
	*We control for individual level variables.
	*We control for survey wave fixed effects
	
	reg political_preferences d_daughter ///
	child_2 child_3 child_4 child_5 child_6 child_7 child_8 child_9 child_10 ///
    child_11 child_12 child_13 child_14 child_15 child_16 child_17 child_18 child_19 ///
	father age direct_mig indirect_mig west_region education i.syear
	

* EXTRACT AND EXPORT RESULTS * 

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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
