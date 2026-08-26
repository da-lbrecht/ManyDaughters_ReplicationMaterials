********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

//clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *

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
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA *

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

* FILTERS *
 
	keep if inlist(pop,1,2) // keeping participants from private households
	keep if sexor==0 // keeping hetero individuals
	keep if biovalid==31 // keeping parents (dropping people without children)
	keep if nett1==1 // keeping subjects with successful interview
	keep if syear==2019 // we test this hypothesis in the last available data for which the outcome is available: 2019
	bysort hid (syear): gen byte hid_to_drop = (sumkids[_N] > 3)
	bysort hid (syear): replace hid_to_drop = 1 if (sumkids[_N-1] > 3) // dropping few 	cases in which the two parents give different reports
	drop if hid_to_drop ==1 // drop families that on the last wave report 4 children or more

	
* GENERATE VARIABLES * 

	gen age = 2025-gebjahr
	gen age2 = age^2
	gen number_daughters=0

	foreach i of numlist 1/19 {                // loop over 01 … 19
		local v = "kidsex" + string(`i',"%02.0f")
		quietly replace number_daughters = number_daughters + (`v' == 2)
	}
	
	gen d_daughter = (number_daughters>=1 & number_daughters!=.) // any daughter? conditional on having any children
	
	replace plh0004 = 10-plh0004 if plh0004>=0 // Reverse coding (higher number = left-leaning political attitude)

	
* HYPOTHESIS TEST FOR H2 *

	* The outcome is the the individual's political orientation (plh0004), ranging from value 0 (extreme right-leaning) to value 10 (extreme left-leaning). This outcome being a higher number, on a likert scale from 0 to 10, indicates greater support for left-wing politics.

	* Accordingly, we run a linear OLS regression analysis with plh0004 as dependent variable and the dummy variable d_daughter as independent variable. We control for number of children fixed effects, sex, whether the participant was born in Germany, age and age^2.

	gl add_controls germborn age age2
	reg plh0004 d_daughter i.sumkids sex $add_controls if plh0004>=0, vce(cluster hid)


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
		local interpretation "negative coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di "Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile

********************************************************************************

exit
