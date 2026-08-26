********************************************************************************
****************** Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set more off
*set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2

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

* LOAD AND MERGE DATA & FILTERS *

	
	use pid hid syear sex gebjahr using "$IN/ppathl.dta", clear
	keep if sex > 0
	tempfile ppathl_temp
	save "$IN/ppathl_temp", replace
	
	use pid syear plj0620 using "$IN/pl.dta", clear
	keep if plj0620 >= 0
	merge 1:1 pid syear using "$IN/ppathl_temp", keep(match) nogenerate

* last complete/largest panel wave in our 5% data sample // adapt if necessary
	keep if syear==2016
	
	merge 1:1 pid using "$IN/biobirth.dta", keep(match) nogenerate
	
	merge m:1 hid syear using "$IN/hgen.dta", keep(match) nogenerate keepusing(hghinc)
	keep if hghinc > 0

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================


* GENERATE VARIABLES * (Please adapt according to your needs)

	gen first_child_daughter = kidsex01 == 2
	gen is_man = sex == 1
	gen is_man_first_child_daughter=first_child_daughter*is_man


* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)

 reg plj0620 first_child_daughter is_man is_man_first_child_daughter gebjahr hghinc, cluster(hid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[is_man_first_child_daughter]

		* (ii) standard error
		local se = _se[is_man_first_child_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[is_man_first_child_daughter] / _se[is_man_first_child_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[is_man_first_child_daughter]
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
