********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]
* Research team: 01138o

* ============================
* Setup * 
* ============================

* clear all
version 19.5 
set maxvar 10000 

* INSTALL PACKAGES * 
* ssc install outreg2
* ssc install ivreg2

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

* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	keep pid hid syear sex gebjahr migback

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex*)
	
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0308_v2)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* FILTERS * 

	egen num_missings = rowmiss(_all)
	keep if num_missings == 0
	keep if pid > 0 & hid > 0 & syear > 0 & sex > 0 & gebjahr > 0 & migback > 0 
	
	keep if kidsex01 > 0
	
	gen valid_kidsex = 1
	foreach var of varlist kidsex* {
		replace valid_kidsex = 0 if !inlist(`var', 1, 2, -2)
	}
	keep if valid_kidsex == 1
	
	keep if plh0308_v2 > 0


* GENERATE VARIABLES * 

	gen male = (sex == 1)
	gen age = syear - gebjahr
	gen age2 = age*age
	gen migbackdummy = (migback != 1)
	
	gen firstbornisdaughter = (kidsex01 == 2)
	gen anydaughter = (kidsex01 == 2) | (kidsex02 == 2) | (kidsex03 == 2) | (kidsex04 == 2) | (kidsex05 == 2) | (kidsex06 == 2) | (kidsex07 == 2) | /// 
	(kidsex08 == 2) | (kidsex09 == 2) | (kidsex10 == 2) | (kidsex11 == 2) | (kidsex12 == 2) | (kidsex13 == 2) | (kidsex14 == 2) | ///
	(kidsex15 == 2) | (kidsex16 == 2) | (kidsex17 == 2) | (kidsex18 == 2) | (kidsex19 == 2)
	
	keep pid hid syear male age age2 migbackdummy firstbornisdaughter anydaughter plh0308_v2
	
	
* HYPOTHESIS TEST FOR H1 * 

	* We run an IV regression with plh0308_v2 as the dependent variable and anydaughter as the independent variable, with anydaughter being instrumtended by firstbornisdaughter.
	* Controls are male, age, age2, and migbackdummy.

	ivreg2 plh0308_v2 male age age2 migbackdummy (anydaughter = firstbornisdaughter), cluster(hid) 


* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[anydaughter]

		* (ii) standard error
		local se = _se[anydaughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_model = e(df_m)
		local df_residual = e(N) - e(df_m) - 1
		
		* (v) p-value
		local p_value = _b[anydaughter] / _se[anydaughter]
		local p_value = 2 * ttail((e(N) - e(df_m) - 1), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[anydaughter]
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
