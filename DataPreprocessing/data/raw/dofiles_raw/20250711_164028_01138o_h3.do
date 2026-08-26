********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
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

* LOAD AND MERGE DATA, FILTERS, GENERATE VARIABLES * 
	
	* ============================
	* Males
	* ============================
	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	keep pid parid hid syear sex gebjahr migback
	
	* Filter
	egen num_missings = rowmiss(_all)
	keep if num_missings == 0
	keep if pid > 0 & parid > 0 & hid > 0 & syear > 0 & sex > 0 & gebjahr > 0 & migback > 0 
	keep if (sex == 1)
	
	* Generate variables
	gen male_age = syear - gebjahr
	gen male_age2 = male_age*male_age
	gen male_migbackdummy = (migback != 1)
	keep pid parid hid syear male_age male_age2 male_migbackdummy

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex*)
	
	* Filter
	egen num_missings = rowmiss(_all)
	keep if num_missings == 0
	keep if kidsex01 > 0
	gen valid_kidsex = 1
	foreach var of varlist kidsex* {
		replace valid_kidsex = 0 if !inlist(`var', 1, 2, -2)
	}
	keep if valid_kidsex == 1
	
	* Generate variables
	gen firstbornisdaughter = (kidsex01 == 2)
	gen anydaughter = (kidsex01 == 2) | (kidsex02 == 2) | (kidsex03 == 2) | (kidsex04 == 2) | (kidsex05 == 2) | (kidsex06 == 2) | (kidsex07 == 2) | /// 
	(kidsex08 == 2) | (kidsex09 == 2) | (kidsex10 == 2) | (kidsex11 == 2) | (kidsex12 == 2) | (kidsex13 == 2) | (kidsex14 == 2) | ///
	(kidsex15 == 2) | (kidsex16 == 2) | (kidsex17 == 2) | (kidsex18 == 2) | (kidsex19 == 2)
	keep pid parid hid syear male_age male_age2 male_migbackdummy firstbornisdaughter anydaughter
	
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pli0038_h pli0003_h pli0007_h pli0047_v1 pli0024_h pli0028_h /// 
	pli0043_h pli0012_h pli0016_h pli0040 pli0054 pli0011 pli0049_h pli0031_h pli0034_v4 pli0044_h pli0019_h pli0022_h pli0046 pli0055 pli0057) //Use pli0047_v1 instead of pli0047_h because pli0047_h is not available.
	
	* Filter
	egen num_missings = rowmiss(_all)
	keep if num_missings == 0
	keep if syear == 2001 | syear == 2003 | syear == 2005 | syear == 2007 | syear == 2009 | syear == 2011 | syear == 2013 | syear == 2015 | syear == 2017 | syear == 2019
	keep if pli0038_h >= 0 & pli0003_h >= 0 & pli0007_h >= 0 & pli0047_v1 >= 0 & pli0024_h >= 0 & pli0028_h >= 0 & pli0043_h >= 0 & pli0012_h >= 0 & pli0016_h >= 0 & pli0040 >= 0 & pli0054 >= 0 & /// 
	pli0011 >= 0 & pli0049_h >= 0 & pli0031_h >= 0 & pli0034_v4 >= 0 & pli0044_h >= 0 & pli0019_h >= 0 & pli0022_h >= 0 & pli0046 >= 0 & pli0055 >= 0 & pli0057 >= 0
	
	* Generate variables
	gen male_weeklyhours_paidwork = pli0038_h * 5 + pli0003_h + pli0007_h + pli0047_v1 * 5 + pli0024_h + pli0028_h
	gen male_weeklyhours_housework = pli0043_h * 5 + pli0012_h + pli0016_h + pli0040 * 5 + pli0054 + pli0011 + pli0049_h * 5 + pli0031_h + pli0034_v4
	gen male_weeklyhours_family = pli0044_h * 5 + pli0019_h + pli0022_h + pli0046 * 5 + pli0055 + pli0057
	keep pid parid hid syear male_age male_age2 male_migbackdummy firstbornisdaughter anydaughter male_weeklyhours_paidwork male_weeklyhours_housework male_weeklyhours_family
	rename pid pid_male
	rename parid pid_female

	save "$DATA/h3_dataset_males.dta", replace // save merged dataset
	clear 
	
	* ============================
	* Females
	* ============================
	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	keep pid parid hid syear sex gebjahr migback
	
	* Filter
	egen num_missings = rowmiss(_all)
	keep if num_missings == 0
	keep if pid > 0 & parid > 0 & hid > 0 & syear > 0 & sex > 0 & gebjahr > 0 & migback > 0 
	keep if (sex == 2)
	
	* Generate variables
	gen female_age = syear - gebjahr
	gen female_age2 = female_age*female_age
	gen female_migbackdummy = (migback != 1)
	keep pid parid hid syear female_age female_age2 female_migbackdummy

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex*)
	
	* Filter
	egen num_missings = rowmiss(_all)
	keep if num_missings == 0
	keep if kidsex01 > 0
	gen valid_kidsex = 1
	foreach var of varlist kidsex* {
		replace valid_kidsex = 0 if !inlist(`var', 1, 2, -2)
	}
	keep if valid_kidsex == 1
	
	* Generate variables
	gen firstbornisdaughter = (kidsex01 == 2)
	gen anydaughter = (kidsex01 == 2) | (kidsex02 == 2) | (kidsex03 == 2) | (kidsex04 == 2) | (kidsex05 == 2) | (kidsex06 == 2) | (kidsex07 == 2) | /// 
	(kidsex08 == 2) | (kidsex09 == 2) | (kidsex10 == 2) | (kidsex11 == 2) | (kidsex12 == 2) | (kidsex13 == 2) | (kidsex14 == 2) | ///
	(kidsex15 == 2) | (kidsex16 == 2) | (kidsex17 == 2) | (kidsex18 == 2) | (kidsex19 == 2)
	keep pid parid hid syear female_age female_age2 female_migbackdummy firstbornisdaughter anydaughter
	
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pli0038_h pli0003_h pli0007_h pli0047_v1 pli0024_h pli0028_h /// 
	pli0043_h pli0012_h pli0016_h pli0040 pli0054 pli0011 pli0049_h pli0031_h pli0034_v4 pli0044_h pli0019_h pli0022_h pli0046 pli0055 pli0057) //Use pli0047_v1 instead of pli0047_h because pli0047_h is not available.
	
	* Filter
	egen num_missings = rowmiss(_all)
	keep if num_missings == 0
	keep if syear == 2001 | syear == 2003 | syear == 2005 | syear == 2007 | syear == 2009 | syear == 2011 | syear == 2013 | syear == 2015 | syear == 2017 | syear == 2019
	keep if pli0038_h >= 0 & pli0003_h >= 0 & pli0007_h >= 0 & pli0047_v1 >= 0 & pli0024_h >= 0 & pli0028_h >= 0 & pli0043_h >= 0 & pli0012_h >= 0 & pli0016_h >= 0 & pli0040 >= 0 & pli0054 >= 0 & /// 
	pli0011 >= 0 & pli0049_h >= 0 & pli0031_h >= 0 & pli0034_v4 >= 0 & pli0044_h >= 0 & pli0019_h >= 0 & pli0022_h >= 0 & pli0046 >= 0 & pli0055 >= 0 & pli0057 >= 0
	
	* Generate variables
	gen female_weeklyhours_paidwork = pli0038_h * 5 + pli0003_h + pli0007_h + pli0047_v1 * 5 + pli0024_h + pli0028_h
	gen female_weeklyhours_housework = pli0043_h * 5 + pli0012_h + pli0016_h + pli0040 * 5 + pli0054 + pli0011 + pli0049_h * 5 + pli0031_h + pli0034_v4
	gen female_weeklyhours_family = pli0044_h * 5 + pli0019_h + pli0022_h + pli0046 * 5 + pli0055 + pli0057
	keep pid parid hid syear female_age female_age2 female_migbackdummy firstbornisdaughter anydaughter female_weeklyhours_paidwork female_weeklyhours_housework female_weeklyhours_family
	rename pid pid_female
	rename parid pid_male

	save "$DATA/h3_dataset_females.dta", replace // save merged dataset
	
	* ============================
	* Households
	* ============================
	
	merge 1:1 hid pid_male pid_female syear firstbornisdaughter anydaughter using "$DATA/h3_dataset_males.dta", nogen keep(match)
	* Merging by firstbornisdaughter and anydaughter makes sure that male and female have the same values for these variables.
	
	gen timediff = abs(male_weeklyhours_paidwork - female_weeklyhours_paidwork) + abs(male_weeklyhours_housework - female_weeklyhours_housework) + abs(male_weeklyhours_family - female_weeklyhours_family)
	
	label language EN // change language of the labels to English

	save "$DATA/h3_dataset_households.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* HYPOTHESIS TEST FOR H3 * 

	* We run an IV regression with timediff as the dependent variable and anydaughter as the independent variable, with anydaughter being instrumtended by firstbornisdaughter.
	* Controls are male_age, female_age, male_age2, female_age2, male_migbackdummy, female_migbackdummy, and syear fixed effects.

	ivreg2 timediff male_age female_age male_age2 female_age2 male_migbackdummy female_migbackdummy i.syear (anydaughter = firstbornisdaughter), cluster(hid) 


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
		quietly levelsof hid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[anydaughter]
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


********************************************************************************

exit
