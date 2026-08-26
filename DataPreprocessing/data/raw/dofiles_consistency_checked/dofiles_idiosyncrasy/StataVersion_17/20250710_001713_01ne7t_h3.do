********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: RT 01ne7t

* ============================
* Setup 
* ============================

* clear all
version 17 
set maxvar 10000 

* INSTALL PACKAGES * 
// no packages used

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
	quietly use "$IN/ppathl.dta", clear
	quietly merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	quietly merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	quietly merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	quietly merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)
	quietly label language EN 
	quietly save "$DATA/h3_dataset.dta", replace 


* ============================
* Analysis
* ============================

* Filters * 

	* this bit of code will set negative values to missing
	quietly ds, has(type numeric)
	foreach var in `r(varlist)' {
		quietly replace `var' = . if `var' < 0
	}
	
	* drop people that have never had a kid
	quietly gen had_kid = 0
	local maxkids = 19
	forvalues i = 1/`maxkids' {
		local idx = string(`i', "%02.0f")
		quietly replace had_kid = 1 if !missing(kidsex`idx')
	}
	quietly egen ever_had_kid = max(had_kid), by(pid)
	quietly gen never_had_kid = (ever_had_kid == 0 | missing(ever_had_kid))	
	quietly drop if never_had_kid == 1


* GENERATE VARIABLES * 

	* a daughter dummy variable that is 0 in the years before their first daughter and 1 afterwards
	quietly gen first_daughter_year = .
	quietly gen daughter = 0
	local maxkids = 19  
	forvalues i = 1/`maxkids' {
		local sex = string(`i', "%02.0f")
		quietly replace first_daughter_year = kidgeb`sex' ///
		if missing(first_daughter_year) & (kidsex`sex' == 2)
			quietly replace daughter = 1 if (kidsex`sex' == 2) & daughter == 0
	}
	
	* age and age-squared (for non-linear effect)
	quietly gen age = syear - gebjahr
	quietly gen age2 = age*age
	
	* as we are specifically interested in households, we limit our sample further to those that live in the same household 
	preserve
		keep pid hid syear
		rename pid parid
		rename hid par_hid
		duplicates drop parid syear, force
		tempfile partner_hhid
		save `partner_hhid'
	restore
	* Step 2: Merge partner's household ID into main dataset
	merge m:1 parid syear using `partner_hhid', keep(master match) nogen
	* Step 3: Generate indicator for living with partner
	gen lives_with_partner = (par_hid == hid) if !missing(parid)
	replace lives_with_partner = 0 if missing(parid)
 	drop if lives_with_partner == 0
	
	* also looking for opposite sex couples, as this is about gender equality
	preserve
		keep pid syear sex
		rename pid parid
		rename sex partner_sex
		duplicates drop parid syear, force
		tempfile partner_sex
		save `partner_sex'
	restore
	merge m:1 parid syear using `partner_sex', keep(master match) nogen
	gen same_sex_partner = (sex == partner_sex) if !missing(sex) & !missing(partner_sex)
	drop if same_sex_partner == 1
	
	
	* now we create a variable on the ratio of housework per day for the couple
	preserve
		keep pid pli0043_h syear
		rename pid parid
		rename pli0043_h par_pli0043_h
		duplicates drop parid syear, force
		tempfile hw_per_day
		save `hw_per_day'
	restore
	merge m:1 parid syear using `hw_per_day', keep(master match) nogen
	
	
	* this variable has clearly erroneous values (e.g. sometimes people report 24 hours)
	* so we drop if greater than 8 hours per day (99th percentile)
	drop if pli0043_h > 8
	
	gen pli0043_prop = pli0043_h / (pli0043_h+par_pli0043_h) if !missing(pli0043_h, par_pli0043_h)
	
	* gender equality implies this proportion should become closer to 0.5, so take absolute value of difference to 0.5
	gen pli0043_abs = abs(0.5-pli0043_prop)

	* now to have only one partner from the couple in the dataset, drop all men
	drop if sex == 1
	
	
* HYPOTHESIS TEST FOR H3 * 

	* For our identification strategy, we assume a child's gender is essentially random
	* Actually having a child is much more endogenous, so we only include people that have had at least one child
	* The number of children is also endogenous, but for power considerations we control for rather than selecting, for example, parents with only one child
	* we then control for other factors that may plausibly influence the dependent variable:
	* -age and age-squared (for non-linear effect of age)
	* -the year of the survey
	* -whether the respondent was born in Germany or not
	* -whether the respondent lives in East/West Germany or not
	* We focus on the pli0043_h variable - the self-reported hours per day spent on housework.  We take the ratio of that variable for mixed-gender couples that live in the same household.  Gender equality means this variable should be close to 0.5, in that housework is shared equally.  Given how the variable is set up (absolute value of difference from 0.5), this means that more gender equal behavior means the variable should be decreasing (in that it is getting closer to 0.5)
	
	xtset pid syear
	
	xtreg pli0043_abs daughter i.sumkids age age2 i.syear i.germborn i.sampreg, vce(cluster pid)
	

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[daughter]

		* (ii) standard error
		local se = _se[daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local z = _b[daughter] / _se[daughter]
		local p_value = 2 * normal(-abs(`z'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[daughter]
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
