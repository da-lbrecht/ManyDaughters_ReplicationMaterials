********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
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
	quietly save "$DATA/h1_dataset.dta", replace 


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

* HYPOTHESIS TEST FOR H1 * 

	* For our identification strategy, we assume a child's gender is essentially random
	* Actually having a child is much more endogenous, so we only include people that have had at least one child
	* The number of children is also endogenous, but for power considerations we control for rather than selecting, for example, parents with only one child
	* we then control for other factors that may plausibly influence the dependent variable:
	* -age and age-squared (for non-linear effect of age)
	* -the respondent's gender
	* -the year of the survey
	* -whether the respondent was born in Germany or not
	* -whether the respondent lives in East/West Germany or not
	* For the dependent variable, we use 'plj0625: position of women: husband should have the final say'.  we find all the plj062X: position of women: variables suitable but selected this one as there appears to be slightly more valid responses for this question
	* we would like to run a panel (xt) for this, but as the independent variable was only elicited in one wave we do not have sufficient variation, so we use a standard approach
	* given the variable is 1 = strongly disagree and 7 = agree fully, support for gender equality would be a negative coefficient on the daughter variable
	* also given the dependent variable is Likert, we use an ordered logit (see Liddell & Kruschke 2018 https://doi.org/10.1016/j.jesp.2018.08.009) 
	
	xtset pid syear
	
	
	ologit plj0625 daughter i.sumkids age age2 i.sex i.syear i.germborn i.sampreg, vce(cluster pid)
	

* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[daughter]

		* (ii) standard error
		local se = _se[daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable (n/a for ologit)
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
		scalar coef_daughter = _b[daughter]
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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
