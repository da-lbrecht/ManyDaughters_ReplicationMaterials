*************************************************************************
******************  Do-File H1: #ManyDaughters Project ******************
*************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000
/*
* CONFIGURE FILE PATHS * (Please do not change)
* the following code automatically sets paths based on where you saved the distributed file-package on your system *


if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}


*global ROOT "/Users/nan/Desktop/#ManyDaughters_RT_AnalysisPackage"  //comment out when submitting the do file.

global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for datasets
global OUT "$ROOT/out" // folder for output (results, tables)
*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
		keepusing(plh0298_v2 plh0302_v2 plh0308_v2)
		
		
	/* 
	NOTE: we dropped the variables asked in 2010-2013 from pl and biol, as these were only administered to samples L1-L3
	As requested, we also now combine the remaining outcome variables into a single index.
	We have changed our code such that we now produce a single estimate with this index, rather than a series of regression estimates using all of the separate DVs.
		
		keepusing(plh0298_v1 plh0298_v2 plh0302_v1 plh0302_v2 plh0308_v1 plh0308_v2 ///
				  plh0301 plh0303 plh0307 plh0309)

	* biol questions
	* merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches) ///
		* keepusing(lb1110 lb1114 lb1120 lb1113 lb1115 lb1119 lb1121)
	*/

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* dropping the refugee samples as stated in the PAP
	drop if psample == 17 | psample == 18 | psample == 19


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Taking care of missing values
	foreach var of varlist gebjahr sex kidgeb01-kidsex19 plh0298_v2-plh0308_v2 {
		replace `var' = . if `var' < 0
	}
	
	* Total number of kids in 2018 (the year that outcomes are collected)
	egen numkids2018 = anycount(kidgeb01-kidgeb19), values(1900 / 2018)
	
	gen numkids = . 
	replace numkids = numkids2018 if syear == 2018 & numkids2018 != 0
	
	
	* Generate variables for number of GIRLS at time of interview
	forvalues i = 1 / 9 {		
		replace kidgeb0`i' = . if kidsex0`i' != 2
	}
	
	forvalues i = 0 / 9 {		
		replace kidgeb1`i' = . if kidsex1`i' != 2
	}
	
	egen numgirls2018 = anycount(kidgeb01-kidgeb19), values(1900 / 2018)

	* Generate dummy variables for whether participant has at least one daughter (1) or not (0) in 2018
	gen ndaughters = .
	
	replace ndaughters = numgirls2018 if syear == 2018 & numkids2018 > 0 & numkids2018 <= 19
	
	gen d_daughter = .
	replace d_daughter = 0 if ndaughters == 0
	replace d_daughter = 1 if ndaughters > 0 & ndaughters <= 19
	
	
	
	* Creating the outcome index
	gen childsuffer6_7pt = plh0298_v2
	recode childsuffer6_7pt (1 = 7) (2 = 6) (3 = 5) (5 = 3) (6 = 2) (7 = 1)
	qui su childsuffer6_7pt if d_daughter == 0
	local mu = `r(mean)'
	local sd = `r(sd)'
	replace childsuffer6_7pt = (childsuffer6_7pt - `mu') / `sd'
	
	gen childsuffer3_7pt = plh0302_v2
	recode childsuffer3_7pt (1 = 7) (2 = 6) (3 = 5) (5 = 3) (6 = 2) (7 = 1)	
	qui su childsuffer3_7pt if d_daughter == 0
	local mu = `r(mean)'
	local sd = `r(sd)'
	replace childsuffer3_7pt = (childsuffer3_7pt - `mu') / `sd'
	
	
	gen shareequally_7pt = plh0308_v2
	qui su shareequally_7pt if d_daughter == 0
	local mu = `r(mean)'
	local sd = `r(sd)'
	replace shareequally_7pt = (shareequally_7pt - `mu') / `sd'
	
	gen I = (childsuffer6_7pt + childsuffer3_7pt + shareequally_7pt) / 3
	 
	
	
	/* 
	NOTE: here is the original loop and outcome creation code:
	
	foreach var of varlist gebjahr sex kidgeb01-kidsex19 plh0298_v1-lb1121 {
		replace `var' = . if `var' < 0
	}
	
	* Combining outcome variables asked in pl and biol
	* Outcomes are only asked in 2010,2011,2012,2013 (to psamples L1,L2 and L3) and 2018
	* Note that in a handful (N=14) of cases in 2012, identical answers seem to have been provided in both pl and biol
	* Variables are recoded such that higher values indicate greater support for gender equality
	
		* children under 6 suffer if  mom works
		egen childsuffer6_4pt = rowmax(plh0298_v1 lb1110)
	
		gen childsuffer6_7pt = plh0298_v2
		recode childsuffer6_7pt (1 = 7) (2 = 6) (3 = 5) (5 = 3) (6 = 2) (7 = 1)
	
	
		* children under 3 suffer if mom works
		egen childsuffer3_4pt = rowmax(plh0302_v1 lb1114)
	
		gen childsuffer3_7pt = plh0302_v2
		recode childsuffer3_7pt (1 = 7) (2 = 6) (3 = 5) (5 = 3) (6 = 2) (7 = 1)	
	
	
		* best if man and woman work same amount so they can share the responsibility for family and household equally
		egen shareequally_4pt = rowmax(lb1120 plh0308_v1)
		recode shareequally_4pt (1 = 4) (2 = 3) (3 = 2) (4 = 1)
	
		gen shareequally_7pt = plh0308_v2
		
		
		* Women Should Rather Care About Family Than Career
		egen familyovercareer = rowmax(plh0301 lb1113)
		
		
		* Men should be involved in housework
		egen malehousework = rowmax(plh0303 lb1115)
		recode malehousework (1 = 4) (2 = 3) (3 = 2) (4 = 1)

		
		* Men Care more About Financial Security than Women
		egen mencarefinance = rowmax(lb1119 plh0307)
		
		
		* Working Mothers are Equally Emotionally Warm
		egen momwarmth = rowmax(lb1121 plh0309)
		recode momwarmth (1 = 4) (2 = 3) (3 = 2) (4 = 1)
	
	
	* Generate variable on total number of kids at time of interview
	egen numkids2010 = anycount(kidgeb01-kidgeb19), values(1900 / 2010)
	egen numkids2011 = anycount(kidgeb01-kidgeb19), values(1900 / 2011)
	egen numkids2012 = anycount(kidgeb01-kidgeb19), values(1900 / 2012)
	egen numkids2013 = anycount(kidgeb01-kidgeb19), values(1900 / 2013)
	egen numkids2018 = anycount(kidgeb01-kidgeb19), values(1900 / 2018)

	gen numkids = .
	
	foreach i in 2010 2011 2012 2013 2018 {
	
		replace numkids = numkids`i' if syear == `i' & numkids`i' != 0
	
	}
	
	
	* Generate variables for number of GIRLS at time of interview
	forvalues i = 1 / 9 {		
		replace kidgeb0`i' = . if kidsex0`i' != 2
	}
	
	forvalues i = 0 / 9 {		
		replace kidgeb1`i' = . if kidsex1`i' != 2
	}
	
	egen numgirls2010 = anycount(kidgeb01-kidgeb19), values(1900 / 2010)
	egen numgirls2011 = anycount(kidgeb01-kidgeb19), values(1900 / 2011)
	egen numgirls2012 = anycount(kidgeb01-kidgeb19), values(1900 / 2012)
	egen numgirls2013 = anycount(kidgeb01-kidgeb19), values(1900 / 2013)
	egen numgirls2018 = anycount(kidgeb01-kidgeb19), values(1900 / 2018)
	
		
	* Generate dummy variables for whether participant has at least one daughter (1) or not (0) at the time of the interview
	gen ndaughters = .
	
	foreach i in 2010 2011 2012 2013 2018 {
		
		replace ndaughters = numgirls`i' if syear == `i' & numkids`i' > 0 & numkids`i' <= 19
		
	}

	gen d_daughter = .
	replace d_daughter = 0 if ndaughters == 0
	replace d_daughter = 1 if ndaughters > 0 & ndaughters <= 19
	
	*/
	
	


* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Respondents with 1 or more daughters will be more likely than respondents without daughters to express attitudinal support for gender equality. 
	* We run a model with I as our dependent variable and d_daughter as our independent variable.
	* As specified in the PAP, all models control for the number of children, the gender of the respondent and the year of birth.
	* We also specified a one-sided test  for the coefficient on d_daughter in the PAP.
	
	
	* run the regression
	reg I d_daughter i.sex gebjahr i.numkids, robust
	
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
		local p_value = ttail(e(df_r), abs(`p_value'))  // for one-tailed test

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
	
	

	/* 
	NOTE: here is our old version:
	
	* Respondents with 1 or more daughters will be more likely than respondents without daughters to express attitudinal support for gender equality. 
	* We run a series of OLS regressions with childsuffer6_4pt-momwarmth as our dependent variables and the dummy variable d_daughter as independent variable.
	* As specified in the PAP, all models control for the number of children, the gender of the respondent and the year of birth.
	* We also specified a one-sided test  for the coefficient on d_daughter in the PAP.

* RUN MODELS IN A LOOP, EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Set up the base file	
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "outcome, coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file close myfile
	
	

	* Now looping throug the DVs,
	* Extract the required statistics

	foreach var of varlist childsuffer6_4pt-momwarmth {
	
		* run the regression
		reg `var' d_daughter i.sex gebjahr i.numkids, cluster(pid)
		
		* (0) variable name
		local varname = "`e(depvar)'"
	
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
		local p_value = ttail(e(df_r), abs(`p_value'))  // for one-tailed test

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
		if coef_d_daughter > 0 {
			local interpretation "in line with hypothesis"
		} 
		else {
			local interpretation "not in line with hypothesis"
		}

	// Display the saved statistics
	di "DV: `e(depvar)'"
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
	file open myfile using "$OUT/h1_results.csv", write append
	file write myfile "`e(depvar)', `coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile
	
	}

*/


********************************************************************************

exit
