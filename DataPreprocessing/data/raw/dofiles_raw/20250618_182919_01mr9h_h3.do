************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000



* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system *

if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}


* global ROOT "/Users/nan/Desktop/#ManyDaughters_RT_AnalysisPackage"  //comment out when submitting the do file.

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
		keepusing(pli0040 pli0043_h pli0044_h)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* dropping the refugee samples as stated in the PAP
	drop if psample == 17 | psample == 18 | psample == 19


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Taking care of missing values
	foreach var of varlist gebjahr sex kidgeb01-kidsex19 pli0040 pli0043_h pli0044_h {
		replace `var' = . if `var' < 0
	}
	
	* Outcomes are asked in all years from 2000 - 2020
	* renaming the DVs
	rename pli0040 errands
	rename pli0043_h housework
	rename pli0044_h childcare
	
	* THIS IS NEW CODE THAT WE ADDED SO THAT WE HAVE ONLY 1 DV PER HYPOTHESIS
	gen I = errands + housework + childcare
	
	* WE WANT TO CALCULATE THE DIFFERENCE IN I BETWEEN MEN AND WOMEN IN A COUPLE
	* WHERE COUPLES ARE IDENTIFIED BY PARID
	* WE DO THIS BY SUBSETTING THE DATA BETWEEN MALE AND FEMALE RESPONDENTS
	* AND THEN MERGING THE MALE SUBSET INTO THE FEMALE SUBSET
	
	preserve
	
	keep if sex == 1 // male only dataset
	
	rename pid tparid
	
	rename I Idad
	
	save "$DATA/h3_dataset_men.dta", replace

	restore
	
	keep if sex == 2 // female only dataset
	
	
	
	* Generate variable on total number of kids at time of interview
	forvalues i = 2000 / 2020 {
		
		egen numkids`i' = anycount(kidgeb01-kidgeb19), values(1900 / `i')
		
	}

	gen numkids = .
	
	forvalues i = 2000 / 2020 {
	
		replace numkids = numkids`i' if syear == `i' & numkids`i' != 0
	
	}
	
	* Generate variables for number of GIRLS at time of interview
	forvalues i = 1 / 9 {		
		replace kidgeb0`i' = . if kidsex0`i' != 2
	}
	
	forvalues i = 0 / 9 {		
		replace kidgeb1`i' = . if kidsex1`i' != 2
	}
	
	forvalues i = 2000 / 2020 {
		
		egen numgirls`i' = anycount(kidgeb01-kidgeb19), values(1900 / `i')
		
	}
	
	* Generate dummy variables for whether participant has at least one daughter (1) or not (0) at the time of the interview
	gen ndaughters = .
	
	forvalues i = 2000 / 2020 {
	
		replace ndaughters = numgirls`i' if syear == `i' & numkids`i' > 0 & numkids`i' <= 19
		
	}

	gen d_daughter = .
	replace d_daughter = 0 if ndaughters == 0
	replace d_daughter = 1 if ndaughters > 0 & ndaughters <= 19
		
	
	
		
	* NOW MERGING IN THE MALE DATASET AND CALCULATING THE DV
	
	rename parid tparid
	drop if tparid < 0 // 
	
	merge 1:1 syear tparid using "$DATA/h3_dataset_men.dta", nogen keep(matches) keepusing(Idad)
	
	
	* CALCULATING THE MOTHER-FATHER GAP IN I
	gen DI = abs(I - Idad)
	

	
	
* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* The hypothesis is that couples with daughters will have a smaller "mother-father gap" in the time spent on errands, housework, etc. than couples without daughters.
	* We estimate the following model using OLS:
	
	reg DI d_daughter i.numkids, cluster(pid)

	* As specified in the PAP, the models control for the number of children 
	* We also specified a one-sided test  for the coefficient on d_daughter in the PAP.

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
		local p_value = ttail(e(df_r), abs(`p_value'))

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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile
	
	
/*

NOTE HERE IS THE OLD CODE:

	* Female Respondents with 1 or more daughters will spend LESS time than female respondents without daughters on (i) errands and shopping, (ii) housework such as washing, cooking and cleaning, and (iii) looking after children.
	* Male Respondents with 1 or more daughters will spend MORE time than male respondents without daughters on (i) errands and shopping, (ii) housework such as washing, cooking and cleaning, and (iii) looking after children.	
	
	* We run a series of OLS regressions with (i) - (iii) as our dependent variables and the dummy variable d_daughter as independent variable.
	* These models are run separately for male vs. female respondents
	* As specified in the PAP, the models control for the number of children and and the year of birth of the respondent
	* We also specified a one-sided test  for the coefficient on d_daughter in the PAP.

	
	
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Set up the base file	
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "R_gender, outcome, coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file close myfile
	
	* Now looping throug the DVs for FEMALE RESPONDENTS
	* Extract the required statistics

	foreach var of varlist errands-childcare {
	
		* run the regression
		reg `var' d_daughter gebjahr i.numkids if sex == 2, cluster(pid)
		
		* (0) variable name and R gender
		local R_gender = "Female"
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
		local p_value = ttail(e(df_r), abs(`p_value'))

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
	file open myfile using "$OUT/h3_results.csv", write append
	file write myfile "`R_gender', `e(depvar)', `coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile
	
	}
	
	
	* Now looping throug the DVs for MALE Respondents
	* Extract the required statistics

	foreach var of varlist errands-childcare {
	
		* run the regression
		reg `var' d_daughter gebjahr i.numkids if sex == 1, cluster(pid)
		
		* (0) variable name and R gender
		local R_gender = "Male"
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
		local p_value = ttail(e(df_r), abs(`p_value'))

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
	file open myfile using "$OUT/h3_results.csv", write append
	file write myfile "`R_gender', `e(depvar)', `coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile
	
	}


*/


********************************************************************************

exit
