************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
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
		keepusing(plh0004)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* dropping the refugee samples as stated in the PAP
	drop if psample == 17 | psample == 18 | psample == 19



* GENERATE VARIABLES * (Please adapt according to your needs)

	* Taking care of missing values
	foreach var of varlist gebjahr sex kidgeb01-kidsex19 plh0004 {
		replace `var' = . if `var' < 0
	}
	
	* Outcomes are only asked in 2005, 2009, 2014 and 2019
	* plh0004 is coded such that lower values denote more leftwing attitudes
	
	* Generate variable on total number of kids at time of interview
	foreach i in 2005 2009 2014 2019 {
		
		egen numkids`i' = anycount(kidgeb01-kidgeb19), values(1900 / `i')
		
	}

	gen numkids = .
	
	foreach i in 2005 2009 2014 2019 {
	
		replace numkids = numkids`i' if syear == `i' & numkids`i' != 0
	
	}

	* Generate variables for number of GIRLS at time of interview
	forvalues i = 1 / 9 {		
		replace kidgeb0`i' = . if kidsex0`i' != 2
	}
	
	forvalues i = 0 / 9 {		
		replace kidgeb1`i' = . if kidsex1`i' != 2
	}
	
	foreach i in 2005 2009 2014 2019 {
		
		egen numgirls`i' = anycount(kidgeb01-kidgeb19), values(1900 / `i')
		
	}
	
	
	* Generate dummy variables for whether participant has at least one daughter (1) or not (0) at the time of the interview
	gen ndaughters = .
	
	foreach i in 2005 2009 2014 2019 {
		
		replace ndaughters = numgirls`i' if syear == `i' & numkids`i' > 0 & numkids`i' <= 19
		
	}

	gen d_daughter = .
	replace d_daughter = 0 if ndaughters == 0
	replace d_daughter = 1 if ndaughters > 0 & ndaughters <= 19
		
	
	
	
	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Respondents with 1 or more daughters will be more likely than respondents without daughters to self-rate as more left-leaning. 
	* We run a OLS regression with plh0004 as our dependent variable and the dummy variable d_daughter as independent variable.
	* As specified in the PAP, the model controls for the number of children, the gender of the respondent and the year of birth.
	* We also specified a one-sided test  for the coefficient on d_daughter in the PAP.



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* run the regression
	reg plh0004 d_daughter i.sex gebjahr i.numkids, cluster(pid)
		

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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
