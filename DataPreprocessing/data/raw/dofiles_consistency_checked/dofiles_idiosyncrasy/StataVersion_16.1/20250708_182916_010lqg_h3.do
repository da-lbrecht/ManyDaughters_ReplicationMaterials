********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *  
* ============================

* clear all
version 16.1 // indicate the version of stata you are using here
//set maxvar 10000 // extend the maximum number of variables to 10,000 but doesn't work in this licensed version of stata that we have available - no problem, we will just work around this
set seed 020502
* INSTALL PACKAGES *  
* none for us

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

	* generate indicator variable for whether the person has daughters (1) or not (0)	

	* generate indicator variable for whether the person has daughters (1) or not (0) where 0 is literally any other 	
	* expression of the variable and includes missings and invalid answers, etc. 	

	use "$IN/biobirth.dta", clear // use biobirth as a starting dataset
		
	keep pid cid kidsex* bioyear kidgeb* // calculate for each survey year if the person had a female child in that year
	
	forvalues y = 2000/2020 {
		gen hasdaughters`y' = 0
		forvalues i = 1/19 {
			local sexvar = "kidsex" + string(`i', "%02.0f")
			local yearvar = "kidgeb" + string(`i', "%02.0f")
			replace hasdaughters`y' = 1 if `sexvar' == 2 & `yearvar' <= `y' & `yearvar' != .
		}
	}

	keep pid cid hasdaughters* 
	
	reshape long hasdaughters, i(pid) j(syear)
		
	save "$DATA/hasdaughters.dta", replace // save indicator variable

	* generate indicator variable for gender equality in the household  
	* we use reported "last word" on financial decisions in the household

	use pid syear cid plc0011 using "$IN/pl.dta", clear
	keep if plc0011 > 0 // dismissing all nonresponses or missings
	drop if pid == .
	drop if cid == .
	
	gen household_GEM = . 
	replace household_GEM = 1 if plc0011 == 3 // support for gender equality in the household indicated when "both partners" have the last word
	replace household_GEM = 0 if plc0011 < 3 // reference category: either the respondent or their partner have the last word, not both
	
	save "$DATA/householdgem.dta", replace // save indicator variable
	
	* Merge 
	merge 1:1 pid cid syear using "$DATA/hasdaughters.dta", nogen keep(master matches)
	
	label language EN 
	
	save "$DATA/h3_dataset.dta", replace // save merged dataset

	
* ============================
* Analysis
* ============================

* FILTERS *  
 
	* none


* GENERATE VARIABLES *  

	* see above, variables generated as part of data preparation


* HYPOTHESIS TEST FOR H3 *  

	* We run a logistic mixed effects model with random intercepts at the individual (pid) and household (cid) levels.
	* We predict if the last word on financial decisions in the household (used to operationalize gender equality in the 
	* household) is shared (1 = shared, 0 = unilateral) from whether the person has daughters (1 = yes, 0 = everything else), 
	* controlling for a year-fixed effect.

	*	xtmelogit household_GEM hasdaughters syear || cid: || pid:, intpoints(3)

	
	* Because the model is not fully computationally reproducible, we change the estimation approach. We instead use a logistic
	* panel regression with random effects at the level of participants (pid). 
	* We predict if the last word on financial decisions in the household (used to operationalize gender equality in the 
	* household) is shared (1 = shared, 0 = unilateral) from whether the person has daughters (1 = yes, 0 = everything else), 
	* controlling for a year-fixed effect.
	
	xtset pid
	xtlogit household_GEM hasdaughters syear, re


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[hasdaughters]

		* (ii) standard error
		local se = _se[hasdaughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local z = _b[hasdaughters] / _se[hasdaughters]
		local p_value = 2 * normal(-abs(`z'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[hasdaughters]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
