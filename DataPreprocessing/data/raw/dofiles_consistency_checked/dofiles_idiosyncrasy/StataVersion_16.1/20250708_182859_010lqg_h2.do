********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
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

	* generate indicator variable for left-leaning politics support via party preference 

	use pid syear cid plh0012_h plh0012_v6 using "$IN/pl.dta", clear

	gen partypref = plh0012_h
	replace partypref = plh0012_v6 if partypref == .
	keep if partypref > 0 // drop all missings and nonresponse
	drop if partypref == . 
	
	gen left_leaning = . 

	* recode stated party preferences according to Wahlomat 2024 for the European Elections, cutoffs according to our own
	* perception, where left_leaning = 1 incidates a left-leaning preference, while left_leaning = 0 points to center/right
	* mixed / preferences are discarded
	
	replace left_leaning = 1 if inlist(partypref, 1, 5, 6, 9, 26)         // SPD, Greens, Die Linke, SPD/Greens, Piraten
	replace left_leaning = 0 if inlist(partypref, 2, 3, 4, 7, 13, 14, 27, 30)  // CDU, CSU, FDP, far-right, CDU/CSU, CDU/FDP, AfD
	replace left_leaning = . if inlist(partypref, 10, 11, 12, 31)         // SPD/CDU, SPD/AfD, SPD/far-right
	replace left_leaning = . if inlist(partypref, 8, 15, 25, 26, 28, 29, 31) // Other/mixed/unknown
	
	keep pid cid syear left_leaning
	
	save "$DATA/leftleaning.dta", replace // save indicator variable

	* Merge 
	merge 1:1 pid cid syear using "$DATA/hasdaughters.dta", nogen keep(master matches)
	label language EN 
	
	save "$DATA/h2_dataset.dta", replace // save merged dataset
	

* ============================
* Analysis
* ============================

* FILTERS *  
 
	* none


* GENERATE VARIABLES *  

	* see above, variables generated as part of data preparation

	
* HYPOTHESIS TEST FOR H2 *  

	* We run a logistic mixed effects model with random intercepts at the individual (pid) and household (cid) levels.
	* We predict the party preference for parties on the left of the German spectrum (used to operationalize left-leaning 
	* political preferences) from whether the person has daughters (1 = yes, 0 = everything else), controlling for a year-
	* fixed effect.

		
	* xtmelogit left_leaning hasdaughters  syear || cid: || pid: , intpoints(3)
	
	* Because the model is not fully computationally reproducible, we change the estimation approach. We instead use a logistic
	* panel regression with random effects at the level of participants (pid). 
	* We predict the party preference for parties on the left of the German spectrum (used to operationalize left-leaning 
	* political preferences) from whether the person has daughters (1 = yes, 0 = everything else), controlling for a year-
	* fixed effect.

	xtset pid
	xtlogit left_leaning hasdaughters syear, re  



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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
