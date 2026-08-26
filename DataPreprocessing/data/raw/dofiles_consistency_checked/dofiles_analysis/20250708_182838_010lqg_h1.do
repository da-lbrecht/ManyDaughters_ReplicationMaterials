********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
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


	
	* support for gender equality measures as latent variable from support for state involvement in childcare, caring for 
	* sick and elderly to indicate GEM that reduce the load of carework traditionally faced by women

	use pid syear cid plh0016 plh0017 plh0018 plh0019 plh0020 plh0021 plh0022 plh0023 plh0024 plh0025 plh0026 pla0009_v1 pla0009_v2 pla0009_v3 using "$IN/pl.dta", clear
	
	drop if pid == . // drop data we cannot recognize
	drop if cid == .
	
	gen gender = pla0009_v1 // recode different gender variables from varying waves of the survey
	replace gender = pla0009_v2 if gender == -8
	replace gender = pla0009_v3 if gender == -8
	
	drop pla0009_v1 pla0009_v2 pla0009_v3
	
	foreach v in plh0016 plh0017 plh0018 plh0019 plh0020 plh0021 plh0022 plh0023 plh0024 plh0025 plh0026 {
		replace `v'  = . if `v' < 1
	} // recode values that are indicative of nonresponse or other problems with the data
	
	foreach var of varlist plh0016-plh0026 {
    replace `var' = 6 - `var'
} // reverse code variable to indicate greater support for state involvement as higher values



	
	sem (Support_state ->  plh0016 plh0017 plh0018 plh0019 plh0020 plh0021 plh0022 plh0023 plh0024 plh0025 plh0026) 
	predict support_state, latent(Support_state) // construct latent variable

	save "$DATA/supportstate.dta", replace // save latent variable
	
	
	merge 1:1 pid cid syear using "$DATA/hasdaughters.dta", nogen keep(master matches)
	label language EN 
	
	
	save "$DATA/h1_dataset.dta", replace // save full data for h1
	

	
* ============================
* Analysis
* ============================

* FILTERS * 
 
	* none


* GENERATE VARIABLES * 

	* see above, variables generated as part of data preparation


* HYPOTHESIS TEST FOR H1 * 

	* We run a linear mixed effects model with random intercepts at the individual (pid) and household (cid) levels.
	* We predict the support for state-involvement to relieve care work (used to operationalize attitude towards gender
	* equality measures) from whether the person has daughters (1 = yes, 0 = everything else), controlling for a year-
	* fixed effect. 

	
	* mixed support_state hasdaughters syear || cid: || pid:, reml
	
	* Because the model does not converge in the full data set, we change the estimation approach. We instead use a linear
	* panel regression (GLS) with random effects at the level of participants (pid). 
	* We predict the support for state-involvement to relieve care work (used to operationalize attitude towards gender
	* equality measures) from whether the person has daughters (1 = yes, 0 = everything else), controlling for a year-
	* fixed effect. 

	xtset pid
	xtreg support_state hasdaughters syear, re  
	

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

		* (vii) number of participants // adapted by David Albrecht on 14.07.2025 for debugging
		quietly count if e(sample)
        local sample_size = r(N)
        preserve
        keep if e(sample)
        quietly duplicates drop pid, force
        local num_participants = _N
        restore

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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
