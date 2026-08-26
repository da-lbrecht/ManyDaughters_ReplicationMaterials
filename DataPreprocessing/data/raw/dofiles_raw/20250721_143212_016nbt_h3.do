********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2

* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}


// global ROOT "C:\Users\mandwei1\sciebo\Kollaborationen\Many Daughters\#ManyDaughters_RT_AnalysisPackage"   //we had to set this manually //commented out by David Albrecht for repruction check
global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for datasets
global OUT "$ROOT/out" // folder for output (results, tables)

*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidgeb01 kidgeb02 kidmon01 kidmon02 kidsex01 kidsex02)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgmonth)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pli0044_h pli0043_h)
	* keep women, merge them to their partners
	preserve
	keep if sex==1
	drop parid 
	rename pid parid
	rename pli0044_h p_pli0044_h
	rename pli0043_h p_pli0043_h
	keep parid p_* syear
	save "$DATA/partner_info.dta", replace
	restore
	keep if sex==2 & parid>0 //keep only one observation per household (use women as anchor)
	merge 1:1 parid syear using "$DATA/partner_info.dta", nogen keep(3)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Note: Because of the merging process, the sample only cosists of heterosexual couples already.
	
	keep if kidgeb01==2017 | kidgeb01==2018 // first child 2016 or 2017
	keep if syear == 2019 // year of outcome observation
	keep if pli0044_h>0 & pli0044_h!=. & pli0043_h>0 & pli0043_h!=.  // Info on outcome (woman)
	keep if p_pli0044_h>0 & p_pli0044_h!=. & p_pli0043_h>0 & p_pli0043_h!=. // Info on outcome (partner)
	keep if (kidgeb02>0 & kidmon02>0  & kidsex01>0) | kidgeb02==-2 // Info on second child
 


* GENERATE VARIABLES * (Please adapt according to your needs)

	mvdecode kidgeb01  kidmon01  kidsex01 kidsex02, mv(-1/-9) // negatives to missing
	mvdecode kidgeb02 kidmon02, mv(-1, -3/-7) // negatives to missing, except -2 (no second child)
	recode kidgeb02 (-2=0) // zero if no second child
	replace kidgeb02 = 0 if kidgeb02>2018 | (kidgeb02==2018 & kidmon02>pgmonth & syear==2018) // zero if second child born after interview
	replace kidsex02=0 if kidgeb02==0 // don't use second childs gender if born after interview
	
	recode kidsex01 (1=0 "Son") (2=1 "Daughter"), gen(d_daughter) // 1st Child gender variable
	label var d_daughter "1st Child Daughter"
	
	recode kidsex02 (.=0 "No second child") (1=1 "Son") (2=2 "Daughter"), gen(second_gender) // 2nd Child gender variable
	label var second_gender "Gender of second child"
	
	gen time_men = p_pli0044_h+p_pli0043_h
	gen time_women = pli0044_h+pli0043_h
	gen Y_women = (((time_women)-(time_men))/(time_women))*100


* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Description of the model:
	* Our outcomevariable is Y = Gender Care Gap within the Couple. Denotes the percentual difference between women's and men's time spent doing household work or childcare.
	* We want to test whether having a daughter affects this outcome, and we want to know whether the birth year (or time passed since birth, which are identical here) changes this effect. We also want to control for potential additional children born before the outcome information was collected.
	* We hence run a linear OLS model on Y including a dummy variable for the first child being a daughter, which we interact with birth year. We also include a control for the gender of the second child.
	
	reg Y i.d_daughter##i.kidgeb01 i.second_gender
	              *NOTE: NO ONE HAS A SECOND CHILD IN THIS SAMPLE OR A DAUGHTER BORN IN 2018 THEREFORE SOME MARGINS MAY BE EMPTY IN OUTPUT             
    margins, dydx(d_daughter) at(kidgeb01=(2017 2018)) 
	matrix list r(table)
    scalar coeff_2017 = r(table)[1,3]
    scalar coeff_2018 = r(table)[1,4]
	 scalar ste_2017 = r(table)[2,3]
    scalar ste_2018 = r(table)[2,4]
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = coeff_2017
        local coefficient_2018 = coeff_2018
		* (ii) standard error
		local se = ste_2017
		local se_2018 = ste_2018
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = coeff_2017 / ste_2017
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		local p_value_2018 = coeff_2018 / ste_2018
		local p_value_2018 = 2 * ttail(e(df_r), abs(`p_value_2018'))
		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = coeff_2017
		local interpretation "positive coefficient is in line with hypothesis"
		scalar coef_d_daughter_2018 = coeff_2018
		if coef_d_daughter_2018 > 0 {
			local interpretation_2018 "in line with hypothesis"
		} 
		else {
			local interpretation_2018 "not in line with hypothesis"
		}

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Coefficient interaction: `coefficient_2018'"
	di "Standard Error interaction: `se_2018'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "p-value 2018: " `p_value_2018'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"
	di "Interpretation of directionality interaction: " "`interpretation_2018'"
	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,coefficient_2018,se_2018,family,residual_df,model_df,p_value, p_value_2018, num_obs,num_participants,interpretation, interpretation_2018"
	file write myfile "`coefficient', `se',`coefficient_2018',`se_2018',  `test_stat_family', `df_residual', `df_model',`p_value', `p_value_2018',`num_obs',`num_participants',`interpretation' `interpretation_2018'"
	file close myfile

 

exit
