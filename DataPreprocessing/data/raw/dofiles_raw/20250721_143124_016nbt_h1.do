********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 18 
set maxvar 10000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

/*
* CONFIGURE FILE PATHS * (Please do not change)
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
/*
*/
*/
* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA *

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidgeb01 kidgeb02 kidmon01 kidmon02 kidsex01 kidsex02)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgmonth)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0298_v2)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS *
 
	keep if kidgeb01==2016 | kidgeb01==2017 // first child 2016 or 2017
	keep if plh0298_v2>0 & plh0298_v2!=. // Info on outcome
	keep if (kidgeb02>0 & kidmon02>0  & kidsex01>0) | kidgeb02==-2 // Info on second child


* GENERATE VARIABLES * (Please adapt according to your needs)

	mvdecode plh0298_v2 kidgeb01  kidmon01  kidsex01 kidsex02, mv(-1/-9) // negatives to missing
	mvdecode kidgeb02 kidmon02, mv(-1, -3/-7) // negatives to missing, except -2 (no second child)
	recode kidgeb02 (-2=0) // zero if no second child
	replace kidgeb02 = 0 if kidgeb02>2018 | (kidgeb02==2018 & kidmon02>pgmonth & syear==2018) // zero if second child born after interview
	replace kidsex02=0 if kidgeb02==0 // don't use second childs gender if born after interview
	
	recode plh0298_v2 (1=7 "Stimme überhaupt nicht zu") (2=6) (3=5) (4=4) (5=3) (6=2) (7=1 "Stimme voll zu"), gen(Y)
	label var Y "Children under age of 6 don't suffer if mother works"
	
	recode kidsex01 (1=0 "Son") (2=1 "Daughter"), gen(d_daughter) // 1st Child gender variable
	label var d_daughter "1st Child Daughter"
	
	recode kidsex02 (.=0 "No second child") (1=1 "Son") (2=2 "Daughter"), gen(second_gender) // 2nd Child gender variable
	label var second_gender "Gender of second child"


* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Description of the model:
	* Our outcomevariable is Y = Children under age of 6 suffer if mother works
	* We want to test whether having a daughter affects this outcome, and we want to know whether the birth year (or time passed since birth, which are identical here) changes this effect. We also want to control for potential additional children born before the outcome information was collected.
	* We hence run a linear OLS model on Y including a dummy variable for the first child being a daughter, which we interact with birth year. We also include a control for the gender of the second child.
	reg Y i.d_daughter##i.kidgeb01 i.second_gender, cluster(hid)
	margins, dydx(d_daughter) at(kidgeb01=(2016 2017))
	matrix list r(table)
    scalar coeff_2016 = r(table)[1,3]
    scalar coeff_2017 = r(table)[1,4]
	 scalar ste_2016 = r(table)[2,3]
    scalar ste_2017 = r(table)[2,4]
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = coeff_2016
        local coefficient_2017 = coeff_2017
		* (ii) standard error
		local se = ste_2016
		local se_2017 = ste_2017
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = coeff_2016 / ste_2016
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		local p_value_2017 = coeff_2017 / ste_2017
		local p_value_2017 = 2 * ttail(e(df_r), abs(`p_value_2017'))
		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		
		local interpretation "positive coefficient is in line with hypothesis"
		local interpretation_2017 = "positive coefficient is in line with hypothesis"

		
	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Coefficient interaction: `coefficient_2017'"
	di "Standard Error interaction: `se_2017'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "p-value 2017: " `p_value_2017'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"
	di "Interpretation of directionality interaction: " "`interpretation_2017'"
	* Export results to csv 
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,coefficient_2017,se_2017,family,residual_df,model_df,p_value,p-value 2017, num_obs,num_participants,interpretation, interpretation_2017" _n
	file write myfile "`coefficient', `se',`coefficient_2017',`se_2017',  `test_stat_family', `df_residual', `df_model',`p_value',`p_value 2017',`num_obs',`num_participants',`interpretation' `interpretation_2017'" _n
	file close myfile


********************************************************************************

exit
