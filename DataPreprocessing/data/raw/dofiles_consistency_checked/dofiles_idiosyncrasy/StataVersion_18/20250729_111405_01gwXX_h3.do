********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Author: [REDACTED]

//clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)

/*
* CONFIGURE FILE PATHS * (Please do not change)
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

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English
	
	
	* keep the variables that will be used 
	keep cid pid hid syear sex kidsex01 plh0174 gebjahr partner parid 

	compress 

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

	use "$DATA/h3_dataset", clear 
* FILTERS * (Please adapt according to your needs)
 
	* only keep participants with at least one child 
	keep if inlist(kidsex01, 1, 2)
	
	* only keep particpants with valid reply on satisfaction with housework 
	keep if inrange(plh0174,0,10)
	
	* only keep particpants with partner in household
	keep if inrange(partner,1,3)
	*sort hid syear pid 
	
	* household members 
	egen hh = count(hid), by(hid syear)
	*g a = syear - gebjahr 
	keep if hh == 2 
	
	* two different genders in the household
	egen g = total(sex), by(hid syear)
	keep if g == 3 
	

* GENERATE VARIABLES * (Please adapt according to your needs)

	* generate dummy variable for whether participant's first child is female (1) or not (0)
	gen byte d_daughter = kidsex01 == 2 
	
	* generate variables for gender equality in the household, as measured by behaviors or outcomes 
	*fre plh0323 plh0324 plh0174 pli0046 
	
	* difference in satisfaction with housework 
	sort hid syear pid 
	g diff = abs(plh0174 - plh0174[_n-1]) if hid == hid[_n-1] & syear == syear[_n-1]
	
	* recode so higher is more equal 
	g gender_eq_hh = 10 - diff 

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)
*** Having daughters leads to more gender equality in the household (as measured by behaviors or outcomes). *** 

		/* Verbal description of your model: Gender of first child is exogeneous, later children may be endogeneous. Household permanence may be endogeneous, but hard to do anything about when the outcome is measured at the household level. Regress the outcome on dummy variable indicating whether the first child is a daughter, for parents. Include indicators for survey years because of time trends and because some outcomes may be combinations of variables from different years. Cluster standard errors on individual respondent.  */ 

	* Regression  

	reg gender_eq_hh d_daughter i.syear, vce(cl hid)

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

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
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
