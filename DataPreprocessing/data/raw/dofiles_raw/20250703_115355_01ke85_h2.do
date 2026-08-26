********************************************************************************
*************************  Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:
ssc install outreg2

* CONFIGURE FILE PATHS * 
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

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. 

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * 
 
	* Restrict based on the variables we use in the regression analysis


* GENERATE VARIABLES * 

	* Outcome: Left-leaning political preference
	
	// We generate a dummy variable for left-leaning based on the general party preference
	recode plh0012_h (1 5 6 9 =1 "left-leaning") ( 2 3 4 7 27 26 =0 "not left-leaning") (-5/-1 2/30=.) ,gen(left)
	replace left = 0 if plh0011_h ==2
	lab var left "Dummy: Left-leaning political preference based on general party preference"

	
	* Treatment: Daughters 

	// Loop through each suffix to know who has girls and who has boys for each year
	*First, for boys
	gen has_boy =.
	local suffixes "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19"

	foreach suffix in `suffixes' {
		replace has_boy = 1 if kidsex`suffix' == 1 & kidgeb`suffix' > 1900  & kidgeb`suffix' <= syear 
	}

	*Second, for girls
	gen has_girl =.
	local suffixes "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19"

	foreach suffix in `suffixes' {
		replace has_girl = 1 if kidsex`suffix' == 2 & kidgeb`suffix' > 1900  & kidgeb`suffix' <= syear 
	}

	// We generate a variable to know each year who has a boy and who has a girl
	gen d_daughter = .
	replace d_daughter = 0 if  has_boy == 1 & has_girl == .
	replace d_daughter = 1 if  has_boy == . & has_girl == 1
	label variable d_daughter "Yearly indicator of only daugthers or only boys"
	label define d_daughter 0 "Only boys" 1 "Only daugthers"
	label values d_daughter d_daughter


	* Additional variables 
	// We generate a variable for age
	gen age = (syear - gebjahr) if syear > 0 & syear != . & gebjahr > 0 &  gebjahr!=.
	label var age "Age"
	
	// We generate a variable for age squared
	gen age_sq = age*age
	label var age_sq "Age, squared"
	
	// We make sure that the variable sex has only two values
	replace sex =. if sex <1
	replace sex =. if sex >2 & sex !=.
	
	
	
* HYPOTHESIS TEST FOR H2 * 

	* Please add a verbal description of your model here
	/*
	We run a linear probability model with year fixed effects and cluster robust 
	standard errors at the individual level.
	
	*/	

	reg left d_daughter age age_sq i.sex i.syear , vce(cluster pid)

	
	

* EXTRACT AND EXPORT RESULTS *

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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
