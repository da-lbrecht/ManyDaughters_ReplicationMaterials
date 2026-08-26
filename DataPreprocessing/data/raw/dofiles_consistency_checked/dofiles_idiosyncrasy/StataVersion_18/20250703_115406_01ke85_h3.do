********************************************************************************
*************************  Do-File H3: #ManyDaughters Project ******************
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

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS *
 
	* Restrict based on the variables we use in the regression analysis


* GENERATE VARIABLES * 

	* Outcome: Gender equality in the household (as measured by behaviors or outcomes)

	// We generate a variable to measure housework hours
	gen housework = .
	replace housework = pli0043_h if pli0043_h  > -1

	// We generate a couple id 
	gen coupleid = pid if sex == 1 & parid > 0 & parid != .
	replace coupleid = parid if sex == 2 & parid > 0 & parid != .

	// We generate the housework of the wife
	sort coupleid syear
	gen	partner_housework =	housework if sex == 2 & housework > -1 & housework < 100 	
	by coupleid syear: egen	housework_wf = max(partner_housework) if coupleid != .	
	drop partner_housework
	label variable housework_wf "Housework hours of the female partner"

	// We calculate the housework shares of the husband
	gen total_housework = .
	replace total_housework = housework + housework_wf if housework != . & housework_wf  != . & sex == 1
	label variable total_housework "Total housework of both partners"
	gen husband_hw_share = (housework / total_housework) if housework != . & total_housework  != . & sex == 1
	label variable husband_hw_share "Housework share of husband"

	// We generate the absolute housework difference, where values closer to 0 indicate equality
	gen abs_gap_housework = abs(housework - housework_wf) if sex ==1 & coupleid !=.
	label variable abs_gap_housework "Absolute housework gap (husband housework - wife housework)"
	
	// We reverse code the housework gap, negative values = inequality, 0 = equality
	gen hh_gender_equal = .
	replace  hh_gender_equal = abs_gap_housework*(-1)
	label variable hh_gender_equal "Gender equality in the household (based on husband housework - wife housework)"

	// We topcode the housework gap, differences of 10 hours or more as threshold
	replace hh_gender_equal =-10 if hh_gender_equal <-10
	

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
	
	// We generate a variable for the partners age
	sort coupleid syear
	gen prov_partner_age = age if sex == 2 	
	by coupleid syear:	egen wife_age  = max(prov_partner_age) if coupleid != . 
	drop prov_partner_age
	label variable wife_age "Wife yearly indicator of age"
	
	// We generate a variable for the partners age squared 
	gen wife_age_sq = wife_age*wife_age
	label variable wife_age_sq "Wife age squared"
	
		


* HYPOTHESIS TEST FOR H3 * 

	* Please add a verbal description of your model here
	/*
	We run a linear regression model with year fixed effects and cluster robust 
	standard errors at the individual level. 	
	
	*/	

	reg hh_gender_equal d_daughter age age_sq wife_age wife_age_sq i.syear , vce(cluster pid)
	


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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
