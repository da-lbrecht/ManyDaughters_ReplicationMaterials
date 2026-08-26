********************************************************************************
*********************  Do-File H2: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
version 19 // indicate the version of stata you are using here
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
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgnuts1_ew)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS: EXCLUSION CRITERIA * 
 
* Only respondents in years 2005, 2009, 2014 and 2019 with at least one kid during the panel
	keep if inlist(syear, 2005, 2009, 2014, 2019)

	by pid, sort: egen totkids = sum(sumkids)
	drop if totkids == 0
	drop totkids

* GENERATE VARIABLES * 

	* Generate dummy variable for participant having at least one daughter (1) or not (0) in syear
	
	gen d_daughter_2005 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2005 = 1 if `sexvar' == 2 & `yearvar' <= 2005
	}

	gen d_daughter_2009 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2009 = 1 if `sexvar' == 2 & `yearvar' <= 2009
	}

	gen d_daughter_2014 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2014 = 1 if `sexvar' == 2 & `yearvar' <= 2014
	}
	

	gen d_daughter_2019 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2019 = 1 if `sexvar' == 2 & `yearvar' <= 2019
	}
	
	gen 	d_daughter = 0
	replace	d_daughter = 1 if d_daughter_2005 == 1 & syear == 2005
	replace	d_daughter = 1 if d_daughter_2009 == 1 & syear == 2009
	replace	d_daughter = 1 if d_daughter_2014 == 1 & syear == 2014
	replace	d_daughter = 1 if d_daughter_2019 == 1 & syear == 2019
	
	drop d_daughter_2005 d_daughter_2009 d_daughter_2014 d_daughter_2019
	
	
	* Controls in the regression

	* Gender (dummy for female respondent)
	
	gen 	female = 0
	replace	female = 1 if sex == 2
	
	* Age: We use year of birth to generate age (Note: Not clear to us whether we can use bioage)
	gen 	age  = syear - gebjahr
	gen		age2 = (age * age)/100

	* Replace missing values for non-valid responses to control variable (level of in education, marital status, religion, income...)
	replace pgisced11 = . 	if 	pgisced11 <= 0
	replace pgfamstd = . 	if 	pgfamstd <= 0
	replace plh0258_h = . 	if  plh0258_h <= 0
	replace pglabnet = . 	if  pglabnet  <= 0

	* German (dummy variable if the nationality is German)
	gen		Germany = 0		
	replace Germany = 1 	if 	pgnation == 1
	
	* Transformation of labour income into logs (Note: There seems to be a problem when using net labour income pglabnet)
	gen  	log_pglabnet = ln(pglabnet)

	
* HYPOTHESIS TEST FOR H2 * 

	* We test for the effect of having at least one daughter (d_daughter) on political 
	* preferences (plh0004) using a panel regression model with fixed effects at the 
	* individual level. We ommit variables that do not vary at the individual level across time, 
	* (female, nationality, religion or East-German). In addition, we do not include the level of 
	* education (pgisced11) because this is only available in 2014 and 2019 (including this variable
	* implies that we no longer have a panel becuase the number of observations and groups coincide). 
	
	xtset pid syear
	xtreg plh0004 d_daughter age age2 i.pgfamstd i.pgemplst log_pglabnet i.syear, fe cluster(pid)
	
	* NOTE: In H2 we test for the effect of having daughters on political preferences. Our PAP specifies that we will control for the same variables as in H1. Some of these controls are automatically excluded from the analysis when we conduct a panel regression model with fixed effects at the individual level; in particular, female, nationality, religion or East-German do not vary at the individual level across time, thus the regression analysis omits these variables. In addition, we noted that the education level cannot be included in the analysis because this variable (pgisced11) is only available in 2014 and 2019. If we include the level of education we no longer have a panel because we will get the same number of observations and groups; i.e., there will only be one observation per group. 
	
	
	

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
