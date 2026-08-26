********************************************************************************
*********************  Do-File H3: #ManyDaughters Project **********************
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

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * 
 
* Keep households with the same heterosexual partner and at least one kid during the panel
* Years 2009, 2011, 2013, 2015, 2017, 2019

* We keep spouse, registered partner (partner == 1) who are married (pgfamstd ==1)

	keep if partner == 1 & pgfamstd ==1 
	keep if inlist(syear, 2009, 2011, 2013, 2015, 2017, 2019)

* Only respondents with kids at some point during the panel.

	by cid, sort: egen totkids = sum(sumkids)
	drop if totkids == 0
	drop totkids

* We keep ONLY heterosexual partners living together (1 men + 1 women).
* We eliminate observations from households with more than 1 couple
	by cid syear, sort: egen totgender = sum(sex)
	keep if totgender == 3
	
	
	
* Generate dummy variable for whether participant has at least one daughter (1) or not (0) in syear
	
gen d_daughter_2009 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2009 = 1 if `sexvar' == 2 & `yearvar' <= 2009
	}

gen d_daughter_2011 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2011 = 1 if `sexvar' == 2 & `yearvar' <= 2011
	}

	
gen d_daughter_2013 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2013 = 1 if `sexvar' == 2 & `yearvar' <= 2013
	}
	
gen d_daughter_2015 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2015 = 1 if `sexvar' == 2 & `yearvar' <= 2015
	}

gen d_daughter_2017 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2017 = 1 if `sexvar' == 2 & `yearvar' <= 2017
	}

gen d_daughter_2019 = 0
	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter_2019 = 1 if `sexvar' == 2 & `yearvar' <= 2019
	}
	
	gen 	d_daughter = 0
	replace	d_daughter = 1 if d_daughter_2009 == 1 & syear == 2009
	replace	d_daughter = 1 if d_daughter_2011 == 1 & syear == 2011
	replace	d_daughter = 1 if d_daughter_2013 == 1 & syear == 2013
	replace	d_daughter = 1 if d_daughter_2015 == 1 & syear == 2015
	replace	d_daughter = 1 if d_daughter_2017 == 1 & syear == 2017	
	replace	d_daughter = 1 if d_daughter_2019 == 1 & syear == 2019
	
	
drop d_daughter_2009 d_daughter_2011 d_daughter_2013 d_daughter_2015 d_daughter_2017 d_daughter_2019
		
	
* GENERATE VARIABLES *

* Dependent variable: percentage of housework by men in the household.
* We need to compute the number of hours on housework per week by adding hours of 
* housework over the week (pli0012_h: Saturday, pli0016_h: Sunday, pli0043_h: day week)

	replace pli0012_h = . 	if pli0012_h <0 
	replace pli0016_h = . 	if pli0016_h <0 
	replace pli0043_h = . 	if pli0043_h <0 

	gen 	housework_pid 		= pli0012_h + pli0016_h + 5*pli0043_h 
	by 		cid syear, sort: egen housework_cid = sum(housework_pid)
	gen 	housework_partner 	= housework_cid - housework_pid

	gen 	phmale = housework_pid / housework_cid if sex == 1
	
	* Age: We use year of birth to generate age 
	gen 	age  = syear - gebjahr
	gen		age2 = (age * age) / 100
	
	
	* Replace missing values for non-valid responses to control variable (level of in education, net labour income...)
	replace pgisced11 = . 	if 	pgisced11 <= 0
	replace pglabnet = . 	if  pglabnet  <= 0

	*Generate dummy for full-time job
	gen 	fulltime = 0 
	replace fulltime = 1 if pgemplst == 1
	
	* Generate the variables for the partner in the same row
	by cid syear, sort: egen totage = sum(age)
	by cid syear, sort: egen totedu = sum(pgisced11)
	by cid syear, sort: egen totincome = sum(pglabnet)
	by cid syear, sort: egen totfull = sum(fulltime)
	
	gen 	age_partner 	= totage-age
	gen 	edu_partner 	= totedu - pgisced11
	gen 	inc_partner 	= totincome - pglabnet
	gen 	full_partner 	= totfull-fulltime

	gen		share_income   = pglabnet/totincome
	
	gen 	age_partner_2 	= (age_partner * age_partner)/100
	
	keep if sex==1
	
	
	
	
* HYPOTHESIS TEST FOR H3 * 

* We conduct our analysis running panel data fixed effects at the household level 
* Our dependent variable is the percertage of housework by men in the housegold (phmen)
* The set of independent variable includes age, age2, level ofd education, percentage of
* net labor income earned by husband, employment status (dummies for full-time), total 
* number of hours devoted to housework in the household (housework_cid) and year dummies 
* We include fixed effects at the household level (cid)

	xtset cid syear
	xtreg phmale d_daughter age age2 age_partner age_partner_2 pgisced11 edu_partner share_income fulltime full_partner housework_cid  i.syear , fe cluster(cid)

	* NOTE: Our analysis for H3 focuses on the percentage of housework by men in the household (phmale), which takes values from 0 to 1. In the pre-analysis plan we did not think of the possibility of defining phmale = 0.5 for "egalitarian households" where none of the spouses do any housework (they probably hired someone else to do this job).
	

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
