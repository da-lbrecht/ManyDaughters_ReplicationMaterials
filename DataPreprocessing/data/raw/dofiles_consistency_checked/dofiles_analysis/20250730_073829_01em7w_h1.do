********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

cd "$PATH" 
* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2


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
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset

	use "$DATA/h1_dataset.dta", clear

* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)

	* Keep only relevant variables
	keep kidgeb01-kidgeb19 kidsex01-kidsex19 sumkids cid pid syear sex psample ///
	     plh0301 plh0303 plh0307 plh0308_v1 plh0309 gebjahr germborn sampreg ///
	     partner hghinc pgbilzeit 

	* Drop if no biological kids ever
	drop if kidsex01 <= 0

	* Keep only observations in sample L1, L2, L3 
	keep if inlist(psample, 12, 13, 14)


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Count number of children and daughters born before or during the survey year
	gen num_children = 0
	gen num_daughters = 0

	forvalues i = 1/19 {
		local idx : display %02.0f `i'
		
		gen child_born_`i' = (kidgeb`idx' <= syear & kidgeb`idx' > 0)
		gen is_daughter_`i' = (child_born_`i' == 1 & kidsex`idx' == 2)

		replace num_children = num_children + child_born_`i'
		replace num_daughters = num_daughters + is_daughter_`i'
	}

	* Proportion of daughters among children
	gen prop_daughters = .
	replace prop_daughters = num_daughters / num_children if num_children > 0

	drop child_born_* is_daughter_*

	* First child female dummy (instrumental variable)
	gen first_child_female = .
	replace first_child_female = 1 if kidsex01 == 2 & !missing(kidsex01)
	replace first_child_female = 0 if kidsex01 == 1 & !missing(kidsex01)

	* Gender equality attitudes index (reverse code where needed)
	replace plh0301 = . if plh0301 < 0
	replace plh0303 = . if plh0303 < 0
	replace plh0303 = 5 - plh0303 if !missing(plh0303)
	replace plh0307 = . if plh0307 < 0
	replace plh0308_v1 = . if plh0308_v1 < 0
	replace plh0308_v1 = 5 - plh0308_v1 if !missing(plh0308_v1)
	replace plh0309 = . if plh0309 < 0
	replace plh0309 = 5 - plh0309 if !missing(plh0309)

	egen gendereq = rowmean(plh0301 plh0303 plh0307 plh0308_v1 plh0309)

	* Control variables
	replace sex = 0 if sex == 1
	replace sex = 1 if sex == 2

	replace germborn = 0 if germborn == 1
	replace germborn = 1 if germborn == 2

	rename sampreg westgerm
	replace westgerm = . if westgerm <= 0
	replace westgerm = 0 if westgerm == 2

	replace partner = . if partner < 0 | partner == 5
	replace partner = 1 if partner != 0

	replace pgbilzeit = . if pgbilzeit < 0

	gen age = syear - gebjahr
	gen age_sq = age^2

	gen ln_hghinc = log(hghinc)

	* Dummies for number of kids
	forvalues k = 0/7 {
		gen kids_`k' = .
		replace kids_`k' = 1 if sumkids == `k'
		replace kids_`k' = 0 if sumkids != `k' & !missing(sumkids)
	}
	
	* Dummy if 8 kids or more
	gen kids_8plus = .
	replace kids_8plus = 1 if sumkids >= 8
	replace kids_8plus = 0 if sumkids <8 & !missing(sumkids)


* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Hypothesis: Having a higher proportion of daughters increases support for gender equality
	* Model: 2SLS IV regression with gendereq as DV, prop_daughters as endogenous IV, and first_child_female as instrument
	* Controls: sex, partner, germborn, westgerm, pgbilzeit, age_sq, ln_hghinc, and number of children dummies

	ivregress 2sls gendereq (prop_daughters = first_child_female) ///
		sex partner germborn westgerm pgbilzeit age age_sq ln_hghinc kids_*, vce(robust)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[prop_daughters]

		* (ii) standard error
		local se = _se[prop_daughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_model = e(df_m)

		* (v) p-value
		local z_stat = _b[prop_daughters] / _se[prop_daughters]
		local p_value = 2 * normal(-abs(`z_stat'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[prop_daughters]
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
	file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
