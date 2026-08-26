********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2
ssc install reghdfe // (added by David Albrecht 02. June 2025)
ssc install ftools // (added by David Albrecht 02. June 2025)

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

	save "$DATA/h1_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* GENERATE VARIABLES * (Please adapt according to your needs)

	* Example: generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* For illustrative purposes, we will generate the variable randomly:
	gen age = syear - gebjahr
	recode sex (2 = 0), gen(male)
	recode plh0004 (-8 -5 -1 =.), gen(leftleaningness)
	replace leftleaningness  = 10 - leftleaningness
	recode partner (-2 5=.) (1 2 3 4 = 1), gen(partner_dummy)
	foreach i of varlist plj0621 plj0622 plj0623 plj0624 plj0625 {
		recode `i' (-8 -5 -1 = .), gen(`i'_nm)
	}
	gen corigin_nm = corigin if corigin!=-1

	recode plh0301 (-8 -5 -1 =.), gen(women_career)
	recode pli0043_h (-5 -3 -2 -1 = .), gen(hours_chores) 
	recode pgisced97 ( -1 = .), gen(edu_level) 

	gen daughter_ever = 0
	gen ndaughter_ever = 0
	gen nkids_ever = 0
	foreach i of varlist kidsex* {
		replace daughter_ever = 1 if `i'==2
		replace ndaughter_ever = ndaughter_ever+1 if `i'==2
		replace nkids_ever = nkids_ever+1 if `i'==1|`i'==2
	}
	gen d_daughter = 0
	gen ndaughter_now = 0
	gen nkids_now = 0
	forvalues i = 1(1)19 {
		local i = string(`i',"%02.0f")
		replace d_daughter = 1 if kidsex`i'==2&kidgeb`i'<syear
		replace ndaughter_now = ndaughter_now+1 if kidsex`i'==2&kidgeb`i'<syear
		replace nkids_now = nkids_now+1 if (kidsex`i'==1|kidsex`i'==2)&kidgeb`i'<syear
	}
	gen daughter_male = d_daughter*male

* FILTERS * (Please adapt according to your needs)
 
	* Example: drop minors and those that never have children
	drop if age<18
	drop if nkids_ever == 0

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* We estimate a linear regression of our measure of gender equality on having at least one daughter since the year before the survey, net of comprehensive sex-specific FE

	global controls i.age#i.sex i.syear#i.sex i.nkids_now#i.sex i.edu_level#i.sex i.corigin_nm#i.sex
	reghdfe women_career d_daughter, absorb($controls) cluster(i.hid)


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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile

********************************************************************************

exit
