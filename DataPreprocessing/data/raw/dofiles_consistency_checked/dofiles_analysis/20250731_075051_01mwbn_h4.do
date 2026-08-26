********************************************************************************
******************  Do-File H1: #ManyDaughters Project *************************
********************************************************************************

* Authors: [REDACTED]
* Research team ID: 01mwbn

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2

// Set working directory
*cd "C:\Users\[REDACTED]\Nextcloud\Shared\ManyDaughters\#ManyDaughters_RT_AnalysisPackage"
*cd C:\Users\jradl\Nextcloud\Shared\ManyDaughters\#ManyDaughters_RT_AnalysisPackage

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
	merge m:1 pid using "$IN/biobirth.dta", nogen keepusing(kidpnr* kidgeb* kidsex* kidmon*) keep(master matches)

	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keepusing(pgisced97 pgmonth) keep(master matches)
	
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keepusing(plh0308_v2 plh0258_h) keep(master matches)
	
	* hgen contains generated variables on household level from the household interview; use for household net income
	// merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS I*
* We want to filter on one generated variable (nkids), so we have another set of filtering 
* commands ()"FILTERS II") after the GENERATE VARIABLES part

	// Exclude cases with missingness on child-related variables to simplify handling below
	// Drop cases with incomplete info on at least one kid's birth year/month or sex
	// (there seem to be no children characterized as non-binary)
	gen drop = 0
	foreach var of varlist kidgeb* kidmon* {
		replace drop = 1 if `var' < 0 & `var' != -2 // -2 are the "does not apply" missings
	}
	drop if drop
	drop drop


* GENERATE VARIABLES * (Please adapt according to your needs)

		
	// Recode -1 etc. to missing 
	mvdecode _all, mv(-9/-1 = .)

	// Age
	gen age = syear - gebjahr if !mi(syear, gebjahr)
	// 5-year groups
	egen agegr = cut(age), at(18 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105) icodes
	local agegr 0 "18-24"
	forvalues i = 1/16 {
		local agegr `agegr' `i' "`=20+`i'*5'-`=24+`i'*5'"
	}
	label define agegr `agegr'
	label values agegr agegr

	// Education
	gen edu:edu = 1 if pgisced97 <= 2
	replace edu = 2 if inlist(pgisced97, 3, 4)
	replace edu = 3 if inlist(pgisced97, 5, 6)
	label define edu 1 "Low (ISCED-97: 0-2)"  2 "Med (ISCED-97: 3-4)"  3 "High (ISCED-97: 5-6)" 

	// Religion
	recode plh0258_h (1/3 7 = 1) (4 8 9 10 = 2) (5/6 = 3), gen(religion)
	label define religion 1 "Christian" 2 "Islamic" 3 "Other"
	label values religion religion
	tab religion, miss 

	// Number of children

	// Was child #x born by the time of the interview in syear
	gen nkids = 0
	gen ndaughters = 0
	gen nsons = 0

	// Remove leading zeros in child variables
	forvalues i = 1/9 {
		rename kid*0`i' kid*`i'
	}

	forvalues i = 1/19 {
		replace nkids = nkids + 1 if kidgeb`i' <= syear & kidmon`i' <= pgmonth 
		replace ndaughters = ndaughters + 1 if kidgeb`i' <= syear & kidmon`i' <= pgmonth & kidsex`i' == 2
		replace nsons = nsons + 1 if kidgeb`i' <= syear & kidmon`i' <= pgmonth & kidsex`i' == 1
	}	

	// Any daughter variable
	gen anydaughter = ndaughters > 0 if !mi(ndaughters)

	// Age at first birth
	egen yrfrstbrth = rowmin(kidgeb*)
	gen agefrstbrth = yrfrstbrth - gebjahr
	tab agefrstbrth
	egen agefrstbrthgr = cut(agefrstbrth), at(10 18 25 30 35 40 45 50 55 60 65) icodes
	local agefrstbrthgr 0 "< 18" 1 "18-24"
	forvalues i = 2/9 {
		local agefrstbrthgr `agefrstbrthgr' `i' "`=15+`i'*5'-`=19+`i'*5'"
	}
	label define agefrstbrthgr `agefrstbrthgr'
	label values agefrstbrthgr agefrstbrthgr


* FILTERS II*
 
	// Adults only (18+)
	drop if age < 18

	// Drop childless respondents (or more precisely, respondent-years)
	drop if nkids == 0


* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	// Our preferred measure of support for gender equality is plh0308_v2:
	// "It’s best if the man and the woman work the same amount so they can share the responsibility for taking care of the
	// family and household equally" - with higher values indicating stronger agreement and therefore greater support for equality

	// As detailed in the preregistration, we restrict the estimation to fathers (i.e., male respondents with at least one child) 
	// We use a simple linear regression, controlling for the number of kids, assuming that child sex and thereby whether respondent has
	// a daughter is as good as random conditional on the number of children (noting that decisions to have further children 
	// may depend on the sex composition of those already born, which makes this assumption somewhat questionable.)
	
	// A few standard controls are included to increase precision; however, contrary to what we preregistered, we do not include 
	// religion because of too many missing values

	// Fit separate regressions for fathers and mothers, weighted with the cross-sectional weight phrf (as preregistered)
	// We have to use iweights here for use with suest, but that does not affect point estimates and SEs will be reestimated
	// by suest anyway

	// Note: suest encountered a "base variable conflict" here when using "i.agegr", apparently because there is no
	// complete-case father in the 18-24 group in our test sample, hence the switch to "ib1.agegr"
	reg plh0308_v2 anydaughter i.nkids ib1.agegr i.edu i.agefrstbrthgr if sex == 1 [iw=phrf]
	estimates store fathers

	reg plh0308_v2 anydaughter i.nkids ib1.agegr i.edu i.agefrstbrthgr if sex == 2 [iw=phrf]
	estimates store mothers

	// Combine estimates, clustering standard errors by hid 
	// (hid denotes unique combinations of partners and stays constant over time,
	// as can be seen by running "sort hid syear pid; list hid syear pid parid, sepby(hid)";
	// could be further refined by also accounting for individual-level dependencies across years for individuals 
	// who switch households, but this would require extra work and was not pre-registered)
	suest fathers mothers, vce(cluster hid)

	// Use lincom to calculate the difference in coefficients
	lincom [fathers_mean]anydaughter - [mothers_mean]anydaughter


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = r(estimate)

		* (ii) standard error
		local se = r(se)

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual NA
		local df_model NA

		* (v) p-value
		local p_value = r(estimate) / r(se)
		local p_value = 2 * (1 - normal(abs(`p_value')))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_anydaughter = _b[anydaughter]
		local interpretation "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di "Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
