********************************************************************************
******************  Do-File H3: #ManyDaughters Project *************************
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
	merge 1:1 pid syear using "$IN/pl.dta", nogen keepusing(pli* plh0258_h) keep(master matches)
	
	* hgen contains generated variables on household level from the household interview; use for household net income
	// merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


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

	// Drop respondents without valid parid (outcome is the diff in unpaid work hours in couples)
	drop if mi(parid)

* Further data preparation: calculating unpaid work hours and merging partner's hours and sex

	// Our preferred measure of gender equality in the household ("as measured by behaviors or outcomes" is 
	// the difference in weekly unpaid work hours between father and female partner (which need not be the mother of the father's children), 
	// calculated as the father's hours minus the partner's hours, so higher values mean greater egalitarianism. 
	// Unpaid work hours are calculated as the sum of reported average weekly hours spent on housework, errands/shopping, and repairs.
	// Weekly hours for each partner are calculated as:
	// 5 times reported average housework/errands/repairs hours on weekdays 
	//       + reported average housework/errands/repairs hours on Sundays
	//       + reported average housework/errands/repairs hours on Saturdays. 

	
	foreach x of varlist pli0043_v3 pli0040 pli0049_v3 pli0012_v3 pli0054 pli0031_v4 pli0016_v4 pli0011 pli0034_v4 {
		replace `x' = . if  `x' < 0
	}

	gen unpdwrk =   pli0043_v3 * 5 + pli0012_v3 + pli0016_v4 + /// Housework
					pli0040 * 5 + pli0054 + pli0011 + /// Errands
					pli0049_v3 * 5 + pli0031_v4 + pli0034_v4 // Repairs

	// Note: "pli0054" is not labeled as "Errands/Saturday" just "Errands", but after inspecting https://www.diw.de/documents/publikationen/73/diw_01.c.824201.de/diw_ssp1024.pdf
	// (where the "Saturday" qualifier is also missing) and the questionnaire for 2019 as well as the distribution of the variable
	// we are convinced that tis variable records the hours for Saturdays				

	// Merge partner's hours
	preserve
	keep pid syear unpdwrk sex
	rename pid parid
	rename unpdwrk part_unpdwrk
	rename sex part_sex
	tempfile partinfo
	save `partinfo'
	restore

	merge 1:1 parid syear using `partinfo', nogen

* FILTERS III*
	
	// Drop childless respondents (or more precisely, respondent-years)
	// (we did not do this before the merge because fathers living with women who have no children are part of the target pop)
	drop if nkids == 0

	// Keep only fathers
	keep if sex == 1

	// Keep only those with female partners
	keep if part_sex == 2


* Further data preparation: create outcome variable (father's minus female partner's unpaid work hours)

	gen diff_unpdwrk = unpdwrk - part_unpdwrk
	tab diff_unpdwrk

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)


	// As detailed in the preregistration, we restrict the estimation to fathers (i.e., male respondents with at least one child) 
	// We use a simple linear regression, controlling for the number of kids, assuming that child sex and thereby whether respondent has
	// a daughter is as good as random conditional on the number of children (noting that decisions to have further children 
	// may depend on the sex composition of those already born, which makes this assumption somewhat questionable.)
	
	// A few standard controls are included to increase precision; however, contrary to what we preregistered, we do not include 
	// religion because of too many missing values

	// Standard errors are clustered at the person level and observations are weighted with the cross-sectional weight phrf (as preregistered)

	reg diff_unpdwrk anydaughter i.nkids i.agegr i.edu i.agefrstbrthgr i.syear [pw=phrf], vce(cluster pid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[anydaughter]

		* (ii) standard error
		local se = _se[anydaughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[anydaughter] / _se[anydaughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
