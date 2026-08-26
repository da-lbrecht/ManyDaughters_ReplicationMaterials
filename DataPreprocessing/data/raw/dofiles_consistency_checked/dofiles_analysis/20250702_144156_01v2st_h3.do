********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED] and [REDACTED], using a template by Tina Wang and David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
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
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset

	* Set up the panel structure
	xtset pid syear
	
* ============================
* Analysis
* ============================

	* Drop responses in which one of the survey items required to construct the outcome variable was not available
	keep if pli0040>=0 & pli0054>=0 & pli0011>=0 & pli0043_h>=0 & pli0012_h >=0 &pli0016_h >=0 &    pli0044_h >=0 &pli0019_h >=0 &pli0022_h >=0 &pli0046 >=0 &pli0055 >=0 &pli0057 >=0 &pli0049_h >=0 &pli0031_h >=0 &pli0034_v4 >=0
	
	* Create variable: total hours worked in the household as specified in the PAP (workdays are multiplied by 5, as it is the average per day).
	gen hours_household = .	
	replace hours_household = 5*pli0040 + 5*pli0054+ 5*pli0011+ 5*pli0043_h+ 5*pli0012_h +pli0016_h + pli0044_h +pli0019_h +pli0022_h +pli0046 +pli0055 +pli0057 +pli0049_h +pli0031_h +pli0034_v4
	keep if hours_household != .
	
	* remove if partner is not living in the same house
	keep if parid>0

	* Add data from partner (sex, hours worked in the household)
	preserve
	keep pid syear sex hours_household
	rename pid parid 
	rename sex partner_sex
	rename hours_household partner_hours_household
	duplicates drop       // Remove duplicates (if any)
	tempfile partner_data
	save `partner_data'   // Save to tempfile
	restore
	merge m:1 parid syear using `partner_data', generate(merge_result) keep(matched master)
	drop merge_result

* FILTERS
	* Inclusion criteria as described in the PEP:
	* Exclude respondents without children: the respondent has, in at least one year of the sample period, at least one biological child (sumkids> 0)
	keep if sumkids>0
	
	* Exclude nonbinary respondents: the respondent's gender is clearly identified as either male or female
	keep if inlist(sex,1,2)
	
	* Exclude respondents whose gender changed: and the same during the entire sample period (to facilitate the test of gender-specific hypotheses, using \texttt{biobirth/sex}), and
	bysort pid (syear): gen change = sex != sex[1]
	bysort pid: egen dropme = max(change)
	drop if dropme == 1
	
    * Exclude respondents with non-binary children
	forval i = 1/19 {
		local var = "kidsex" + string(`i', "%02.0f")
		keep if inlist(`var', 1, 2, -2)	// male, female, does not apply
	}

	* For this hypothesis, we restrict the sample to respondents whose partner lives in the same household (pld0133)
	* We have removed "maybe partner, maybe spouse"
	keep if inlist(partner, 1, 2)
	
	* remove same sex partner
	keep if sex != partner_sex

	

* GENERATE VARIABLES
	* Create "has_child": is 0 before the birth of the first child, starts to be 1 in the year of the birth of the first child
	gen has_child=0
	replace has_child=1 if syear>=kidgeb01

	* Create "has_daughter": is 0 before the birth of the first daughter, starts to be 1 in the year of the birth of the first daughter
	gen has_daughter = 0
	forvalues i = 1/19 {
		local j = string(`i', "%02.0f")
		replace has_daughter = 1 if syear >= kidgeb`j' & kidsex`j' == 2
	}
	
		
	* Create age of the respondent (in the year of the survey)
	gen age=syear-gebjahr
	
	* Create age of the respondent squared
	gen age2=age^2
	
	* Individual's number of sons and daughters
	gen numbersons=0
	gen numberdaughters=0
	
	forval i = 1/19 {
		local j = string(`i', "%02.0f")
		replace numbersons = numbersons + 1 if kidsex`j' == 1 & syear>=kidgeb`j'
		replace numberdaughters = numberdaughters + 1 if kidsex`j' == 2 & syear>=kidgeb`j'
	}

	* OUTCOME VARIABLE
	* To measure gender equality in household labor for each household and year, we construct an index based on the difference in work hours between partners. The measure ranges from 0 (complete inequality) to 1 (perfect equality).
	gen equity = 1 - abs(hours_household - partner_hours_household) / (hours_household + partner_hours_household)
	

* HYPOTHESIS TEST FOR H3
	* Our model is a fixed effects panel regression estimating how having a daughter relates to gender equity in household labor within households over time.
	* Standard errors are clustered at the household level.
	* Dependent variable: equity - continuous index measuring equality in household labor between partners (see above)
	* Key predictor: has_daughter - whether the household has at least one daughter in a given year
	* Controls: whether the household has any children (has_child), age (age), age squared (age2), and number of sons and daughters (numbersons, numberdaughters).
	xtreg equity has_daughter has_child age age2 numbersons numberdaughters, fe cluster(cid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[has_daughter]

		* (ii) standard error
		local se = _se[has_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value (one-sided test)
		local p_value = _b[has_daughter] / _se[has_daughter]
		if _b[has_daughter] > 0 {
			local p_value = ttail(e(df_r), abs(`p_value'))
		}
		else {
			local p_value = 1-ttail(e(df_r), abs(`p_value'))
		}
		

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[has_daughter]
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
