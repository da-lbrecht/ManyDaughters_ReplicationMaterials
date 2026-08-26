********************************************************************************
****************** Do-File H4: #ManyDaughters Project ******************
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

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS
	* Inclusion criteria as described in the PEP:
	* Exclude respondents without children: the respondent has, in at least one year of the sample period, at least one biological child (sumkids> 0)
	keep if sumkids>0
	
	* Exclude nonbinary respondents: the respondent's gender is clearly identified as either male or female
	* COMMENT: there are none?
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

	* Only include respondents who have filled out all survey questions required for the outcome variable (see below)
	keep if plj0625>0 & plj0624>0 & plj0622>0	

	* Create dummy for male
	gen male = 0
	replace male = 1 if sex==1
	
	* OUTCOME VARIABLE
	* The outcome variable measuring support for gender equality is an aggregated index constructed from three survey items: plj0625, plj0624, and plj0622
	* Each is measured on a 1-7 likert scale and contributes equally to the index 'support_gender_equality'
	gen support_gender_equality = (plj0625 + plj0624 + plj0622 - 3) /18
	keep if support_gender_equality!=.

	
* HYPOTHESIS TEST FOR H4
	* Our model is a linear regression (OLS) estimating how having a daughter relates to support for gender equality for men, compared to women. The model includes an interaction term between male and has_daughter as the key predictor. We control for age, age2, has_child, numberdaughters, and numbersons. Standard errors are clustered at the household (cid) level.
	* The predictor of interest is the interaction term male × has_daughter, which captures whether the effect of having a daughter differs by respondent gender.
	* Remark: The survey items (plj0625, plj0624, and plj0622) used to construct the outcome variable are available for only a single year per respondent, not across three years as initially assumed. Therefore, we could not implement the panel regression specified in the PAP and instead used an OLS model. The variables and controls remain consistent with those outlined in the PAP.
	reg support_gender_equality male##has_daughter has_child age age2 numbersons numberdaughters, cluster(cid) baselevels


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.male#1.has_daughter]

		* (ii) standard error
		local se = _se[1.male#1.has_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value (one-sided test)
		local p_value = _b[1.male#1.has_daughter] / _se[1.male#1.has_daughter]
		if _b[1.male#1.has_daughter] > 0 {
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
		scalar coef_d_daughter = _b[1.male#1.has_daughter]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
