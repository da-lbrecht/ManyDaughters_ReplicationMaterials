********************************************************************************
******************       Do-File H4: #ManyDaughters Project   ******************
********************************************************************************

* Authors: 

* ============================
* Setup * (Please adapt according to your needs)
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

di "$ROOT"

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
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgisced11 pgfamstd pgemplst)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plj0621 plj0622 plj0623 plj0624 plj0625 pla0009_v2)
	
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hgnuts1 hgnuts1_ew)

	* hl contains variables on household level from the household interview;
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) keepusing(hid syear hlc0005_h hlf0019_h hlk0006)

	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
	**** CREATE OUTCOME VARIABLE ****	

	mvdecode plj0621 plj0622 plj0623 plj0624 plj0625, mv(-8/-1 = .)
	drop if missing(plj0621, plj0622, plj0623, plj0624, plj0625)

	* reverse coding 
	gen plj0623_r = 8 - plj0623  // undid reverse coding
	gen plj0624_r = 8 - plj0624  // undid reverse coding
	gen plj0625_r = 8 - plj0625  // undid reverse coding

	** factor analysis 
	pwcorr plj0621 plj0622 plj0623_r plj0624_r plj0625_r

	factor plj0621 plj0622 plj0623_r plj0624_r plj0625_r, factors(1)
	predict gender_equality
	
	
	*** CREATE TREATMENT VARIABLE ***
	* Does a row have a daughter
	gen d_daughter = 0
	foreach var of varlist kidsex01-kidsex19 {
	    replace d_daughter = 1 if `var' == 2
	}

	* Does have kids?
	gen has_kids = (sumkids > 0)

	*** CREATE CONTROL VARIABLES

	* Gender
	gen male = (pla0009_v2 == 1)

	** Age
	gen age = syear - gebjahr

	* Married 
	mvdecode pgfamstd, mv(-9/-1 = .)

	gen married = 1 if pgfamstd==1 | pgfamstd==6 | pgfamstd==7 | pgfamstd==8
	replace married = 0 if inrange(pgfamstd, 2, 5)

	* highest education
	mvdecode pgisced11, mv(-9/-1 = .)

	* employment status
	mvdecode pgemplst, mv(-9/-1 = .)
	gen labor_force_status = .
	label define labor_lbl 1 "Full-Time" 2 "Part-Time" 3 "Not in Labor Force"

	replace labor_force_status = 1 if pgemplst == 1
	replace labor_force_status = 2 if inlist(pgemplst, 2, 3, 4)
	replace labor_force_status = 3 if inlist(pgemplst, 5, 6)

	* income category
	gen income_cat = .

	replace income_cat = 1 if hlc0005_h < 1000
	replace income_cat = 2 if inrange(hlc0005_h, 1000, 1999)
	replace income_cat = 3 if inrange(hlc0005_h, 2000, 2999)
	replace income_cat = 4 if inrange(hlc0005_h, 3000, 3999)
	replace income_cat = 5 if inrange(hlc0005_h, 4000, 4999)
	replace income_cat = 6 if inrange(hlc0005_h, 5000, 5999)
	replace income_cat = 7 if inrange(hlc0005_h, 6000, 6999)
	replace income_cat = 8 if inrange(hlc0005_h, 7000, 7999)
	replace income_cat = 9 if inrange(hlc0005_h, 8000, 8999)
	replace income_cat = 10 if inrange(hlc0005_h, 9000, 9999)
	replace income_cat = 11 if hlc0005_h >= 10000


	* East/West Germany
	mvdecode hgnuts1_ew, mv(-9/-1 = .)
	
	* Number of people in household
	* We are approx-ing this with hlk0006 (highest person number) since ///
	  hgnrpers is not included in the data 
	mvdecode hlk0006, mv(-9/-1 = .)  
	
	
* FILTERS *
	* Drop 99th percentile families size
	tabulate hlk0006
	** Drop 99th percentile
	* Step 1: Get the 99th percentile
	summarize hlk0006, detail
	* Step 2: Use the percentile cutoff to drop values above it
	drop if hlk0006 > r(p99)
	
	* Drop child-less families 
	drop if has_kids == 0

	tabulate has_kids
	tabulate d_daughter
	
	* Drop missing values
	drop if missing(hlk0006, hgnuts1_ew, income_cat, labor_force_status, ///
		pgisced11, married, age, male)
		
	* Only private households
	keep if pop==1 | pop==2
	
	* Tables after filters
	tabulate has_kids
	tabulate d_daughter
	
* FILTERS *
	* Drop 99th percentile families size
	tabulate hlk0006
	** Drop 99th percentile
	* Step 1: Get the 99th percentile
	summarize hlk0006, detail
	* Step 2: Use the percentile cutoff to drop values above it
	drop if hlk0006 > r(p99)
	
	* Drop child-less families 
	drop if has_kids == 0

	tabulate has_kids
	tabulate d_daughter
	
	* Drop missing values
	drop if missing(hlk0006, hgnuts1_ew, income_cat, labor_force_status, ///
		pgisced11, married, age, male)
		
	* Only private households
	keep if pop==1 | pop==2
	
	tabulate has_kids
	tabulate d_daughter

* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)

	* Estimate the effect of having a daughter on gender equality attitudes using OLS,
	* allowing for heterogeneity by respondent gender through an interaction between 
	* daughter status (i.d_daughter) and respondent sex (i.male).
	*
	* The outcome variable, gender_equality, is a factor score derived from multiple SOEP attitude items
	* using principal factor analysis. Items were reverse-coded where appropriate so that higher values
	* reflect more egalitarian gender views.
	*
	* The treatment variable i.d_daughter is a binary indicator equal to 1 if the respondent has 
	* at least one daughter. Identification leverages the assumption that, conditional on covariates,
	* the presence of a daughter is as-good-as-random—treated as a natural experiment based on the 
	* quasi-random assignment of child sex.
	*
	* Control variables include age, marital status, education level (PGISCED11), household income 
	* (in 1000€ bands), labor force status, region (NUTS1_ew), and household size (hlk0006).
	* Standard errors are clustered at the household (hid) level.
	reg gender_equality i.d_daughter##i.male ///
		age i.married i.pgisced11 ///
		i.income_cat i.labor_force_status ///
		i.hgnuts1_ew hlk0006, vce(cluster hid)

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.d_daughter#1.male] 

		* (ii) standard error
		local se = _se[1.d_daughter#1.male]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.d_daughter#1.male] / _se[1.d_daughter#1.male]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.d_daughter#1.male]
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
