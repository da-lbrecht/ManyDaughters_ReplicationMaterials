********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
version 16.1 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2

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

* LOAD AND MERGE DATA *

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
	*merge


* ============================
* Analysis
* ============================

* FILTERS *

*** 1. Keep respondents with a biological child ***
	*keep if biochild_h == 1 // Deviation from PAP: Variable does not exist in dataset, instead use any birth 
	keep if sumkids > 0 	
	
*** 2. Generate independent variable ***
	*gen has_daughter = 0
*replace has_daughter = 1 if ///
*( pld0175 == 2 | pld0183 == 2 | pld0187 == 2 | pld0191 == 2 | ///
*pld0195 == 2 | pld0199 == 2 | pld0203 == 2)

gen has_daughter = 0
foreach var of varlist kidsex01-kidsex19 {
    replace has_daughter = 1 if `var' == 2
}

*** 3. Generate control variables ***
	*PAP: gen num_children = d11107 if d11107 >0
	gen num_children = sumkids
	
	gen youngest_child_age = pla0003
	gen parent_age = syear - gebjahr if syear >0 & gebjahr > 0

* too many missing values in pgisced11, use pgpsbil instead
	clonevar parent_education = pgpsbil if pgpsbil >0
	gen parent_gender = sex
	*PAP: xtile income_quartile = hghinc , nq (4) if hghinc>0
	replace hghinc = . if hghinc <0
	xtile income_quartile = hghinc , nq (4)

	
*** 1. Generate dependent variables ***

* Clean variables - replace all negative responses (does not apply, etc) with missing
foreach var of varlist plh0301 plh0357 plj0611 plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 {
    replace `var' = . if `var' < 0 
}

* A. gender equality defined by same rights
clonevar gender_attitude1 = plj0620 if plj0620 > 0 

* B. gender equality as principal components of gender equality attitudes
*keep if plj0611 > 0 & plj0621 > 0 & plj0622 > 0 & plj0623 > 0 & plj0624 > 0 & plj0625 > 0

* Invert items with opposite meaning

gen plj0623_inv = 7 - plj0623
gen plj0624_inv = 7 - plj0624
gen plj0625_inv = 7 - plj0625

* Standardize variables
egen plj0621_std = std(plj0621)
egen plj0622_std = std(plj0622)
egen plj0623_inv_std = std(plj0623_inv)
egen plj0624_inv_std = std(plj0624_inv)
egen plj0625_inv_std = std(plj0625_inv)

* Principal component analysis
pca plj0621_std plj0622_std ///
   plj0623_inv_std plj0624_inv_std plj0625_inv_std

* Generate first principal component
predict pc1, score
rename pc1 gender_attitude2


* HYPOTHESIS TEST FOR H4 * 


* keep relevant year
keep if syear == 2016


* Regression model
gen female = (sex == 2)
gen female_with_daughter = has_daughter * female


reg gender_attitude2 has_daughter female female_with_daughter i.num_children c.parent_age c.youngest_child_age i.parent_education i.parent_gender i.income_quartile, robust


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[female_with_daughter]

		* (ii) standard error
		local se = _se[female_with_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[female_with_daughter] / _se[female_with_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_female_with_daughter = _b[female_with_daughter]
		local interpretation "negative coefficient is in line with hypothesis"

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
