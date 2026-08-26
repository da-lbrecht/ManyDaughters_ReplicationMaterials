********************************************************************************
************************** H4: #ManyDaughters Project **************************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 16.1 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
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

* LOAD AND MERGE DATA *

	use "$IN/pl.dta", clear // use pl as a starting dataset
	
	* Merge pl with biobirth, which contains information on fertility history	
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex01 kidgeb01) 
	
	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
/* REMARKS: 

1. The provided data set pl does not include the variable pla0009_h although it was listed on paneldata.org as being part of the data set. We use variables pla0009_v2 and pla0009_v3 to construct pla0009_h ourselves. 

2. Our pre-analysis plan did not specify how we deal with cases where individuals provided inconsistent responses for their year of birth or sex. In this code, we set the corresponding variable to missing if an individual provided inconsistent responses across the years. 

*/


* GENERATE VARIABLES Part I* (Regressors)

	* Generate dummy variable for whether participant has a first-born daughter (1) or first-born son (0)
	gen d_daughter = kidsex01==2
	replace d_daughter=. if kidsex01<1 | kidsex01==. // Set invalid responses to missing
	
	* Generate variable for individual being male based on plh0009 (considering sex observations across years)
	gen pla0009_h = pla0009_v2
	replace pla0009_v3 = -2 if  pla0009_v3==3
	replace pla0009_h = pla0009_v3 if syear>=2019
	replace pla0009_h=. if pla0009_h<0 // Set invalid responses to missing
	bysort pid: egen sex_pla = mean(pla0009_h)
	gen male = sex_pla==1
	replace male = . if sex_pla!=1&sex_pla!=2 // Replace with missings if inconsistent responses across years 
	
	* Generate categorical variable for age in 2012 (considering year of birth observations across years)
	replace ple0010_h=. if ple0010_h<0 // Set invalid responses to missing
	bysort pid: egen yearofbirth_min = min(ple0010_h)
	bysort pid: egen yearofbirth_max = max(ple0010_h)
	gen yearofbirth = yearofbirth_max if yearofbirth_min==yearofbirth_max // Results in missing if inconsistent responses across years
	gen age_2012 = 2012-yearofbirth
	gen age_cat = 1 if age_2012>=18 & age_2012<=24
	replace age_cat = 2 if age_2012>=25 & age_2012<=30
	replace age_cat = 3 if age_2012>=31 & age_2012<=40
	replace age_cat = 4 if age_2012>=41 & age_2012<=50
	replace age_cat = 5 if age_2012>=51 & age_2012<=64
	replace age_cat = 6 if age_2012>=65 & age_2012!=.


* FILTERS *
 
	* Exclude participants who have a missing/invalid value for one of the variables that are used to construct the outcome variable below
	keep if plh0301>0 & plh0301!=. & plh0303>0 & plh0303!=. & plh0307>0 & plh0307!=.
	
	* Exclude participants with missing values in regressors (which also excludes individuals below 18 based on age variable and individuals without children)
	keep if d_daughter!=. & male!=. & age_cat!=.
	
	* Exclude participants without hid
	keep if hid!=.

	* Exclude participants whose first-born child was born in 2012 or later
	keep if kidgeb01<2012 & kidgeb01>0
	
	
* GENERATE VARIABLES Part II* (Outcome variable; generate here since standardization uses filtered sample)
	
	* Generate outcome variable based on plh0301, plh0303, and plh0307
	gen plh0303_reverse = 5-plh0303 // Reverse code variable such that higher values mean more gender equality
	foreach var in plh0301 plh0303_reverse plh0307 { // Standardize input variables
		gen base = `var' if d_daughter==0
		egen mbase = mean(base)
		egen sdbase = sd(base)
		replace `var' = (`var'-mbase)/sdbase
		drop base mbase sdbase
	}
	gen attitude = (plh0301 + plh0303_reverse + plh0307)/3 // Create outcome variable



* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)

	* Description of model: We conduct an OLS regression, where we regress the outcome variable (gender equality attitudes) on a dummy for whether the first-born child is female, a dummy for whether the respondent is male, and their interaction. We additionally include a categorical variable for the age of the respondent in 2012 as regressors. We cluster standard errors on the household level. To test the hypothesis, we test whether we can reject that the coefficient of the interaction term is smaller than or equal to zero at the 5% significance level (based on a one-sided t-test).

	reg attitude d_daughter##male i.age_cat, vce(cluster hid)


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
		local p_value = ttail(e(df_r), `p_value')

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.d_daughter#1.male]
		local interpretation "positive coefficient is in line with hypothesis"
		
		* (iv) check that sample size does not fall below pre-specified minimum 
		if `num_participants' < 1500 {
			local remark "DO NOT INTERPRET RESULTS SINCE EFFECTIVE SAMPLE SIZE IS BELOW PRE-SPECIFIED MINIMUM (1500)"
		} 
		else {
			local remark ""
		}

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
	di "Remark: `remark'"
	
	* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation, remark" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation',`remark'" _n
	file close myfile


********************************************************************************

exit
