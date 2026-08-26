********************************************************************************
************************** H2: #ManyDaughters Project **************************
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

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
/* REMARKS: 

1. The provided data set pl does not include the variable pla0009_h although it was listed on paneldata.org as being part of the data set. We use variables pla0009_v2 and pla0009_v3 to construct pla0009_h ourselves. 

2. Our pre-analysis plan did not specify how we deal with cases where individuals provided inconsistent responses for their year of birth or sex. In this code, we set the corresponding variable to missing if an individual provided inconsistent responses across the years. 

*/


* GENERATE VARIABLES * 

	* Generate dummy variable for whether participant has a first-born daughter (1) or first-born son (0)
	gen d_daughter = kidsex01==2
	replace d_daughter=. if kidsex01<1 | kidsex01==. // set invalid responses to missing
	
	* Generate variable for individual being male based on plh0009 (considering sex observations across years)
	gen pla0009_h = pla0009_v2
	replace pla0009_v3 = -2 if  pla0009_v3==3
	replace pla0009_h = pla0009_v3 if syear>=2019
	replace pla0009_h=. if pla0009_h<0 // set invalid responses to missing
	bysort pid: egen sex_pla = mean(pla0009_h)
	gen male = sex_pla==1
	replace male = . if sex_pla!=1&sex_pla!=2 // replace with missings if inconsistent responses across years 
	
	* Generate variable for year of first observation of plh0004 (input for outcome variable) after first child was born
	gen after = syear if syear>kidgeb01 & kidgeb01>0 & plh0004>=0 & plh0004!=.
	bysort pid: egen first = min(after)
	drop after
	
	* Generate categorical variable for age in year of first relevant observation (considering year of birth observations across years)
	replace ple0010_h=. if ple0010_h<0
	bysort pid: egen yearofbirth_min = min(ple0010_h)
	bysort pid: egen yearofbirth_max = max(ple0010_h)
	gen yearofbirth = yearofbirth_max if yearofbirth_min==yearofbirth_max // results in missing if inconsistent responses across years
	gen age_first = first-yearofbirth
	gen age_cat = 1 if age_first>=18 & age_first<=24
	replace age_cat = 2 if age_first>=25 & age_first<=30
	replace age_cat = 3 if age_first>=31 & age_first<=40
	replace age_cat = 4 if age_first>=41 & age_first<=50
	replace age_cat = 5 if age_first>=51 & age_first<=64
	replace age_cat = 6 if age_first>=65 & age_first!=.
	
	* Generate outcome variable
	replace plh0004=. if syear<first | plh0004<0
	bysort pid: egen political = mean(plh0004) 
	

* FILTERS * 
	
	* Exclude participants who have a missing value in one of the regressors or outcome variable (which also excludes individuals below 18 based on age variable and individuals without children)
	keep if d_daughter!=. & male!=. & age_cat!=. & political!=. & first!=.
	
	* Exclude participants without cid 
	keep if cid!=. 
	
	* Keep only one row per individual
	keep if syear==first

	
* HYPOTHESIS TEST FOR H2 *

	* Description: We conduct an OLS regression, where we regress the outcome variable (political leaning) on a dummy for whether the first-born child is female. We additionally include a dummy for whether the respondent is male, a categorical variable for the age of the respondent in the survey year of the first considered observation of plh0004 (i.e., the first observation after the birth of the first child), and a dummy for the year of the first considered observation of plh0004 as regressors. We cluster standard errors on the household level. To test the hypothesis, we test whether we can reject that the coefficient of the dummy for the first-born child being female is greater than or equal to zero at the 5% significance level (based on a one-sided t-test).

	reg political d_daughter male i.age_cat i.first, vce(cluster cid)


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
		local p_value = 1-ttail(e(df_r), `p_value')

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
		local interpretation "negative coefficient is in line with hypothesis"
		
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation, remark" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation',`remark'" _n
	file close myfile


********************************************************************************

exit
