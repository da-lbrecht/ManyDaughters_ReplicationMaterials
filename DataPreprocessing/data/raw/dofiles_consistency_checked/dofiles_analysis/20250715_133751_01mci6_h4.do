********************************************************************************
********************** Do-File H4: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

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
* Data Preparation 			 * 
* ============================

* LOAD AND MERGE DATA *

	use "$IN/ppathl.dta", clear
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)

	label language EN // change language of the labels to English
	
	save "$DATA/h4_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* FILTERS *
 
	* Reduce sample to private households
	keep if inlist(pop,1,2)
	drop if netto != 10 
	drop if syear < 2006 // inglehart not available before
	
* GENERATE VARIABLES *

	* GENERATE DAUGHTER VARIABLE *	
			
		foreach v in kidgeb01 kidgeb02 kidgeb03 kidgeb04 kidgeb05 kidgeb06 ///
		kidgeb07 kidgeb08 kidgeb09 kidgeb10 kidgeb11 kidgeb12 kidgeb13 kidgeb14 ///
		kidgeb15 kidgeb16 kidgeb17 kidgeb18 kidgeb19 {
			gen cnt_`v' = 1 if `v' <= syear & `v' > 0
		}
				
		foreach var of varlist kidsex01-kidsex19 {	
			recode `var' (-2/1=0) (2=1), into(daughter`var')
		}
		
		foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
			replace cnt_kidgeb`x' = cnt_kidgeb`x' * daughterkidsex`x' 
		}

		egen daughter = rowmean(cnt_kidgeb01-cnt_kidgeb19)
		recode daughter (0 = 0) (0.01/1 = 1)
		recode daughter (.=0)
		
	* SUPPORT FOR GENDER EQUALITY AS MEASURED BY ATTITUDES - INGLEHART INDEX (AVAILABLE IN 2006 & 2016) *

	tab1 plh0054 plh0056 plh0058 plh0061 , m
	recode plh0054 plh0056 plh0058 plh0061 (-8 -1 = .)
	
	recode plh0056 plh0061 (1=1) (2=0.5) (3/4 = 0), into(pmat1 pmat2)
	gen inglehart = pmat1 + pmat2
	recode	inglehart (2=1.5) // correct for interviewer mistakes; respondent chose 2 first important instead of first and second most important
	
	* CONTROLS *

	gen age = syear - gebjahr 
	replace age = . if age < 18 
	
	recode sex (1=1) (2=0), into(male)
	
	recode syear (2006=0) (2016=1), into(year01)
	
	* FILTER FOR VARIABLES
	gen filter = 0
	replace filter = 1 if inglehart != . & daughter != . & male != . & age != .
	
	*REDUCE DATASET
	keep pid syear male age daughter inglehart filter year01 phrf

	drop if inglehart == .
	
	bys pid: gen nowaves = _N
	tab nowaves
	drop if nowaves == 1	
	
	sort pid syear
	bys pid: gen inglediff = inglehart[_n+1] - inglehart[_n]
	
* HYPOTHESIS TEST FOR H4 *

	****************************************************************************
	*Verbal description of the model:	
	* Postmaterialistic attitudes (variables: plh0056 and plh0061 
	* from the Inglehart index, collected in 2006 and 2016) 
	* are used as a proxy for individual support for gender equality 
	* because postmaterialist values are strongly and positively 
	* correlated with support for gender equality: if postmaterialist values are 
	* stronger in 2016 than in 2006, this indicates a stronger support for 
	* gender equality.
	
	* Accordingly, we run a linear OLS regression analysis with the change score 
	* of postmaterialist values between 2006 and 2016 (variable: inglediff) 
	* as dependent variable and the dummy variables daughter and sex (variable: 
	* male) as independent variables. 
	* To test whether having daughters leads to a larger increase in the support 
	* of gender equality for men than for women (hypothesis 4), we include an 
	* interaction between the variables daughter and male. 
	* We further control for age in the model. 
	* We estimate robust standard errors. 
	* The regression is weighted (weight: phrf).
	****************************************************************************

	reg inglediff i.daughter##i.male age if filter == 1 [aw=phrf], vce(r)
	
* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.daughter#1.male]

		* (ii) standard error
		local se = _se[1.daughter#1.male]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.daughter#1.male] / _se[1.daughter#1.male]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
