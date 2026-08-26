********************************************************************************
********************** Do-File H2: #ManyDaughters Project **********************
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
	
	save "$DATA/h2_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* FILTERS *
 
	* Reduce sample to private households
	keep if inlist(pop,1,2)
	drop if netto != 10 
	
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
		recode daughter (. = 0) 
		
	* LEFT-LEANING POLITICAL PREFERENCES (AVAILABLE IN 2005, 2009, 2014 & 2019)*

	tab plh0004, m
	recode plh0004 (-8 -1 = .)
	
	gen moreleft = 10-plh0004 // reverse item so that higher values indicate increasing left-wing preference
	lab def left 10"fully left-wing" 0"fully right-wing"
	lab val moreleft left
	
	* CONTROLS *

	gen age = syear - gebjahr 
	replace age = . if age < 18 
	
	recode sex (1=1) (2=0), into(male)
	
	*REDUCE DATASET
	keep pid syear male age daughter moreleft phrf
		
	drop if moreleft == .
	
	bys pid: gen nowaves = _N
	tab nowaves
	drop if nowaves == 1
	
	sort pid syear
	bys pid: gen leftdiff = moreleft[_n] - moreleft[_n-1]
	recode leftdiff (min/-6 =.) (6/max = .)
		
	* FILTER FOR VARIABLES
	gen filter = 0
	replace filter = 1 if leftdiff != . & daughter != . & male != . & age != .
	
* HYPOTHESIS TEST FOR H2 *

	****************************************************************************
	*Verbal description of the model:	
	* Respondents' self-reported political attitude (variable: plh0004, 
	* collected in 2005, 2009, 2014, and 2019).
	* is used to measure left-leaning political preferences (hypothesis 2). 
	* The original item is reversed so that higher values indicate stronger 
	* left-leaning political preferences.
	
	* Accordingly, we run a linear OLS regression analysis with the change score 
	* of left-leaning political preferences (variable: leftdiff) 
	* as the dependent variable, and the dummy variable daughter as the 
	* independent variable.
	* We further control for age, sex, and survey year (variable: syear) 
	* in the model. 
	* We estimate cluster-robust standard errors (cluster variable: pid). 
	* The regression is weighted (weight: phrf).
	****************************************************************************
	
	reg leftdiff i.daughter male age i.syear if filter == 1 [aw=phrf], cluster(pid)

* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.daughter]

		* (ii) standard error
		local se = _se[1.daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.daughter] / _se[1.daughter]
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
