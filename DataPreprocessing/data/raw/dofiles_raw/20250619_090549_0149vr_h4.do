********************************************************************************
****************** Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup
* ============================

* clear all
version 14 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

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
	keep pid hid syear sex gebjahr pop
	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(pid sumkids kidgeb* kidsex*)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pid syear pgfamstd pgisced97 pglfs)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pid syear plh0308* plh0395i05)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

use "$DATA/h4_dataset.dta", clear

* FILTERS
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES
	
	
	* setting all missing values to missing
	qui mvdecode _all, mv(-1/-10)
	
	* generate a variable capturing the number of children
	
	forvalues x=1/9 {
		rename kidgeb0`x' kidgeb`x'
		}
		
	gen nchildren=0 if sumkids==0
	replace nchildren=0 if syear<kidgeb1
	forvalues x=1/19 {
		replace nchildren=`x' if syear>=kidgeb`x'
		}
	drop kidgeb*

	* generate a variable capturing the number of daughters (Key explanatory variable)
	
	forvalues x=1/9 {
		rename kidsex0`x' kidsex`x'
		}
		
	label define label_kidsex 0 "male" 1 "female"
	forvalues x=1/19 {
		replace kidsex`x' = kidsex`x'-1
		label values kidsex`x' label_kidsex
		}
	
	forvalues x=1/19 {
		replace kidsex`x' = . if nchildren<`x'
		}
	
	egen ndaughters = rowtotal(kidsex*), missing
	replace ndaughters = 0 if nchildren==0
	
	drop kidsex*
	
	* generate a variable capturing support for gender equality (Outcome variable)
	
	label define agree 0 "no" 1 "yes"
	recode plh0308_v2 (1/5 = 0) (6/7 = 1)
	label values plh0308_v2 agree
	recode plh0308_v1 (2/4 = 0)
	label values plh0308_v1 agree
	recode plh0395i05 (1/5 = 0) (6/7 = 1)
	label values plh0395i05 agree

	egen sge = rowtotal(plh0308_v1 plh0308_v2 plh0395i05), missing

	
	* Control Variables
	*******************
	
	* Marital status
	label define label_famst 1 "married" 2 "single" 3 "seperated/divorced" 4 "widowed"
	recode pgfamstd (1 6 7 = 1) (3 = 2) (2 4 8 = 3) (5 = 4)	
	label values pgfamstd label_famst
	
	* Education
	label define label_ed 1 "less than secondary" 2 "secondary" 3 "higher"	
	recode pgisced97 (0/2 = 1) (3/4 = 2) (5/6 = 3)
	label values pgisced97 label_ed
	
	* Labor force status
	label define label_lfs 1 "employed" 2 "not employed" 3 "unemployed"
	recode pglfs (min/0 = .)  (1/5 8/10 13 = 2) (6 = 3) (11 12 = 1)
	label values pglfs label_lfs
	
	* Age and age squared 
	gen age = syear - gebjahr
	gen age_sq = age^2
	
	* Household size
	bys hid syear: egen hhsize = count(pid) 

	
* HYPOTHESIS TEST FOR H4 

	* We estimate an OLS regresssion with our constructed indicator of support for gender equality (sge) as the dependent variable on the interaction effect between the number 
	* of daughters and sex of the respondent as well as further control variables and their interactions with sex (number of children, marital status, age, age squared,
	* household income, household size, education, labor force status and the survey year).
	* We calculate the marginal effects of the number of daughters on the probability of reporting strong support for gender equality for men and women separately 
	* and determine their difference.
	* A positive difference of the marginal effect between fathers and mothers (with a p-value of less than 0.1) supports H4.

	reg sge c.ndaughters##i.sex i.nchildren##i.sex i.pgfamstd##i.sex c.age##i.sex c.age_sq##i.sex ///
		c.hghinc##i.sex c.hhsize##i.sex i.pgisced97##i.sex i.pglfs##i.sex i.syear##i.sex	
		
	margins, dydx(ndaughters) over(sex) post
	lincom 1.sex-2.sex


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = r(estimate)

		* (ii) standard error
		local se = r(se)

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		
		local p_value = r(estimate) / r(se)
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = `coefficient'
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
