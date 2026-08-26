********************************************************************************
******************  Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
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

* LOAD AND MERGE DATA 

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

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h1_dataset.dta", clear

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)
	
	
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
	label define label_famst 1 "married" 2 "single" 3 "separated/divorced" 4 "widowed"
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
	
* HYPOTHESIS TEST FOR H1 
	
	* We estimate an OLS regression with our constructed indicator of support for gender equality (sge) as the dependent variable on the number of daughters (ndaughters)
	* as the treatment variable and further control variables (number of children, marital status, sex, age, age squared, household income, household size education
	* labor force status and the survey year. A positive coefficient on the number of daughters (with a p-value smaller than 0.1) empirically supports H1.

	reg sge ndaughters i.nchildren i.pgfamstd i.sex age age_sq hghinc hhsize i.pgisced97 i.pglfs i.syear  

* EXTRACT AND EXPORT RESULTS

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[ndaughters]

		* (ii) standard error
		local se = _se[ndaughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		
		local p_value = _b[ndaughters] / _se[ndaughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[ndaughters]
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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
