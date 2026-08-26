********************************************************************************
******************   Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
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
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) 

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
*only keep Year 2019
keep if syear==2019


*only keep children born before the interview
keep if syear>kidgeb01 & kidgeb01>0


* GENERATE VARIABLES * (Please adapt according to your needs)
set seed 5016

***OUTCOME
**gender equality measure
* higher values refer to more egalitarian attitudes
gen PP=plh0004  
recode PP(-99/-1=.)
tab PP

***Regressor of interest
*Having a daughter - only daughters and sons, no people without children
gen daughter=.
replace daughter=1 if kidsex01==2
replace daughter=0 if kidsex01==1

***drop individuals with multiple children
*variable for second birth should be "-2 does not apply" if couples only have one children
keep if kidsex02==-2

***Control variables
*Year of birth
gen birthyear=ple0010_h 
replace birthyear=. if ple0010_h <0

**Gender of the parent
*binary indicator for being male
gen male=.
replace male=1 if sex==1
replace male=0 if sex==2

**Birth year of the child
gen birthyear_child=kidgeb01
replace birthyear_child=. if kidgeb01<0

**Marital status
gen famstat=pgfamstd
replace famstat=. if pgfamstd<0

**Education
gen yeduc=pgbilzeit
replace yeduc=. if pgbilzeit<0



* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)
/*
Verbal description: 
We regress the left-right-scale (PP) on having a daughter (daughter) and
a set of covariates. The coefficient for daughter is the effect of interest and
shows the effect of having a daughter on the left-right-scale compared to 
individuals who have sons. A smaller value, i.e. a negative coefficient, means 
being more left-leaning.

*/
global controls "i.male c.yeduc i.famstat i.birthyear i.birthyear_child"

reg PP daughter $controls, r


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[daughter]

		* (ii) standard error
		local se = _se[daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[daughter] / _se[daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_daughter = _b[daughter]
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
