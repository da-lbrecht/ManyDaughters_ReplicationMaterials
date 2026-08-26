********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************


* ============================
* Setup * 
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

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

	save "$DATA/h3_dataset.dta", replace // save merged dataset

	
* ============================
* Generate Variables
* ============================

* FILTERS * (Please adapt according to your needs)
 
* only keep participants from private households
keep if inlist(pop,1,2)

*------------
* GENERATE CONTROLS
* compute age by birth year, as the age variable is missing for around 8k observations
gen age =  syear - gebjahr
gen age2 =  age^2
// the "bioage" variable misses some values

* male
gen male = 0
replace male = 1 if sex == 1

*------------
* GENERATE TREATMENT VARIABLE

* generate variable to describe if individual has a daughter and how many
gen num_daughters = 0
foreach i of numlist 1/9 {
	// https://paneldata.org/soep-core/datasets/biobirth/kidsex01
    replace num_daughters = num_daughters + 1 if kidsex0`i' == 2 & syear >= kidgeb0`i'
	// the second if-condition ensures that the child has already been born when the respondent is asked
}
foreach i of numlist 0/9 {
    replace num_daughters = num_daughters + 1 if kidsex1`i' == 2 & syear >= kidgeb1`i'
}

* generate variable to describe if individual has a son and how many
gen num_sons = 0
foreach i of numlist 1/9 {
    replace num_sons = num_sons + 1 if kidsex0`i' == 1 & syear >= kidgeb0`i'
	// the second if-condition ensures that the child has already been born when the respondent is asked
}
foreach i of numlist 0/9 {
    replace num_sons = num_sons + 1 if kidsex1`i' == 1 & syear >= kidgeb1`i'
}

* generate variables
gen num_children = num_sons + num_daughters
gen num_children2 = num_children ^ 2
gen d_daughter = 0
replace d_daughter = 1 if num_daughters > 0 & num_daughters != . 
gen d_son = 0
replace d_son = 1 if num_sons > 0 & num_sons != .

* Quality Check: the two variables almost perfectly align. For consistency, we stick to our own variable
tab sumkids num_children // https://paneldata.org/soep-core/datasets/biobirth/


*------------
* GENERATE OUTCOME VARIABLE
* for the outcome variable, we compute the number of hours in household work women
* pursure on an average day (including week days and the weekend)

gen hhwork_sat = pli0012_h
gen hhwork_sun = pli0016_h
gen hhwork_week = pli0043_h

replace hhwork_sat = . if hhwork_sat < 0 | hhwork_sat == 24
replace hhwork_sun = . if hhwork_sun < 0 | hhwork_sun == 24
replace hhwork_week = . if hhwork_week < 0 | hhwork_week == 24 // replaces one outlier that reports to do house work 24/7

gen hhwork = 5/7*hhwork_week + 1/5*hhwork_sat + 1/5*hhwork_sun


* ============================
* Analysis
* ============================


* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)
* for panel regressions:
* here, we regress the number of hours worked in the household by female respondents on the 
* number of daughters and controls in a fixed-effects panel data regression. A negative value suggests that 
* having daughters is associated with women spending less time on housework on an average day.

xtset pid syear
xtreg hhwork num_daughters i.num_children age age2 i.syear if num_children > 0 & male == 0, fe vce(cluster pid)


	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[num_daughter]

		* (ii) standard error
		local se = _se[num_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[num_daughter] / _se[num_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_num_daughter = _b[num_daughter]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile




********************************************************************************

exit
