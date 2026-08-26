********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
// Edited by David Albrecht for reproducibility check
/* if strpos("C:/Users/Giorgia/Desktop/TCD/TCD/#ManyDaughters_RT_AnalysisPackage/#ManyDaughters_RT_AnalysisPackage", "code") {
    global ROOT "C:/Users/Giorgia/Desktop/TCD/TCD/#ManyDaughters_RT_AnalysisPackage/#ManyDaughters_RT_AnalysisPackage/.."
}
else {
    global ROOT "C:/Users/Giorgia/Desktop/TCD/TCD/#ManyDaughters_RT_AnalysisPackage/#ManyDaughters_RT_AnalysisPackage"
}
*/
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

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
use "$DATA/h2_dataset.dta", clear 

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)


*1. Main Dependent Variable: Political Attitudes 

/* Use the variable: plh0004, Political Attitude: that goes from far left (0) to far right (10).
 First, clean the variable imputing missing if "Question this year not part of the survey [-8]",
 "Not included in this version of the questionnaire [-5]", "No answer [-1]".
 -1 is left, + 1 is right now. 
*/
 
gen clean_plh0004 = plh0004
replace clean_plh0004 =. if  plh0004 < 0
egen      political_score = std(-clean_plh0004)
label var political_score "Std. political prefs (higher = more left)"


**2. Main Dependent Variable: Generate d_daughter 

sort pid syear 
* Recode the gender for each kids of the respondent
forvalues i = 1/19 {
    local ii : display %02.0f `i'
    gen    kidgender`ii' = kidsex`ii'
    recode kidgender`ii' (1=0)
    recode kidgender`ii' (2=1)
	recode kidgender`ii' (-1=.)
	recode kidgender`ii' (-2=.)

}


* Recode the yob for each kids of the respondent
forvalues i = 1/19 {
    local ii : display %02.0f `i'
    gen    kidbirth`ii' = kidgeb`ii'
	recode kidbirth`ii' (-2=.)

}

* Create a dummy = 1 when the yob of the kid is = to the year of the survey
forvalues i = 1/19 {
    local ii : display %02.0f `i'
    gen     kid_birth_dummy`ii' = 0
	replace kid_birth_dummy`ii' = 1 if syear == kidbirth`ii'

}

* Gen an absorbing variable that takes value 1 each kids was born

forvalues i = 1/19 {
    local ii : display %02.0f `i'
	bys pid: gen forward`ii' = (sum(kid_birth_dummy`ii') > 0)
    bys pid:replace  forward`ii' =. if sumkids ==0 
}

* Gen an absorbing variable that takes value 1 each daughter was born 

forvalues i = 1/19 {
    local ii : display %02.0f `i'
	bys pid: gen forward_daughter`ii' = (sum(kid_birth_dummy`ii') > 0) & kidgender`ii'==1
	bys pid: replace forward_daughter`ii' = 1 if kidbirth`ii'<syear & kidgender`ii'==1
	bys pid: replace forward_daughter`ii' = . if sumkids==0

}

forvalues i = 1/19 {
    local ii : display %02.0f `i'
	bys pid: gen forward_son`ii' = (sum(kid_birth_dummy`ii') > 0) & kidgender`ii'==0
	bys pid: replace forward_son`ii' = 1 if kidbirth`ii'<syear & kidgender`ii'==0
	bys pid: replace forward_son`ii' = . if sumkids==0

}




* Gen Whether the respondent has a son or not. 

forvalues i = 1/19 {
    local ii : display %02.0f `i'
    gen kidgender_2`ii' = kidsex`ii'
    recode kidgender_2`ii' (1=1)
    recode kidgender_2`ii' (2=0)
	recode kidgender_2`ii' (-1=.)
	recode kidgender_2`ii' (-2=.)

}






* Generate a time invariant daughter/son dummy

egen sum_daughters = rowtotal(kidgender*), missing


gen     daughter =. 
replace daughter = 0 if sum_daughters ==0
replace daughter = 1 if sum_daughters >0 & sum_daughters!=. 



* as for daughter, create a dummy which is equal to 1 if there is at least one son
egen sum_sons = rowtotal(kidgender_2*), missing

gen     son =. 
replace son = 0 if sum_sons ==0
replace son = 1 if sum_sons >0 & sum_sons!=. 


* Generate a time variant daughter dummy


egen    d_daughter = rowmax(forward_daughter*)
replace d_daughter=. if daughter ==.

egen     d_son= rowmax(forward_son*)
replace d_son=. if son ==.


* Gen a time variant kids variable 

egen  kids_variant = rowtotal(forward_daughter* forward_son*)

* Other Controls

* Recode No answer and Not included in the survey as missing. 	
gen     marital_status = pgfamstd
replace marital_status =. if  pgfamstd ==-1
replace marital_status =. if  pgfamstd ==-3
replace marital_status =. if  pgfamstd <0

replace corigin =. if  corigin <0


* Gen Age and Age Square 	
	
gen age = syear - gebjahr

gen age_sq = age*age 

* Recode yrs of education--> pgbilzeit: -2 and -1 coded as missing. 

gen     years_education = pgbilzeit
replace years_education =. if pgbilzeit ==-2 | pgbilzeit ==-1

* Recode Employment Status 
gen     employment_status = pgemplst
replace employment_status =. if pgemplst==-1


* Gen Female 

gen     female =1 if sex ==2
replace female =0 if sex ==1 

	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)
xtset pid syear

	* FE model

	* We run a FE model analysis with political_score as dependent variable and the dummy variable d_daughter as the main independent variable
	* Controls included: d_son  i.kids_variant  i.marital_status  years_education employment_status
	* Individual FE, Survey Year FE 
global controls d_son  i.kids_variant  i.marital_status  years_education employment_status

reghdfe political_score d_daughter  $controls, a(syear pid) vce(cluster pid)
	 
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = _se[d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local tstat = "t"


		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter] / _se[d_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
		local interpretation "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic: `tstat'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `tstat', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile



********************************************************************************

exit
