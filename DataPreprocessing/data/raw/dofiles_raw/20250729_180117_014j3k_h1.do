********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
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
ssc install swindex

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

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
use "$DATA/h1_dataset.dta", clear 

* FILTERS * 
* Only keep participants from private households
keep if inlist(pop,1,2)


* GENERATE VARIABLES * 

* 1. Main Explanatory Variable: Gender Equality Attitudes 

gen       equality_housework =. 
replace   equality_housework = 1 if plh0303  ==1 | plh0303 ==2 
replace   equality_housework = 0 if plh0303  ==3 | plh0303 ==4 
label var equality_housework "Men should be involved in housework, = 1 if agree"
	
gen       equality_family_career =. 
replace   equality_family_career = 1 if plh0301  ==3 | plh0301 ==4
replace   equality_family_career = 0 if plh0301  ==1| plh0301 ==2
label var equality_family_career "Women Should Rather Care About Family Than Career, = 1 if disagree"
	
gen       equality_working_mothers =. 
replace   equality_working_mothers =1 if plh0309 ==1 | plh0309 ==2
replace   equality_working_mothers =0 if plh0309 ==3 | plh0309 ==4
label var equality_working_mothers "Working Mothers can provide the same emotional support, = 1 if agree"
	
foreach var in 	plh0301  plh0303  plh0309 {
	recode `var' (-8=.) (-5=.) (-1=.)
	
}
	
global equality1 plh0303  plh0309 
global equality2 plh0301  

* Create a variance weighted index (following Anderson 2008) to combine the measures of gender equality 
swindex 	plh0301  plh0303  plh0309, gen(gender_equality_attitude_index) flip($equality1) d


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
replace employment_status =. if pgemplst==-2


* Gen Female 

gen     female =1 if sex ==2
replace female =0 if sex ==1 

* HYPOTHESIS TEST FOR H1 * 

	* OLS Linear Regression. 
	* We run a linear OLS regression analysis with gender_equality_attitude_index as dependent variable and the dummy variable d_daughter as the main independent variable
	* Controls included: d_son  i.kids_invariant  female i.marital_status native years_education age age_sq i.corigin
 
	
	global controls d_son i.kids_variant  female i.marital_status  years_education i.employment_status age age_sq i.corigin 
	

	reg  gender_equality_attitude_index d_daughter $controls, robust 



* EXTRACT AND EXPORT RESULTS * 

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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,test statistic,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `tstat', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
