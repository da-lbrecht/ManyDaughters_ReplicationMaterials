********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

//clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2
ssc install swindex

/*
* CONFIGURE FILE PATHS * (Please do not change)
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

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
use "$DATA/h4_dataset.dta", clear

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
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

gen d_daughter_female = female*d_daughter
* HYPOTHESIS TEST FOR H4 *

	* OLS Linear Regression. 
	* We run a linear OLS regression analysis with gender_equality_attitude_index as dependent variable and the dummy variable d_daughter as the main independent variable
	* Controls included: d_son  i.kids_variant  female i.marital_status native years_education age age_sq i.corigin
	* We interact the gender of the respondent with d_daughter
 
	
	global controls d_son i.kids_variant  i.marital_status  years_education i.employment_status age age_sq i.corigin 
		
	
	
	reg  gender_equality_attitude_index d_daughter female d_daughter_female $controls, robust 

	
	
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

*---------------------------------------------------*
* 1. Run regression with interaction
*---------------------------------------------------*
regress gender_equality_attitude_index ///
    d_daughter female d_daughter_female $controls, robust

*---------------------------------------------------*
* 2. Capture common stats
*---------------------------------------------------*
local df_residual = e(df_r)
local df_model    = e(df_m)
local num_obs     = e(N)
quietly levelsof pid if e(sample), local(pids)
local num_participants : word count `pids'

*---------------------------------------------------*
* 3. Extract for each variable
*---------------------------------------------------*
foreach var in d_daughter d_daughter_female {
    * (i) coefficient
    local coef_`var' = _b[`var']
    * (ii) standard error
    local se_`var'   = _se[`var']
    * (iii) t‑statistic
    local t_`var'    = `coef_`var'' / `se_`var''
    * (v) p‑value
    local p_`var'    = 2 * ttail(`df_residual', abs(`t_`var''))

    * test family
    local test_stat_family = "t"

    * (viii) interpretation ONLY for the interaction
    local int_`var' = "negative coefficient is in line with hypothesis"
}


*---------------------------------------------------*
* 4. Display on screen (now showing t‑stat)
*---------------------------------------------------*
di as txt "Variable           Coef.      SE       t‑stat     p‑value    Interpretation"
foreach var in d_daughter d_daughter_female {
    di as result ///
        "`var'        " %9.4f `coef_`var''    "  " ///
        %7.4f `se_`var''    "  " ///
        %7.4f `t_`var''     "  " ///
        %7.4f `p_`var''     "  " ///
        "`int_`var''"
}

*---------------------------------------------------*
* 5. Export to CSV (use t_stat instead of family)
*---------------------------------------------------*
file open myfile using "$OUT/h4_results.csv", write replace
file write myfile ///
    "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n

foreach var in /*d_daughter*/ d_daughter_female {
    file write myfile ///
        "`coef_`var'',`se_`var'',`test_stat_family',`df_residual',`df_model',`p_`var'',`num_obs',`num_participants',`int_`var''" _n
}

file close myfile

exit
