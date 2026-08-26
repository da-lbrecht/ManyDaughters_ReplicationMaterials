********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
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

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
	use "$DATA/h3_dataset.dta", clear 

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)

*1. Main Dependent Variable: Generate d_daughter 


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

egen    d_son= rowmax(forward_son*)
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

* Reshape the dataset 


* (a) Create a couple identifier that is the same for both partners
gen long pmin = cond(pid < parid, pid, parid)
gen long pmax = cond(pid < parid, parid, pid)

* 2) generate one numeric grouping variable
egen long couple_id = group(pmin pmax)

* 3) clean up
drop pmin pmax									   

drop if parid ==-2 

* clean our behaviour gender equality varibale: actuall hours of work per week
recode pgtatzeit (-1=.) (-2=.) (-3=.)


keep pid hid couple_id syear sex pgtatzeit marital_status  sumkids age birthregion parid d_daughter  d_son age_sq kids_variant  years_education employment_status

* 1) give each partner an "order" within the couple–year
bysort couple_id syear (pid): gen byte partner = _n


* (c) Reshape to wide so we get one line per couple-year
reshape wide pid sex pgtatzeit marital_status  sumkids age birthregion parid d_daughter d_son age_sq kids_variant years_education employment_status, ///
    i(couple_id syear) j(partner)

	
* replace employment_status =. if NA
replace employment_status1=. if employment_status1==-2
replace employment_status2=. if employment_status2==-2

* Restrict to couples who start to have the children together 

gen     same_kid = 0
replace same_kid = 1 if kids_variant1 ==kids_variant2

drop if same_kid ==0


gen     same_dau = 0
replace same_dau= 1 if d_daughter1 ==d_daughter2

drop if same_dau ==0


gen     same_son = 0
replace same_son= 1 if  d_son1 ==d_son2

drop if same_son ==0
* Drop homosexual couples as- gender roles are probably different 

gen flag_homo =1 if sex1 == sex2 
drop if flag_homo ==1

gen hours_female = .


* Main Explanatory Variable: Difference in Hours Worked within the couple

forvalues j = 1/2 {
    replace hours_female = pgtatzeit`j' ///
        if sex`j'==2
}


gen hours_male = .

forvalues j = 1/2 {
    replace hours_male = pgtatzeit`j'  ///
        if sex`j'==1
}



* does gap in hours worked between male and female become more negative or more positive? 
* negative in couples with daughter mum workds less than dad, compared to those without daughters
gen diff_hours = hours_female - hours_male



* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* We run a FE model analysis with diff_hours as dependent variable and the dummy variable d_daughter as the main independent variable
	* Controls included: d_son  i.kids_invariant  i.marital_status  years_education employment_status
	* Couple FE, Survey Year FE 

rename d_daughter1 d_daughter
reghdfe diff_hours d_daughter d_son1 i.kids_variant1 years_education1 years_education2 i.employment_status1 i.employment_status2, a(syear couple_id ) vce(cluster couple_id)


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
		quietly levelsof couple_id if e(sample), local(couples)
		local num_couples : word count `couples'

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
	di	"Number of couples: " `num_couples'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,test statistic,residual_df,model_df,p_value,num_obs,num_couples,interpretation" _n
	file write myfile "`coefficient', `se', `tstat', `df_residual', `df_model',`p_value',`num_obs',`num_couples',`interpretation'" _n
	file close myfile


********************************************************************************

exit
