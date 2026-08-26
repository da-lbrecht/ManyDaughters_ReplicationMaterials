********************************************************************************
********************** Testing H3: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

*------------------------------------------------------------------------*
* H3 : Having daughters leads to more gender equality in the household
* (as measured by behaviors or outcomes).
*------------------------------------------------------------------------*
 
/*
	 Assumptions
	*----------------------------
	
	Same as in H1:
	    -- Child's gender is randomly assigned. 
		-- Parents are a selected subsample (child of any gender can affect political preferences)
	
	From this assumptions we arrive to similar conclusions concerning the relevant 
	comparison and treatment groups. 
	
    There is an additional assumption here. More equal households are those where
	men and women have a similar distribution of tasks. The amount of hours worked in 
	childcare is similar, the amount of time worked for pay is similar, etc. This 
	does not have to be the case. Households where members specialize in different
	domains (housework, paid employment) are not egalitarian, even if this specialization
	reflects only economic considerations. 
	
			
	   Implementation
	 *------------------------------ 
	
	The unit of analysis for this question is the household, as we consider how
	different is the task allocation between men and women. We measure the differences
	using Duncan segregation index. While it was originally intended to measure
	occupational segregation by gender, race, etc, it can be adapted for the analysis
	of household tasks. In our implementation, we consider the differences in the 
	fraction of time that men and women allocate to tasks. Higher numbers show greater 
	differences. 
	
	The list of tasks considered in the analysis are: 
		* pli0038_h - Job, apprenticeship, second job (including travel time to and from work)
		* pli0040 - Errands (shopping, trips to government agencies, etc.)
		* pli0043_v3 - Housework (washing, cooking, cleaning)
		* pli0044_v3 - Childcare
		* pli0046 - Care and support of persons in need of care
		* pli0047_v1 - Education or further training (also school, university)
		* pli0049_v3 - Repairs on and around the house, car repairs, garden work
		* pli0051 - Hobbies and other free-time activities


	The implementation requires additional steps, when compared to other hypothesis, 
	as we need to first match individuals and create households. Second, we focus on 
	children born to a couple, i.e. we do not consider patchwork families. Third, 
	we need verify that information on tasks is completed (not only missing values)
	and that respondents record at least one task.
	
	We include only households that meet the following criteria
		- They do not have children at the time of their  first observation 
		- They will eventually have children
		- They are heterosexual couples
		- In households where there are more than two couples, we keep the youngest couple 
		- We exclude households with patchwork families (children from previous couples)
	
	We estimate the effects of parenting a daughter using a diff-in-diff approach. 
	Following the new litereature on the topic, we first estimate a regression that 
	includes many (many) interactions, to avoid forbidden comparisons. We obtain
	estimates for every cohort, and every time since the event. Finally, we average
	the results to obtain a single coefficient. 
		*/

* ========================== *
* 			SETUP			 *
* ========================== *

//clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* New packages
 
ssc install jwdid
ssc install reghdfe
ssc install hdfe
ssc install ftools

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

* ============================== *
* 		Data Preparation		 *
* ============================== *



use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
 
* biobirth contains information on fertility history
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
* pgen contains user-friendly data on the individual level that are consolidated from different sources
* merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
* pl contains all variables from the individual questionnaire
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgnation)
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pli0038_h pli0040 pli0043_v3 pli0044_v3 pli0046 pli0047_v1 pli0049_v3 pli0051)
* hgen contains generated variables on household level from the household interview; use for household net income
merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

label language EN // change language of the labels to English

save "$DATA/h3_dataset.dta", replace // save merged dataset

 

* ============================== *
* 			Analysis			 *
* ============================== *

use "$DATA/h3_dataset.dta", clear


** Sample selection
keep if inlist(pop,1,2)
keep if parid>0

* Keep only those who will eventually become parents (then kidpnr01 is non-missing)
keep if kidpnr01>0&!mi(kidpnr01)


* The variables which contain activity (in hours per standard weekday) in the household
global activity_vars "pli0038_h pli0040 pli0043_v3 pli0044_v3 pli0046 pli0047_v1 pli0049_v3 pli0051"

* Drop all observations that are missing at least one activity 
foreach var in $activity_vars {
    drop if  `var' < 0
}

egen declared_no_h = rowtotal($activity_vars)
drop if declared_no_h==0


* Recode "No answer" category and missing "." as -100. "Does not apply" 
* (when a person does not have ith child) is not changed
foreach varname of varlist kidpnr*{
	replace `varname' = -100 if (`varname'==-1|mi(`varname'))
}



* Creating the variable that shows seggregation
foreach var in $activity_vars {
    replace `var' = `var'/declared_no_h
}

** New variables
gen age = syear - gebjahr

* Create variables for partner (partner_*)

preserve
keep pid syear kidpnr01-kidpnr17 hid  $activity_vars age sex

rename (kidpnr* $activity_vars age sex hid) partner_=
rename pid parid
tempfile partner_kid
save `partner_kid'
restore

* If someone's parid is the other person's pid, they match
merge m:1 parid syear using `partner_kid', keep(match)

* Both partners are in the same household
gen same_hh = (hid == partner_hid)


* Keep only couples with the same biological children: We drop patchwork families
gen same_bio_child = 1
forvalues i = 1/5{
    local k = string(`i', "%02.0f")
    replace same_bio_child = 0 if kidpnr`k' != partner_kidpnr`k'
}
 

* Keep only couples with the same biological children: We drop patchwork families
keep if same_bio_child == 1 & same_hh == 1 

* Keep only heterosexual couples
gen same_sex = (sex == partner_sex)
drop if same_sex == 1


* We want ot have only one couple per household. We keep the youngest
duplicates report hid syear
duplicates tag hid syear, gen(duplicates)

* Drop older couples in households with more than two couples
gen age_mean = (age+partner_age) /2
bys hid syear : egen min_avg_age = min(age_mean)
drop if (min_avg_age != age_mean) & duplicates==3


* Now there should be 2 copies of each HH
duplicates report	 hid syear
duplicates drop 	 hid syear , force




gen has_daughter = 0
gen has_boy = 0

* Loop over kidsex01 to kidsex19
forvalues i = 1/19 {
    local kidsex = "kidsex" + string(`i', "%02.0f")
    local kidgeb = "kidgeb" + string(`i', "%02.0f")
 
* We want the value to be -100 if a child is not yet born
	replace `kidsex' = -100  if `kidgeb'>syear  
    replace has_daughter = 1 if `kidsex' == 2
	replace has_boy      = 1 if `kidsex' == 1
	
}

gen first_obs = 0
bysort hid (syear): replace first_obs = 1 if _n == 1

**Exclude always treated
gen already_treated = (has_daughter==1 | has_boy==1 )& first_obs==1
by hid: egen m_already_treated = mean(already_treated)
drop if m_already_treated>0




** GENERATE VARIABLES FOR THE ANALYISIS

* Generate the division of labor index
foreach var in $activity_vars {
    gen diff_`var' = abs(`var'- partner_`var')
}

egen segregation_index = rowtotal(diff_*)
replace segregation_index = -0.5*segregation_index


* Age of each partner
gen tage_fem = age*(sex==2) + partner_age*(partner_sex==2)
gen tage_mal = age*(sex==1) + partner_age*(partner_sex==1)

egen initial_age_fem = min(tage_fem) , by(hid)
egen initial_age_mal = min(tage_mal) , by(hid)

* Get the first year of treatment per household
gen treated_year = syear if has_daughter == 1
bysort hid : egen first_treated_year = min(treated_year)
replace first_treated_year = 0 if mi(first_treated_year)
 


*************************************************** HYPOTHESIS TEST FOR H3 ***************************************************

jwdid segregation_index      , ivar(hid) tvar(syear) gvar(first_treated_year) never
scalar nobs = e(N) 


estat event, pretrend
scalar p_test = `r(pre_p)'

estat simple
mat b = r(table)
scalar daughter= el(b ,1,1)
scalar se_daughter= el(b ,2,1)
scalar p_daughter= el(b ,4,1)
 
if p_test <0.1{
	di "WARNING: evidence of difference in pretrends" 
}
 
* if the first test passes, then for statistical power we should use all pre treatement as controls 
if p_test >  0.1{
if p_test == . {
	di "There are not enough observations to compute SD in the full model" 
	di "We proceed to use later treated as control units" 
	}
	
jwdid segregation_index   , ivar(hid) tvar(syear) gvar(first_treated_year)

estat simple
mat b = r(table)
scalar daughter= el(b ,1,1)
scalar se_daughter= el(b ,2,1)
scalar p_daughter= el(b ,4,1)


}


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

* Extract the required statistics

* (i) coefficient estimate
local coefficient = daughter


* (ii) standard error
local se = se_daughter

* (iii) family of the test statistic (e.g. z, t, F, Chi²)
local test_stat_family = "z"

// * (iv) residual and model degrees of freedom (DFs) if applicable
// local df_residual = e(df_r)
// local df_model    = e(df_m)
//
// * (v) p-value
// local p_value = _b[d_daughter] / _se[d_daughter]
// local p_value = 2 * ttail(e(df_r), abs(`p_value'))
local p_value = p_daughter

* (vi) number of observations
local num_obs = nobs

* (vii) number of participants
quietly levelsof hid if e(sample), local(participants)
local num_participants : word count `participants'

* (viii) interpretation of directionality
local interpretation "positive coefficient is in line with hypothesis"


*(viii) Potential issues
if p_test <0.05 {
local issue "concern about parallele trends"
}
else {
local issue "not in this regression"
}

// Display the saved statistics
di "Coefficient: `coefficient'"
di "Standard Error: `se'"
di "Test Statistic Family: `test_stat_family'"
di "Residual Degrees of Freedom: `df_residual'"
di "Model Degrees of Freedom: `df_model'"
di "p-value: " `p_value'
di "Number of observations: " `num_obs'
di "Number of participants: " `num_participants'
di "Interpretation of directionality: " "`interpretation'"
di "Potential issues: " "`issue'"


* Export results to csv
file open myfile using "$OUT/h3_results.csv", write replace
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile


