********************************************************************************
********************** Testing H2: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

*------------------------------------------------------------------------*
* H2: Having daughters leads to more left-leaning political preferences. *
*------------------------------------------------------------------------*

/*
	 Assumptions
	*----------------------------
	
	Same as in H1:
	    -- Child's gender is randomly assigned. 
		-- Parents are a selected subsample (child of any gender can affect political preferences)
	
	From this assumptions we arrive to similar conclusions concerning the relevant 
	comparison and treatment groups. 
	
	
	   Implementation
	 *------------------------------ 
	
	We consider sympathy for a political party as an indicator of right/left political 
	leaning. We create a binary variable that equals one if an individual shows support
	for a political party on the left of the spectrum. 
	
	The approach has two clear advantages. First, the question on political preferences
	is available every year. This allows clean before-after comparisons. Second, studying a 
	binary variable is considerably simpler than using multinomial models that consider
	voting for each party. In the latter case, a difference in differences approach 
	such as the one we propose would be much more complex. 
	
	Our approach has two shortcomings. First, when mapping political parties into a single axes
	(left-right) we use the data from the Chapel Hill Expert Survey
	(https://www.chesdata.eu/ches-europe). The database contains expert assessments
	of where partie are located along many dimensions. 
	Our approach might be an oversimplification for two reasons. First, parties can 
	vary in terms of economic orientation (traditional axis), but also in terms of 
	social orientation / preferences for environment. We priorized the first axis
	when deciding on their orientation. Second, we assume that political
	parties are persistent in their views. They are left or right throghout the entire
	30 years of data. In this case, ches data show that the two-camps (left and right)
	are relatively stable, even if within these groups the relative position of political
	parties changes. 
	
	We estimate the effects of parenting a daughter using a diff-in-diff approach. 
	Following the new litereature on the topic, we first estimate a regression that 
	includes many (many) interactions, to avoid forbidden comparisons. We obtain
	estimates for every cohort, and every time since the event. Finally, we average
	the results to obtain a single coefficient. 
	
	In our diff-in-diff, control units include never treated, and not yet treated. 
			*/
			
* ============================
* Setup  
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
ssc install jwdid
ssc install reghdfe
ssc install hdfe
ssc install ftools

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

* LOAD AND MERGE DATA *  

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
 
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	 merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgnation)
 	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing( plh0012_v6 )
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset




* ========================= *
* 		New variables 		* 
* ========================= *

use "$DATA/h2_dataset.dta", clear 

drop if pgnation!=1
keep if inlist(pop,1,2)
drop if  plh0012_v6<0 | mi(plh0012_v6) 

//Disregard missing anwsers to our questions of interest
* Create new variable where 1 = left wing party

/*
left_wing_party
[1] [de] SPD
[5] [de] Buendnis90/Green
[6] [de] Die Linke
[9] [de] SPD/Buendnis90/Green
[16] [de] Buendnis90/Green
[17] [de] SPD/Die Linke
[26] [de] Piratenpart
*/

gen left_wing_party =  inlist(plh0012_v6,1,5,6,9,16,17,26)


* There are no duplicates
duplicates report pid syear

* Keep only those who will eventually become parents (then kidpnr01 is non-missing)
keep if kidpnr01 > 0 & !mi(kidpnr01)

* Generate variable which == 1 when a person already has a daughter/boy
gen age = syear - gebjahr
 
gen has_daughter	= 0
gen has_boy			= 0
gen n_of_kids		= 0

* Loop over kidsex01 to kidsex19
forvalues i = 1/19 {
    local kidsex = "kidsex" + string(`i', "%02.0f")
    local kidgeb = "kidgeb" + string(`i', "%02.0f")

* We recode the value to -100 if a child is not yet born
	replace `kidsex' = -100 if `kidgeb'>syear  
    replace has_daughter = 1 if `kidsex' == 2
	replace has_boy		 = 1 if `kidsex' == 1
}


bys pid syear: egen initial_age = min(age)

* Drop those who already have a daughter when first encountered in our data
gen first_obs = 0
bysort pid (syear): replace first_obs = 1 if _n == 1
gen already_treated = (has_daughter == 1 | has_boy == 1 ) & first_obs==1
bysort pid (syear): egen m_already_treated = mean(already_treated)
drop if m_already_treated>0


* Get the first year of treatment per household
gen treated_year = syear if has_daughter == 1
bysort pid (syear): egen first_treated_year = min(treated_year)
replace first_treated_year = 0 if mi(first_treated_year)

*******************  HYPOTHESIS TEST FOR H2 ***************************
 
xtset pid syear
jwdid left_wing_party , ivar(pid) tvar(syear) gvar(first_treated_year)   never  
estat simple


estat event , pretrend
scalar p_test = `r(pre_p)'
estat plot

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
	
jwdid left_wing_party   , ivar(pid) tvar(syear) gvar(first_treated_year)  
estat event
estat plot
mat b = r(table)
scalar daughter= el(b ,1,1)
scalar se_daughter= el(b ,2,1)
scalar p_daughter= el(b ,4,1)
}

 
****************** EXTRACT AND EXPORT RESULTS *********************************

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = b[1,1]

		* (ii) standard error
		local se = b[2,1]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		*local df_residual = e(df_r)
		*local df_model = e(df_m)

		* (v) p-value
		local p_value =b[4,1]
	 

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'
	
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter = b[1,1]
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
	file write myfile "`coefficient',`se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
