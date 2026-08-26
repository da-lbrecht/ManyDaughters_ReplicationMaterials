********************************************************************************
*********************** Do-File H2: #ManyDaughters Project *********************
********************************************************************************

* Authors: [REDACTED]

* ============================
**#  Setup * 
* ============================

* clear all
version 18 
set maxvar 10000 

* INSTALL PACKAGES * 
* ssc install outreg2

* CONFIGURE FILE PATHS * 
/*
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global IN "$ROOT/data/raw"
global DATA "$ROOT/data/derived" 
global OUT "$ROOT/out" 

*/

* ============================
**#  Data Preparation * 
* ============================

use "$IN/ppathl.dta", clear
keep pid hid syear parid partner sex gebjahr migback phrf

* Merge month of interview
merge 1:1 pid syear using "$IN/pgen.dta", nogen keepusing(pgmonth pgpsbil)

* Merge fertility information
merge m:1 pid using "$IN/biobirth.dta", keepusing(kidsex* kidmon* kidgeb*) nogen
	
* Merge political preferences
merge 1:1 pid syear using "$IN/pl.dta", keepusing(plh0004) nogen
	
* Merge state of residence. 
// NOTE: this is a deviation from the PAP. We assumed we would have access to 
// pequiv data, where the state is contained in the variable  l11101
// This should have no impact on our empirical analysis
merge m:1 hid syear using "$IN/hgen.dta", keepusing(hgnuts1) nogen	
ren hgnuts l11101 // to reflect the variable name in our PAP

label language EN
sort pid syear


* ============================
**#  Data editing 
* ============================

**********************************************
* Identify sex composition of children at time of survey

// set variables to missing if on information in data
qui recode kidgeb* kidmon* kidsex* pgmonth (-8 -5 -3 -2 -1 = .)

// rename variables for easier loop
forvalues x = 1/9 {
	rename kidgeb0`x' kidgeb`x'
	rename kidmon0`x' kidmon`x'
	rename kidsex0`x' kidsex`x'
}

// Loop over all children whether they were born by the time of survey
forvalues x = 1/19 {
	// 1 if monthly information available
	gen 	child_exis`x'=1 if ym(kidgeb`x',kidmon`x')<ym(syear,pgmonth) & pgmonth<.
	// 2 if annual information available
	replace child_exis`x'=1 if kidgeb`x'<syear
	}

// Recode children's sex to equal 1 if child is a girl	
recode kidsex* (1=0) (2=1)	
forvalues x = 1/19 {
	gen child_sex`x'=kidsex`x' if child_exis`x'==1
	}	
	
// Average sex of children per person-year
egen avg_sex=rowmean(child_sex*)	
tab avg_sex

// Define treatment indicator
gen 	daughters = 1 if avg_sex==1	
replace daughters = 0 if avg_sex==0	
lab var daughters "Treatment indicator (only daughters vs. only sons)"
tab daughters

// Only keep these observations
keep if daughters<.	
	
	
**********************************************
* Edit sample restrictions

// Age of youngest child
forvalues x = 1/19 {
	gen age_child`x'=int((ym(syear,pgmonth)-ym(kidgeb`x',kidmon`x'))/12) // interview 
	replace age_child`x'=syear-kidgeb`x' if age_child`x'==.	
	replace age_child`x'=. if child_exis`x'!=1
}
egen age_youngest_child=rowmin(age_child*)
lab var age_youngest_child "Age of the oldest child"

// Household with youngest child aged 0 to 5
keep if age_youngest_child<=5

// two-parent different sex households
bys hid syear: keep if _N==2 

sort hid syear pid
bys hid syear: egen m1=mean(sex)
gen diff_sex=(m1==1.5)	

keep if diff_sex==1	
	
**********************************************
* Editing control variables in estimation

gen age_first_birth = kidgeb1-gebjahr	
lab var age_first_birth "Age at first birth"
	
egen num_child=rowtotal(child_exis*)	
lab var num_child "Number of children"	

gen age_old_child=int((ym(syear,pgmonth)-ym(kidgeb1,kidmon1))/12) // interview month available
replace age_old_child=syear-kidgeb1 if age_old_child==.	
lab var age_old_child "Age of the oldest child"

* ============================
**# Analysis
* ============================

* FILTERS *

// Keep every observation with non-missing data for all variables specified in PAP
global analysis_var phrf hid age_old_child num_child l11101 migback sex	///
					age_first_birth daughters plh0004 pgpsbil pid syear
keep $analysis_var
recode $analysis_var (-8 -5 -3 -2 -1 =.) 
qui reg $analysis_var
keep if e(sample)

**********************************************
* Create dependent variable
recode plh* (-8 -5 -1 = .)
gen left_pref = 11 - plh0004
egen std_left_pref_H2=std(left_pref)
lab var std_left_pref_H2 "Index of left-leaning political preferences"


* HYPOTHESIS TEST FOR H2 * 
// The dependent variable is a standardised index of left-leaning preferences
// Higher values indicate more left-leaning preferences
// Variable of interest is the independent variables daughters, which equals 1
	// if respondents only have daughters vs. only sons
// We run a linear OLS regression analysis with additional 
	// pre-determined covariates
	
reg std_left_pref_H2 daughters i.age_first_birth i.sex i.migback i.l11101 ///
i.pgpsbil i.num_child i.age_old_child i.syear [aw=phrf], vce(cluster hid)

**# EXTRACT AND EXPORT RESULTS * 

* Extract the required statistics

	* (i) coefficient estimate
	local coefficient = r(table)[1,1]

	* (ii) standard error
	local se = r(table)[2,1]

	* (iii) family of the test statistic (e.g. z, t, F, Chi²)
	local test_stat_family = "t"

	* (iv) residual and model degrees of freedom (DFs) if applicable
	local df_residual = e(df_r)
	local df_model = e(df_m)

	* (v) p-value
	local p_value =  r(table)[4,1]

	* (vi) number of observations
	local num_obs = e(N)

	* (vii) number of participants
	quietly levelsof pid if e(sample), local(participants)
	local num_participants : word count `participants'

	* (viii) interpretation of directionality
	scalar coef_d_daughter = _b[daughter]
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
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
