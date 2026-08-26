********************************************************************************
*********************** Do-File H3: #ManyDaughters Project *********************
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
	
* Merge time use information
merge 1:1 pid syear using "$IN/pl.dta", ///
	keepusing(pli0040 pli0043_h pli0044_h pli0046 pli0049_h pli0038_v4) nogen
	
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
qui recode kidgeb* kidmon* kidsex* pgmonth (-5 -3 -2 -1 = .)

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



***********************************************
* Restrict data at household level

forvalues x = 1/19 {
	gen age_child`x'=int((ym(syear,pgmonth)-ym(kidgeb`x',kidmon`x'))/12) // interview 
	replace age_child`x'=syear-kidgeb`x' if age_child`x'==.	
	replace age_child`x'=. if child_exis`x'!=1
}
egen age_youngest_child=rowmin(age_child*)
lab var age_youngest_child "Age of the oldest child"

// Household with youngest child aged 0 to 5
keep if age_youngest_child<=5
 
// Keep only households with 2 adults
bys hid syear (pid): keep if _N==2
// Keep different-sex households
bys hid syear: egen mix_sex=mean(sex)
keep if mix_sex==1.5


// Sort data such that the female observation is first:
	// As analysis is at the household-level, all variables will be assigned to
	// the female respondent
gsort hid syear -sex	
	
**********************************************
* Editing control variables in estimation

by hid syear: gen age_first_birth_F = kidgeb1-gebjahr if _n==1	
lab var age_first_birth_F "Age at first birth (woman)"

by hid syear: gen age_first_birth_M = kidgeb1[_n+1]-gebjahr[_n+1] if _n==1	
lab var age_first_birth_M "Age at first birth (man)"
	
by hid syear: gen migback_F=migback if _n==1
lab var migback_F "Migration background (woman)"

by hid syear: gen migback_M=migback[_n+1] if _n==1
lab var migback_M "Migration background (man)"

by hid syear: gen pgpsbil_F=pgpsbil if _n==1
lab var pgpsbil_F "Education (woman)"

by hid syear: gen pgpsbil_M=pgpsbil[_n+1] if _n==1
lab var pgpsbil_M "Education (man)"	
	
egen num_child=rowtotal(child_exis*)	
lab var num_child "Number of children"	

gen age_old_child=int((ym(syear,pgmonth)-ym(kidgeb1,kidmon1))/12) // interview month available
replace age_old_child=syear-kidgeb1 if age_old_child==.	
lab var age_old_child "Age of the oldest child"


**********************************************
* Create dependent variable
recode pli* (-8 -5 -3 -1 = .) (-2 = 0)

// Paid work per person
by hid syear: gen MW_F=pli0038_v4 if _n==1
by hid syear: gen MW_M=pli0038_v4[_n+1] if _n==1

// Domestic work per person
by hid syear: gen DW_F=pli0040+pli0043_h+pli0044_h+pli0046+pli0049_h if _n==1
by hid syear: gen DW_M=pli0040[_n+1]+pli0043_h[_n+1]+pli0044_h[_n+1]+pli0046[_n+1]+pli0049_h[_n+1] if _n==1

// Winsorise to total of 20 hours of paid and unpaid work for men and women
// Women
gen total_F=MW_F+DW_F
replace MW_F=MW_F*(20/total_F) if total_F>=20
replace DW_F=DW_F*(20/total_F) if total_F>=20
// Men
gen total_M=MW_M+DW_M
replace MW_M=MW_M*(20/total_M) if total_M>=20
replace DW_M=DW_M*(20/total_M) if total_M>=20

// Create Specialisation Index
gen SI=1-abs((DW_F/(DW_F+DW_M))-(MW_F/(MW_F+MW_M)))

* ============================
**# Analysis
* ============================

* FILTERS * 
// Keep every observation with non-missing data for all variables specified in PAP
global analysis_var phrf hid age_old_child num_child l11101 migback_* ///
					age_first_birth_* daughters SI pgpsbil_* pid syear
keep $analysis_var
recode $analysis_var (-8 -5 -3 -2 -1 =.) 
qui reg $analysis_var
keep if e(sample)

egen std_SI_H3=std(SI)
lab var std_SI "Index of gender inequality in the household"



* HYPOTHESIS TEST FOR H3 * 
// The dependent variable is a standardised index of gender equality in the household
// Higher values indicate a more equality
// Variable of interest is the independent variables daughters, which equals 1
	// if respondents only have daughters vs. only sons
// We run a linear OLS regression analysis with additional 
	// pre-determined covariates
	
reg std_SI_H3 daughters i.age_first_birth_* i.migback_* i.l11101 ///
i.pgpsbil_* i.num_child i.age_old_child i.syear [aw=phrf], robust

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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
