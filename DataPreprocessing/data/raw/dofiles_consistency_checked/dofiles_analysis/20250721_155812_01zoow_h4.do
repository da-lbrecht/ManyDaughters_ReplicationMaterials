********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: Blake H. [REDACTED] and [REDACTED] (adapted from template by Tina Wang and David Albrecht)
* Created: 8 July 2025
* Last Updated: 21 July 2025

* ============================
* Setup *
* ============================


*`c(pwd)' script below does not direct to correct folder -- manually set main directory
// cd "/Users/bheller/Dropbox/1. Projects/Many Daughters/Analysis/#ManyDaughters_RT_AnalysisPackage/" // Commented out by David Albrecht for reproducibility check


* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:
* ssc install outreg2
* ssc install egenmore

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

disp "$IN"
disp "$DATA"
disp "$OUT"
*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income and state of residence
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgnuts1)

	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * 
 
 
*Keep only respondents with valid answers to plh0308_v2
replace plh0308_v2=. if plh0308_v2<0
drop if mi(plh0308_v2)

*confirm that all responses to plh0308_v2 occur during the 2018 survey year
assert syear==2018
 

*Keep only respondents with at least one child with known sex when survey was administered
	foreach var of varlist kidgeb01-kidgeb19{
	replace `var'=. if `var'<0
}

*generate count of children/daughters
gen num_daughters=0
gen num_kids=0
foreach n in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18{
replace num_daughters=num_daughters+1 if kidsex`n'==2 & kidgeb`n'<=2018
replace num_kids=num_kids+1 if (kidsex`n'==1|kidsex`n'==2) & kidgeb`n'<=2018
}

drop if num_kids==0


* GENERATE VARIABLES * 

*standardize measure of support for gender equality
su plh0308_v2
local mean=r(mean)
local sd=r(sd)
gen std_support_gen_eq=(plh0308_v2-`mean')/`sd'


*generate covariates
gen r_male = (sex==1) if !mi(sex)
gen r_age = syear- gebjahr

*interact male gender with number of daughters
gen num_daughters_MALE = r_male*num_daughters

*generate educational attainment indicators (educ_*)
replace pgpsbil =. if pgpsbil<0
replace pgpbbil01 = . if pgpbbil01<0
replace pgpbbil02 = . if pgpbbil02<0

*impute "Intermediate School Degree" (the most common value of pgpsbil for individuals with non-missing pgpbbil01) to individuals with non-missing vocational degree data but missing educational attainment
replace pgpsbil=2 if mi(pgpsbil)&!mi(pgpbbil01)
*impute "Upper Secondary Degree" (the most common value of pgpsbil for individuals with non-missing pgpbbil02) to individuals with non-missing university degree data but missing educational attainment
replace pgpsbil=4 if mi(pgpsbil)&!mi(pgpbbil02)
tab pgpsbil, gen(r_educ_)
gen mi_educ=mi(pgpsbil)&mi(pgpbbil01)&mi(pgpbbil02)
gen r_educ_vocational=!mi(pgpbbil01)
gen r_educ_academic=!mi(pgpbbil02)
foreach var of varlist r_educ*{
	replace `var'=0 if mi(`var') & mi_educ==1
}

gen r_full_time = (pgemplst==1)
gen r_part_time = (pgemplst==2|pgemplst==4)
gen r_voc_training = (pgemplst==3|pgemplst==6)
gen r_unemployed = (pgemplst==5)
gen mi_work_status = (pgemplst==.|pgemplst<0)


replace pglabgro=. if  pglabgro<0
replace pgsndjob=. if pgsndjob<0
egen indiv_income = rowtotal(pglabgro pgsndjob)
bys hid syear: egen r_hhld_income = total(indiv_income)

replace pgfamstd=. if pgfamstd<0
gen r_married=(pgfamstd==1)
gen r_separated=(pgfamstd==2)
gen r_single=(pgfamstd==3)
gen r_divorced=(pgfamstd==4)
gen r_widowed=(pgfamstd==5)
gen r_spouse_abroad=(pgfamstd==6)
gen r_registered_partnership=(inlist(pgfamstd,7,8))
gen mi_marital_status=mi(pgfamstd)

replace plh0258_h=. if plh0258_h<0
gen r_catholic=(plh0258_h==1)
gen r_other_christian = (inlist(plh0258_h,2,3,7))
gen r_muslim = (inlist(plh0258_h,4,8,9,10))
gen r_other_non_christian = (plh0258_h==5)
gen r_no_affiliation=(plh0258_h==6)
gen mi_religion= (plh0258_h==.)


*country of origin can't be negative for regression—replace -1 with 0
replace corigin = 0 if corigin==-1

*identify hhlds with missing data on state of residence
replace hgnuts1=0 if mi(hgnuts1)




* HYPOTHESIS TEST FOR H4 * 

	* We standardize responses to plh0308_v2, a question that asks whether respondents believe it is "Best if man and woman work the same amount so they can share the responsibility" to construct our dependent variable, std_support_gen_eq.  We take this to a good measure of support for gender equality, where effects can be measured by gender (H4). If H4 is true and having daughters leads to a larger increase in support for gender equality for male parents, having daughters will induce male respondents to increase their scale score response to plh0308_v2 (i.e., answering closer to 7; agreeing or agreeing more strongly with the statement) moreso than female respondents (β_numDaughters_MALE>0). We estimate the parameter of interest, the coefficient on numDaughters_MALE, from a linear OLS regression analysis with std_support_gen_eq as dependent variable and the count variable num_daughters_MALE as independent variable, controlling for demographic characteristics, missing data, country of origin, state of residence, total family size, and survey wave. We weight responses using the individual weight (phrf) provided by the SOEP.


	reg std_support_gen_eq num_daughters num_daughters_MALE r_* mi_* i.corigin i.syear i.hgnuts1 num_kids [aweight=phrf] if num_kids!=0, r


* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[num_daughters_MALE]

		* (ii) standard error
		local se = _se[num_daughters_MALE]

		* (iii) family of the test statistic (one-sided t-statistic)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value from one-sided test
		local coef_sign = sign(_b[num_daughters_MALE])
		disp `coef_sign'
		test num_daughters_MALE
		local p_value = ttail(r(df_r),`coef_sign'*sqrt(r(F)))
		display "H_1: coef<=0  p-value = `p_value'"

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation 
		scalar coef_num_daughters = _b[num_daughters_MALE]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
