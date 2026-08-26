********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2

* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global IN "$ROOT\data\raw" // folder, in which the original datasets are stored
global DATA "$ROOT\data\derived" // folder for datasets
global OUT "$ROOT\out" // folder for output (results, tables)

*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	keep pid syear sex partner sexor sampreg

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plj0621 plj0622 plj0623 plj0624 plj0625)
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex* gebjahr)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgisei08)
	* bioparen
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches) keepusing(freli mreli fisei08 misei08)
	* biol
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches) keepusing(lr3193 lb0285 lb0287_h lb0290_h lb0293_h lb0296_h lb0299_h lb0302_h lb0305_h lb0308_h)

	
	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
* n (person)=4,508; n(person-years)=35,228
	sort pid syear
	egen person_max=tag(pid)	
	sum person_max if person_max==1
	
* GENERATE VARIABLES * (Please adapt according to your needs)

	*------------------------------------------------------------------------------*
	* 1.1 CONSTRUCT DEPENDENT VARIABLES
	*------------------------------------------------------------------------------*

	* H1: Gender Equality Attitudes Index
	* Reverse code items where necessary
	recode plj0621 plj0622 plj0623 plj0624 plj0625  (-8 -5 -1 =.)
	gen plj0621_r = plj0621  // No recoding needed (1-7; higher = more equality)
	gen plj0622_r = plj0622  // No recoding needed
	gen plj0623_r = 8 - plj0623  // Reverse values (7-1)
	gen plj0624_r = 8 - plj0624  // Reverse values
	gen plj0625_r = 8 - plj0625  // Reverse values

	* Create mean index
	egen GenderEquality = rowmean(plj0621_r plj0622_r plj0623_r plj0624_r plj0625_r)

	* Check internal consistency
	alpha plj0621_r plj0622_r plj0623_r plj0624_r plj0625_r, std
	corr plj0621_r plj0622_r plj0623_r plj0624_r plj0625_r


	*------------------------------------------------------------------------------*
	* 1.2 CONSTRUCT INDEPENDENT VARIABLE: DaughterHH
	*------------------------------------------------------------------------------*
	gen n_sons = 0
	gen n_daughters = 0

	forvalues i = 1/19 {
		local varname = "kidsex" + string(`i', "%02.0f")
		replace `varname' = . if `varname' == -1
		quietly replace n_sons = n_sons + 1 if `varname' == 1
		quietly replace n_daughters = n_daughters + 1 if `varname' == 2
	}

	gen n_children = n_sons + n_daughters

	gen DaughterHH = .
	replace DaughterHH = 0 if n_children == 0
	replace DaughterHH = 1 if n_sons > n_daughters | (n_sons > 0 & n_daughters == 0)
	replace DaughterHH = 2 if n_sons == n_daughters & n_children > 0
	replace DaughterHH = 3 if n_daughters > n_sons | (n_daughters > 0 & n_sons == 0)

	label define daughter_lbl 0 "No children" 1 "More sons" 2 "Equal share" 3 "More daughters"
	label values DaughterHH daughter_lbl
	
	* Gender
	recode sex (2=0), gen(Man)

	*------------------------------------------------------------------------------*
	* 1.3 CONSTRUCT CONTROL VARIABLES
	*------------------------------------------------------------------------------*
	
	* Number of children == n_children

	* Age groups of children
	recode lb0287_h lb0290_h lb0293_h lb0296_h lb0299_h lb0302_h lb0305_h lb0308_h (-5 -3 -2 -1 = .) 

	* Define value label for age groups
	label define agegroup_lbl ///
		0 "Infant (0–<1)" ///
		1 "Toddler (1–<4)" ///
		2 "Preschooler (4–<6)" ///
		3 "School age (6–<13)" ///
		4 "Teenager (13–17)" ///
		5 "Adult (18+)", replace

	* Loop through 8 child birthyear variables
	forvalues i = 1/8 {
		* Get the correct variable name (e.g. lb0287_h, lb0290_h, ...)
		local varnum = 287 + 3 * (`i' - 1)
		local byear = "lb0" + string(`varnum') + "_h"

		* Generate age and category only if birth year is not missing
		gen age_child`i' = syear - `byear' if `byear' != .

		gen childagegroup`i' = .
		replace childagegroup`i' = 0 if age_child`i' < 1 & age_child`i'!=. // Infant
		replace childagegroup`i' = 1 if age_child`i' >= 1 & age_child`i' < 4 // Toddler
		replace childagegroup`i' = 2 if age_child`i' >= 4 & age_child`i' < 6 // Preschooler
		replace childagegroup`i' = 3 if age_child`i' >= 6 & age_child`i' < 13 // School age
		replace childagegroup`i' = 4 if age_child`i' >= 13 & age_child`i' <= 17  // Teenager
		replace childagegroup`i' = 5 if age_child`i' >= 18 & age_child`i'!=. // Adult
		
		* Apply value label
		label values childagegroup`i' agegroup_lbl
	}


	* SES (use maximum of father's or mother's ISEI if respondent's is missing)
	recode pgisei08 (-8 -2 -1 =.)
	gen SES = pgisei08

	* Partnership status
	recode partner (-2=.) (0=0) (1 2 3 4 5=1), gen(Partner)

	* Sexual orientation
	gen SexualOrientation = .
	replace SexualOrientation = 0 if sexor == 0  // Probably heterosexual
	replace SexualOrientation = 1 if sexor==1  // Probably bi/homosexual

	* Religion // Proxy Parents
	* Father's religion
	gen Religion_Father = .
	replace Religion_Father = 1 if inlist(freli, 1, 2, 3, 7)    // Christian
	replace Religion_Father = 2 if freli == 4                   // Muslim
	replace Religion_Father = 3 if freli == 5                   // Other
	replace Religion_Father = 4 if inlist(freli, 6, -2)         // Non-religious

	* Mother's religion
	gen Religion_Mother = .
	replace Religion_Mother = 1 if inlist(mreli, 1, 2, 3, 7)    // Christian
	replace Religion_Mother = 2 if mreli == 4                   // Muslim
	replace Religion_Mother = 3 if mreli == 5                   // Other
	replace Religion_Mother = 4 if inlist(mreli, 6, -2)         // Non-religious

	* Optional: assign value labels
	label define relig_lbl 1 "Christian" 2 "Muslim" 3 "Other" 4 "Non-religious"
	label values Religion_Father relig_lbl
	label values Religion_Mother relig_lbl


	* Age (continuous)
	gen Age = syear - gebjahr

	* Region
	recode sampreg (-2 = .) (1 = 0) (2 = 1), gen(Region)

	label define region_lbl 0 "West Germany" 1 "East Germany"
	label values Region region_lbl
	

* FILTERS * (Please adapt according to your needs)

	/* SIGNIFICANT DEVIATIONS FROM THE PAP:
		* We have valid data on the dependent variable for only N = 371 individuals, none of whom have observations in consecutive years. 
		* Due to the limited number of valid cases, we deviated from several restrictions specified in the pre-analysis plan (PAP):
			- We include single-year observations (i.e., individuals observed in only one year).
			- We do not restrict the sample to children living in the same household.
			- We do not apply listwise deletion for missing values in independent variables, as this would reduce the sample size to zero.
			- We restrict the control variables to those available for all 371 valid cases.
		* Because we only have single-wave observations per individual, panel data analysis is not feasible. 
		* We therefore conduct an OLS regression with standard errors clustered at the individual (pid) level.
	*/

	keep if GenderEquality!=.
	tab syear, mis
	xtset pid syear
	xtdescribe
	bysort pid: gen n_obs = _N
	* Keep only women and men: not neccesary since their are no other genders represented in the data
	

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)
* Control variables: Woman n_children i.childagegroup1-i.childagegroup8 SES Partner SexualOrientation i.Religion_Father i.Religion_Mother Age Region
	reg GenderEquality Man##i.DaughterHH, vce(cluster pid)
	reg GenderEquality 	Man##i.DaughterHH n_children, vce(cluster pid)
	reg GenderEquality 	Man##i.DaughterHH n_children Partner, vce(cluster pid)
	reg GenderEquality 	Man##i.DaughterHH n_children Partner Age , vce(cluster pid)
	* Full model:
	reg GenderEquality 	Man##i.DaughterHH n_children Partner Age Region, vce(cluster pid)
	
	* Please add a verbal description of your model here	
	reg GenderEquality 	Man##i.DaughterHH n_children Partner Age Region, vce(cluster pid)
	/* We assume that GenderEquality, a continuous index of gender-egalitarian attitudes, reflects support for gender equality (Hypothesis 4).
	   To test whether the effect of having daughters differs between men and women, we run a linear OLS regression with GenderEquality as the dependent variable and an interaction term between gender (Woman) and DaughterHH (child gender composition) as the key independent variable.
	   Control variables include number of children, partnership status, age, and region.
	   We cluster standard errors at the individual level (pid)..
	*/

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.Man#3.DaughterHH]

		* (ii) standard error
		local se = _se[1.Man#3.DaughterHH]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.Man#3.DaughterHH] / _se[1.Man#3.DaughterHH]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.Man#3.DaughterHH]
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
