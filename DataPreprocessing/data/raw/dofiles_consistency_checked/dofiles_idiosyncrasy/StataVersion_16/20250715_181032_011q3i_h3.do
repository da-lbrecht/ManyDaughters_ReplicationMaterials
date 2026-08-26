********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
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
	
	* Merge with individual questionnaire (pl)
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master match) ///
    keepusing(pli0043_h pli0044_h pli0046 pli0049_h)
	rename (pli0043_h pli0044_h pli0046 pli0049_h) ///
	       (p1_housework p1_childcare p1_caregiving p1_repairs)
		   
	* merge partner id
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgpartnr)
	* Rename partner ID
	rename pgpartnr pid_partner
	drop if pid_partner==. | pid_partner==-2 // no partner match
	save "$DATA/h3_dataset_p1.dta", replace 
	
		   
	* Merge with individual questionnaire (pl -- PARTNER)
	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	keep pid syear sex partner sexor sampreg
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master match) ///
    keepusing(pli0043_h pli0044_h pli0046 pli0049_h)
	rename (pli0043_h pli0044_h pli0046 pli0049_h) ///
	       (p2_housework p2_childcare p2_caregiving p2_repairs)
		   rename pid pid_partner
	save "$DATA/h3_dataset_p2.dta", replace 
	
	* Merge p1 & p2
	use "$DATA/h3_dataset_p1.dta", clear
	merge 1:1 pid_partner syear using "$DATA/h3_dataset_p2.dta", nogen keep(master match) ///
    keepusing(p2_housework p2_childcare p2_caregiving p2_repairs)
	order pid* p1* p2*
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex* gebjahr)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgisei08)
	* bioparen
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches) keepusing(freli mreli fisei08 misei08)
	* biol
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches) keepusing(lr3193 lb0285 lb0287_h lb0290_h lb0293_h lb0296_h lb0299_h lb0302_h lb0305_h lb0308_h)

	
	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
* n (person)=3,125; n(person-years)=18,731
	sort pid syear
	egen person_max=tag(pid)	
	sum person_max if person_max==1
	
* GENERATE VARIABLES * (Please adapt according to your needs)

	* ------------------------------------------------------------------------------
	* 1.1 CONSTRUCT DEPENDENT VARIABLE: GenderEqualityHHᵢₜ
	* ------------------------------------------------------------------------------
	* H3: Gender Equality in Household Behavior
	* pid: p1_housework p1_childcare p1_caregiving p1_repairs
	* pid_partner: 2_housework p2_childcare p2_caregiving p2_repairs
	recode p*_housework p*_childcare p*_caregiving p*_repairs (-8 -5 -3 -2 -1 = .)
	
	* Step 1: Calculate absolute differences in hours per domain (inequality)
	gen diff_housework   = abs(p1_housework   - p2_housework)   if !missing(p1_housework, p2_housework)
	gen diff_childcare   = abs(p1_childcare   - p2_childcare)   if !missing(p1_childcare, p2_childcare)
	gen diff_caregiving  = abs(p1_caregiving  - p2_caregiving)  if !missing(p1_caregiving, p2_caregiving)
	gen diff_repairs     = abs(p1_repairs     - p2_repairs)     if !missing(p1_repairs, p2_repairs)

	* Step 2: Calculate average inequality across all valid domains (0 = equality)
	egen GenderInequality = rowmean(diff_housework diff_childcare diff_caregiving diff_repairs)

	* Step 3: Reverse code to obtain equality measure (higher = more equality)
	* Note: This rescales the variable based on the maximum observed inequality
	summarize GenderInequality if GenderInequality < ., meanonly
	gen GenderEqualityHH = r(max) - GenderInequality
	label variable GenderEqualityHH "Behavioral Gender Equality (rescaled)"

	
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

	*------------------------------------------------------------------------------*
	* 1.3 CONSTRUCT CONTROL VARIABLES
	*------------------------------------------------------------------------------*

	* Gender
	recode sex (1=0) (2=1), gen(Woman)
	
	* Number of children == n_children
	
	* Child in same household
	recode lr3193 (-8 -5 -2 = .) (1 = 1 "In HH") (2 3 4 5 6 = 0 "Elsewhere"), gen(ChildInHH)

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
	   We deviated from several restrictions specified in the pre-analysis plan (PAP) due to data limitations:
	   - We did not restrict the sample to children living in the same household, as this would have resulted in zero valid cases.
	   - We excluded certain control variables, such as children's age and socio-economic status (SES), because including them would have substantially reduced the sample size.
	   - Instead of using the respondent’s own religion, we used the religion of their parents as a proxy, since this was the only information available.
	*/

	keep if GenderEqualityHH!=.
	tab syear, mis
	xtset pid syear
	xtdescribe
	bysort pid: gen n_obs = _N
	* Keep only individuals with at least 2 observations
	keep if n_obs >= 2
	* Keep only women and men: not neccesary since their are no other genders represented in the data
	
	* Check missing values before listwise deletion
	foreach var in  DaughterHH ///
		Woman Partner SexualOrientation Religion_Mother Religion_Father SES Age Region n_children {
		gen miss_`var' = missing(`var')
	}

	egen total_miss = rowtotal(miss_*)
	gen pct_miss = total_miss / 10 * 100

	* Apply listwise deletion if missing < 10%
	tab pct_miss
	drop if pct_miss > 10

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)
* Control variables: Woman n_children i.childagegroup1-i.childagegroup8 SES Partner SexualOrientation i.Religion_Father i.Religion_Mother Age Region
 
	* Estimate random effects model with panel-robust standard errors
	** Paneldataanalysis with RE models not possible bc we need at least 2 observations (we dont have)
	xtset pid syear
	xtdescribe
	xtreg GenderEqualityHH i.DaughterHH, re vce(robust)
	xtreg GenderEqualityHH i.DaughterHH Woman, re vce(robust)
	xtreg GenderEqualityHH i.DaughterHH Woman n_children, re vce(robust)
	xtreg GenderEqualityHH i.DaughterHH Woman n_children SES, re vce(robust)
	xtreg GenderEqualityHH i.DaughterHH Woman n_children Partner, re vce(robust)
	xtreg GenderEqualityHH i.DaughterHH Woman n_children Partner SexualOrientation, re vce(robust)	
	xtreg GenderEqualityHH i.DaughterHH Woman n_children Partner SexualOrientation i.Religion_Father i.Religion_Mother, re vce(robust)
	xtreg GenderEqualityHH i.DaughterHH Woman n_children Partner SexualOrientation i.Religion_Father i.Religion_Mother Age, re vce(robust)
	* Full model
	xtreg GenderEqualityHH i.DaughterHH Woman n_children Partner SexualOrientation i.Religion_Father i.Religion_Mother Age Region, re vce(robust)
	

	* Please add a verbal description of your model here	
	xtreg GenderEqualityHH i.DaughterHH Woman n_children Partner SexualOrientation i.Religion_Father i.Religion_Mother Age Region, re vce(robust)
	/* We assume that GenderEqualityHH, a continuous measure of behavioral gender equality in the household (based on the relative division of labor between partners), reflects support for gender equality (Hypothesis 3).
	   Accordingly, we run a random-effects linear panel regression with GenderEqualityHH as the dependent variable and DaughterHH (categorical: child gender composition) as the key independent variable.
	   Control variables include gender (Woman), number of children, sexual orientation, parental religion (mother/father), age, and region.
	   Due to collinearity, partnership status was omitted from the model.
	   We use panel-robust standard errors and cluster at the individual level (pid).
	*/


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

		* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[3.DaughterHH]

		* (ii) standard error
		local se = _se[3.DaughterHH]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z" // Adapted by DA during consitency check

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value // Adapted by DA during consitency check
		local z_stat = _b[3.DaughterHH] / _se[3.DaughterHH]
		local p_value = 2 * (1 - normal(abs(`z_stat')))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[3.DaughterHH]
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
