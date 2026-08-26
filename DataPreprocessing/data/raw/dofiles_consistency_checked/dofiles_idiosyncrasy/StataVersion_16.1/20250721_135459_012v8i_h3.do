********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
version 16.1 // indicate the version of stata you are using here
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
	*merge


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
*** 1. Keep respondents with a biological child ***
	*keep if biochild_h == 1 // Deviation from PAP: Variable does not exist in dataset, instead use any birth 
	keep if sumkids > 0 	
	
*** 2. Generate independent variable ***
	*gen has_daughter = 0
*replace has_daughter = 1 if ///
*( pld0175 == 2 | pld0183 == 2 | pld0187 == 2 | pld0191 == 2 | ///
*pld0195 == 2 | pld0199 == 2 | pld0203 == 2)

gen has_daughter = 0
foreach var of varlist kidsex01-kidsex19 {
    replace has_daughter = 1 if `var' == 2
}

*** 3. Generate control variables ***
	*PAP: gen num_children = d11107 if d11107 >0
	gen num_children = sumkids
	
	gen youngest_child_age = pla0003
	gen parent_age = syear - gebjahr if syear >0 & gebjahr > 0

* too many missing values in pgisced11, use pgpsbil instead
	clonevar parent_education = pgpsbil if pgpsbil >0
	gen parent_gender = sex
	*PAP: xtile income_quartile = hghinc , nq (4) if hghinc>0
	replace hghinc = . if hghinc <0
	xtile income_quartile = hghinc , nq (4)
	
	

*** 3. Generate dependent variable ***

* Generate weekly hours for childcare and housework
gen hours_childcare_week = .
replace hours_childcare_week = pli0019_h + pli0022_h + pli0044_h if pli0019_h > 0 & pli0022_h > 0 & pli0044_h > 0

gen hours_housework_week = .
replace hours_housework_week = pli0012_h + pli0016_h + pli0043_h if pli0012_h > 0 & pli0016_h > 0 & pli0043_h > 0

* keep only partnered individuals
keep if parid > 0

* Prepare partner data
preserve
keep pid sex hours_childcare_week hours_housework_week syear
rename pid parid
rename sex partner_sex
rename hours_childcare_week partner_childcare
rename hours_housework_week partner_housework
tempfile partnerdata
save `partnerdata'
restore

* Merge partner data
merge 1:1 parid syear  using `partnerdata'
keep if !missing(hours_childcare_week) & !missing(partner_childcare) & ///
        !missing(hours_housework_week) & !missing(partner_housework)
keep if _merge == 3
		
* Male share of childcare
gen male_childcare = .
replace male_childcare = hours_childcare_week if sex == 1
replace male_childcare = partner_childcare if partner_sex == 1

gen total_childcare = hours_childcare_week + partner_childcare
gen male_share_childcare = male_childcare / total_childcare

* Male share of housework
gen male_housework = .
replace male_housework = hours_housework_week if sex == 1
replace male_housework = partner_housework if partner_sex == 1

gen total_housework = hours_housework_week + partner_housework
gen male_share_housework = male_housework / total_housework



* HYPOTHESIS TEST FOR H3 * 

* Regression model -- WE DECIDE TO GO WITH THE MALE SHARE HOUSEWORK IN THE ANALYSIS AS ONLY ONE OUTCOME VARIABLE CAN BE SELECTED FOR REPORTING.
reg male_share_housework has_daughter i.num_children c.parent_age c.youngest_child_age ///
    i.parent_education i.parent_gender i.income_quartile, robust
	
*reg male_share_childcare has_daughter i.num_children c.parent_age c.youngest_child_age ///
    *i.parent_education i.parent_gender i.income_quartile, robust

	
	
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[has_daughter]

		* (ii) standard error
		local se = _se[has_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[has_daughter] / _se[has_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_has_daughter = _b[has_daughter]
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