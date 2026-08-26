********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
*Set working directory
*cd 
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000


* INSTALL PACKAGES * 

*ssc install outreg2

* CONFIGURE FILE PATHS *
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
	merge m:m pid using "$IN/biol.dta", nogen keep(master matches)
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches)
	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)
	
merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) 
	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* CHILDREN VARIABLES * 
	
	*Generate the dummy variable for children
	gen d_child=0
	replace d_child=1 if sumkids>0
	
	*Generate dummy variables for childrens.
	forvalues i = 2/19 {
    gen child_`i' = (sumkids == `i')
}
	*Generate the dummy variable for at least one daughter
	gen d_daughter = 0

	foreach var of varlist kidsex01-kidsex19 {
		replace d_daughter = 1 if `var' == 2
		}
	
	*Keep only the adults with at least one children
	keep if d_child==1
	
	*Partner variables for no singles.
	gen double parid_nosingle=.
	replace parid_nosingle=parid if parid>0
	format parid_nosingle %12.0g
	
* GENERATE CONTROL VARIABLES * 	
	*Gender variables
	gen father=0
	replace father=1 if sex==1
	
	*Age
	gen age=syear-gebjahr
	
	*Migration background
	gen direct_mig=0
	replace direct_mig=1 if migback==2
	
	gen indirect_mig=0
	replace indirect_mig=1 if migback==3
	
	*Region of birth
	gen west_region=.
	replace west_region=1 if sampreg==1
	replace west_region=0 if sampreg==2
	
	*Education
	gen education=.
	replace education=pgbilzeit if pgbilzeit>=0
	
* HOUSEWORK VARIABLE *
	
	*Generate the housework variable by steps:
	
	* 1. Generate the housework variables.
		*Childcare
	gen childcare = cond(pli0019_h >= 0, pli0019_h, 0) + ///
                cond(pli0022_h >= 0, pli0022_h, 0) + ///
                cond(pli0044_h >= 0, pli0044_h, 0)

	replace childcare = . if (pli0019_h < 0 | missing(pli0019_h)) & ///
                      (pli0022_h < 0 | missing(pli0022_h)) & ///
                      (pli0044_h < 0 | missing(pli0044_h))
					  
		*Repairs
	gen repairs = cond(pli0031_h >= 0, pli0031_h, 0) + ///
              cond(pli0034_v1 >= 0, pli0034_v1, 0) + ///
              cond(pli0049_h >= 0, pli0049_h, 0)

	replace repairs = . if (pli0031_h < 0 | missing(pli0031_h)) & ///
                     (pli0034_v1 < 0 | missing(pli0034_v1)) & ///
                     (pli0049_h < 0 | missing(pli0049_h))
					 
		*Chores
	gen chores = cond(pli0012_h >= 0, pli0012_h, 0) + ///
             cond(pli0016_h >= 0, pli0016_h, 0) + ///
             cond(pli0043_h >= 0, pli0043_h, 0)

	replace chores = . if (pli0012_h < 0 | missing(pli0012_h)) & ///
                    (pli0016_h < 0 | missing(pli0016_h)) & ///
                    (pli0043_h < 0 | missing(pli0043_h))
	
	* 1. Calculate the individual's total hours spent on childcare and chores (I–N)
	egen own_hours = rowtotal(childcare repairs chores)
	
	* 2. Save a temporary file with each individual's hours and sex to later match with their partner
	tempfile partner_data
	preserve
	keep pid own_hours partner sex syear
	keep if partner==1 | partner==2 | partner==3 | partner==4
	rename pid parid_nosingle                 // rename to match the partner's ID in the main file
	rename own_hours hours_partner
	rename sex sex_partner
	save partner_data, replace
	restore
	
	* 3. Keep only for the ones who have spouse or partners that are probably spouse.
	keep if partner==1 | partner==2 | partner==3 | partner==4

	
	* 4. Merge partner's hours and sex using parid
	merge 1:1 parid_nosingle syear using partner_data 
		
	* 5. Keep only matched and master's observations.
	keep if _m!=2
	drop _m
	
	* 6. Calculate housework ratio
	* - only if both partners have data
	* - only if couple is opposite-sex
	
	gen housework = .
	replace housework = own_hours / (own_hours + hours_partner) if !missing(own_hours, hours_partner) & sex != sex_partner
	
* HYPOTHESIS TEST FOR H3 * 

	*We will be tested using OLS regressions, with standard errors clustered at the individual level. We will interpret a two-sided p-value <0.05 in the significance test of the coefficient of interest as statistically significant evidence.
	*If having daughters increase the amount of hours that men dedicate to housework, we should observe that the d_daughter regressor is positive.
	*We control for individual level variables.
	*We control for survey wave fixed effects
	
	reg housework d_daughter ///
	child_2 child_3 child_4 child_5 child_6 child_7 child_8 child_9 child_10 ///
    child_11 child_12 child_13 child_14 child_15 child_16 child_17 child_18 child_19 ///
	father age direct_mig indirect_mig west_region education i.syear


* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = _se[d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter] / _se[d_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
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
