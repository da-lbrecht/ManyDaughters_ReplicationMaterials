********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht
* Revised by Research Team RT01m03n: [REDACTED], [REDACTED]


* ============================
* Setup * 
* ============================

* clear all
version 17.0 // indicate the version of stata used here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* ssc install outreg2
* ssc install ftools
* ssc install reghdfe, replace

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

	* Merge ppathl with all datasets, relevant for the analysis. 

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	** Merge also with biol data to gather the remaining original variables needed.
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches)

	
	label language EN // change language of the labels to English
	
* Reduce dataset to retain only relevant variables.
keep pid hid syear ///
     sex ///
     kidsex* kidgeb* ///
	 plh0308_v1 lb1120 plh0308_v2 ///
	 gebjahr sumkids pgpartz ///
	 pbleib	

	save "$DATA/h4_dataset_RT01m03n.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * 
 
	* 1. Generate flags per child if that child is a daughter born before 2000. 
gen byte daughter_pre2000 = 0

forvalues i = 1/19 {
    local k = string(`i', "%02.0f")  // formats 1 as "01", 2 as "02", ..., 19 as "19"
    quietly replace daughter_pre2000 = 1 if kidsex`k' == 2 & kidgeb`k' < 2000
}

	* 2. Drop participants with daughters born before 2000
drop if daughter_pre2000 == 1

* GENERATE VARIABLES *

	* 1. Generate main predictor: binary variable to measure whether a given respondent has (1) one or multiple daughters or (0) no children or one or multiple sons born by the current year.
	** Using variables kidgeb* kidsex*. Binary assessed at all panel waves (2000-2020)
	*** Note: The answer "Does not apply (coded as -2)" in var kidgeb* is assumed to measure absence of children, thus coded as 0

gen byte daughter_any = 0

forvalues i = 1/19 {
    local k = string(`i', "%02.0f")
    quietly replace daughter_any = 1 if kidsex`k' == 2 & kidgeb`k' <= syear
    quietly replace daughter_any = . if kidgeb`k' == -3 | kidgeb`k' == -1
}

	* 2. Recoding original sex variable so that 1 indicates being Male.
recode sex (1=1) (2=0), gen(sex_bin)
	label define sexlbl 0 "Female" 1 "Male"
    label values sex_bin sexlbl
	
	* 3. Generate main outcome: respondent's attitude toward gender equality. 
	** Using the following items: f12p116k (in pl seems defined as plh0308_v1; assessed in 2012), lb1120 (assessed between 2010-2013), plh0308_v2 (assessed in 2018).
	*** Note: The data documentation suggested that lb1120 was assessed in 2020. This was thus what we indicated in the pre-analysis plan (PAP). However, the measure was instead available in the dataset in the years 2010 to 2013. We used it as available.
	
	* 3.1 Recode missing values to Stata system missing (.)
foreach var in plh0308_v1 plh0308_v2 lb1120 {
    replace `var' = . if inlist(`var', -8, -5, -1)
}

	* 3.2 Reverse code plh0308_v1 and lb1120 (they're 1 = most egalitarian, 4 = least). 
	*** Note: This reverse coding was not indicated in the PAP.
gen plh0308_v1_r = 5 - plh0308_v1 if !missing(plh0308_v1)
gen lb1120_r     = 5 - lb1120     if !missing(lb1120)

	* 3.3 Standardize all three variables (puts them on the same metric)
	*** Note: The three individual measures were standardized to harmonize their scale. plh0308_v2 was on a range 1-7 different from the other two measures (1-4). The standardizing procedure was not indicated in the PAP but we consider it is needed to correctly run the analysis.
egen z_plh0308_v1_r = std(plh0308_v1_r)
egen z_lb1120_r = std(lb1120_r)
egen z_plh0308_v2 = std(plh0308_v2)

	* 3.4 Compute average across standardized items (higher = more egalitarian)
	*** Note: because the item lb1120, differently from what we indicated in the PAP, is available between the years 2010-2013 but not in 2020, we generate an index by averaging the three measures so that 1) if a value is only available for one variable in a given year, the index will take the value of the non-missing variable, 2) if values are available for more than one variable, the index will take the mean of any of the variables that were available.
egen gender_attitudes = rowmean(z_plh0308_v1 z_lb1120 z_plh0308_v2)


	* 4. Generate covariates: age and partnership status.
	
	** 4.1. Generate age:computed as survey year - year of birth.
gen age = syear - gebjahr

	** 4.2. Generate partnership status: using the pgpartz variable to assess the respondent's partnership status, distinguishing between (0) no partner and (1) spouse/registered partner, partner, probably spouse/registered partner, probably partner.
gen partner_status = .
	replace partner_status = 0 if pgpartz == 0
	replace partner_status = 1 if inlist(pgpartz, 1, 2, 3, 4)
	label define partner_lbl 0 "No partner" 1 "Has partner"
	label values partner_status partner_lbl

	

* HYPOTHESIS TEST FOR H4 * 

	* Fixed effects model.

	** We assume that the three items combined above in gender_attitudes are an appropriate measure to "capture" support for gender equality (hypothesis 4). At higher values, the respondent is more likely to support gender equality.

	** We run a fixed effects model with gender_attitudes as the dependent variable and the dummy variable daughter_any as the main independent variable. 
	
	** set data as panel so we can use Stata's panel data xt commands and the time series operators. 
xtset pid syear
		
	** We predict respondents' attitudes toward gender equality as a function of whether they had one or multiple daughters in the preceding panel wave. The model controls for age, sumkids (number of children), and partner_status (partnership status) also measured at the preceding panel waves. To include in the model all the predictors assessed in the preceding panel wave, we transformed them before running the model using the time series operator L1. 
foreach var in daughter_any age sumkids partner_status {
    gen `var'_L1 = L1.`var'
}
	
	** This model builds on the one we estimated to test H1. Compared to the model estimated for H1, we added an interaction between the main predictor daughter_any_L1 and an indicator for being female or male sex_bin. Generate the interaction term as a separate variable to simplify the extraction of the coefficient.
gen daughter_sex_interaction = daughter_any_L1 * sex_bin
	
	** We estimate cluster-robust standard errors at the individual level (using the pid variable) and employ weights to adjust the estimation to attrition bias, using the inverse staying probability weight (pbleib). 
	*** Note: No design weight, in contrast to what we indicated in the PAP, because a design weight was not available in the data. The variable sumkids is omitted from the model due to collinearity with the fixed effects.
	
reghdfe gender_attitudes daughter_any_L1 ///
        age_L1 sumkids_L1 partner_status_L1 /// 
		daughter_sex_interaction /// interaction term
  [pweight = pbleib] /// applying the weight 
  , absorb(pid) /// fixed effects option
  vce(cluster pid)
  
 

* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[daughter_sex_interaction]

		* (ii) standard error
		local se = _se[daughter_sex_interaction]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[daughter_sex_interaction] / _se[daughter_sex_interaction]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_interaction = _b[daughter_sex_interaction]
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
	file open myfile using "$OUT/h4_results.csv", write replace  // Renamed to original naming convention by David Albrecht on 15.07.2025
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
