********************************************************************************
********************** Do-File H1: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

*clear all
version 16.1 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
* install all packages you need here, for example:
*ssc install outreg2
/*
* CONFIGURE FILE PATHS * 
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
	keep gebjahr syear partner pid parid sex hid piyear
	

	* Merge ppathl with all datasets, that are relevant for the analysis. 

		* biobirth contains information on fertility history
		merge m:1 pid using "$IN/biobirth.dta", keepusing(kidgeb01 kidgeb02 kidgeb03 kidgeb04 kidgeb05 kidgeb06 kidgeb07 kidgeb08 kidgeb09 kidgeb10 kidgeb11 kidgeb12 kidgeb13 kidgeb14 kidgeb15 kidgeb16 kidgeb17 kidgeb18 kidgeb19 kidmon01 kidmon02 kidmon03 kidmon04 kidmon05 kidmon06 kidmon07 kidmon08 kidmon09 kidmon11 kidmon10 kidmon12 kidmon13 kidmon14 kidmon15 kidmon16 kidmon17 kidmon18 kidmon19 kidsex01 kidsex02 kidsex03 kidsex04 kidsex05 kidsex06 kidsex07 kidsex08 kidsex09 kidsex10 kidsex11 kidsex12 kidsex13 kidsex14 kidsex15 kidsex16 kidsex17 kidsex18 kidsex19 sumkids) gen(merge_check)
		drop merge_check
		
		* pgen contains user-friendly data on the individual level that are consolidated from different sources
		merge 1:1 pid syear using "$IN/pgen.dta", keep(master matches) keepusing(pgbilzeit) gen(merge_check)
		drop merge_check
		
		* pl contains all variables from the individual questionnaire
		merge 1:1 pid syear using "$IN/pl.dta", keep(master matches) keepusing(iyear pmonin plj0621 plj0622 plj0623 plj0624 plj0625 plh0004  plc0015_h plb0057_h1 plb0471_h plb0474_h pli0038_h pli0043_h pli0012_h pli0016_h pli0044_h pli0019_h pli0022_h) gen(merge_check)
		drop merge_check
		
		* hgen contains generated variables on household level from the household interview
		merge m:1 hid syear using "$IN/hgen.dta", keepusing(hghinc) gen(merge_check)
		drop merge_check

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* GENERATE VARIABLES * 

	* Generate dummy variable for whether participant has at least one child (1) or not (0)
	foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
	replace kidgeb`x' = . if kidgeb`x' <0
	replace kidmon`x' = . if kidmon`x' <0
	gen birthdate`x' = string(kidgeb`x') + "-" + string(kidmon`x', "%02.0f")
	gen child`x'_birthdate = monthly(birthdate`x', "YM")
	format child`x'_birthdate %tm 
	drop birthdate`x'
	}
	
	replace piyear = . if piyear <0
	replace pmonin = . if pmonin <0 // We prespecified to use imonth. Since this variable was not available we are using pmonin.
	gen interviewdateh = string(piyear) + "-" + string(pmonin, "%02.0f")
	gen interviewdate = monthly(interviewdateh, "YM")
	format interviewdate %tm 
	drop interviewdateh
	
	gen parent = 0 // We pre-specified to use the interviewdate for all observations. Since there are many (systematic) missings, we first use the survey year for children born in previous years and then the interviewdate (when available) for children born in the same year as the survey.
	foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
	replace parent = 1 if parent == 0 & kidgeb`x' < syear & kidgeb`x' != . & syear != . // child born at least in the previous year
	}
	foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
	replace parent = 1 if parent == 0 & child`x'_birthdate < interviewdate & child`x'_birthdate != . & interviewdate != . // child born in the same year as the interview but in a previous month
	}
	
	*Generate a dummy variable for whether participant has a daughter (1) or not (0)
	gen daughter = 0 
	foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		replace daughter = 1 if (kidsex`x' == 2 & daughter == 0 & kidgeb`x' < syear & kidgeb`x' != . & syear != .) | (kidsex`x' == 2 & daughter == 0 & child`x'_birthdate < interviewdate & child`x'_birthdate != . & interviewdate != .)
	}
	replace daughter = . if kidsex01 < 1 & kidsex02 < 1 & kidsex03 < 1 & kidsex04 < 1 & kidsex05 < 1 & kidsex06 < 1 & kidsex07 < 1 & kidsex08 < 1 & kidsex09 < 1 & kidsex10 < 1 & kidsex11 < 1 & kidsex12 < 1 & kidsex13 < 1 & kidsex14 < 1 & kidsex15 < 1 & kidsex16 < 1 & kidsex17 < 1 & kidsex18 < 1 & kidsex19 < 1 
	
	*Generate the age of the respondent 
	tab gebjahr
	gen age = syear - gebjahr if gebjahr>=0 & syear>=0
	
	gen age_sq = age * age
	
	*Generate a dummy variable whether a respondent lives in the same household as their partner (1) or not (0)
	gen same_household = 0 
	replace same_household = 1 if partner == 1 | partner == 2
	
	* Household income
	replace hghinc = . if hghinc == -1| hghinc == -2 | hghinc == -3 // We prespecified to use hlc0005_h. Since this variable was not available we are using hghinc.
	
	* Education 
	replace pgbilzeit = . if pgbilzeit == -1| pgbilzeit == -2

	* Gender 
	gen female = 0 if sex == 1
	replace female = 1 if sex == 2
	
	gen male = 0 if sex == 2
	replace male = 1 if sex == 1
	
	* Age of oldest child
	gen age_oldest_child = .
	foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
	replace age_oldest_child = syear - kidgeb`x' if syear != . & kidgeb`x' != . & age_oldest_child == .
	}
	replace age_oldest_child = . if parent == 0
	
	gen age_oldest_child_sq = age_oldest_child * age_oldest_child

	* Number of children
	foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
	replace sumkids = sumkids - 1 if (kidgeb`x' > syear & kidgeb`x' != . & syear != . & sumkids>=0) | (child`x'_birthdate > interviewdate & child`x'_birthdate != . & interviewdate != . & kidgeb`x' == syear & sumkids>=0)
	}
	gen nb_children_indicator = .
	replace nb_children_indicator = 0  if sumkids == 1
	replace nb_children_indicator = 1 if sumkids == 2 
	replace nb_children_indicator = 2 if sumkids >= 3
	
	gen nb_children_indicator_1 = .
	replace nb_children_indicator_1 = 0 if nb_children_indicator!=.
	replace nb_children_indicator_1 = 1 if nb_children_indicator == 0
	
	gen nb_children_indicator_2 = .
	replace nb_children_indicator_2 = 0 if nb_children_indicator!=.
	replace nb_children_indicator_2 = 1 if nb_children_indicator == 1
	
	gen nb_children_indicator_3 = .
	replace nb_children_indicator_3 = 0 if nb_children_indicator!=.
	replace nb_children_indicator_3 = 1 if nb_children_indicator == 2

	* Composition of children 
	gen nb_sons = 0
	foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
	replace nb_sons = nb_sons + 1 if (kidgeb`x' < syear & kidsex`x' == 1 & kidgeb`x' != . & syear != .  & kidsex`x' != .) | (child`x'_birthdate < interviewdate & kidsex`x' == 1 & child`x'_birthdate != . & interviewdate != . & kidsex`x' != . & kidgeb`x' == syear)
	}
	
	* Create missing indicators for the relevant covariates
	foreach var in hghinc pgbilzeit age age_sq female age_oldest_child age_oldest_child_sq nb_children_indicator nb_children_indicator_1 nb_children_indicator_2 nb_children_indicator_3 nb_sons{
		gen `var'_missing = 0 
		replace `var'_missing = 1 if `var' == .
		replace `var' = 0 if `var' == .
	}
	
	*Prepare the outcome variable
	replace plj0621 = . if plj0621<0
	replace plj0622 = . if plj0622<0
	replace plj0623 = . if plj0623<0
	replace plj0624 = . if plj0624<0
	replace plj0625 = . if plj0625<0 
	
	replace plj0623 = 8 - plj0623 // Invert variables so that high numbers indicate support for gender equality; plj0621 and plj0622 already correct 
	replace plj0624 = 8 - plj0624
	replace plj0625 = 8 - plj0625


* FILTERS * 
 
	* For H1, we only keep the year 2016
	keep if syear == 2016
	
	* Respondents must have at least one child
	keep if parent == 1
	
	* Information on the gender of children must be available
	keep if daughter != .
	
	* Respondents must have valid observations for at least one of the outcome variables of the hypothesis
	keep if plj0621 != . & plj0622 != . & plj0623 != . & plj0624 != . & plj0625 != . 
	
	* Respondents must be no older than 60 years of age 
	keep if age <= 60
	
	* Respondents must live in the same household as their partner
	keep if same_household == 1


* STANDARDIZATION WITHIN SAMPLE * 

	* Generate gender equality attitudes index, using principal component factor analysis retaining the first factor, in the relavant sample
	factor plj0621 plj0622 plj0623 plj0624 plj0625, pcf factors (1)
	predict gender_equality_index_pcf 
	sum gender_equality_index_pcf
	replace gender_equality_index_pcf = (gender_equality_index_pcf-r(mean))/r(sd)

	
* BALANCE TESTS * 
	// We deviate from PAP by using standard errors clustered at the HH level (rather than heteroskedasticity robust standard errors) as both partners are included in the dataset as individual observations

	reg hghinc daughter if hghinc_missing == 0, cluster(hid) 
	
	reg pgbilzeit daughter if pgbilzeit_missing == 0, cluster(hid) 
	
	reg age daughter if age_missing == 0, cluster(hid) 
	
	reg female daughter if female_missing == 0, cluster(hid) 
	
	reg age_oldest_child daughter if age_oldest_child_missing == 0, cluster(hid) 
	
	reg nb_children_indicator_1 daughter if nb_children_indicator_1_missing == 0, cluster(hid) 
	reg nb_children_indicator_2 daughter if nb_children_indicator_2_missing == 0, cluster(hid) 
	reg nb_children_indicator_3 daughter if nb_children_indicator_3_missing == 0, cluster(hid) 
	
	reg nb_sons daughter if nb_sons_missing == 0, cluster(hid) 


* HYPOTHESIS TEST FOR H1 * 

	* Verbal description of the model: We run a linear OLS regression analysis with gender equality, measured by an index in standard deviations (gender_equality_index_pcf), as dependent variable and the dummy variable daughter as independent variable, controlling for various covariates and using standard errors clustered at the household level.

	global controls hghinc hghinc_missing pgbilzeit pgbilzeit_missing age age_missing age_sq age_sq_missing female female_missing age_oldest_child age_oldest_child_missing age_oldest_child_sq age_oldest_child_sq_missing nb_children_indicator_2 nb_children_indicator_2_missing nb_children_indicator_3 nb_children_indicator_3_missing

	reg gender_equality_index_pcf daughter $controls, cluster(hid) 	// We deviate from PAP by using standard errors clustered at the HH level (rather than heteroskedasticity robust standard errors) as both partners are included in the dataset as individual observations



* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[daughter]

		* (ii) standard error
		local se = _se[daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[daughter] / _se[daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local interpretation "positive coefficient is in line with hypothesis"
		
 // Note on interpretation: 
 // In PAP we specified: "We interpret whether a test result is in line with the hypothesis if we reject the null hypothesis at the five percent significance level."; 
 // It should have been: "We interpret whether a test result is in line with the hypothesis if the coefficient is in the hypothesized direction and we reject the null hypothesis at the five percent significance level."

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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
