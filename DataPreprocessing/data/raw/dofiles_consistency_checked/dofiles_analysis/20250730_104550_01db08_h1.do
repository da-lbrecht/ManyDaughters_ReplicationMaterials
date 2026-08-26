************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 18.5 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
/*
* CONFIGURE FILE PATHS * (Please do not change)
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
*cd "/Users/[REDACTED]/Desktop/PhD Stuff/My Research/ManyDaughters/#ManyDaughters_RT_AnalysisPackage"
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

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	*Save only subset containing valid values of plh0308_v1 and plh0308_v2 with exactly one child
	keep if ((plh0308_v1 > 0 & plh0308_v1 != .) |  (plh0308_v2 > 0 & plh0308_v2 != .)) & (kidsex02 == -2 & kidsex01 > 0 & !missing(kidsex01))
	
	*Replace remaining missing values with actual missings
	replace plh0308_v1 = . if plh0308_v1 < 0
	replace plh0308_v2 = . if plh0308_v2 < 0
	
	*Keep only relevant variables
	keep pid syear pgmonth plh0308_v1 plh0308_v2 kidsex01 kidgeb01 kidmon01 sex gebjahr gebmonat
	
	*Dropping observations who have not already given birth to their baby at the time of the survey
	drop if syear < kidgeb01
	drop if (pgmonth < kidmon01 & syear == kidgeb01 & kidmon01 > 0 & !missing(kidmon01)) 
	*Kids with unknown birth months but born in the same year as the survey are assumed to be already born and zero years old

* GENERATE VARIABLES * (Please adapt according to your needs)

	*Generate standardized outcome
	gen standard_attitude_respondent = .
	
	*Reverse-score outcome in 2012 (so that 'stimme voll' is the maximum, rather than the minimum value)
	replace plh0308_v1 = 8 if plh0308_v1 == 1
	replace plh0308_v1 = 7 if plh0308_v1 == 2
	replace plh0308_v1 = 6 if plh0308_v1 == 3
	replace plh0308_v1 = 5 if plh0308_v1 == 4
	replace plh0308_v1 = plh0308_v1 - 4
	
	*Replace the standardized outcome with the standardized 2012 question for 2012 observations
	qui sum plh0308_v1
	replace standard_attitude_respondent = (plh0308_v1 - r(mean))/r(sd)
	
	*Replace the standardized outcome with the standardized 2018 question for 2018 observations
	qui sum plh0308_v2
	replace standard_attitude_respondent = (plh0308_v2 - r(mean))/r(sd) if missing(standard_attitude_respondent)
	
	*Code first-child-daughter dummy
	gen first_child_daughter = (kidsex01 == 2)
	
	*Code age of child in years
	gen surveymonth = ym(syear, pgmonth)
	gen childbirthmonth = ym(kidgeb01, kidmon01)
	gen age_of_child = floor((surveymonth - childbirthmonth)/12)
	replace age_of_child = syear - kidgeb01 if missing(age_of_child)
	
	*Code age of respondent in years
	gen respondentbirthmonth = ym(gebjahr, gebmonat)
	gen age_respondent = floor((surveymonth - respondentbirthmonth)/12)
	
	*Generate respondent gender
	gen gender_respondent = (sex == 2)
	
	*Rename survey year and personal ID
	rename syear year
	rename pid respondent_ID
	

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	*Testing the code we pre-registered, it appears that the margins() command does not evaluate the average marginal effect of first_child_daughter on standard_attitude_respondent. We suspect that this is because we only have 5% of the dataset at the moment, and treating child age as a factor variable when interacting with first_child_daughter could be yielding a sparsity issue, where not enough children are observed with the same age but different genders. This could potentially be remedied with the receipt of the full data. We thus implement a conditional strategy: if the margins() average marginal effect evaluates, then we will use that average marginal effect, as it comes from the model we pre-registered. If it does not evaluate, then we will change child age from being treated as a factor variable to being treated as a continuous variable. Our testing implies that this parameterization will yield a defined average marginal effect estimate 
	
	*Original pre-registered model
	reg standard_attitude_respondent i.first_child_daughter##i.age_of_child i.gender_respondent i.age_respondent i.year, vce(cluster respondent_ID)
	margins, dydx(*) post
	
	*If the pre-registered model does not evaluate...
	if e(b)[1, 2] == 0 {
		
		*Estimate the modified model
		reg standard_attitude_respondent i.first_child_daughter##c.age_of_child i.gender_respondent i.age_respondent i.year, vce(cluster respondent_ID)
		margins, dydx(*) post
		
	}


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)


	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = e(b)[1, 2]

		* (ii) standard error
		local se = sqrt(e(V)[2, 2])

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = r(table)[4, 2]
		dis `p_value'

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof respondent_ID if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality

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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
