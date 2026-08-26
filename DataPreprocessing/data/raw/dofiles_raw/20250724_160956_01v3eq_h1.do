********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* Research Team: [REDACTED], Rocío Castilla

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 14 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:


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

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h1_dataset.dta", clear


* FILTERS * (Please adapt according to your needs)
 
	* Only keep participants from private households
	keep if inlist(pop,1,2)

	

* GENERATE VARIABLES * (Please adapt according to your needs)

	*Explanatory variables
		
		* Count the number of daughters per respondent / observation
		egen num_daughters = anycount(kidsex01-kidsex19), values(2)
		
		* Generate dummy variable for whether participant has at least one daughter (1) or not (0)
		gen many_daughters=0
		replace many_daughters=1 if num_daughters!=0
		
		
		
	* Outcome variables
	
		* Merging outcomes: plh0308_v1 and plh0308_v2 as they reflect our best understanding of gender equality (i.e., they reflect attitudes towards genders: gender equality in work and family)
		
		* Treat observations with -8 (Question this year not part of the survey), -5 (Not included in this version) and -1 (No answer) as missing:
		replace plh0308_v1=. if plh0308_v1<0
		replace plh0308_v2=. if plh0308_v2<0
		
		/* Rearrange categories for plh0308_v1 (scale is the opposite as plh0308_v2)
		i.e., plh0308_v1: Man/woman are equal in work and household/family; scale 1 completely agree to 4 completely disagree
		plh0308_v2: Best if man and woman work the same amount so they can share the responsibility; scale 1 completely disagree to 7 completely agree*/
		replace plh0308_v1 = 5 - plh0308_v1 if !missing(plh0308_v1)
		
		* Rescale so that both variables have the same number of potential answers
		gen plh0308_v2_rescaled = .
		replace plh0308_v2_rescaled = 1 if inlist(plh0308_v2, 1, 2)      // Strongly disagree
		replace plh0308_v2_rescaled = 2 if inlist(plh0308_v2, 3, 4)      // Somewhat disagree  
		replace plh0308_v2_rescaled = 3 if inlist(plh0308_v2, 5, 6)      // Somewhat agree
		replace plh0308_v2_rescaled = 4 if plh0308_v2 == 7              // Strongly agree

		
		* Merge both into a new variable
		gen gender_equality_attitude = .
		replace gender_equality_attitude = plh0308_v1 if !missing(plh0308_v1)
		replace gender_equality_attitude = plh0308_v2_rescaled if !missing(plh0308_v2)
	
	




* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	/* Assuming that parents do not follow a gender-based fertility interruption rule (like the one-child policy in China, for example), we assume that, once respondents have a child, the sex of the child cannot be chosen. 
	This allows us to use a simple OLS regression to analyze the effects of having at least one daughter on gender equality (as measured by attitudes), after controlling for the number of kids (sumkids)
	Since this question is only asked in the survey from 2018, we do not consider that we should control for a time trend.*/

	reg gender_equality_attitude many_daughters sumkids //The higher the coefficient the more gender equal
	



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[many_daughters]

		* (ii) standard error
		local se = _se[many_daughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[many_daughters] / _se[many_daughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_many_daughters = _b[many_daughters]
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
