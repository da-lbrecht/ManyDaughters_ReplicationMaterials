********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
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
* install all packages you need here:

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

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h3_dataset.dta", clear



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
		
		
	
	*Outcome variables
		
		/* Generate a new variable that counts the total number of hours destined to housework/chores, care (childcare and care for other persons in need) and running errands
		   This will allow us to analyse the gender division of labor within the household*/
		   
		 * Variables:
			* Housework/Chores:
				* pli0043_h: Hours weekday on housework/chores
				* pli0012_h: Hours Saturday on housework/chores
				* pli0016_h: Hours Sunday on housework/chores
				
			* Running Errands:
				* pli0040: Hours weekday running errands
				* pli0054: Hours Saturdays running errands
				* pli0011: Hours Sundays running errands
				
			* Childcare:
				* pli0044_h: Hours weekday childcare
				* pli0019_h: Hours Saturdays childcare
				* pli0022_h: Hours Sundays childcare
				
			* Care for other persons in need:
				* pli0046: Hours weekday care other persons in need
				* pli0055: Hours Saturdays care other persons in need
				* pli0057: Hours Sundays care other persons in need
				
			* Multiply by 5 the number of hours for the variables of weekday:
			gen hrs_housework_weekdays=pli0043_h*5
			gen hrs_errands_weekdays=pli0040*5
			gen hrs_childcare_weekdays=pli0044_h*5
			gen hrs_othercare_weekdays=pli0046*5
			
			* Sum up all of the variables to get the total number of hours dedicated to housework and care
			egen total_carework_hours = rowtotal(hrs_housework_weekdays pli0012_h pli0016_h hrs_errands_weekdays pli0054 pli0011 hrs_childcare_weekdays pli0019_h pli0022_h hrs_othercare_weekdays pli0055 pli0057)
		 
			* Ratio of female working hours with respect of the total number of hours dedicated to housework and care per week (at the household level):
			gen female_carework_hours=0
			replace female_carework_hours=total_carework_hours if sex==2
			gen male_carework_hours=0
			replace male_carework_hours=total_carework_hours if sex==1
			
			bysort cid syear: gen gender_equality_out = abs(female_carework_hours - male_carework_hours)
			
			/*
			*Check whether the distance between female carework and male carework is on average positive:
			
			bysort cid syear: gen diff_carework = female_carework_hours - male_carework_hours
			bysort cid syear: egen mean_diff = mean(diff_carework)
			su mean_diff
			*/
			
			



* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

/* Assuming that parents do not follow a gender-based fertility interruption rule (like the one-child policy in China, for example), we assume that, once respondents have a child, the sex of the child cannot be chosen. 
	This allows us to use a simple OLS regression to analyze the effects of having at least one daughter on gender equality (as measured by outcomes/behaviour within the household), after controlling for the number of kids (sumkids).*/


	reg gender_equality_out many_daughters sumkids //The higher the coefficient the less gender equal
	
	
		

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
		local interpretation "negative coefficient is in line with hypothesis"

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
