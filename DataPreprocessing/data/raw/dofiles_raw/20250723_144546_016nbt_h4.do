********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2

/*NOTE: DAS HAT NICHT FUNKTIONIERT; DAHER GLOBAL MANUELL
* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system */
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}

//global ROOT "C:\Users\mandwei1\sciebo\Kollaborationen\Many Daughters\#ManyDaughters_RT_AnalysisPackage"   //we had to set this manually
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

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	keep if kidgeb01==2016 | kidgeb01==2017 // first child 2016 or 2017
	keep if plh0298_v2>0 & plh0298_v2!=. // Info on outcome
	keep if (kidgeb02>0 & kidmon02>0  & kidsex01>0) | kidgeb02==-2 // Info on second child


* GENERATE VARIABLES * (Please adapt according to your needs)

	mvdecode plh0298_v2 kidgeb01  kidmon01  kidsex01 kidsex02, mv(-1/-9) // negatives to missing
	mvdecode kidgeb02 kidmon02, mv(-1, -3/-7) // negatives to missing, except -2 (no second child)
	recode kidgeb02 (-2=0) // zero if no second child
	replace kidgeb02 = 0 if kidgeb02>2018 | (kidgeb02==2018 & kidmon02>pgmonth & syear==2018) // zero if second child born after interview
	replace kidsex02=0 if kidgeb02==0 // don't use second childs gender if born after interview
	
	recode plh0298_v2 (1=7 "Stimme überhaupt nicht zu") (2=6) (3=5) (4=4) (5=3) (6=2) (7=1 "Stimme voll zu"), gen(Y)
	label var Y "Children under age of 6 suffer if mother works"
	
	recode kidsex01 (1=0 "Son") (2=1 "Daughter"), gen(d_daughter) // 1st Child gender variable
	label var d_daughter "1st Child Daughter"
	
	recode kidsex02 (.=0 "No second child") (1=1 "Son") (2=2 "Daughter"), gen(second_gender) // 2nd Child gender variable
	label var second_gender "Gender of second child"


* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Our outcomevariable is Y = Children under age of 6 suffer if mother works
	* We want to test whether having a daughter affects this outcome, and we want to know whether the birth year (or time passed since birth, which are identical here) changes this effect. We also want to control for potential additional children born before the outcome information was collected. We want to test this effect separately for men and women.
	* We hence run two linear OLS models on Y (one for men, one for women) including a dummy variable for the first child being a daughter, which we interact with birth year. We also include a control for the gender of the second child.
	
	reg Y i.d_daughter##i.kidgeb01 i.second_gender if sex ==1 // Men
	est store est_men
	
	margins, dydx(d_daughter) at(kidgeb01=(2016 2017)) 
	matrix list r(table)
    scalar coeff_2016_male = r(table)[1,3]
    scalar coeff_2017_male = r(table)[1,4]
	scalar ste_2016_male = r(table)[2,3]
    scalar ste_2017_male = r(table)[2,4]
	
	* (i) coefficient estimate
		local coefficient_male = coeff_2016_male
	
        local coefficient_male_2017 = coeff_2017_male
		
	
			* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_male = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_male = e(df_r)
		local df_model_male = e(df_m)

		* (v) p-value
		local p_value_male = coeff_2016_male / ste_2016_male
		local p_value_male = 2 * ttail(e(df_r), abs(`p_value_male'))

		local p_value_2017_male = coeff_2017_male / ste_2017_male
		local p_value_2017_male = 2 * ttail(e(df_r), abs(`p_value_2017_male'))
		* (vi) number of observations
		local num_obs_male = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_male)
		local num_participants_male : word count `participants_male'

		* (viii) interpretation of directionality
		scalar coef_d_daughter_male = coeff_2016_male
		local interpretation "positive coefficient is in line with hypothesis"
		scalar coef_d_daughter_2017_male = coeff_2017_male
		if coef_d_daughter_2017_male > 0 {
			local interpretation_2017_male "in line with hypothesis"
		} 
		else {
			local interpretation_2017_male "not in line with hypothesis"
		}

	// Display the saved statistics
	di "Coefficient male: `coefficient_male'"
	di "Standard Error male: `ste_2016_male'"
	di "Coefficient male 2017: `coefficient_male_2017'"
	di "Standard Error male 2017: `ste_2017_male'"
	di "Test Statistic Family: `test_stat_family_male'"
	di "Residual Degrees of Freedom male: `df_residual_male'"
	di "Model Degrees of Freedom male: `df_model_male'"
	di "p-value_male: " `p_value_male'
	di "p-value_male 2017: " `p_value_2017_male'
	di "Number of observations male: " `num_obs_male'
	di	"Number of participants male: " `num_participants_male'
	di "Interpretation of directionality male: " "`interpretation_male'"
	di "Interpretation of directionality male 2017: " "`interpretation_male_2017'"
	
****female	
	reg Y i.d_daughter##i.kidgeb01 i.second_gender if sex ==2 // Women
	est store est_women
	matrix list r(table)
   scalar coeff_2016_female = r(table)[1,3]
    scalar coeff_2017_female = r(table)[1,4]
	scalar ste_2016_female = r(table)[2,3]
    scalar ste_2017_female = r(table)[2,4]
	
	* (i) coefficient estimate
		local coefficient_female = coeff_2016_female
	
        local coefficient_female_2017 = coeff_2017_female
		
	
			* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_female = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_female = e(df_r)
		local df_model_female = e(df_m)

		* (v) p-value
		local p_value_female = coeff_2016_female / ste_2016_female
		local p_value_female = 2 * ttail(e(df_r), abs(`p_value_female'))

		local p_value_2017_female = coeff_2017_female / ste_2017_female
		local p_value_2017_female = 2 * ttail(e(df_r), abs(`p_value_2017_female'))
		* (vi) number of observations
		local num_obs_female = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_female)
		local num_participants_female : word count `participants_female'

		* (viii) interpretation of directionality
		scalar coef_d_daughter_female = coeff_2016_female
		local interpretation "positive coefficient is in line with hypothesis"
		scalar coef_d_daughter_2017_female = coeff_2017_female
		if coef_d_daughter_2017_female > 0 {
			local interpretation_2017_female "in line with hypothesis"
		} 
		else {
			local interpretation_2017_female "not in line with hypothesis"
		}
	
	// Display the saved statistics
	di "Coefficient female: `coefficient_female'"
	di "Standard Error female: `ste_female'"
	di "Coefficient female 2017: `coefficient_female_2017'"
	di "Standard Error female 2017: `ste_2017_female'"
	di "Test Statistic Family: `test_stat_family_female'"
	di "Residual Degrees of Freedom female: `df_residual_female'"
	di "Model Degrees of Freedom female: `df_model_female'"
	di "p-value_female: " `p_value_female'
	di "p-value_female 2017: " `p_value_2017_female'
	di "Number of observations female: " `num_obs_female'
	di	"Number of participants female: " `num_participants_female'
	di "Interpretation of directionality female: " "`interpretation_female'"
	di "Interpretation of directionality female 2017: " "`interpretation_female_2017'"
	
	* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient_male,se_male,coefficient_2017_male,se_2017_male,family_male,residual_df_male,model_df_male,p_value_male, ,p_value_2017_male, num_obs,p_value_male ,num_participants_male,interpretation_male, interpretation_2017_male,  coefficient_female,se_female,coefficient_2017_female,se_2017_female,family_female,residual_df_female,model_df_female,p_value_female, ,p_value_2017_female, num_obs,p_value_female ,num_participants_female,interpretation_female, interpretation_2017_female"
	file write myfile "`coefficient_male', `se_male',`coefficient_2017_male',`se_2017_male',  `test_stat_family_male', `df_residual_male', `df_model_male',`p_value_male',`p_value_2017_ male',`num_obs_male',`num_participants_male',`interpretation_male, `interpretation_2017_male'`coefficient_female', `se_female',`coefficient_2017_female',`se_2017_female',  `test_stat_family_female', `df_residual_female', `df_model_female',`p_value_female',`p_value_2017_ female',`num_obs_female',`num_participants_female',`interpretation_female ' `interpretation_2017_female'" 
	file close myfile

 

exit
