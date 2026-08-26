********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 19.5 // BE-Basic Edition, indicate the version of stata you are using here
             // maxvar not settable in this version of Stata (maxvar is fixed at 2,048)

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2

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
		
	* The full 'pl.dta' file contains ~4,000 variables, which exceeds Stata's
	* 2,048-variable limit (my version of stata). To avoid this, we create a pl subset
	* containing only the merge keys (pid, syear, hid, cid) and the three
	* questionnaire items needed for H4 (plh0298_v2, plh0302_v2, plh0308_v2).
	
   * pl scontains all variables from the individual questionnaire
	use pid syear hid cid plh0298_v2 plh0302_v2 plh0308_v2 using "$IN/pl.dta", clear
	save "$IN/pl_subset_h4.dta", replace
	
	* Merge pl with all datasets, that we deem relevant for our analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex01 kidgeb01)
	
	* ppathl 
	merge m:1 pid syear using "$IN/ppathl.dta", nogen keep(master matches)
	
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)

	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h4_dataset.dta", clear

* GENERATE VARIABLES * (Please adapt according to your needs)

	*1*Generate DEPENDENT VARIABLE support_gender_equality_attitudes called support_h4
	
	*Recode nonresponse values (–1 through –8) to missing
	mvdecode plh0298_v2 plh0302_v2 plh0308_v2, mv(-1/-8=.)
	*Reverse‐code the "traditional norms" items so higher = more egalitarian
    gen r_plh0298 = 8 - plh0298_v2
    gen r_plh0302 = 8 - plh0302_v2
	*Compute unweighted additive index (range 3–21)
    egen support_idx = rowtotal(r_plh0298 r_plh0302 plh0308_v2)
    *Rescale to [0,1]: (support_idx − 3) / 18
    gen support_h4 = (support_idx - 3) / 18
	

    *2*Generate INDEPENDENT VARIABLE firstborn_child_female called 'daughter_h1'
   
   *Code firstborn_child_female as 1 if firstborn is female (kidsex[01]=2), 0 if male (kidsex[01]=1)
    gen firstborn_child_female = .
    replace firstborn_child_female = 1 if kidsex01 == 2
    replace firstborn_child_female = 0 if kidsex01 == 1
	
	*3* Compute the age group of the firstborn child in 2018
	  * (survey year – birth year of first child)
    gen firstborn_age_2018 = 2018 - kidgeb01    // kidgeb01 = birth year of first child 

      *Categorize into three mutually exclusive age groups:
       *    0 = 0–5 years; 1 = 6–10 years; 2 = 11–18 years
    gen firstborn_age_group_2018 = .
    replace firstborn_age_group_2018 = 0 if inrange(firstborn_age_2018,  0,  5)
    replace firstborn_age_group_2018 = 1 if inrange(firstborn_age_2018,  6, 10)
    replace firstborn_age_group_2018 = 2 if inrange(firstborn_age_2018, 11, 18)
	
	*3*Generate CONTROLS VARIABLES sex, age, and education level
    *Respondent sex → respondent_male (0 = female, 1 = male)
    gen respondent_male = .
    replace respondent_male = 1 if sex == 1       // 
    replace respondent_male = 0 if sex == 2

    *Age in 2018 and its square
    gen age_2018        = 2018 - gebjahr           // 
    gen age_square_2018 = age_2018^2

    *Education level (0=Primary or below; 1=Secondary; 2=Post-secondary; 3=Tertiary)
    gen education_level = .
    replace education_level = 0 if inrange(pgisced11, 0, 1)
    replace education_level = 1 if inrange(pgisced11, 2, 3)
    replace education_level = 2 if inrange(pgisced11, 4, 5)
    replace education_level = 3 if inrange(pgisced11, 6, 8)  // 

* FILTERS *

* 1. Restrict to respondents surveyed in 2018
keep if syear == 2018

* 2. Drop respondents with missing dependent variable
drop if missing(support_h4)
drop if support_h4 < 0

inspect support_h4

* 3. Drop respondents with no firstborn information
drop if missing(firstborn_child_female)
drop if missing(firstborn_age_group_2018)

inspect firstborn_child_female
inspect firstborn_age_group_2018

inspect respondent_male
* 4. Drop respondents missing key controls
drop if missing(respondent_male)
drop if missing(age_2018)
drop if missing(age_square_2018)
drop if missing(education_level)

inspect respondent_male
inspect age_2018
inspect age_square_2018
inspect education_level

* 4. Restrict age
keep if inrange(age_2018, 18, 54)
keep if inrange(age_square_2018, 324, 2916)

*5. Ensure at least one child under 18
keep if firstborn_age_2018 <= 18

*CHECK THE REMAINING SAMPLE SIMPLE
count  

**COUNT RESULTS : 377 OBSERVATIONS
* For the main effect (models 1 and 2), assuming a small effect size of Cohen's d = 0.15. With a significance level of 𝛼 = 0.05 and desired power = 0.80, 
* we would require approximately 485 individuals per group (i.e., 1,940 total observations) to detect a statistically significant difference between 
* respondents with firstborn daughters and those with firstborn sons.

tabulate firstborn_child_female respondent_male
** The results indicate 
* firstborn_child_female = 0 ; respondent_male = 1 : 95 observations ; extrapolation = 1900
* firstborn_child_female = 1 ; respondent_male = 1 : 64 observations ; extrapolation = 1280
* firstborn_child_female = 0 ; respondent_male = 0 : 128 observations ; extrapolatio = 2560
* firstborn_child_female = 1 ; respondent_male = 0 : 90 observations ; extrapolation =1800
*==> exceeds the required per-group sample size for sufficient power.


* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)

	* In the PAP, the results should be presented in a single table with one column. 
	* Our RT misunderstood and thought it implied a single table with multiple columns (and thus the possibility of having multiple estimations). 
	* Accordingly, of the four models proposed in the PAP, we generated and estimated, we chose to extract results only for the control model (Model 2) 
	* and not the age-group effects models (Models 3 and 4). 
	* We present the code for the others models but their results were not extracted.
	
	* We consider a: linear OLS regression

	* Let's assume that the index we built previsously based on plh0298_v2 plh0302_v2 plh0308_v2 is a good measure of support for gender equality (hypothesis 4).
	* If having a firstborn child female, women are more likely to support gender equality as compared to men (1 complete support, 0 absence of support).
	* Accordingly, we run a linear OLS regression with support_h4 as the dependent variable and firstborn_child_female, respondent_female (sex), interaction first_born_child_female#respoendent_male age_2018 
	*(and its square), and education_level as independent variables, using analytic weights (phrf) and clustering standard errors by hid.

	**In the PAP: model 2
	reg support_h4 ib0.firstborn_child_female ib0.respondent_male ib0.firstborn_child_female#ib0.respondent_male age_2018 age_square_2018 education_level [aweight=phrf], cluster(hid)
	
	*Joint hypothesis test wald test
	test 1.firstborn_child_female#1.respondent_male
	
	 //OTHERS MODELS IN THE PAP 
	 * Model 1: daughter only
      *reg support_h4 ib0.firstborn_child_female [aweight=phrf], cluster(hid)

     // Model 3: + age-group interactions
      * reg support_h4 ib0.firstborn_child_female##i.firstborn_age_group_2018 [aweight=phrf], cluster(hid)

    // Model 4: full (all interactions + controls)
     *reg support_h4 ib0.firstborn_child_female##i.firstborn_age_group_2018##ib0.respondent_male i.education_level age_2018 age_square_2018 [aweight=phrf], cluster(hid)

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.firstborn_child_female#1.respondent_male]

		* (ii) standard error
		local se = _se[1.firstborn_child_female#1.respondent_male]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.firstborn_child_female#1.respondent_male] / _se[1.firstborn_child_female#1.respondent_male]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_interaction = _b[1.firstborn_child_female#1.respondent_male]
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
