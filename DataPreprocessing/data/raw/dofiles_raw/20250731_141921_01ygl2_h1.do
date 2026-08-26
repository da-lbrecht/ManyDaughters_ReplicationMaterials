********************************************************************************
********************** Testing H1: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

*----------------------------------------------------------------------------------*
* H1. Having daughters leads to stronger support for gender equality (as measured by attitudes).
*----------------------------------------------------------------------------------*

/* Verbal description:

		Assumptions
		*----------------------------		
		
		Our strategy compares parents of boys to parents of girls. This approach has
		two implicit assumptions. First, that parents are not a random subsample. So, 
		direct comparisons of parents and non-parents does not make much sense. 
		Second, we assume that the gender of the newborn is random. In other words, we pressume
		selective abortion to be rare / non existent in Germany. Given these assumptions, 
		potantial outcomes are independent of the gender of the newborn. So, a 
		difference in means suffices to identify the effect of parenting a daughter.
		
		Initially, we wanted to restrict the sample to individuals who have just had
		their first children. Unfortunately, this was not possible, as the number
		of children born in each year is relatively small, and (more importantly)
		some women might be already expecting a second child. 
		
		So, we opted to rely on the randomness of the gender of the first born to 
		identify the effect of parenting a daughter. The regression does not include 
		any additional variables that were realized after the birth of the first child, 
		as these variables are likely endogenous to the model. Perhaps parents decide
		to spend more time at home if the child is a "girl" ( or a "boy"), in 
		which case controlling for thse variables will block the true effect. 
		Controlling for the number of children is also problematic, as this variable
		is also endogeneous to the gender of the first child. Perhaps more conservative 
		couples have a preference for boys, which means that the subsample of 
		respondents with a single daughter has a different distribution of unobservables
		(traditional views) than families with only one son. 
	
				
		Implementation
		*------------------
		
		The survey contains several variables that reflect gender norms
		(e.g. whether children suffers when one parent works). Unfortunately these variables
		are collected in one survey, which limits the options available. 
		
		Instead of evaluating variables one by one, we use PCA to reduce 
		the dimensionality to the first eigenvalue for the main regression. In this 
		new variable higher values indicate tendency to greater gender equality. 
		A complete analysis would also entail looking at each component separately.
		
		The following variables were used in the PCA. All data was collected in 2012
		* plh0298_v1: Child under 6 suffers if mother works (FID) [2012]
		* plh0302_v1: Child under 3 suffers if mother works (FID) [2012]
		* plh0308_v1: Family: Man and woman should share equally in work and household/family duties
		* plh0301: Women should focus more on the family than on their career
		
		We also introduce some sample restrictions. 
			- We include only parents at the time of the survey (based on date of birth of first child)
			- We focus on couples headed by a German national (variable pop). 
			- We exclude individuals who did not answer all questions on gender attitudes
			- We exclude individuals with a unique year of birth of the first child (required for SE)

		In the regression we also include the year of birth of the firstborn as 
		an additional control. The goal is to restrict comparisons to parents of children
		of the same age. 
		
			*/
	
* ============================
* Setup * 
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000


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
*    Data Preparation    * 
* ============================

* LOAD AND MERGE DATA * 

use "$IN/ppathl.dta", clear 

	use cid pid hid syear parid partner sex gebjahr pop using "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)

	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0301 plh0308_v1  plh0302_v1 plh0298_v1)

	label language EN // change language of the labels to English

save "$DATA/h1_dataset.dta", replace // save merged dataset


*  GENERATE VARIABLES *

use "$DATA/h1_dataset.dta", clear 

** Selecting the sample
keep if syear == 2012

*Drop individuals who did not answer at least one question on gender norms
foreach var in plh0301 plh0308_v1  plh0302_v1 plh0298_v1{
		drop if `var' <0 | mi(`var')			
		}

* Exclude individuals who were childless at the time of the survey, do not have 
* information on gender, or who are foreing nationals.
drop if kidgeb01<0 | kidgeb01>2012
keep if inlist(pop,1,2)

bys kidgeb01 : gen nobs = _N

** New variables
gen age = syear - gebjahr
gen men = sex ==1 if !mi(sex)
gen first_daughter = kidsex01==2 if !mi(kidsex01)

pca plh0298_v1 plh0302_v1 plh0308_v1 plh0301, com(1)
predict gender_equality_attitudes


*** Testing for H1
reg gender_equality_attitudes i.first_daughter i.kidgeb01  if nobs>1 ,  vce(hc3)
 



* EXTRACT AND EXPORT RESULTS *
	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.first_daughter]

		* (ii) standard error
		local se = _se[1.first_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.first_daughter] / _se[1.first_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.first_daughter]
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


 