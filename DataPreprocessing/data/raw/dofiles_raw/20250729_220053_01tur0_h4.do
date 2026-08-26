********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 18
set maxvar 10000 // extend the maximum number of variables to 10,000
* ssc install outreg2

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
	* Keep only measures that will be used in the analysis
    keep pid syear sex gebjahr netto pop migback sampreg 
	* Merge ppathl with datasets.
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0395i* plh0258_h ple0008)
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex* kidgeb*)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgfamstd pgemplst pglabnet pglabgro pgbilzeit)
	* bioparen
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches) keepusing(fsedu msedu)
	
	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* 1. Only keep participants from private households
	keep if inlist(pop,1,2)
	* 2. Only keep respondents who completed the individual questionnaire with household interview (with survey status between 10 and 15). These are all individuals 18 years and older.
	keep if netto>=10 & netto<=15
	* 3. Retain only respondents who had at least one child at any point before or during the sample period.
	rename kidgeb0* kidgeb* // Renaming the variables kidgeb01-kidgeb09 to be kidgeb1-kidgeb9. This will be useful when using the loop "forvalues i(1)19" later.
    keep if syear>=kidgeb1 & kidgeb1>0
	* Retain only respondents who declared the gender of at least one of their children (some did not)
	rename kidsex0* kidsex*
	forvalues i=1(1)19{
		qui g kid_gender_declared`i'=1 if kidsex`i'>0
	}
 	egen at_least_one_kid_gender = rowtotal(kid_gender_declared*)
    keep if at_least_one_kid_gender>0
	drop kid_gender_declared* at_least_one_kid_gender
	


* GENERATE VARIABLES *
sort pid syear

	* Generate dummy variable for respondents with at least one daughter up to the year of the survey (1 if at least one daughter, 0 if not). The following code also creates a variable that measures the total number of daughters each participant has up to the year of the survey.
	forvalues i=1(1)19{
		qui replace kidgeb`i'=. if kidgeb`i'>syear // If the ith child is born in year t, and the observation year is earlier than t, then the variable kidgeb`i' (year of birth of the ith child) is left empty.
		qui replace kidsex`i'=. if kidgeb`i'>syear // If the ith child is born in year t, and the observation year is earlier than t, then the variable kidsex`i' (gender of the ith child) is left empty.
	}
	g Having_Daughters=0
	g Num_Daughters=0
	forvalues i=1(1)19{
		qui replace Having_Daughters=1 if kidsex`i'==2
		qui replace Num_Daughters=Num_Daughters+1 if kidsex`i'==2
	}

	* Generate gender equality variables. The measures used are base on the answers (scale from 1 [Do not agree at all] to 7 [Completely agree]) to the following questionnaire question: 
	// To what extent do you personally agree with the following statements? 
	// 1) It's good that legislators take into account that not everyone can be classified as either male or female (plh0395i01); 
	// 2) It should be accepted in our society that not everyone identifies with their biological sex (plh0395i02); 
	// 3) It's completely natural that some people are born with both male and female biological features (plh0395i03); 
	// 4) Sex assignment is not so important for a person's development (plh0395i04); 
	// 5) Schools and preschools should promote non-gender-typical activities that are not typical for children's gender (plh0395i05);
	// 6) It should be accepted in our society that same-sex couples want to have children (plh0395i06); 
	forvalues i=1(1)6{
		rename plh0395i0`i' Support_Gender_Equality`i'
		qui replace Support_Gender_Equality`i'=. if Support_Gender_Equality`i'<0
		}
	* Since the output Excel file provides space for only one regression output, we build a measure for supporting gender equality based on principal component analysis (PCA). The purpose is to generate a linear combination of the six variables previously defined that better captures the individual's level of support for gender equality. In our case, PCA generates one factor with an eigenvalue greater than one.
	pca Support_Gender_Equality* 
	predict pc1_Support_Gender_Equality, score
	
	* Generate controls
	// Gender of the respondent
	rename sex Gender
	// Age of the respondent
	g Age=syear-gebjahr
	// Marital Status of the respondent
	g Marital_Status=pgfamstd if pgfamstd>0
	// Employment status
	rename pgemplst Employment_Status
	// Income (Net)
	g Income_Net=pglabnet if pglabnet>=0
	replace Income_Net=0 if pglabnet<0 & Employment_Status==5 // Income_Net is assumed to be zero for individuals who are not employed and who did not declaire any income (pglabnet=-2)
	// Education in years
	g Education_In_Years=pgbilzeit if pgbilzeit>=0
	// Number of Children up to the observation year
	g Num_Sons=0
	forvalues i=1(1)19{
		qui replace Num_Sons=Num_Sons+1 if kidsex`i'==1
		}	
	g Num_Children=Num_Daughters+Num_Sons
	// Migraation background
	rename migback Migration_Background
	// Sample region
	g Sample_Region=sampreg if sampreg>0
	// Religion
	g Religion=plh0258_h if plh0258_h>0
	// Parents minimum education level	
	g Parents_Min_Edu=min(msedu, fsedu) if fsedu>=0 & msedu>=0
	// Current health
	g Current_Health=ple0008 if ple0008>0


* HYPOTHESIS TEST FOR H4 *
	* Verbal description of the model: Support_Gender_Equality = β₀ + β₁ * Having_Daughters + Controls + ε 
	
	* We consider a: linear OLS regression

	* Accordingly, we run a linear OLS regression analysis (with robust standard errors) using pc1_Support_Gender_Equality as dependent variable and Having_Daughters, Gender, and Having_Daughters_x_Gender as independent variables.
	g Having_Daughters_x_Gender=Having_Daughters*Gender
	reg pc1_Support_Gender_Equality Having_Daughters Gender Having_Daughters_x_Gender Age Marital_Status Employment_Status Income_Net Education_In_Years Num_Children Migration_Background Sample_Region Religion Parents_Min_Edu Current_Health, vce(robust)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[Having_Daughters_x_Gender]

		* (ii) standard error
		local se = _se[Having_Daughters_x_Gender]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[Having_Daughters_x_Gender] / _se[Having_Daughters_x_Gender]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_Having_Daughters_x_Gender = _b[Having_Daughters_x_Gender]
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
