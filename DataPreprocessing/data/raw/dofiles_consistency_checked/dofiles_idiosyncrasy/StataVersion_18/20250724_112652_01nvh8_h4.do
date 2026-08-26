********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

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

*** Only keep year 2016, which is the only year for which our outcome variable is available
keep if syear==2016

*** Remove observations with missing in any item that is to be included in the treatment variable
keep if plj0621>=0
keep if plj0622>=0
keep if plj0623>=0
keep if plj0624>=0
keep if plj0625>=0


* GENERATE VARIABLES * (Please adapt according to your needs)

*** Reverse three of the gender equality items, such that higher values indicate higher support for gender equality
gen plj0623_new = 8 - plj0623
gen plj0624_new = 8 - plj0624
gen plj0625_new = 8 - plj0625 
table plj0623 plj0623_new
table plj0624 plj0624_new
table plj0625 plj0625_new

*** Generate outcome variable as additive index of individual items:
gen gender_index = plj0621 + plj0622 + plj0623_new + plj0624_new + plj0625_new
tab gender_index
*** Remove observations with missing values:
keep if gender_index!=.


*** Generate treatment variable: Has exactly one child, and that child is female
*** Note: We use sumkids and daughter instead of pld0025 and bgp15307a (which we mentioned in our PAP), since these variables did not contain the desired information for all necessary years
keep if sumkids==1
gen d_daughter=1 if kidsex01==2
replace d_daughter = 0 if d_daughter==.


*** Recode marital status: coded as one if married (1); married, but separated (2); husband/wife abroad (6); civil union living together or apart (7 and 8); coded as zero if single(3); divorced(4); widowed(5)
keep if pgfamstd >=0
gen married = 0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
replace married = 1 if married==.
tab married pgfamstd


*** Remove respondents with missings
keep if gender_index!=. & gender_index>=0 & d_daughter!=. & d_daughter>=0 & married!=. & married>=0 & sex!=. & sex>=0 & gebjahr !=. & gebjahr >=0 & pgemplst !=. & pgemplst >=0 & pgisced97 !=. & pgisced97 >=0


* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)

	* We run a linear OLS regression with clustered standard errors, with several control variables (year of birth, respondent's, education, employment status, and marital status). Categorical control variables are included with dummy variables. We include an interaction term between the respondent's and the only child's gender, representing the treatment variable.
*** Unfortunately, model doesn't really work because we don't have enough cases, see
 tab d_daughter sex, mis
 
 gen male = 1 if sex==1
 replace male = 0 if sex==2
*** nonetheless, we can run it and these issues will likely disappear once the analysis is run on the 95% data
gen interact = d_daughter*male 	
	reg gender_index d_daughter male interact gebjahr i.pgisced97 i.pgemplst i.married, vce(robust)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[interact]

		* (ii) standard error
		local se = _se[interact]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[interact] / _se[interact]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[interact]
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
