********************************************************************************
****************** Do-File H1: #ManyDaughters Project (01oxf2) *****************
********************************************************************************

* Authors: Tina Wang, David Albrecht
* Adjusted by: [REDACTED], [REDACTED] 

* ============================
* Setup * 
* ============================

*clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:
ssc install outreg2
/*
* CONFIGURE FILE PATHS * (Please do not change)
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

use "$DATA/h1_dataset.dta", clear // load the datasets

** determine which variables to keep 

keep hid pid kidsex* sex gebjahr sampreg syear pgfamstd pgemplst pgbilzeit hghinc plh0308_v2 pop

* FILTERS * 
 
* Keep only completed interviews from private households
	keep if inlist(pop,1,2)

	
	* Keep households only with at least one child
gen num_children = 0


* Loop: kidsex01 to kidsex19
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19{
    replace num_children = num_children + inlist(kidsex`i', 1, 2)
}


keep if num_children >= 1
label variable num_children "Number of children"



* GENERATE VARIABLES * 

* Generate dummy variable for whether participant has at least one daughter (1) or not (0)

gen num_daughters = 0
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19{
    replace num_daughters = num_daughters + inlist(kidsex`i', 2)
}
label variable num_daughters "Number of daughters"

gen d_daughter = (num_daughters > 0)
label variable d_daughter "Has at least one daughter (1) or not (0)"

** Generate age variable 

gen age = syear - gebjahr

** Recode sex variable 

replace sex = 0 if sex == 2

** Generate east dummy 

generate east = . 
replace east = 1 if sampreg == 2
replace east = 0 if sampreg == 1


*------------------------------
** Create marital status variable 
** bins: married [1, 7], single [3], been married [2, 4, 5, 6, 8]

gen marital_cat = .  // create the variable

// Married: [1] Married, [7] Registered Same-Sex Partnership living together
replace marital_cat = 1 if inlist(pgfamstd, 1, 7)

// Single: [3] Single
replace marital_cat = 2 if pgfamstd == 3

// Been Married: [2] Separated, [4] Divorced, [5] Widowed, [6] Partner abroad, [8] Registered same-sex partnership living apart
replace marital_cat = 3 if inlist(pgfamstd, 2, 4, 5, 6, 8)

// label the categories
label define marital_lbl 1 "Married" 2 "Single" 3 "Been married"
label values marital_cat marital_lbl


** Create employment status variable 

gen emp_cat = .

* 1 = Full-time employment
replace emp_cat = 1 if pgemplst == 1

* 2 = Part-time (regular or marginal, vocational training and other small groups)
replace emp_cat = 2 if inlist(pgemplst, 2, 3, 4, 6, 7)

* 3 = Not employed
replace emp_cat = 3 if pgemplst == 5

* Label
label define emp_cat_lbl ///
    1 "Full-time" ///
    2 "Part-time" ///
    3 "Unemployed"

label values emp_cat emp_cat_lbl


** EXCLUDE VARIABLES WITH MISSINGS **

* Dependent variable

keep if plh0308_v2 >= 0 
drop if plh0308_v2 == .

* Sex 

drop if sex != 1 & sex != 0

* Age 

drop if age < 18

* Region 

drop if east == . 

* Education

drop if pgbilzeit < 0 

* Household income 

drop if hghinc < 0

* Generate ln(hghinc)

gen ln_hghinc = ln(hghinc)

* Employment Status 
drop if emp_cat == .

* Marital Status 
drop if marital_cat == .





* HYPOTHESIS TEST FOR H1 * 

* A linear OLS regression testing whether having daughters is associated with stronger support for gender equality conditional having at least one child (measured by agreement with statement plh0308_v2 "Best if man and woman work the same amount so they can share the responsibility" (1 = completely disagree, 7 = fully agree)"). We selected this question because it directly captures gender role attitudes in the labor market. Higher values indicate greater support for gender equality. 

* Outcome variable: plh0308_v2 
* Main explanatory variable: d_daughter (a dummy equal to 1 if the respondent has at least one daughter (and 0 otherwise))

* We control for standard demographic covariates:
* - sex (0 = female, 1 = male),
* - age,
* - years of education (pgbilzeit),
* - log of household net income (ln_hghinc),
* - number of children,
* - region (east = 1 if living in East Germany),
* and include fixed effects for survey year (syear).

* We also control for employment status and marital status to account for socioeconomic status and life-cycle differences.

* The model is estimated using heteroskedasticity-robust standard errors.


local controls sex age pgbilzeit ln_hghinc num_children east i.emp_cat i.marital_cat i.syear

	reg plh0308_v2 d_daughter `controls', robust


* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics
		
		matrix b = e(b)
		matrix V = e(V)
		
		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = sqrt(V[1,1])

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter] / _se[d_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
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
	di "Significance5: " " `significance5'"
	di "Significance10: " " `significance10'"

	* Export results to csv 
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation,significance(5%),significance(10%)" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation',`significance5',`significance10'" _n
	file close myfile


********************************************************************************

exit
