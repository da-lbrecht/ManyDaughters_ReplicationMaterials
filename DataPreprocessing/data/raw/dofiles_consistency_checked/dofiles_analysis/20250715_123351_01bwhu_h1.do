********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht, Mikkel [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

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
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex01 kidgeb01 kidgeb02 kidmon01 kidmon02)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pglabgro)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plj0620-plj0625 plh0004)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================



* FILTERS * 
 
	* only keep households with kids
	keep if inlist(kidsex01,1,2)
	
	* only keep households with information about year of first birth
	keep if kidgeb01>-1
	
	* only keep if surveyed after first birth
	keep if syear>kidgeb01
	
	* drop households where first birth is more than one child 
	drop if kidgeb01==kidgeb02 & kidmon01==kidmon02 

	
	
	
* GENERATE VARIABLES * 

* outcome 1:

foreach x of var plj0620-plj0625 {
	*tab syear if `x'!=-8 & `x'!=-5 & !missing(`x')
	replace `x'=. if inlist(`x',-8,-5)
	gen `x'_missing=`x'==-1
	egen `x'_std=std(`x')
	replace `x'_std=0 if `x'_missing==1
}

replace plj0623_std=-plj0623_std
replace plj0624_std=-plj0624_std
replace plj0625_std=-plj0625_std

gen geneq=plj0620_std+plj0621_std+plj0622_std+plj0623_std+plj0624_std+plj0625_std
egen geneq_std=std(geneq)

gen missings=plj0620_missing+plj0621_missing+plj0622_missing+plj0623_missing+plj0624_missing+plj0625_missing
replace geneq_std=. if missings>3




* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Model: linear OLS regression
	
	* We use the variable geneq_std (generated above) as a measure of support for gender equality (measured by attitudes). 
	* The variable is an index based on six questions relating to gender equality (plj0620-plj0625).
	* If the respondent has a higher value on the index, this indicates more support for gender equality.
	
	* Accordingly, we run a linear OLS regression analysis with geneq_std as dependent variable and kidsex01 as the independent variable,
	* additionally controlling for the sex and birth year of the respondent.

	
	reg geneq_std kidsex01 sex gebjahr, robust
	
	
	

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[kidsex01]

		* (ii) standard error
		local se = _se[kidsex01]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[kidsex01] / _se[kidsex01]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[kidsex01]
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
