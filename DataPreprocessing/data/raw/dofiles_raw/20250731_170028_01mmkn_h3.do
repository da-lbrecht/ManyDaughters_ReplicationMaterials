********************************************************************************
****************** Updated from Example Do-File H1: #ManyDaughters Project ******************

* clear all

version 18.5 
*StataNow/BE 18.5 for Windows (64-bit x86-64)


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
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) //changed to pl.dta during reproducibility check
	* can replace p1_subset with just p1. We used a subset of p1 because our version of Stata restricts the number of variable
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English



* ============================
* Analysis
* ============================

drop if sex < 0

// Construct has_daughter variable
gen has_daughter = 0

foreach i of numlist 1/9 {
    replace has_daughter = 1 if kidsex0`i' == 2
}
foreach i of numlist 10/19 {
    replace has_daughter = 1 if kidsex`i' == 2
}


rename has_daughter d_daughter

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)

* HYPOTHESIS TEST FOR H3*


/*
🔹 Explanation of Included Variables
Variable	Label (if available)	Rationale
d_daughter	Dummy for having a daughter	Main explanatory variable
sex	Gender	Controls for respondent's sex
gebjahr	Year of birth	Controls for cohort effects
migback	Migration background	May affect cultural norms
pglabnet	Net labor income	Proxy for individual socioeconomic status
pgemplst	Employment status	Controls for labor market participation
pgpsbil	School-leaving degree	Controls for education level
pgcasmin	CASMIN education level	Finer-grained educational classification
pgisco08	Occupational classification (ISCO-08)	Proxy for type of job held
hghinc	Household net income	Household-level economic control
*/	

/*
here are candidate dependent variables that could plausibly reflect gender attitudes, roles, or equality:

Variable	Likely Meaning (based on SOEP naming convention)
plb0427	Could reflect attitudes toward division of labor
plb0528_h	Mother's work vs. child relationship
plb0538	Possibly beliefs about female education/work
plb0541	Possibly about male breadwinner norm

(These names match common SOEP variable names for gender role statements; exact labels can be confirmed with the SOEP documentation.)
*/

reg plb0528_h i.d_daughter##i.sex gebjahr migback, robust

* Save regression sample
gen byte regsample = e(sample)


* EXTRACT AND EXPORT RESULTS * Section added by Tina Wang based on PAP

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.d_daughter]

		* (ii) standard error
		local se = _se[1.d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.d_daughter] / _se[1.d_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		preserve
		quietly keep if regsample==1 & !missing(pid)
		quietly duplicates drop pid, force
		local num_participants = _N
		restore

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.d_daughter]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile

	
exit
