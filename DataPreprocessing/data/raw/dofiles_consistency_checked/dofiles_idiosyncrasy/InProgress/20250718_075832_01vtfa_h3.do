********************************************************************************
*********************** Do-File H3: #ManyDaughters Project *********************
********************************************************************************

* Authors: [REDACTED]
* Last Update: 14.07.2025

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16 // indicate the version of stata you are using here
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

capture log close
log using "$OUT/h3_log.txt", text replace
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

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	*keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Example: generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* For illustrative purposes, we will generate the variable randomly:
	*set seed 123
	*gen d_daughter = runiformint(0,1)
	
	* basic treatment var
	tab pld0026
	
	bysort syear: tab pld0026
	
	* treatment var (non-absorbing)
	gen d_daughter = .
	replace d_daughter = 1 if pld0026 >= 1
	replace d_daughter = 0 if pld0026 == -2
	tab d_daughter

	* treatment var (absorbing)
	sort pid syear
	by pid: gen byte d_daughter_abs = sum(d_daughter == 1) > 0 

	tab d_daughter d_daughter_abs	
	bysort syear: tab d_daughter_abs
	tab d_daughter_abs
	
	* Outcome var
	tab pli0046
	replace pli0046 = . if pli0046 < 0
	
	tab pli0043_h
	replace pli0043_h = . if pli0043_h < 0
	
	* Identifier
	replace hid = . if hid < 0
	replace pid = . if pid < 0

	
	* Sort by household and year
	sort hid syear

	* For each household×year, sum up female and male hours
	by hid syear: egen woman_hours = total(cond(sex==2, pli0043_h, 0))
	by hid syear: egen   man_hours = total(cond(sex==1, pli0043_h, 0))

	tab woman_hours man_hours
	
	* Control variable
	tab bioage
	replace bioage = . if bioage < 0 
	tab bioage
	
	gen bioage_sq = (bioage^2)/100 
	sum bioage_sq, d
	
	replace hghinc = . if hghinc < 0
	
	replace pgbilzeit = . if pgbilzeit < 0

	replace ple0008 = . if ple0008 < 0

	
********************************************************************************
* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)
********************************************************************************
	* Please add a verbal description of your model here

	* As an example, and for illustrative purposes we consider a: linear OLS regression

	* For illustrative purposes, let's further assume that having a job that requires hard manual labor (plb0085) is a good measure of support for gender equality (hypothesis 1), 
	* if having a job with less manual labor, the respondent is more likely to support gender equality.
	* Accordingly, we run a linear OLS regression analysis with plb0085 as dependent variable and the dummy variable d_daughter as independent variable
	
	xtset pid syear

	* Estimate the fixed‐effects model
	xtreg man_hours d_daughter_abs bioage bioage_sq i.migback hghinc pgbilzeit ple0008, fe 
	estimates store FE

	* Estimate the random‐effects (pooled‐OLS + RE)
	xtreg man_hours d_daughter_abs bioage bioage_sq i.migback hghinc pgbilzeit ple0008, re 
	estimates store RE

	* Hausman test
	hausman FE RE, sigmamore
	
	xtset pid syear
	xtreg man_hours d_daughter_abs i.syear bioage bioage_sq i.migback hghinc pgbilzeit ple0008, fe cluster(pid)

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter_abs]

		* (ii) standard error
		local se = _se[d_daughter_abs]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter_abs] / _se[d_daughter_abs]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter_abs]
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


********************************************************************************

exit
