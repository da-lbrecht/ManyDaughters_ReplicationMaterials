********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht, [REDACTED]

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
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(gebjahr)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	// merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) 
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pld0026 plh0308_v2)
	* hgen contains generated variables on household level from the household interview; use for household net income
	// merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Only keep participants from private households
	keep if inlist(pop,1,2)
	
	keep cid pid hid syear gebjahr pop pld0026 plh0308_v2
	
	* only keep people 18 and older
	drop if syear - gebjahr < 18
	
	* get rid of everyone that never encountered the dep_var question
	bysort pid: egen n_non8 = total(plh0308_v2 > 0)
	drop if n_non8 == 0
	drop n_non8


* GENERATE VARIABLES * (Please adapt according to your needs)

	* create indicators for having daugthers
	gen has_daughters = .
	replace has_daughters = 0 if pld0026 == -2
	replace has_daughters = 1 if pld0026 > 0
	
	* carry forward daugher indicator
	sort pid syear
	by pid: gen d_filled = has_daughters
	by pid: replace d_filled = d_filled[_n-1] if missing(d_filled)
	drop has_daughters
	rename d_filled has_daughters
	
	* set missing values on dep var for -8
	replace plh0308_v2 = . if plh0308_v2 < 0
	
	* only keep first row where both are non-missing
	egen first_both_year = min(cond(!missing(plh0308_v2) & !missing(has_daughters), syear, .)), by(pid)
	keep if syear == first_both_year
	drop first_both_year
	
	* dep var
	gen h1_dep_var = plh0308_v2
	

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Standard OLS regression with robust standard errors. We regress the main dependent variable on an indicator variable for having at least one daughter.

	reg h1_dep_var has_daughters, vce(robust)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[has_daughters]

		* (ii) standard error
		local se = _se[has_daughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[has_daughters] / _se[has_daughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[has_daughters]
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
