********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000
/*
* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

* CONFIGURE FILE PATHS * (Please do not change)
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
*Add my cd as it does not pick it up
*global ROOT "/Users/[REDACTED]jal/Dropbox/many_daughters/#ManyDaughters_RT_AnalysisPackage/"
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

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Gnerate dummy variable for whether participant has at least one daughter (1) or not (0)

	gen d_daughter_aux=kidsex01==2
	*make sure it appears also before birth
	egen d_daughter=max(d_daughter_aux), by(pid)
	
		*drop missing values
	foreach var in plh0395i01 plh0395i02 plh0395i03 plh0395i04 plh0395i05 plh0395i06 plh0004{
		gen clean_`var'=`var' if `var'>0
	}
	
	*pca of gender attitudes
	pca clean_plh*
	predict pc1
	
	
	*drop more missing values
		foreach var in kidmon01 kidgeb01 syear pmonin{
		gen clean_`var'=`var' if `var'>0
	}

	
	gen male=sex==1
// Construct month counts since year 0

gen birth_month_num  = clean_kidgeb01 * 12 + clean_kidmon01


egen maxbirth_month_num=max(birth_month_num), by(pid)


gen survey_month_num = clean_syear * 12 + clean_pmonin

// Compute distance in months (event time)
gen distance_in_months = survey_month_num - maxbirth_month_num
gen post=distance_in_months>=0 & distance_in_months!=.
gen distance_in_years = clean_syear-clean_kidgeb01

*adjust naming so that d_daughter is treatment
gen d_daughter_pre=d_daughter
replace d_daughter=d_daughter*post

*outcome varible
gen left=10-clean_plh0004


* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Please add a verbal description of your model here
	
	* I wanted to estimate a panel model with individual FE
	* For H1 I could not run the PAP model because there are very few observations and the model involve lots of FE. 
	* But here it works. (distance in years FE, no distance_in_months FE, but linear control)
 
 
reghdfe left   d_daughter  , absorb(pid distance_in_months) vce(cluster pid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = _se[d_daughter]

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

	* Export results to csv 
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
