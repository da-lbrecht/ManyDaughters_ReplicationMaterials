********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************
/*
clear all

// cd "C:\Users\roone\Desktop\#ManyDaughters_RT_AnalysisPackage" // Commented out by David Albrecht for reproducibility check
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

	save "$DATA/h3_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* USE DATA:
	use "$DATA\h3_dataset.dta"
	
* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)

* GENERATE VARIABLES * (Please adapt according to your needs)

	set seed 123
	xtset pid syear
	sort pid syear
	gen d_daughter = 0 if pld0026 == -2
	replace d_daughter = 1 if pld0026 == 1 | pld0026 == 2 | pld0026 == 3 | pld0026 == 4 | pld0026 == 6 | pld0026 == 9
	tab d_daughter
	
	* Step 1: Create year indicators for first 1 and first 0
	bysort pid (syear): egen first_1year = min(cond(d_daughter == 1, syear, .))
	bysort pid (syear): egen first_0year = min(cond(d_daughter == 0, syear, .))

	* Step 2: Count total non-missing observations per id
	bysort pid: egen total_obs = total(!missing(d_daughter))

	* Step 3: Generate new variable
	gen d_daughter_filled = .
	* Rule 1: Leave missing if always missing
	replace d_daughter_filled = . if total_obs == 0
	* Rule 2: Years before first 1 are 0 if a 0 ever appeared
	replace d_daughter_filled = 0 if syear < first_1year & !missing(first_0year) & total_obs > 0
	* Rule 3: Years from first 1 onward are 1
	replace d_daughter_filled = 1 if syear >= first_1year & total_obs > 0
	* Rule 4: If no 1 exists but 0 exists, set all years after 0 to 0
	replace d_daughter_filled = 0 if missing(first_1year) & syear >= first_0year & total_obs > 0

	* Step 4: Clean up
	drop first_1year first_0year total_obs	
	drop d_daughter
	rename d_daughter_filled d_daughter

	
* HYPOTHESIS TEST FOR H3 * 

	* This analysis tests the hypothesis that having daughters leads to more gender equality in the household (as measured by behaviors or outcomes).
	
	* The dependent variable is pli0043_h, which captures "Housework hours per weekday".
	
	* The key independent variable is d_daughter, a dummy equal to 1 if the respondent has at least one daughter, and 0 otherwise.

	* The regression is estimated using a fixed effect model.
	
	* Control variables include respondent gender (sex), year of birth (gebjahr), marital status (pgfamstd), and years of education (pgbilzeit), to account for demographic and socioeconomic differences in gender attitudes.

	* Expected finding: If having daughters increases gender equality in the household, we expect a negative and significant interaction term between d_daughter and female, reflecting that the increase in housework is smaller for women, who already bear a larger share of domestic work.
	
	
	*H3:
	preserve
	drop if pgfamstd <0 | pgfamstd ==. 
	keep if (pgfamstd != -3) & (pgfamstd != -1) 
	xtreg pli0044_h d_daughter##sex gebjahr i.pgfamstd pgbilzeit i.syear if (pli0044_h != -8) & (pli0044_h != -5) & (pli0044_h != -1) & (pgfamstd != -3) & (pgfamstd != -1) & (pgbilzeit != -2) & (pgbilzeit != -1), fe r
*	restore



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.d_daughter#2.sex]

		* (ii) standard error
		local se = _se[1.d_daughter#2.sex]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.d_daughter#2.sex] / _se[1.d_daughter#2.sex]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local interpretation "negative coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: `p_value'"
	di "Number of observations: `num_obs'"
	di "Number of participants: `num_participants'"
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
