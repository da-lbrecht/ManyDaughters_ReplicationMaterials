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

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
//ssc install outreg2
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

	save "$DATA/h2_dataset_RT0119X.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	*Keep participants with a reported gender
	drop if missing(sex) //NOTE: PAP incorrectly used pla0047 as the gender variable, which is rarely asked 
	
	*Keep participants with a reported marital status
	drop if missing(pgfamstd) //NOTE: PAP incorrectly used plj0626 as the married variable, which is rarely asked 
	drop if pgfamstd < 0
	
	*Keep participants with a reported education levels
	drop if missing(pgbilzeit)
	drop if pgbilzeit < 0 //NOTE: PAP incorrtly used plg0076 as education status, which is rarely asked
	
	*Keep Participants who answered all Political Attitude Question
	drop if missing(plh0004)
	drop if plh0004 < 0
	

* GENERATE VARIABLES * (Please adapt according to your needs)

	*Generate dummy variable for whether participant has at least one daughter (1) or not (0)
	set seed 123
	
	//NOTE: PAP incorrectly used pld0025 "Do you have Daughters" but that was not asked in the years with other data 
	forvalues x = 1/9{
		replace kidsex0`x' = 999 if kidsex0`x' < 1 //All non-answers are recoded as 999
	}
	forvalues x = 10/19{
		replace kidsex`x' = 999 if kidsex`x' < 1 //All non-answers are recoded as 999
	}
	
	egen d_daughter = rowmin(kidsex*)
	replace d_daughter = 0 if d_daughter == 2 | d_daughter == 999 //=2 has only sons, =999 no children
	keep if d_daughter == 0 | d_daughter == 1
	
	*Generate Maritial Status.
	replace pgfamstd = 0 if pgfamstd == 3 //Single coded as zero
	replace pgfamstd = 2 if pgfamstd == 4 //Married but Seperated counted as Divorced
	replace pgfamstd = 3 if pgfamstd == 5 //Widowed as own category
	replace pgfamstd = 1 if pgfamstd == 6 //Living abraod counted as married
	replace pgfamstd = 1 if pgfamstd == 7 //Married LGBT
	replace pgfamstd = 2 if pgfamstd == 8 //Seperated LGBT
	
	*Recode Political Attitudes so Higher Numbers are more liberal
	replace plh0004 = plh0004 * -1
	replace plh0004 = plh0004 + 11
	
	

	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* OLS is estimated with Political Attitudes (plh0004) as the dependent variables
	* plh0004 directly asks respondants to place themselves on a left--right scale. 
	* Controls are then included for: income, education, marital status, and year 
	* The dependent variable of interest is d_daughter, a dummy variable which =1 if have a daughter 


		reg plh0004 d_daughter plc0013_h pgbilzeit i.pgfamstd i.syear, robust


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
	file open myfile using "$OUT/h2_results.csv", write replace // Renamed to original naming convention by David Albrecht on 02.07.2025
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
