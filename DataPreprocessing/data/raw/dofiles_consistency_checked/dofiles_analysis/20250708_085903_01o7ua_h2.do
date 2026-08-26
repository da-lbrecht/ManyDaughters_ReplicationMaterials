********************************************************************************
****************** 		Do-File H2: #ManyDaughters Project 		****************
********************************************************************************

* Authors: Research Team 01o7ua
* We consider this variant of analysis as our first choice, because we find it more interesting than longitudinal analysis. Additionally, the larger sample size should give more reliable results.

* ============================
* Setup * 
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
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


*	I kept your code as requested, but below I adjusted the folders to my needs
// global IN "C:\Source - Data - ManyDaughters\data\raw" // Commented out by David Albrecht on 08.07.2025
// global DATA "C:\Source - Data - ManyDaughters\data\derived" // Commented out by David Albrecht on 08.07.2025
// global OUT "C:\Users\mmikucka\Dropbox\Health_Inequalities\ManyDaughters\Analysis\out" // Commented out by David Albrecht on 08.07.2025
*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	keep pid hid syear 		parid partner 		sex gebjahr      sexor sexorinfo

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(pid biokids sumkids kidpnr* kidgeb* kidsex*)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pid syear pgfamstd)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pid syear plh0004)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English
	
	compress
	save "$DATA/h2_dataset_quasiexperiment.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h2_dataset_quasiexperiment.dta", clear 


* FILTERS * 
	
	
	* keep observations from 2005, 2009, 2014, and 2019
	keep if syear==2005 | syear==2009 | syear==2014 | syear==2019
	
	
	* keep respondents 18+ years old
	gen age = syear-gebjahr
	keep if age>=18 
	
	
	* keep heterosexual respondents
	keep if sexor!=2
	
	
	* keep respondents with at least 1 child
	gen nr_children = 0
	levelsof(syear), loc(year)
	foreach y in `year' {
		foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
			replace nr_children = nr_children + 1 if (kidgeb`i'<=`y' & kidgeb`i'>1900) & syear==`y'
		}	
	}
	keep if nr_children>=1
	
	
* GENERATE VARIABLES * 

	* DEPENDENT VARIABLE: left-wing political attitudes
	* -----------------------------------------
		gen Left = 10*(10-plh0004) if plh0004>=0 & plh0004<=10 /* reversed scale + rescaled into 0-100*/
	
	
	
	* MAIN PREDICTOR: gender of the 1st child 
	* -----------------------------------------
	gen first_daugh = kidsex01==2 if kidsex01>=1 
	
	
	
	* CONTROL VARIABLES
	* -----------------------------------------
	* Control variable: year of birth of the 1st child
	
	gen brth_y_first = kidgeb01 if kidgeb01>=1900
	sum brth_y_first
	gen Cbrth_y_first = brth_y_first - r(mean) /* variable centred at grand mean */
	
	
	
	* FOR THE DELAYED EFFECTS VARIANT: age of the 1st daughter
	gen fd_age = syear - kidgeb01 if first_daugh==1 & kidgeb01>=1900
	
	gen first_daugh05 = first_daugh==1 & fd_age>=0 & fd_age<=5
	gen first_daugh614 = first_daugh==1 & fd_age>=6 & fd_age<=14
	gen first_daugh15plus = first_daugh==1 & fd_age>=15
	
	* age of the 1st child
	gen fch_age = syear - kidgeb01 if kidgeb01>=1900
	
	* control: age (centred)
	sum age
	gen Cage = age-r(mean)
	
	
	
	
* HYPOTHESIS TEST FOR H2 * 

	* We test whether people having a daughter as the first child have more left-leaning political attitudes than those having son as their first child
	* We run a random effects model with:
	* Dependent var: left political attitudes
	* Main predictor: first_daughter
	* Controls: year of birth of the 1st child (centred on the grand mean)
	
	
	xtset pid syear
	xtreg Left first_daugh c.Cbrth_y_first##c.Cbrth_y_first , re cluster(pid)
	

	/* DELAYED EFFECTS VARIANT	
	xtreg Left first_daugh05 first_daugh614 first_daugh15plus c.Cbrth_y_first##c.Cbrth_y_first c.Cage##c.Cage, re cluster(pid)
	*/

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[first_daugh]

		* (ii) standard error
		local se = _se[first_daugh]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[first_daugh] / _se[first_daugh]
		* local p_value = 2 * ttail(e(df_r), abs(`p_value'))
		local p_value = 2*(1-normal(abs(`p_value')))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_daughter = _b[first_daugh]
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
	// file open myfile using "$OUT/h2_results_quasiexperiment.csv", write replace // Restored original naming convention David Albrecht on 09.07.2025
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
