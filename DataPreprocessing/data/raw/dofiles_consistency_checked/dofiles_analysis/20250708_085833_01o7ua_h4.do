********************************************************************************
****************** 		Do-File H4: #ManyDaughters Project 		****************
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
// global IN "C:\Source - Data - ManyDaughters\data\raw" // Commented out by David Albrecht on 09.07.2025
// global DATA "C:\Source - Data - ManyDaughters\data\derived" // Commented out by David Albrecht on 09.07.2025
// global OUT "C:\Users\mmikucka\Dropbox\Health_Inequalities\ManyDaughters\Analysis\out" // Commented out by David Albrecht on 09.07.2025
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
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pid syear plh0298_* plh0302_* plh0308_*)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English
	
	compress
	save "$DATA/h4_dataset_quasiexperiment.dta", replace // save merged dataset


* ============================
* Analysis
* ============================


use "$DATA/h4_dataset_quasiexperiment.dta", clear 


* FILTERS * 
	
	* keep 2012 and 2018 observations
	keep if syear==2012 | syear==2018
		
		
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


	* DEPENDENT VARIABLE: attitudes supporting gender equality
	* -----------------------------------------
	
	* (1) reverse the scales (higher values = stronger support for equality)
	gen Suffer6_12 = plh0298_v1 if plh0298_v1>=1 & plh0298_v1<=4 /* original */
	gen Suffer3_12 = plh0302_v1 if plh0302_v1>=1 & plh0302_v1<=4 /* original */
	gen SameShare_12 = 5-plh0308_v1 if plh0308_v1>=1 & plh0308_v1<=4 /* reverse */
	*
	gen Suffer6_18 = 8-plh0298_v2 if plh0298_v2>=1 & plh0298_v2<=7 /* reverse */
	gen Suffer3_18 = 8-plh0302_v2 if plh0302_v2>=1 & plh0302_v2<=7 /* reverse */
	gen SameShare_18 = plh0308_v2 if plh0308_v2>=1 & plh0308_v2<=7	/* original */
	
	* (2) convert into percentile scores
	foreach var in Suffer6 Suffer3 SameShare {
		* percentile scores for 2012
		foreach x in 12 18 {
			egen R`var'_`x' = rank(`var'_`x') /* rank of an observation */
			egen C`var'_`x' = count(`var'_`x') /* number of observations */
			gen P`var'_`x' = (R`var'_`x' - 0.5)/C`var'_`x' /* percentile score with a 0.5 correction in order to refer to the center of the category */
		}
	}
	
	* (3) gender attitudes index as a weighted sum
	gen GEIndex = 0.5 * PSameShare_12 + 0.25 * PSuffer6_12 + 0.25 * PSuffer3_12 if syear==2012
	replace GEIndex = 0.5 * PSameShare_18 + 0.25 * PSuffer6_18 + 0.25 * PSuffer3_18 if syear==2018
	* to allow max 1 missing 2012:
	replace GEIndex = (0.5 * PSameShare_12 + 0.25 * PSuffer6_12)/0.75 if (syear==2012 & PSuffer3_12==.)
	replace GEIndex = (0.5 * PSameShare_12 + 0.25 * PSuffer3_12)/0.75 if (syear==2012 & PSuffer6_12==.)
	replace GEIndex = (0.25 * PSuffer6_12 + 0.25 * PSuffer3_12)/0.5 if (syear==2012 & PSameShare_12==.)
	* to allow max 1 missing 2018:
	replace GEIndex = (0.5 * PSameShare_18 + 0.25 * PSuffer6_18)/0.75 if (syear==2018 & PSuffer3_18==.)
	replace GEIndex = (0.5 * PSameShare_18 + 0.25 * PSuffer3_18)/0.75 if (syear==2018 & PSuffer6_18==.)
	replace GEIndex = (0.25 * PSuffer6_18 + 0.25 * PSuffer3_18)/0.5 if (syear==2018 & PSameShare_18==.)
	
	* (4) rescale the index to 0-100
	replace GEIndex = GEIndex*100

	
	* MAIN PREDICTOR: gender of the 1st child 
	* -----------------------------------------
	gen first_daugh = kidsex01==2 if kidsex01>=1 
	
	
	* MAIN MODERATOR: gender 
	* -----------------------------------------
	gen Man = sex==1
	
	
	* CONTROL VARIABLES
	* -----------------------------------------
	* Control variable: year of birth of the 1st child
	
	gen brth_y_first = kidgeb01 if kidgeb01>=1900
	sum brth_y_first
	gen Cbrth_y_first = brth_y_first - r(mean)
	
	* FOR THE DELAYED EFECTS VARIANT: age of the 1st daughter
	gen fd_age = syear - kidgeb01 if first_daugh==1 & kidgeb01>=1900
	
	gen first_daugh05 = first_daugh==1 & fd_age>=0 & fd_age<=5
	gen first_daugh614 = first_daugh==1 & fd_age>=6 & fd_age<=14
	gen first_daugh15plus = first_daugh==1 & fd_age>=15
	
	* age of the 1st child
	gen fch_age = syear - kidgeb01 if kidgeb01>=1900
	
	* control: age (centred)
	sum age
	gen Cage = age-r(mean)
	
* HYPOTHESIS TEST FOR H4 * 

	* We test whether MAN having a daughter as the first child support gender equality more than women in the same situation
	* We run a random effects model with:
	* Dependent var: gender attitudes index
	* Main predictor: first_daughter ## Man
	* Controls: year of birth of the 1st child (centred on the grand mean)
	
	xtset pid syear
	xtreg GEIndex first_daugh##Man c.Cbrth_y_first##c.Cbrth_y_first , re cluster(pid)
	
	/* DELAYED EFECTS VARIANT
	xtreg GEIndex (first_daugh05 first_daugh614 first_daugh15plus)##Man c.Cbrth_y_first##c.Cbrth_y_first c.Cage##c.Cage, re cluster(pid)
	*/

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.first_daugh#1.Man]

		* (ii) standard error
		local se = _se[1.first_daugh#1.Man]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.first_daugh#1.Man] / _se[1.first_daugh#1.Man]
		* local p_value = 2 * ttail(e(df_r), abs(`p_value'))
		local p_value = 2*(1-normal(abs(`p_value')))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_daughter = _b[1.first_daugh#1.Man]
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
	// file open myfile using "$OUT/h4_results_quasiexperiment.csv", write replace // Restored original naming convention David Albrecht on 09.07.2025
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
