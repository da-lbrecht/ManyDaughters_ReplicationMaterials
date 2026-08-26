********************************************************************************
****************** H3 Analysis: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)

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

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 

* GENERATE VARIABLES * (Please adapt according to your needs)

	* Generate dependent variable: Household Gender Equality
	display "Creating household gender equality variable..."
	
	* Equality in the responsibility in household (plh0308_v1)
	gen hh_equality_raw = plh0308_v1
	recode hh_equality_raw (-8 -5 -1 = .)
	
	* Reverse coding of plh0308_v1 to indicate higher support for equal distribution of tasks
	* Assuming original scale where lower values = more traditional/unequal arrangements
	* and higher values = more equal arrangements
	* Check the original scale first and adjust if needed
	gen hh_equality = hh_equality_raw if !missing(hh_equality_raw)
	
	* Alternative reverse coding if scale goes in opposite direction:
	* Uncomment the line below if higher original values mean more traditional arrangements
	* replace hh_equality = (max_value + 1) - hh_equality_raw if !missing(hh_equality_raw)
	
	* Keep only cases with non-missing data on household equality
	keep if !missing(hh_equality)
	
	* Generate independent variable: Having daughters
	display "Creating daughters variable..."
	
	* Count total number of children (non-missing kidsex values)
	egen total_children = rownonmiss(kidsex01-kidsex19)
	
	* Count daughters (kidsex == 2)
	egen total_daughters = anycount(kidsex01-kidsex19), values(2)
	
	* Create binary indicator for having at least one daughter
	gen d_daughter = (total_daughters >= 1) if total_children > 0
	replace d_daughter = . if total_children == 0
	
	* Generate control variables
	display "Creating control variables..."
	
	* Age
	gen age = bioage
	replace age = . if bioage == -2
	
	* Gender (female dummy)
	gen female = (sex == 2) if !missing(sex)
	
	* Education (years)
	gen education = pgbilzeit
	recode education (-2 = .) (-1 = .)
	
	* Partner status
	recode partner (2=1) (3=1)
	
	* Employment status
	gen employed = (pgemplst == 1) if !missing(pgemplst)
	
	* Log household income
	gen log_hhincome = ln(pglabgro)

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* H3: Having daughters leads to more gender equality in the household (as measured by behaviors or outcomes)
	* We test whether respondents with at least one daughter have higher household gender equality
	* than respondents with no daughters
	
	* Linear OLS regression with bootstrapped standard errors clustered at household level
	* Model: hh_equality = a + β1×d_daughter + controls + ε

	bootstrap, reps(2000) seed(12345): ///
		regress hh_equality d_daughter age female education partner employed log_hhincome, ///
		vce(cluster cid)

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

        matrix results = r(table) 

        local col = colnumb(results, "d_daughter") 

        local p_value = results[4, `col'] 

        * local p_value = _b[d_daughter] / _se[d_daughter] 

        * local p_value = 2 * ttail(e(df_r), abs(`p_value')) 

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
