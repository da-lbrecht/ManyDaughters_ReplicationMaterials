********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*  ssc install outreg2

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

	*Retriving individual data	
	use "$IN/pl.dta", clear // use biol as a starting dataset
	
	* Merge biol with other datasets

	* bio_birth contains biographic information 
	merge m:1 pid cid using "$IN/biobirth.dta", nogen keep(master matches)
	
	* biol contains information to define the independent variable having a daughter
	merge m:1 pid syear using "$IN/biol.dta", nogen keep(master matches)
	
	* Keep just pid, syear, and individual-level variables plb0115 and pli0043_v3
	keep pid hid syear plb0115 pli0043_v3 plc0011 lb0288_v1 lb0290_h lb0287_h
	
	* We define our variable to identify responders having a daughter and a son
	gen d_daughter = (lb0288_v1 == 2) if (lb0290_h == -2)&(lb0288_v1 >= 0)
	label variable d_daughter "1 if the responder has only one daughter and zero if has only a son"
	
	* We define the variable that limits the analysis to responders with one child (the firstborn)
	gen year_first_born = lb0287_h if (lb0290_h == - 2) & (lb0287_h >= 0)
    label variable year_first_born "Year of the firstborn when the individual has only one child"
	
	gen sample = (syear - year_first_born >= 1) if!missing(year_first_born)
	label variable sample "1 if the individual's one child firstborn has at least one year wrt the dep. var.'"
	
	tempfile data
	save `data', replace
	
	* Load your individual-level SOEP dataset to distinguish couple data
	use "$IN/ppathl.dta", clear
	
	* Keep only observations where both the person and their partner are identified
	drop if parid == -2
	
	* Preserve key variables 
	keep pid parid sex syear hid partner
	
	* Save as partner 1
	tempfile p1
	save `p1', replace

	* Now switch roles to get partner 2's info
	rename pid pid2
	rename parid pid
	rename pid2 parid
	rename sex sex2
		
	* Merge with original file to get info from both partners
	merge 1:1 pid parid syear using `p1', keep(match) nogen
	
	* Rename variables from the original dataset (now partner 1)
	rename pid pid1
	rename parid pid2
	rename sex sex1

	* Keep critical variables
	keep hid syear pid1 sex1 pid2 sex2
	
	tempfile couples
	save `couples', replace
	
	use `data', clear
	
	* Keep and rename key variables
	keep pid syear hid plb0115 pli0043_v3 plc0011
	rename plb0115 plb0115_1
	rename pli0043_v3 pli0043_v3_1
	rename plc0011 plc0011_1
	rename pid pid1
	
	* Merge data to partner 1
	merge m:1 pid1 syear using `couples', nogen keep(match)
	
	* Save as partner 2
	tempfile p2
	save `p2', replace
	
	use `data', clear
	
	* Keep key variables
	keep pid syear hid plb0115 pli0043_v3 plc0011 d_daughter sample
	
	rename plb0115 plb0115_2
	rename pli0043_v3 pli0043_v3_2
	rename plc0011 plc0011_2
	rename pid pid2
	
	drop if hid == .
	
	* Merge with individual data
	merge m:1 pid2 syear using `p2', nogen keep(match)
	
	sort hid syear
	
	*Let's identify couples and drop duplicates
	gen long min_id = min(pid1, pid2)
	gen long max_id = max(pid1, pid2)
	
	* Ensure deterministic sorting
	sort syear min_id max_id pid1 pid2

	*Keep only one row per couple-year deterministically
	bysort syear min_id max_id (pid1 pid2): keep if _n == 1

	*Generate reproducible couple_id after deduplication
	egen couple_id = group(min_id max_id)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* HYPOTHESIS TEST FOR H3

	* The pre-analysis plan (PAP) initially proposed using an index of behaviors as the dependent variable. This index was to include three components: who in the household has the final say in financial decisions (plc0011), the average working hours of each partner (lm0621), and the weekly time each partner devotes to housework (pli0043_h). However, due to the very limited or no overlap in the availability of data for lm0621, plc0011, and pli0043_h, constructing a reliable index was not feasible. As a result, we revised our approach and now use the weekly time devoted to housework (pli0043_v3) as the sole behavioral indicator of gender equality within the household. Specifically, we measure the absolute difference in housework hours between partners, with larger values indicating greater inequality in the division of domestic labor.
	
	* To test H3, we model the absolute difference in housework hours (abs_pli0043_v3) as a function of whether the household includes a daughter (d_daughter). The model controls for year fixed effects (i.syear) to consider unobserved heterogeneity due to variation over time. Since differences are calculated across couples living in the same household, we consider that controlling for household fixed effect is not needed. 
	
	gen abs_pli0043_v3 = abs(pli0043_v3_1 - pli0043_v3_2)
	label variable abs_pli0043_v3 "Absolute value of the difference between partners for pli0043_v3"

	reg abs_pli0043_v3 d_daughter i.syear if sample == 1
	
* EXTRACT AND EXPORT RESULTS

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
		quietly levelsof pid1 if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
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
