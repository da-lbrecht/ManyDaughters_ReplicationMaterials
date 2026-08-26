********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:
* ssc install egenmore

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

* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

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
**Variable shaping and selection

	* Helper vars
	gen age_resp = syear-gebjahr
	
	recode kidgeb* (-3 -2 -1=.)
	egen youngest_byear = rowmax(kidgeb*)
	egen oldest_byear   = rowmin(kidgeb*)
	
	
forvalues n = 1/19 {

	if `n'< 10 {
	   local suffix "0`n'"
	} 
	else {
	   local suffix "`n'"
	}
	
	replace kidgeb`suffix' = . if kidgeb`suffix' > syear
	replace kidsex`suffix' = . if kidgeb`suffix' > syear

}

	recode kidgeb* (nonmissing=1)
	egen num_children = rowtotal(kidgeb*)
	
	recode kidsex* (2=1) (nonmissing=0)
	egen num_daughters = rowtotal(kidsex*)
	

* Treatment 
	gen has_daughter = num_daughters>0


* Controls
	recode migback (2 3 =1) (nonmissing=0), gen(migback_bin)
	gen female = sex==2
	gen male = sex==1
	egen hhinc_quant = xtile(hghinc), n(4) by(syear)
	gen age_youngest = syear-youngest_byear
	replace age_youngest = . if youngest_byear>syear
	gen age_oldest = syear-oldest_byear
	replace age_oldest = . if oldest_byear>syear
	
* Filters (Exclusion criteria)
	keep if inlist(pop,1,2) // only keep participants from private households
	keep if num_children>0 // only people with kids
	drop if num_daughters>num_children // drop inconsistent number of children (1 person in 5% sample)
	keep if inrange(age_resp, 18,65)
	
	* Missing values in control vars
	drop if missing(pgbilzeit) | pgbilzeit==-2 | pgbilzeit==-1
	drop if hghinc < 0
	drop if pgfamstd < 0
	drop if pgemplst < 0
	drop if sampreg < 0
	drop if migback < 0
	

* Hours dedicated to childcare and chores during weekdays
	gen hours_childcare = pli0044_h
	replace hours_childcare = . if hours_childcare<0

	gen hours_chores = pli0043_h
	replace hours_chores = . if hours_chores<0

	gen hours_housework = hours_childcare + hours_chores

	* Merge partner info
	drop if parid<0
	preserve
	keep pid syear hours_housework female
	rename pid parid
	rename hours_housework hours_housework_partner
	rename female female_partner
	tempfile partners 
	save "`partners'"
	restore
	merge 1:1 parid syear using "`partners'"
	keep if _merge==3

	* Selections
	keep if female != female_partner
	drop if missing(hours_housework) | missing(hours_housework_partner)

	gen hours_man = .
	replace hours_man = hours_housework if !female
	replace hours_man = hours_housework_partner if female

	gen hours_couple = hours_housework + hours_housework_partner
	gen his_housework = hours_man / hours_couple
	replace his_housework = .5 if hours_housework==0 & hours_housework_partner==0 // equal division when both reported zero hours
	
	
* HYPOTHESIS TEST FOR H3* 

	* Please add a verbal description of your model here

	* OLS regression with robust standard errors and individual fixed effects, with his_housework as the outcome, 
	* has_daughter as the treatment and a host of control variables specified below
	xtset pid syear
	xtreg his_housework has_daughter age_resp i.hhinc_quant pgbilzeit i.pgfamstd i.pgemplst i.sampreg num_children syear, vce(robust) fe 

	

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient =  _b[has_daughter]

		* (ii) standard error
		local se = _se[has_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[has_daughter] / _se[has_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[has_daughter]
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
