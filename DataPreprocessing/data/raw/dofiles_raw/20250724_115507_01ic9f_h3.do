********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
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
*ssc install outreg2

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

***
* Prepare the biobirth data set so that it contains only individuals with only boys or only girls


	use "$IN/biobirth.dta", clear // use biobirth as a starting dataset
	
*drop individuals with missing information on at least one of their children's sex
	forvalues x = 1/19 {
		local varname = cond(`x' < 10, "kidsex0`x'", "kidsex`x'")
		drop if `varname' == -1
	}

* recode missing values for kidsex variables
	forvalues x = 1/19 {
		local varname = cond(`x' < 10, "kidsex0`x'", "kidsex`x'")
		replace `varname' = . if `varname' == -2
	}

* preparation for treatment indicator: only girls (vs. only boys) 	
	egen maxsex = rowmax(kidsex??)	
	egen minsex = rowmin(kidsex??)	

* drop individuals without children
	drop if maxsex == .
* drop individuals with children of different sex
	drop if maxsex != minsex
* drop individuals with unknown birth year of the first child
	keep if kidgeb01 > 0 & kidgeb01 < .

* generate the treatment indicator: only girls (vs. only boys)
	gen 	d_daughter = 1 if maxsex == 2
	replace d_daughter = 0 if maxsex == 1

* keep only relevant variables
	keep pid sex gebjahr kidpnr01 kidsex01 kidgeb01 d_daughter	
	save "$DATA/temp_biobirth.dta", replace // save dataset


*********
* LOAD AND MERGE DATA * 
	use pid syear pli0043_h pli0012_h pli0016_h pli0044_h pli0019_h pli0022_h pli0049_h pli0031_h pli0034_v4 using "$IN/pl.dta", clear // pl contains all variables from the individual questionnaire
	
	merge 1:1 pid syear using "$IN/ppathl.dta", nogen keep(master matches) keepusing(migback phrf) // ppathl contains background characteristics

	merge m:1 pid using "$DATA/temp_biobirth.dta", nogen keep(matches) // this is the above prepared dataset
	
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgpartnr) // pgen contains user-friendly data on the individual level that are consolidated from different sources 
	
	label language EN // change language of the labels to English

	
****
* APPLY SAMPLE RESTRICTIONS
	keep if inlist(syear, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021) /*outcome was only measured in these years*/

	drop if (gebjahr < 0) | (gebjahr == .) // drop individuals with missing birth year info
	keep if inlist(sex, 1, 2) // drop individuals with missing gender info

	keep if syear > kidgeb01 // keep only individuals who answered the question for the outcome variable when the first child was already born
	keep if syear < kidgeb01 + 18 // keep only individuals who answered the question for the outcome variable when the first child was 18 years or younger
	
	drop if migback < 0 | migback == .
	drop if kidpnr01 < 0 | kidpnr01 == .  
	drop if phrf < 0 | phrf == .
	
	drop if pgpartnr < 0 | pgpartnr == . // drop individuals without a partner ID
	
* GENERATE FURTHER VARIABLES *
	gen 	mh = inrange(migback,2,3)
	mvdecode pli0043_h pli0012_h pli0016_h pli0044_h pli0019_h pli0022_h pli0049_h pli0031_h pli0034_v4, mv(-5 -4 -3 -2 -1)	

	gen housework = (5 * pli0043_h + pli0012_h + pli0016_h) + (5 * pli0044_h + pli0019_h + pli0022_h) + (5 * pli0049_h + pli0031_h + pli0034_v4)
	
	* keep only relevant variables
	keep pid syear housework mh phrf sex gebjahr kidpnr01 kidgeb01 d_daughter pgpartnr
	save "$DATA/h3_dataset.dta", replace // save merged dataset

***************	
	* construct a data set that contains only information for men
	keep if sex == 1

	drop pgpartnr
	rename pid pgpartnr
	foreach var of varlist housework mh phrf sex gebjahr kidpnr01 kidgeb01 d_daughter {
		rename `var' `var'_man
	}	
	
	order pgpartnr syear housework d_daughter 
	save "$DATA/h3_dataset_men.dta", replace // save merged dataset
	
****************
* Merge information at the couple level
	use 	"$DATA/h3_dataset.dta", clear
	keep 	if sex == 2
	foreach var of varlist pid housework mh phrf sex gebjahr kidpnr01 kidgeb01 d_daughter  {
		rename `var' `var'_woman
	}	
	merge  1:1 pgpartnr syear using 	"$DATA/h3_dataset_men.dta", keep(match) nogen


*********
* Apply couple-level restrictions
	keep 	if kidpnr01_woman == kidpnr01_man
	keep	if d_daughter_woman == d_daughter_man 
		
	
***********
* Construct couple-level variables
	gen 	gender_equality = 1 - abs((housework_woman / (housework_man + housework_woman)) - 0.5)  /*outcome*/
	gen 	phrf_c = (phrf_man + phrf_woman)/2 /*couple-level weighting factor*/


* ============================
* Analysis
* ============================

* FILTER: Further Sample Selection


* HYPOTHESIS TEST FOR H3 * 
/*To test H3, we will estimate the following linear regression model:

y_3ct=α+βd_c+X_1c γ_1  + X_4t γ_4+ε_ct              (3)


y_3ct:  Gender equality in the household 
d_c: Dummy indicating whether couple c has only biological daughters
X_1c: Vector of fixed effects for the birth year of the first child
X_4t: Vector of survey year fixed effects
ε_ct: Error term

*/


	
* HYPOTHESIS TEST FOR H3 * 
regress gender_equality d_daughter_woman i.kidgeb01_woman i.syear [pweight=phrf_c], vce(cluster kidpnr01_woman)





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
		local p_value = ttail(e(df_r), abs(`p_value'))  /*p-value for one-sided test as stated in PAP*/

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
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
