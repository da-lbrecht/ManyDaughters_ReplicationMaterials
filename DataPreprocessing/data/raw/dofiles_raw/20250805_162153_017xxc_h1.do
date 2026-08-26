********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

//clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

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

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

	use "$DATA/h1_dataset.dta", clear 
	
	
* FILTERS * (Please adapt according to your needs)
 
	* Only keep participants from private households
	keep if inlist(pop,1,2)
	* Only keep equality attitudes variables are non-missing (effectively excluding all survey years but 2016-2018)
	keep if plj0621 >= 0 & plj0621 != . 
	keep if plj0622 >= 0 & plj0622 != . 
	keep if plj0623 >= 0 & plj0623 != . 
	keep if plj0624 >= 0 & plj0624 != . 
	keep if plj0625 >= 0 & plj0625 != . 
	
	* Only keep participants that give correct answers on birthyears of their children-fixed-effects
	foreach n in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 {
		drop if kidgeb`n' == -3 // answer inprobable
		drop if kidgeb`n' == -1 // no answer
	}
	
* GENERATE VARIABLES * (Please adapt according to your needs)


	* generate dummy variable for whether participant has at least one daughter (1) or not (0) up until survey year
	gen any_daughter = 0 
	foreach n in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 {
		replace any_daughter = 1 if kidsex`n'==2 & kidgeb`n'<=syear 
	}

	* gender equality attitudes, index components
	gen gender_equ_job1 = (plj0621)
	gen gender_equ_job2 = (plj0622)
	gen gender_equ_earning = (plj0623)
	gen gender_equ_educ = (plj0624)
	gen gender_equ_hus = (plj0625)
	
	* gender attitudes, index components. Reverse scale for gender_equ_earning, gender_equ_educ, & gender_equ_hus
	recode gender_equ_earning gender_equ_educ gender_equ_hus  (1=7) (2=6) (3=5) (5=3) (6=2) (7=1)
	
	* gender attitudes index as mean of index components
	egen gender_att_index = rowmean(gender_equ*)
	
	* sumkids, capped to 5
	gen sumkids_until_syear = 0 
	foreach n in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		replace sumkids_until_syear = sumkids_until_syear + 1 if kidgeb`n'<=syear & kidgeb`n'> 0 & kidgeb`n'!= .
	}	
	
	gen sumkids_capped = sumkids_until_syear
	replace sumkids_capped = 5 if sumkids_until_syear >= 5 & sumkids_until_syear != . 
	
	*  age at survey year
	gen age = syear - gebjahr
	
* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Please add a verbal description of your model here:
	/* OLS where we regress attitudes towards gender attitutes on whether a parent has any daughter born before attitudes are surveyed, 
	controlling for number-of-children-fixed-effects, and clustering standard errors on the level of the treatment (the household)*/

	reg gender_att_index any_daughter i.sumkids_capped age, vce(cluster hid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[any_daughter]

		* (ii) standard error
		local se = _se[any_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[any_daughter] / _se[any_daughter]
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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
