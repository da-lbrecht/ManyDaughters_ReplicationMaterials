********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

//clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
ssc install outreg2
/* Adapted by David Albrecht for reproducibility check
ssc install gtools
ssc install did_multiplegt_dyn
*/

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

	use "$DATA/h3_dataset.dta", clear 


* FILTERS * (Please adapt according to your needs)
 
	* Only keep participants from private households
	keep if inlist(pop,1,2)
	
	* Only keep participants that give correct answers on birthyears of their children-fixed-effects
	foreach n in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 {
		drop if kidgeb`n' == -3 // answer inprobable
		drop if kidgeb`n' == -1 // no answer
	}
	
	
	* Only keep participants with partners that are part of SOEP, i.e. have their own ID)
	keep if parid > 0 & parid != . 
	
	
	* Only keep participants with stable partners over their time in SOEP
		* Count number of distinct partner IDs per individual
		bysort pid (parid syear): gen parid_changed = parid != parid[_n-1]
		bysort pid (parid syear): replace parid_changed = 0 if _n == 1
		bysort pid: gen n_changes = sum(parid_changed)

		* If no change, then stable
		bysort pid (syear): gen stable = (n_changes[_N] == 0)	
	keep if stable
	
	
	* Only consider couples with where data from partner is available 
	
		* Create temporary partner dataset
		preserve
		keep pid syear pli0043_h  sex // keep PID and year for merging; sex; and housework (to contruct dependent variable)
		rename pid parid
		rename pli0043_h pli0043_h_par
		rename sex sex_par
		* Save temporary file to memory
		tempfile partnerdata
		save `partnerdata'

		* Restore original data
		restore

		* Merge partner info back to main dataset
		merge m:1 parid syear using `partnerdata', keep(match master) 
		tab _merge 	
		
	keep if _merge == 3
		
	* We are interested only in partnerships with male and female partners present. We furthermore only need one member of the household
	keep if sex == 1 & sex_par == 2 // essentialy keeping men with female partners
		
	* We are only interested observations with data on househousework for both partners
	keep if pli0043_h >= 0 & pli0043_h != . 
	keep if pli0043_h_par >= 0 & pli0043_h_par != .
	
	* We are only interested in observations where partners do at least some housework
	keep if pli0043_h + pli0043_h_par > 0

* GENERATE VARIABLES * (Please adapt according to your needs)

	* Our dependent variable is the share of housework done by men in a partnership (with men and women)
	gen housework_pid = pli0043_h if pli0043_h >= 0 & pli0043_h != . 
	gen housework_par = pli0043_h_par if pli0043_h_par >= 0 & pli0043_h_par != . 
	
	gen sum_housework = housework_pid + housework_par
	gen share_male_housework = housework_pid/sum_housework
	
	
	* generate dummy variable for whether participant has at least one daughter (1) or not (0) up until survey year
	gen any_daughter = 0 
	foreach n in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		replace any_daughter = 1 if kidsex`n'==2 & kidgeb`n'<=syear & kidgeb`n'> 0 & kidgeb`n'!= .
	}
	
	* sumkids, capped to 5
	gen sumkids_until_syear = 0 
	foreach n in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		replace sumkids_until_syear = sumkids_until_syear + 1 if kidgeb`n'<=syear & kidgeb`n'> 0 & kidgeb`n'!= .
	}	
	
	gen sumkids_until_syear_capped = sumkids_until_syear
	replace sumkids_until_syear_capped = 5 if sumkids_until_syear_capped >= 5 & sumkids_until_syear_capped != . 
	


* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	/* Heterogeneity robuts DiD model, following Chaisemartin and D'Haultfoeuille (2024), for a case with 
		staggered and binary treatment. We regress attitudes towards the share of male work in total household work (in hours, during weekdays) 
		in a given year on whether a parent has any daughter born before attitudes are surveyed, 
		controlling for number-of-children-fixed-effects.
		Up to four multiple event study effects are estimated. Total average effects are considered.  */

	xi: did_multiplegt_dyn share_male_housework pid syear any_daughter if sumkids_until_syear > 0, graph_off controls(sumkids_until_syear) effects(4) 


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient =  e(Av_tot_effect)		
		
		* (ii) standard error
		local se = e(se_avg_total_effect)

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = "N/A"
		local df_model = "N/A"

		* (v) p-value
		//local p_value = e(Av_tot_effect) / e(se_Av_tot_effect)
		//local p_value = 2 * ttail(e(df_r), abs(`p_value'))
		
		local z_stat = `coefficient' / `se'
		local p_value = 2 * normal(-abs(`z_stat'))

		* (vi) number of observations
		local num_obs = e(N_switchers_effect_average) 


		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local interpretation "not linearly interpretable"

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
