********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
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

*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", keep(1 3) keepus(kidpnr01 kidpnr02 kidgeb01 kidgeb02 kidsex01 kidsex02 kidmon01 kidmon02) nogen
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", keep(1 3) keepus(pgpsbil pgfamstd pgemplst pglabgro) nogen
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", keep(1 3) keepus(plh0308_v2 plh0333 plh0004 pli0040 pli0043_h pli0044_h pli0046) nogen
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 pid using "$IN/bioparen.dta", keep(1 3) keepus(fsedu msedu) nogen

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	keep if inrange(netto,10,19)


* GENERATE VARIABLES * (Please adapt according to your needs)

	set seed 123

* GENERATE VARIABLES * (Please adapt according to your needs)

	
	*Change non valid/missing values to NA
	mvdecode _all, mv(-1=. \ -2=.t \ -3=.x \ -5=.y \ -8=.z)

	*Recode gender
	gen male=(sex==1)

	*Generate age and age²
	gen age=piyear-gebjahr
	replace age=syear-gebjahr if missing(age)
	gen age2= age^2

	*Generate education dummy
	gen abitur=0 if pgpsbil>0
	replace abitur=1 if pgpsbil==4

	*Define daughter dummy
	gen daughter=(kidsex01==2) 
	
	*Define sample period
	xtset pid syear
	keep if inrange(syear, 2005,2019)
	
	*Omit observations if second child is born
	drop if syear>=kidgeb02
	
	*Aggregate time spent on tasks in household
	egen time_spent=rowtotal(pli0040 pli0043_h pli0044_h pli0046)
	
	*Define labor earnings
	gen labor_earn=pglabgro
	replace labor_earn=0 if pglabgro<0
	
	*Keep if first child is born after 2005
	keep if kidgeb01>2005
	
	*Define post treatment dummy
	gen post_dum= 0 if !missing(kidgeb01)
	replace post_dum= 1 if syear>=kidgeb01
	
	*Keep if full information on following variables
	keep if !missing(time_spent, age, age2, kidgeb01, abitur, labor_earn, pgemplst )

	*Keep if partner ID is known
	keep if !missing(parid)
	
	*Get time spent on household tasks from partner
	preserve
	keep pid syear time_spent
	rename pid parid
	rename time_spent time_spent_partner
	tempfile parid
	save `parid' , replace
	restore
	merge 1:1 parid syear using `parid'
	
	*Total time spent as household
	gen time_spent_couple=time_spent+time_spent_partner 
	
	*Relative time spent compared to partner
	gen time_allocation=time_spent/time_spent_couple
	
	*Only keep if relative time allocation could be calculated
	drop if time_allocation==.
	
	*Only keep if at least one pre and one post period observed
	bys pid hid: egen maxpost=max(post_dum)
	bys pid hid: egen minpost=min(post_dum)
	drop if maxpost==minpost
	


* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* To identify the causal effect of having a daughter on support towards gender equality in the
	* household (as measured by behaviours), we use the panel structure of the data and employ a
	* difference-in-differences (DiD) approach. Specifically, we compare within-individual changes
	* in support towards gender equality before and after the birth of a child between two groups:
	* parents whose first child is a daughter (treatment group) and those whose first child is a
	* son (control group). As the gender of the first-born child is random by nature, we can
	* exploit this exogenous variation to identify causal treatment effects. We use a Two-Way-
	* Fixed-Effect setting accounting for individual, household, and survey year fixed effects. To
	* measure support for gender equality (by behaviors), we use a variable that states the average
	* weekday amount of time an individual spends on all of the following tasks compared to their
	* partner: running errands, household chores, child care and care for other persons.

	* CHANGED SPECIFICATION: In the PAP (as writte above) we wanted to control for household fixed effects. This
	* can not be done, as the variable of interest (daughter) is constant across households and would therefore
	* be fully absorbed in the household fixed effects. For the regression model we corrected this and REMOVED
	* the household fixed effects
	
	xtset pid syear
	xtreg time_allocation post_dum##daughter age age2 i.abitur labor_earn i.pgemplst, fe vce(cluster hid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

		* (i) coefficient estimate
		local coefficient = _b[1.post_dum#1.daughter]

		* (ii) standard error
		local se2 = _se[1.post_dum#1.daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.post_dum#1.daughter] / _se[1.post_dum#1.daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.post_dum#1.daughter]
		local interpretation "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se2'"
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
	file write myfile "`coefficient', `se2', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
