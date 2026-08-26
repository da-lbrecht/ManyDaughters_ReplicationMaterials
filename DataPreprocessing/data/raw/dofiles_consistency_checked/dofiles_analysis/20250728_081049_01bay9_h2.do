********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
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

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	keep if inrange(netto,10,19)


* GENERATE VARIABLES * (Please adapt according to your needs)

	set seed 123
	
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

	*Only include individuals up until a secon child is born
	drop if syear>=kidgeb02
	gen child=(syear>=kidgeb01)

	*Define political attitudes
	gen politicalattitude=.
	sum plh0004 
	replace politicalattitude=(plh0004-r(mean))/r(sd)

	*Define labor earnings
	gen labor_earn=pglabgro
	replace labor_earn=0 if pglabgro<0

	*Keep if first child is born in our observation periods
	keep if kidgeb01>2005

	*Generate post dummy
	gen post_dum= 0 if !missing(kidgeb01)
	replace post_dum= 1 if syear>=kidgeb01

	*Only keep individuals with full information in:
	keep if !missing(politicalattitude, age, age2, kidgeb01, abitur, labor_earn, pgemplst )

	*Only keep individuals where parid is known
	keep if !missing(parid)

	*Drop individuals who seperated after child born and still in observation period
	foreach y of numlist 2005 2009 2014 2019{
	gen aux=parid if syear==`y'
	bys pid: egen parid_`y'=max(aux)
	drop aux
	}

	gen breakup09=1 if parid_2005!=parid_2009 & syear==2009 & child==1
	gen breakup14=1 if parid_2014!=parid_2009 & syear==2014 & child==1
	gen breakup19=1 if parid_2014!=parid_2019 & syear==2019 & child==1

	replace post_dum=. if breakup09 == 1 &syear==2009
	replace post_dum=. if breakup14 == 1 &syear==2014
	replace post_dum=. if breakup19 == 1 &syear==2019

	drop if post_dum==.
	
	
	*Only keep if at leas one pre and one post period observed by pid and hid
	bys pid hid: egen maxpost=max(post_dum)
	bys pid hid: egen minpost=min(post_dum)
	drop if maxpost==minpost
	
	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* To identify the causal effect of having a daughter on political preferences, we use the panel
	* structure of the data and employ a difference-in-differences (DiD) approach. Specifically, we
	* compare within-individual changes in outcomes before and after the birth of a child between
	* two groups: parents whose first child is a daughter (treatment group) and those whose first
	* child is a son (control group). As the gender of the first-born child is random by nature, we
	* can exploit this exogenous variation to identify causal treatment effects. We use a Two-Way-
	* Fixed-Effect setting accounting for individual, household, and survey year fixed effects. To
	* measure political preference, we use a variable in which individuals indicate their political
	* views on a Likert scale ranging from 0 (far left) to 10 (far right).
	
	* CHANGED SPECIFICATION: In the PAP (as writte above) we wanted to control for household fixed effects. This
	* can not be done, as the variable of interest (daughter) is constant across households and would therefore
	* be fully absorbed in the household fixed effects. For the regression model we corrected this and REMOVED
	* the household fixed effects
	
	xtset pid syear
	xtreg politicalattitude post_dum##daughter age age2 i.abitur labor_earn i.pgemplst, fe vce(cluster hid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

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
		local interpretation "negative coefficient is in line with hypothesis"

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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se2', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
