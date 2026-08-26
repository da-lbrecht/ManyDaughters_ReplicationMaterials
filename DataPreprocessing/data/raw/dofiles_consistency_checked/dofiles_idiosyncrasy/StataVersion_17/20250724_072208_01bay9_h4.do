********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
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

	save "$DATA/h4_dataset.dta", replace // save merged dataset


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
	
	*Generate parents education dummy
	gen parent_abitur=(fsedu==4) | (msedu==4)
	
	*Define labor income
	gen labor_earn=pglabgro
	replace labor_earn=0 if pglabgro<0
	
	*Recode political attitudes (voting outcome)
	replace plh0333=29 if plh0333==-5 & inrange(psample,17,19)
	clonevar party_vote=plh0333
	
	*Define and standardize attitudes towards gender equality
	sum plh0308_v2 if syear==2018
	gen equalityattitude=(plh0308_v2-r(mean))/r(sd)

	*Define dummies for first and second child
	gen first_child18=1 if kidgeb01<=2018
	gen second_child18=1 if kidgeb02<=2018
	
	
	*** Sample restriction
	
	*Only keep if first child born before or in 2018 and no second child at 2018
	keep if first_child18==1 & second_child18!=1

	*Only keep cross section 2018
	keep if syear==2018

	*Only keep if full information on following variables
	keep if !missing(equalityattitude,age, daughter, migback, kidgeb01, abitur, parent_abitur, party_vote, labor_earn, pgemplst )

	*Only keep if partner is also in sample (to allow for household fixed effects)
	*keep if !missing(parid)
	*preserve
	*keep parid
	*rename parid pid
	*tempfile parid
	*save `parid' , replace
	*restore
	*merge 1:1 pid using `parid'
	*keep if _merge==3 // only keep observations if we have all variables from both partners.
	*drop _merge

* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)

	* To identify the causal effect of having a daughter on support towards gender equality (as
	* measured by attitudes), we estimate a cross-sectional OLS regression in a sample of parents
	* who in 2018 only have exactly one child (first-born). Specifically, we use the fact that the
	* sex of the first-born child is random by nature for the identification of the effect of having
	* a daughter compared to having a son. Our dependent variable is surveyed in 2018 and
	* captures attitudes towards equal sharing of employment and care work within couples. The coefficient 
	* of interest captures the interaction term between father/mother and daughter/son. We
	* include only individuals for whom we also have valid observations for their partner, in order
	* to account for household fixed effects.

	* CHANGED SPECIFICATION: In the PAP (as writte above) we wanted to control for household fixed effects. This
	* can not be done, as the variable of interest (daughter) is constant across households and would therefore
	* be fully absorbed in the household fixed effects. For the regression model we corrected this and REMOVED
	* the household fixed effects

	reg equalityattitude i.male##(i.daughter c.age c.age2 i.migback i.kidgeb01 i.abitur i.parent_abitur i.party_vote c.labor_earn i.pgemplst), vce(cluster hid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.male#1.daughter]

		* (ii) standard error
		local se2 = _se[1.male#1.daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.male#1.daughter] / _se[1.male#1.daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_daughter = _b[1.male#1.daughter]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se2', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
