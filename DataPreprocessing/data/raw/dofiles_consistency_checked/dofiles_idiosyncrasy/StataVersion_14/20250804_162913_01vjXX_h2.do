********************************************************************************
*********************  Do-File H2: #ManyDaughters Project **********************
********************************************************************************

* Authors: Tina Wang, David Albrecht, adapted by 01vjXX

* ============================
* Setup * 
* ============================

* clear all
version 14 
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* ssc install outreg2

* CONFIGURE FILE PATHS * 
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
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing (pid kidsex01 kidpnr01 kidgeb01 kidmon01)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	*merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing (pid syear hid plh0012_h)
	* hgen contains generated variables on household level from the household interview; use for household net income
	*merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* GENERATE VARIABLES * 

	drop eintritt erstbefr austritt psample letztbef sampreg  nett1 casemat todjahr todinfo immiyear germborn corigin gebmonat loc1989 migback miginfo germborninfo corigininfo immiyearinfo sexor sexorinfo locinfo birthregion birthregion_ew arefback arefinfo rv_id prgroup pbleib phrf phrf0 phrf1

	** recode all missings
	mvdecode _all, mv(-8/-1 = .)
	
	** 	
	xtset pid syear
	sort pid syear
	
	** quick look at data
	xtdescribe, patterns(25)
	
	** recode sex of child - this is a universal variable that does not yet show WHEN the birth was 
	tab kidsex01
	recode kidsex01 (1=0) (2=1), gen(kidsex01F) // change coding from 1,2 to 0,1 - 1 is female, 0 is male
	
	** identify when birth occurred 
	gen hadChild = .
	replace hadChild = 1 if syear == kidgeb01 // marks year where a child is born
		
	gen hadChild2 = .
	replace hadChild2 = 1 if hadChild==1 & pid==pid // 
	replace hadChild2 = 1 if l.hadChild==1 & pid==l.pid // 
	replace hadChild2 = 1 if l2.hadChild==1 & pid==l2.pid // 
	replace hadChild2 = 1 if l3.hadChild==1 & pid==l3.pid //
	replace hadChild2 = 1 if l4.hadChild==1 & pid==l4.pid //
	replace hadChild2 = 1 if l5.hadChild==1 & pid==l5.pid //
	replace hadChild2 = 1 if l6.hadChild==1 & pid==l6.pid //
	replace hadChild2 = 1 if l7.hadChild==1 & pid==l7.pid //
	replace hadChild2 = 1 if l8.hadChild==1 & pid==l8.pid //
	replace hadChild2 = 1 if l9.hadChild==1 & pid==l9.pid //
	replace hadChild2 = 1 if l10.hadChild==1 & pid==l10.pid //
	replace hadChild2 = 1 if l11.hadChild==1 & pid==l11.pid //
	replace hadChild2 = 1 if l12.hadChild==1 & pid==l12.pid //
	replace hadChild2 = 1 if l13.hadChild==1 & pid==l13.pid //
	replace hadChild2 = 1 if l14.hadChild==1 & pid==l14.pid //
	replace hadChild2 = 1 if l15.hadChild==1 & pid==l15.pid //
	replace hadChild2 = 1 if l16.hadChild==1 & pid==l16.pid //
	replace hadChild2 = 1 if l17.hadChild==1 & pid==l17.pid //
	replace hadChild2 = 1 if l18.hadChild==1 & pid==l18.pid //
	replace hadChild2 = 1 if l19.hadChild==1 & pid==l19.pid //
	replace hadChild2 = 1 if l20.hadChild==1 & pid==l20.pid //

	** now indicate whether that child was a boy or girl
	gen hadDaughter = .
	replace hadDaughter = 1 if hadChild2==1 & pid==pid & kidsex01F == 1 // 
	replace hadDaughter = 0 if hadChild2==1 & pid==pid & kidsex01F == 0 // by definition children whose sex is unknown are exluded through this approach
	replace hadDaughter = 0 if syear < kidgeb01 // 
	
	** preparation to take out those who do not have child in the observation period
	bysort pid (syear): egen ever_child = max(hadChild)
	
	*** Create leftVote 
	tab plh0012_h
	recode plh0012_h (1=1)(2=0)(3=0) (4=0) (5=1) (6 =1) (7=0) (8=0) (9=1) (10=0) (11=0) (12=0) (13=0) (14=0) (15=0) (16=1) (17=1) (18=0) (19=0) (20=0) (21=0) (22=0) (23=0) (24=0) (25=0) (26=0) (27=0) (30=0) (31=0), gen(leftVote)
	
	** make sure measurement exists before and after childbirth 
	gen timing_birth = syear - kidgeb01 // years before and after first childbirth
	gen has_pre = .
	replace has_pre = 1 if timing_birth <= -1 & leftVote != . // has a before measurement
	gen has_post = .
	replace has_post = 1 if timing_birth >=  1 & leftVote != . // has an after measurement

	bysort pid: egen any_pre = max(has_pre) // checks for any pre measurement
	bysort pid: egen any_post = max(has_post) // checks for any post measurement
	
	
* FILTERS * 
	drop if ever_child == . // take out observations of those who never have a child

	keep if any_pre == 1 & any_post == 1 // only keep individuals with pre and post measurement


	
* HYPOTHESIS TEST FOR H2 * 
	* I run a fixed effects model. I assume that political attitudes can be measured through political party preference (plh0012_h). 
	* Specifically, preference for a left-leaning party (leftVote  == 1).
	* I run a fixed-effects model with standard errors clustered at the individual level.
	* The dummy variable leftVote is the dependent variable and the dummy variable hadDaughter is the independent variable. 
	* I also include year fixed effects.

	* xtset pid syear // already did this earlier to create lag variable
	xtreg leftVote hadDaughter i.syear, fe vce(cluster pid) // deviation from PAP_ birthOrder variable is not included as I only look at firstborn children. BirthOrder was mistakenly mentioned in the PAP. 


* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[hadDaughter]

		* (ii) standard error
		local se = _se[hadDaughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[hadDaughter] / _se[hadDaughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[hadDaughter]
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
