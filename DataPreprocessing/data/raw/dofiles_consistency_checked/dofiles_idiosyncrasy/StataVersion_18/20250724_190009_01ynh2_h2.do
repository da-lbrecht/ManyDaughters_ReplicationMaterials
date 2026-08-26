********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* SETUP * 
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* ssc install outreg2

* CONFIGURE FILE PATHS * 
/*
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
* DATA PREPARATION * 
* ============================

* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear // 
	keep sex pid hid syear corigin


	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(pid kidpnr01-kidpnr19 ///
		kidsex01-kidsex19 kidgeb01-kidgeb19 sumkids)

	* biol 
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches) keepusing(pid hid lb0285)

	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pid hid syear pgpsbil pglabnet pgbilzeit)

	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pid hid syear ple0010_h plj0010 pld0133 plj0122_h plh0258_h plh0004 plh0007 pld0131_v1 pld0132_h)

	* hl contains variables on household level
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) keepusing(hid syear hlc0005_h)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset

* ============================
* DATA MANAGEMENT
* ============================

*--------------------------------------------------
***DEPENDENT VARIABLE: political attitude
*--------------------------------------------------

	**recode missing values
	recode plh0004  (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .)

*--------------------------------------------------
***Inclusion Criteria: giving birth to a child
*--------------------------------------------------
	keep if sumkids != 0 

*--------------------------------------------------
***INDEPENDENT VARIABLE: gebyear of first daughter 	
*--------------------------------------------------
 
	gen gebyear_daughter = 0
	replace gebyear_daughter = kidgeb01 if kidsex01 == 2
	replace gebyear_daughter = kidgeb02 if kidsex02 == 2 & kidsex01 != 2
	replace gebyear_daughter = kidgeb03 if kidsex03 == 2 & kidsex01 != 2 & kidsex02 != 2
	replace gebyear_daughter = kidgeb04 if kidsex04 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2
	replace gebyear_daughter = kidgeb05 if kidsex05 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2
	replace gebyear_daughter = kidgeb06 if kidsex06 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2
	replace gebyear_daughter = kidgeb07 if kidsex07 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2
	replace gebyear_daughter = kidgeb08 if kidsex08 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2
	replace gebyear_daughter = kidgeb09 if kidsex09 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2
	replace gebyear_daughter = kidgeb10 if kidsex10 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2
	replace gebyear_daughter = kidgeb11 if kidsex11 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2 & kidsex10 != 2
	replace gebyear_daughter = kidgeb12 if kidsex12 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2 & kidsex10 != 2 & kidsex11 != 2
	replace gebyear_daughter = kidgeb13 if kidsex13 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2 & kidsex10 != 2 & kidsex11 != 2 & kidsex12 != 2
	replace gebyear_daughter = kidgeb14 if kidsex14 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2 & kidsex10 != 2 & kidsex11 != 2 & kidsex12 != 2 & kidsex13 != 2
	replace gebyear_daughter = kidgeb15 if kidsex15 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2 & kidsex10 != 2 & kidsex11 != 2 & kidsex12 != 2 & kidsex13 != 2 & kidsex14 != 2
	replace gebyear_daughter = kidgeb16 if kidsex16 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2 & kidsex10 != 2 & kidsex11 != 2 & kidsex12 != 2 & kidsex13 != 2 & kidsex14 != 2 & kidsex15 != 2
	replace gebyear_daughter = kidgeb17 if kidsex17 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2 & kidsex10 != 2 & kidsex11 != 2 & kidsex12 != 2 & kidsex13 != 2 & kidsex14 != 2 & kidsex15 != 2 & kidsex16 != 2
	replace gebyear_daughter = kidgeb18 if kidsex18 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2 & kidsex10 != 2 & kidsex11 != 2 & kidsex12 != 2 & kidsex13 != 2 & kidsex14 != 2 & kidsex15 != 2 & kidsex16 != 2 & kidsex17 != 2
	replace gebyear_daughter = kidgeb19 if kidsex19 == 2 & kidsex01 != 2 & kidsex02 != 2 & kidsex03 != 2 & kidsex04 != 2 & kidsex05 != 2 & kidsex06 != 2 & kidsex07 != 2 & kidsex08 != 2 & kidsex09 != 2 & kidsex10 != 2 & kidsex11 != 2 & kidsex12 != 2 & kidsex13 != 2 & kidsex14 != 2 & kidsex15 != 2 & kidsex16 != 2 & kidsex17 != 2 & kidsex18 != 2


	recode gebyear_daughter (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .)
	

	*daughter born between 2006 and 2018 AND having a boy (gebyear_daughtert == 0)
	keep if gebyear_daughter == 0 | (gebyear_daughter >= 2006 & gebyear_daughter <= 2018)

	**generate DiD Variable 
	
	gen treat = (gebyear_daughter != 0)
	
	gen post = .
	replace post = 0 if gebyear_daughter != 0 & syear < gebyear_daughter
	replace post = 1 if gebyear_daughter != 0 & syear >= gebyear_daughter
	replace post = 0 if gebyear_daughter == 0
	
	gen did = treat * post
	
*--------------------------------------------------
***CONTROLS 
*--------------------------------------------------
	
*------------------------------------------------------	
* GENDER: recode sex
*------------------------------------------------------	
	recode sex (1=0) (2=1)
	label define sex ///
		0 	"[0] male " ///
		1 	"[1] female" ///
		, replace
	label values sex sex 
	
*------------------------------------------------------	
* ORIGIN: born in Germany - corigin
*------------------------------------------------------	

	gen born_germany = .
	replace born_germany = 1 if corigin == 1
	replace born_germany = 0 if corigin > 1
	
	
*------------------------------------------------------	
* EDUCATION: own education - pgpsbil 
*------------------------------------------------------

*INFO: An issue pertained to the variable pgpsbil within category 5 (other degree), which encompassed a substantial number of observations. The interpretation of this category proved to be unfeasible. Therefore, an alternative is employed in the form of the variable pgbilzeit (years of education).

	recode pgbilzeit (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .)
	
*------------------------------------------------------
* AGE: age in syear 
*------------------------------------------------------
	
	gen age = syear - ple0010_h  
	gen ln_age = ln(age)
	
*------------------------------------------------------	
* SINGLEPARENT: Singleparent/ partner in household  
*------------------------------------------------------
*INFO: pld0133 is conditional on having a "Lebensgemeinschaft". Married people living with their partner (pld0131_v1 == 1) are also coded as 1 (partner lives in household)
	
	gen with_partner = .
	*married people living with their partner
	replace with_partner = 1 if pld0131_v1 == 1 
	*partner lives in household
	replace with_partner = 1 if pld0133 == 1
	*partner does not live in household
	replace with_partner = 0 if pld0133 == 2
	*no "Lebensgemeinschaft" = no partner
	replace with_partner = 0 if pld0132_h == 2

	label define with_partner ///
		0 	"[0] no(t with) partner " ///
		1 	"[1] partner lives in household" ///
		, replace
	label values with_partner with_partner


*------------------------------------------------------
* INCOME: Personal contribution to income
*------------------------------------------------------

	recode pglabnet (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .)
	recode hlc0005_h (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .)
	
	gen income_ratio = pglabnet / hlc0005_h


*------------------------------------------------------
* RELIGIOUS: create variable religious
*------------------------------------------------------

	gen religious = .
	replace religious = 1 if inlist(plh0258_h, 1,2,3,4,5,7,8,9,10,11,12)
	replace religious = 0 if plh0258_h == 6
	bysort pid (syear): replace religious = religious[_n-1] if missing(religious)


*------------------------------------------------------
* POLITICS: recode interest in politics
*------------------------------------------------------

	recode plh0007 (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .) (1 = 4) (2 = 3) (3 = 2) (4 = 1)


* ============================
* ANALYSIS
* ============================
	
	**The DiD is based on repeated observations before and after the treatment (birth of a daughter).
	**Since we use fixed effects, we exclude variables that are (nearly) time-invariant, such as gender, education, religiosity, partnership status, birthplace (born in Germany), and number of children.
	
	xtset pid syear
	
	xtreg plh0004 post i.syear ln_age plh0007 income_ratio c.income_ratio#sex, fe vce(cluster pid)



* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[post]

		* (ii) standard error
		local se = _se[post]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[post] / _se[post]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[post]
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
