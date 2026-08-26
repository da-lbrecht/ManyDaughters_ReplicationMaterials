********************************************************************************
****************** Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* SETUP * 
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000


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

***Which Variables do we need from which dataset:
*pl: plj0621 plj0622 plj0623 plj0624 plj0625 plj0620 ple0010_h plj0010 pld0133 plj0122_h plh0258_h
*biol: lb0285 lb0380 lb0381
*biobirth: kidpnr01-kidpnr19 kidsex01-kidsex19 sumkids 
*ppathl: sex
*pgen: pgpsbil pglabnet  
*hl: hlc0005_h


***inclusion criteria:
*only 2016
*at least one child lb0285 > 0


* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear 
	keep sex pid hid syear corigin
	
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(pid kidsex01-kidsex19 sumkids )
	* biol 
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches) keepusing(pid hid lb0285)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pid hid syear pgpsbil pglabnet pgbilzeit)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pid hid syear plj0621 plj0622 plj0623 ///
		plj0624 plj0625 plj0620 plj0010 pld0133 plj0122_h plh0258_h pld0131_v1 pld0132_h ple0010_h)
	* hl contains variables on household level
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) keepusing(hid syear hlc0005_h)

	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset

* ============================
* INCLUSION CRITERIA
* ============================

**Survey Year 2016 and having children 
	keep if syear == 2016 & lb0285 > 0	
	
* ============================
* DATA MANAGEMENT
* ============================

*------------------------------------------------------
*** DEPENDENT VARIABLE: attitudes toward gender equality
*------------------------------------------------------

	**recode missing values 
	recode plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .)
	
	**recode: plj0623 plj0624 plj0625
	recode plj0623 plj0624 plj0625 (1=7) (2=6) (3=5) (5=3) (6=2) (7=1)
	label define scale_recoded ///
		1 	"[1] [de] 1=Stimme voll zu, (Skale 1-7) recoded " ///
		2 	"[2] [de] 2 auf Skala 0-7 recoded" ///
		3 	"[3] [de] 3 auf Skala 0-7 recoded" ///
		4	"[4] [de] 4 auf Skala 0-7 recoded" ///
		5	"[5] [de] 5 auf Skala 0-7 recoded" ///
		6	"[6] [de] 6 auf Skala 0-7 recoded" ///
		7 	"[7] [de] 7=Stimme überhaupt nicht zu, (Skale 1-7) recoded " ///
		, replace
	label values plj0623 plj0624 plj0625 scale_recoded

	**z-standardize - index - z-standardize
	foreach var of varlist plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 {
		egen z_`var' = std(`var')
		}

	gen attitude_gender_equality = (z_plj0620 + z_plj0621 + z_plj0622 + z_plj0623 + z_plj0624 + z_plj0625) / 6
	
	egen z_attitude_gender_equality = std(attitude_gender_equality)
	
	
*------------------------------------------------------	
***INDEPENDENT VARIABLE: Having Daughter 
*------------------------------------------------------
		
	**kidsex == 2 means daughter 
	gen having_daughter = 0 
	replace having_daughter = 1 if kidsex01 == 2 | kidsex02 == 2 | kidsex03 == 2 | ///
		kidsex04 == 2 | kidsex05 == 2 | kidsex06 == 2 | kidsex07 == 2 | ///
		kidsex08 == 2 | kidsex09 == 2 | kidsex10 == 2 | kidsex11 == 2 | kidsex12 == 2 | ///
		kidsex13 == 2 | kidsex14 == 2 | kidsex15 == 2 | kidsex16 == 2 | kidsex17 == 2 | ///
		kidsex18 == 2 | kidsex19 == 2 

*------------------------------------------------------
***CONTROLS	
*------------------------------------------------------

*INFO: Contrary to the PAP, we excluded the variables 'education of father' (lb0380) and 'education of mother' (lb0381), as both variables had only empty values in 2016. We attempted to use lb0091_h and lb0092_h as alternatives, but these variables had insufficient observations. Consequently, we have decided not to use these variables in our analysis.


*------------------------------------------------------
* RELIGIOUS: create variable religious
*------------------------------------------------------
	
	gen religious = .
	replace religious = 1 if inlist(plh0258_h, 1,2,3,4,5,7,8,9,10,11,12)
	replace religious = 0 if plh0258_h == 6
	bysort pid (syear): replace religious = religious[_n-1] if missing(religious)

	
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
*INFO: An issue pertained to the variable pgpsbil within category 5 (other degree), which encompassed a substantial number of observations. The interpretation of this category proved to be unfeasible.
*Therefore, an alternative is employed in the form of the variable pgbilzeit (years of education).

	recode pgbilzeit (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .) 
	
	
*------------------------------------------------------
* AGE: age in 2016 
*------------------------------------------------------

	gen age = 2016 - ple0010_h  
	gen age_ln = ln(age)


*------------------------------------------------------	
* SINGLEPARENT: Singleparent/ partner in household 
*------------------------------------------------------
*INFO: pld0133 is conditional on having a "Lebensgemeinschaft". 
*Married people living with their partner (pld0131_v1 == 1) are also coded as 1 (partner lives in household)

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
* DAUGHTERS DISTANCE
*------------------------------------------------------

	gen daughters_distance = 0 
	replace daughters_distance = 1 if plj0122_h == 1 & having_daughter == 1
	label define distance ///
		0 	"[0] daughter does not live in household or only sons" ///
		1 	"[1] daughter lives in household" ///
		, replace
	label values daughters_distance distance

*------------------------------------------------------
* INCOME: Personal contribution to income
*------------------------------------------------------

	recode pglabnet (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .)
	recode hlc0005_h (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .)
	
	gen income_ratio = pglabnet / hlc0005_h


* ============================
* ANALYSIS
* ============================
		  
	reg z_attitude_gender_equality having_daughter##sex lb0285 pgbilzeit  ///
		c.age_ln reli, vce(cluster pid)
	
	
**The variables daughters_distance, with_partner, and born_germany were either omitted due to collinearity or not available for any observations in the estimation sample. Furthermore, including income_ratio and the interaction term c.income_ratio#sex resulted in a loss of approximately 88% of the observations. As a result, all these variables, although pre-specified in the PAP, were excluded from the final regression models in order to preserve statistical power and ensure model convergence.
**This may be resolved when using the full dataset

*reg z_attitude_gender_equality having_daughter##sex lb0285 pgbilzeit c.age_ln ///
*		income_ratio c.income_ratio#sex reli born_germany with_partner daughters_distance, vce(cluster pid)

	
		
* ============================
* EXTRACT AND EXPORT RESULTS 
* ============================

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.having_daughter#1.sex]

		* (ii) standard error
		local se = _se[1.having_daughter#1.sex]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.having_daughter#1.sex] / _se[1.having_daughter#1.sex]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.having_daughter#1.sex]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
