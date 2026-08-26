********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
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

*pl: pli0043_h pli0012_h pli0016_h pli0044_h pli0019_h pli0022_h pli0054 pli0049_h pld0133 plj0122_h plj0123_h plh0258_h 
*hl: hlc0005_h
*biobirth: gebjahr sumkids 
*biol: lb0285


* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear 
	keep sex pid hid syear corigin

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(pid gebjahr kidpnr01-kidpnr19 kidsex01-kidsex19 sumkids )
	
	* biol 
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches) keepusing(pid hid lb0285)

	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pid syear pli0043_h ///
		pli0012_h pli0016_h pli0044_h pli0019_h pli0022_h pli0054 pli0049_h pld0133 plj0122_h ///
		plj0123_h plh0258_h pli0038_h pld0131_v1 pld0132_h)
		
	* hl contains variables on household level 
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) keepusing(hid syear hlc0005_h)

	label language EN // change language of the labels to English

* ============================
* INCLUSION CRITERIA
* ============================
	
	
*------------------------------------------------------	
* DISTANCE: Children living in the household
*------------------------------------------------------	
	
	
**SYEAR: daughter/son distance only assessed in 2001, 2006, 2011, 2016
*	tab plj0122_h syear 	
*	tab plj0123_h syear
*Given that the variable is only collected over a limited number of years, the analysis is confined to the respective years.


***recode missing values
	replace plj0122_h = . if inlist(plj0122_h, -9, -8, -7, -6, -5, -3, -2, -1)
	replace plj0123_h = . if inlist(plj0123_h, -9, -8, -7, -6, -5, -3, -2, -1)

	recode plj0122_h (0 = 1) (2/6 = 0), gen(d_daughter)
	label define distance 0 "[0] not in household" 1 "[1] in household", replace
	label values d_daughter distance

	recode plj0123_h (0 = 1) (2/6 = 0), gen(d_son)
	label define distances 0 "[0] not in household" 1 "[1] in household", replace
	label values d_son distances

	
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

	
**drop if partner does not live in the same household
	drop if with_partner != 1 
*drop if neither son nor daughter live in the same household
	drop if (d_son == 0 & d_daughter == 0)
	
	keep if inlist(syear, 2001, 2006, 2011, 2016)
	
	save "$DATA/h3_dataset.dta", replace // save merged dataset

	
* ============================
* DATA MANAGEMENT
* ============================


*------------------------------------------------------
*** DEPENDENT VARIABLE: calculate household equality gap
*------------------------------------------------------


*------------------------------------------------------
* Recode missing values (-1 bis -9) 
*------------------------------------------------------
	
	foreach var in pli0043_h pli0012_h pli0016_h pli0044_h pli0019_h pli0022_h pli0054 pli0049_h {
    replace `var' = . if `var' < 0
}

*------------------------------------------------------
* only households with exactly two people of which one is male and one is female
*------------------------------------------------------
	bysort hid (pid): gen n_persons_with_data = _N
	keep if n_persons_with_data == 2


	gen is_woman = sex == 2
	gen is_man   = sex == 1

	egen n_women = total(is_woman), by(hid syear)
	egen n_men   = total(is_man),   by(hid syear)

	keep if n_women == 1 & n_men == 1

*------------------------------------------------------
* Define component list
*------------------------------------------------------
	local components pli0043_h pli0012_h pli0016_h pli0044_h pli0019_h pli0022_h pli0054 pli0049_h

*------------------------------------------------------
* unpaid_work by year
*------------------------------------------------------
	levelsof syear, local(years)
	gen unpaid_work = .

	foreach y of local years {
		local varlist
		foreach v of local components {
			qui count if syear == `y' & !missing(`v')
			if r(N) > 0 {
				local varlist `varlist' `v'
			}
		}

    egen unpaid_work_`y' = rowtotal(`varlist') if syear == `y'
    replace unpaid_work = unpaid_work_`y' if syear == `y'
    drop unpaid_work_`y'
}

*------------------------------------------------------
* woman/man-allocation in the household
*------------------------------------------------------
	gen unpaid_woman = unpaid_work if sex == 2
	gen unpaid_man   = unpaid_work if sex == 1

	bysort hid (sex): replace unpaid_woman = unpaid_woman[_n-1] if missing(unpaid_woman)
	bysort hid (sex): replace unpaid_man   = unpaid_man[_n-1]   if missing(unpaid_man)

*------------------------------------------------------
* calculation of ratio and equality gap
*------------------------------------------------------
	replace unpaid_woman = 0.01 if unpaid_woman == 0
	replace unpaid_man   = 0.01 if unpaid_man == 0

	gen unpaid_work_ratio = unpaid_woman / unpaid_man
	gen equality_gap = abs(1 - unpaid_work_ratio)

*------------------------------------------------------	
* INDEPENDENT VARIABLE: having daughter
*------------------------------------------------------

	**kidsex == 2 means daughter 
	gen having_daughter = 0 
	replace having_daughter = 1 if kidsex01 == 2 | kidsex02  == 2 | kidsex03 == 2 | ///
		kidsex04 == 2 | kidsex05 == 2 	| kidsex06 == 2 | kidsex07 == 2 | kidsex08 == 2 | ///
		kidsex09 == 2 | kidsex10 == 2 | kidsex11 == 2 | kidsex12 == 2 | kidsex13 == 2 | ///
		kidsex14 == 2 | kidsex15 == 2 | kidsex16 == 2 | kidsex17 == 2 | kidsex18 == 2 | kidsex19 == 2  
	

*------------------------------------------------------
***CONTROLS	
*------------------------------------------------------


*------------------------------------------------------
* AGE: mean and difference of parents in the household 
*------------------------------------------------------
	gen age = syear - gebjahr  

	* save age by gender 
	gen woman_age = .
	gen man_age   = .

	bysort hid (sex): replace woman_age = age if sex == 2
	bysort hid (sex): replace man_age   = age if sex == 1

	bysort hid (sex): replace woman_age = woman_age[_n-1] if missing(woman_age)
	bysort hid (sex): replace man_age   = man_age[_n-1] if missing(man_age)

	* mean age in the household and difference between man and woman
	gen avg_parent_age    = (woman_age + man_age) / 2
	gen age_difference    = abs(woman_age - man_age)


*------------------------------------------------------
* INCOME: Household Income
*------------------------------------------------------
	
	recode hlc0005_h (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .)
	rename  hlc0005_h household_income

*------------------------------------------------------
* OCCUPATIONAL WORK
*------------------------------------------------------

	**recode negative values

	recode pli0038_h (-1 = .) (-2 = .) (-3 = .) (-4 = .) (-5 = .) (-6 = .) (-7 = .) (-8 = .) (-9 = .)

	rename  pli0038_h hours_occupation

	gen occ_hours_female = hours_occupation if sex == 2
	gen occ_hours_male   = hours_occupation if sex == 1
	
	**use values for both people in the household
	bysort hid (sex): replace occ_hours_female = occ_hours_female[_n-1] if missing(occ_hours_female)
	bysort hid (sex): replace occ_hours_male   = occ_hours_male[_n-1] if missing(occ_hours_male)

	
*------------------------------------------------------
* RELIGIOUS: Both parents religious (plh0258_h: 1 = Yes, 2 = No)
*------------------------------------------------------

	replace plh0258_h = . if inlist(plh0258_h, -1, -2, -3, -4, -5, -6, -7, -8, -9)

	gen relig = .
	replace relig = 1 if inlist(plh0258_h, 1,2,3,4,5,7,8,9,10,11,12)
	replace relig = 0 if plh0258_h == 6

	gen woman_relig = .
	gen man_relig   = .

	bysort hid (sex): replace woman_relig = relig if sex == 2
	bysort hid (sex): replace man_relig   = relig if sex == 1

	bysort hid (sex): replace woman_relig = woman_relig[_n-1] if missing(woman_relig)
	bysort hid (sex): replace man_relig   = man_relig[_n-1] if missing(man_relig)

	* Generate Dummy for both parents being religious 
	gen both_religious = (woman_relig == 1 & man_relig == 1)

	
* ============================
* ANALYSIS
* ============================

	reg equality_gap having_daughter avg_parent_age age_difference household_income ///
		sumkids both_religious occ_hours_female occ_hours_male i.syear , vce(cluster hid) 
		

* ============================
* EXTRACT AND EXPORT RESULTS 
* ============================

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[having_daughter]

		* (ii) standard error
		local se = _se[having_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[having_daughter] / _se[having_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[having_daughter]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
