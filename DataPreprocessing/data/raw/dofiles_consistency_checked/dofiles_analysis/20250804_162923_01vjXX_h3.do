********************************************************************************
*********************  Do-File H3: #ManyDaughters Project **********************
********************************************************************************

* Authors: Tina Wang, David Albrecht, adapted by 01vjXX

* ============================
* Setup * 
* ============================

* clear all
version 14 // stata version
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

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing (pid kidsex01 kidpnr01 kidgeb01 kidmon01)
	* parent info
	*merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches) keepusing (pid fnr mnr)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing (pid hid pgpartz)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing (pid syear hid pli0043_h pli0046 pli0044_h)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* GENERATE VARIABLES * 
	drop eintritt erstbefr austritt psample letztbef sampreg netto nett1 casemat todjahr todinfo immiyear germborn corigin gebmonat loc1989 migback miginfo germborninfo corigininfo immiyearinfo sexor sexorinfo locinfo birthregion birthregion_ew arefback arefinfo rv_id prgroup pbleib phrf phrf0 phrf1

	** recode all missings
	mvdecode _all, mv(-8/-1 = .)
	
	** set panel data
	xtset pid syear
	sort pid syear 

	** create hadDaughter
	tab kidsex01
	recode kidsex01 (1=0) (2=1), gen(kidsex01F) // change coding from 1,2 to 0,1 - 1 is female, 0 is male
	
	gen hadChild = .
	replace hadChild = 1 if syear == kidgeb01	
		
	** take out those who do not have child in our observation period
	bysort pid (syear): egen ever_child = max(hadChild)
	
	** create running var 
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
	
	replace hadChild2 = 0 if hadChild2 == . 

	** now indicate whether that child was a boy or girl
	gen hadDaughter = .
	replace hadDaughter = 1 if hadChild2==1 & kidsex01F == 1 // 
	replace hadDaughter = 0 if hadChild2==1 & kidsex01F == 0 // 
	replace hadDaughter = 0 if syear < kidgeb01 // if birth has not yet happened

	** look at behavioural indicator of gender equality - care work ratio between women and men in partnership
	* first look at variables
	tab pli0043_h
	tab pli0046 
	tab pli0044_h
	
	*rename variables
	rename pli0043_h hw_hrs // housework hours
	rename pli0046 care_hrs // care work hours
	rename pli0044_h cc_hrs // childcare hours
	
	drop if cc_hrs == . | care_hrs == . | hw_hrs == . // remove observations that have missings on the dependent variable

	gen careHrs = hw_hrs + care_hrs + cc_hrs //  how much each person spends on care work

	** see if anyone has never had partner
	
	* create a flag variable that is 1 when partnered == 1
	tab pgpartz
	gen partnered = (pgpartz == 1| pgpartz == 2 | pgpartz == 3 | pgpartz == 4) 
	
	* now summarize it at the person level
	bysort pid (partnered): egen partnered2 = total(partnered)
	
	* drop observations of those who were never partnered
	drop if partnered2 == 0 // those who were never partnered
	
	** need to match partners to calculate household ratio of care work
	* start by creating dataset on partners ready to merge
	preserve
	keep pid syear hid careHrs kidpnr01 sex // keep relevant variables

	* rename partner variables to prepare for partner merge
	rename pid parid
	rename careHrs partner_careHrs
	rename hid parhid
	rename kidpnr01 parkidpnr01 // match on kids number so we identify parents and not other people living in the house
	rename sex parsex // match that partner is male?

	save partner_data.dta, replace

	restore
	
	* merge partner data 
	merge m:1 parid syear using partner_data.dta, keep(match master) nogen // merge partner data
	
	* make sure we are looking at partners who share first child
	gen hh_flag = .
	replace hh_flag = 1 if kidpnr01 != parkidpnr01
	replace hh_flag = 0 if kidpnr01 == parkidpnr01
	
	keep if hh_flag == 0 // only keep those where base person and partner have same pid for kid
	
	* create household ratio of carework
	gen careRatio = careHrs/partner_careHrs // here households are still doubled 
	
	* need to assume women to have the women/men ratio - get back to this further down - only keep women's observations who stand in for household

	** different checks to make sure:
		* - partners live in the same household
		* - partners live together at childbirth
		* - partners live together one year before childbirth
		* - individual is partnered and has child
		* - partner does not change
		* - measurement of careRatio exists before and after childbirth 
		
	* flag for same household
	gen flag = .
	replace flag = 1 if hid != parhid // flag those who are NOT one household
	replace flag = 0 if hid == parhid
	
	bysort pid  : egen total_flag = total(flag==1)
	
	bysort pid  : egen total_syears = count(syear)
	
	gen flag2 = .
	replace flag2 = 1 if total_syears == total_flag

	drop if flag2 == 1 // drop those who never live with partner
	
	*  flag for not living together year of childbirth
	
	gen flag3 = .
	xtset pid syear
	replace flag3 = 1 if hadChild == 1 & l.hadChild == . & pid == l.pid & flag == 1
	
	bysort pid  : egen total_flag3 = total(flag3==1)
	
	drop if total_flag3 == 1 // those not living together year of childbirth
	
	* flag for not living together one year before childbirth
		
	gen flag4 = .
	replace flag4 = 1 if f.hadChild == 1 & hadChild == . & pid == f.pid & flag == 1
	bysort pid  : egen total_flag4 = total(flag4==1)

	drop if total_flag4 == 1 // those not living together one year before childbirth

	
	* flag for those who are unpartnered and have child
	
	gen partner_change = parid != parid[_n-1] if pid == pid[_n-1] // always missing in first observation. 1 means there was a partner change
	gen after_birth = (syear > kidgeb01)

	gen partner_changed_after_birth = (partner_change == 1 & after_birth == 1)

	gen partner_change_flag = . 
	replace  partner_change_flag = 1 if pid == l.pid & partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l.pid & l.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l2.pid & l2.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l3.pid & l3.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l4.pid & l4.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l5.pid & l5.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l6.pid & l6.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l7.pid & l7.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l8.pid & l8.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l9.pid & l9.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l10.pid & l10.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l11.pid & l11.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l12.pid & l12.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l13.pid & l13.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l14.pid & l14.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l15.pid & l15.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l16.pid & l16.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l17.pid & l17.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l18.pid & l18.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l19.pid & l19.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l20.pid & l20.partner_changed_after_birth == 1
	replace  partner_change_flag = 1 if pid == l21.pid & l21.partner_changed_after_birth == 1

	drop if partner_change_flag == 1
	
	* check that the partner is the is the same
	gen yrs_after_birth = syear - kidgeb01 // create variable years after birth

	gen flag_partner2 = . 
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == -1
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 0
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 1
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 2
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 3
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 4
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 5
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 6
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 7
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 8
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 9
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 10
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 11
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 12
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 13
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 14
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 15
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 16
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 17
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 18
	replace flag_partner2 = 1 if pid == f.pid & parid != f.parid & yrs_after_birth == 19

	drop if flag_partner2 == 1

	* flag those who did not have partner year before birth - this is a double check
	gen no_partner_yr_before = .
	replace no_partner_yr_before = 1 if yrs_after_birth == -1 & pgpartz == 0 // no partner
	
	** drop all flag variables
	drop flag total_flag total_syears flag2 flag3 total_flag3 flag4 total_flag4 partner_change after_birth partner_changed_after_birth partner_change_flag yrs_after_birth flag_partner2 no_partner_yr_before
	
	** make sure measurement exists before and after childbirth 
	gen timing_birth = syear - kidgeb01 // years before and after first childbirth
	gen has_pre = .
	replace has_pre = 1 if timing_birth <= -1 & careRatio != . // has a before measurement
	gen has_post = .
	replace has_post = 1 if timing_birth >=  1 & careRatio != . // has an after measurement

	bysort pid: egen any_pre = max(has_pre) // checks for any pre measurement
	bysort pid: egen any_post = max(has_post) // checks for any post measurement
	
	** Final drops
	drop if ever_child == . // take out observations of those who never have a child
	keep if any_pre == 1 & any_post == 1 // only keep individuals with pre and post measurement

	
	* double check correct household composition: partnered 1 year before birth and at birth. drop observations when partner changes after birth. 
	keep hid pid sex syear parid pgpartz careRatio partner_careHrs hadDaughter timing_birth 
	
	** now need to select down to household level; use women as base. This was not mentioned in the PAP - only heterosexual couples included
	tab sex
	keep if sex == 2 
	
	** check how many people per household are left
	bysort hid (pid): gen tag = pid != pid[_n-1]
	egen n_pids = total(tag), by(hid)
	tab n_pids
	
	save "$DATA/h3_dataset_refined.dta", replace // save merged dataset

	use "$DATA/h3_dataset_refined.dta", clear

* HYPOTHESIS TEST FOR H3 * 

	* I run a fixed effects model. I assume that gender equality behaviour can be measured through the division of care work 
	* measured by the ratio of women's versus men's hours per week spent on three different care work tasks: housework (pli0043_h) care work (pli0046) and childcare (pli0044_h). 
	* The variable is the ratio based on hours per week. 
	* A value >1 means the women performs more care work, a value <1 means the women performs less care work. A value of 1 represents an equal distribution of care work. 
	* I run a fixed-effects model with standard errors clustered at the household level.
	* The constructed variable careRatio is the dependent variable and the dummy variable hadDaughter is the independent variable. 
	* I also include year fixed effects in the model.  
	
	xtset hid syear // deviation from PAP: accidentally put down pid where hid was meant
	sort hid syear
	xtreg careRatio hadDaughter i.syear, fe vce(cluster hid) // deviation from PAP_ birthOrder variable is not included as I only look at firstborn children 
	

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
