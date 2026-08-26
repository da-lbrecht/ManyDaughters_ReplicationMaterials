********************************************************************************
*********************** Do-File H1: #ManyDaughters Project *********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

	version 17 // for reproducibility
	set seed 1984 // for reproducibility
	set processors 1 // for reproducibility - Set to 1 by David Albrecht
	
* INSTALL PACKAGES *
* install all packages you need here
*ssc install outreg2

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

* LOAD AND MERGE DATA *

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

/*
tab syear plh0301 // 2012 only, few cases
tab syear plh0308_v2 // 2018 only
tab syear plj0621 // 2016-2018, but cross-sectional data
tab syear plj0622 // 2016-2018, but cross-sectional data
tab syear plh0309 // 2012 only
*/


* FILTERS *
 
	* Keep if DVs was asked
	drop if inlist(plj0621, -8, -5, -1, .)
	drop if inlist(plj0622, -8, -5, -1, .)
	

* GENERATE VARIABLES *

	* Generate age of respondent
	gen age =  syear - gebjahr
	lab var age "Age"
	
	* Generate age of respondent centered
	sum age
	gen cage = age - r(mean)
	lab var cage "Age centered"

	* Generate education of respondent	
	gen educ = pgbilzeit if pgbilzeit >= 0
	lab var educ "Education (years)"
	
	* Generate education of respondent centered
	sum educ
	gen ceduc = educ - r(mean)
	lab var ceduc "Education centered"
	
	* Generate survey year-month
	gen ym_int = ym(syear, pmonin)
	lab var ym_int "Interview date (year-month)"
	format ym_int %tm
	
	* Generate kid birth year-month
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		gen ym_kid`i' = ym(kidgeb`i', kidmon`i')
		lab var ym_kid`i' "Kid `i' birth (year-month)"
		format ym_kid`i' %tm
	}
		
	* Generate boys birth year-month
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		gen ym_boy`i' = ym(kidgeb`i', kidmon`i') if kidsex`i' == 1
		lab var ym_boy`i' "Boy `i' birth (year-month)"
		format ym_boy`i' %tm
	}
	
	* Generate girls birth year-month
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		gen ym_girl`i' = ym(kidgeb`i', kidmon`i') if kidsex`i' == 2
		lab var ym_girl`i' "Girl `i' birth (year-month)"
		format ym_girl`i' %tm
	}
	
	* Generate years with kid(s)
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		gen y_kid`i' = ym_int - ym_kid`i'
	}
	egen ykids = rowtotal(y_kid01 - y_kid19)
	replace ykids = 0 if ykids < 0
	replace ykids = ykids / 12
	lab var ykids "# of years with kid(s)"
	drop y_kid01 - y_kid19

	* Generate years with son(s)
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		gen y_son`i' = ym_int - ym_boy`i'
	}
	egen ysons = rowtotal(y_son01 - y_son19)
	replace ysons = 0 if ysons < 0
	replace ysons = ysons / 12
	lab var ysons "# of years with son(s)"
	drop y_son01 - y_son19

	* Generate years with daughter(s)
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		gen y_daughter`i' = ym_int - ym_girl`i'
	}
	egen ydaughters = rowtotal(y_daughter01 - y_daughter19)
	replace ydaughters = 0 if ydaughters < 0
	replace ydaughters = ydaughters / 12
	lab var ydaughters "# of years with ydaughter(s)"
	drop y_daughter01 - y_daughter19
	
	* Generate variable - # of kids for participant
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		replace ym_kid`i' = 1 if ym_int >= ym_kid`i'
		replace ym_kid`i' = 0 if ym_int < ym_kid`i'
		format ym_kid`i' %1.0f
	}	
	egen kids = anycount(ym_kid01 - ym_kid19), values(1)
	lab var kids "# of kids"
	
	* Generate variable - # of sons for participant
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		replace ym_boy`i' = 1 if ym_int >= ym_boy`i'
		replace ym_boy`i' = 0 if ym_int < ym_boy`i'
		format ym_boy`i' %1.0f
	}	
	egen sons = anycount(ym_boy01 - ym_boy19), values(1)
	lab var sons "# of sons"
	
	* Generate variable - # of daughters for participant
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		replace ym_girl`i' = 1 if ym_int >= ym_girl`i'
		replace ym_girl`i' = 0 if ym_int < ym_girl`i'
		format ym_girl`i' %1.0f
	}	
	egen daughters = anycount(ym_girl01 - ym_girl19), values(1)
	lab var daughters "# of daughters"
	
	* Generate dummy variable for whether participant has at least one kid (1) or not (0)
	recode kids (0=0 "no kid") (1/19=1 "1+ kids"), gen(kid)
	lab var kid "Has a kid"
	
	* Generate dummy variable for whether participant has at least one son (1) or not (0)
	recode son (0=0 "no son") (1/19=1 "1+ sons"), gen(son)
	lab var son "Has a son"
	
	* Generate dummy variable for whether participant has at least one daughter (1) or not (0)
	recode daughters (0=0 "no daughter") (1/19=1 "1+ daughters"), gen(daughter)
	lab var daughter "Has a daughter"

	* Generate dummy variable for whether participant is a male (1) or not (0)
	recode sex (2=0 "woman") (1=1 "male"), gen(male)
	lab var male "Male"
	lab var sex "Gender"

	
* GENERATE DV: support for gender equality (as measured by attitudes) *

	* DVs: check correlations
	corr plj0621 - plj0625
	
	version 17 // for reproducibility
	set seed 1984 // for reproducibility
	sort pid // for reproducibility
	set processors 2 // for reproducibility
	
	* Generate DV: factor analysis
	factor plj0621 plj0622, ipf factors(1) blanks(0.3)
	predict gendereq
	lab var gendereq "Support for gender equality (attitudes)"

	
* GENERATE IV
	*FIRST KID IS A GIRL *
	gen iv_girl = 0
	replace iv_girl = 1 if ym_girl01 < ym_boy01 & kid == 1
	lab var iv_girl "Instrumental variable (first kid is a girl)"
	
	*FIRST KID IS A BOY *
	gen iv_boy = 0
	replace iv_boy = 1 if ym_girl01 > ym_boy01 & kid == 1
	lab var iv_boy "Instrumental variable (first kid is a boy)"
	

* OTHER CONTROL VARIABLES *
	* life satisfaction
	gen lifesat = .
	replace lifesat = plh0182 if plh0182 >= 0 & plh0182 <= 10
	lab var lifesat "Life satisfaction (0-10)"

	* satisfaction with family life
	gen famsat = .
	replace famsat = plh0180 if plh0180 >= 0 & plh0180 <= 10
	lab var famsat "Family life satisfaction (0-10)"
	
	* interest in politics
	recode plh0007 (4=0 "not at all") (3=1 "some") (2=2 "much") (1=3 "very much") (else = .), gen(polint)
	lab var polint "Interest in politics"	
	
	
* CHECKS *
	sum kids sons daughters kid son daughter ykids ysons ydaughters
	sum gendereq
	sum sex male age cage educ ceduc
	sum syear // just 21 observation in 2018
	sum iv_*
	sum lifesat famsat polint

	
* KEEP ONLY RELEVANT VARIABLES *
	keep ///
		kids sons daughters kid son daughter ykids ysons ydaughters ///
		gendereq ///
		cid pid hid parid partner psample sampreg pop ///
		sex	male age cage educ ceduc ///
		lifesat polint ///
		iv_* ///
		syear ///
		phrf

	
* ORDER VARIABLES *
	order ///
		cid pid hid parid partner psample sampreg pop ///
		iv_* ///
		syear ///
		phrf ///
		kids sons daughters kid son daughter ykids ysons ydaughters ///
		gendereq ///
		sex	male age cage educ ceduc ///
		lifesat polint

	
* FORMAT VARIABLES *
	format %30.0g partner psample pop
	format %20.0g sampreg 
	format %1.0g sex
	format %5.3f gendereq
	format %5.1f cage educ ceduc
	format %4.1f ykids ysons ydaughters

	
* SAVE DATASET *
	sort pid syear
	compress
	codebook, compact
	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h1_dataset.dta", clear
*set seed 12345
*version 15


* HYPOTHESIS TEST FOR H1: Having daughters leads to stronger support for gender equality (as measured by attitudes) *

	* Model: Extended linear regression (eregress) - Basic linear regression with endogenous covariates
	* Dependent variable = support for gender equality (factorial score of plj0621 plj0622)
	* Independent variable: number of years with daughter(s)
	* Control variables: male, age, education, life satisfaction, survey year

/*
	*xtset
	xtset pid syear
	xtsum age educ lifesat gendereq ydaughters // DATA IS CROSS-SECTIONAL / LONGITUDINAL / NO PANEL DATA
		
	*model
	xtreg gendereq ydaughters educ lifesat, fe vce(robust)
*/

	version 17 // for reproducibility
	set seed 1984 // for reproducibility
	sort pid // for reproducibility
	set processors 2 // for reproducibility
	
	*model
	eregress gendereq i.male age educ lifesat i.syear, ///
			endogenous(ydaughters = i.male age educ lifesat i.syear) vce(robust)


* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[ydaughters]

		* (ii) standard error
		local se = _se[ydaughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		*local df_residual = e(df_r)
		local df_residual = e(N) - e(k_eq) - e(k_dv)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[ydaughters] / _se[ydaughters]
		*local p_value = 2 * ttail(e(df_r), abs(`p_value'))
		local p_value = 2 * ttail(`df_residual', abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coefficient = _b[ydaughters]
		local interpretation "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di "Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient, se, family, residual_df, model_df, p_value, num_obs, num_participants, interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
