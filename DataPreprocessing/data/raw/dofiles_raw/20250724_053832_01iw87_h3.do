********************************************************************************
*********************** Do-File H3: #ManyDaughters Project *********************
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
	set processors 2 // for reproducibility
	
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

	
* FILTERS *
 
	* Keep if DVs was asked
	drop if inlist(pli0051, -5, -3, -2, -1, .)
	drop if inlist(pli0043_h, -5, -3, -2, -1, .)
	
	
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

	* Generate # hours of leisure
	clonevar hleisure = pli0051
	lab var hleisure "Hours Weekday Leisure, Hobbies"

	* Generate # hours of housework
	clonevar hhousework = pli0043_h 
	lab var hhousework "Hours Weekday Housework"

	
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
	
	* married
	recode pgfamstd (1/2=1 "Yes") (6/8=1) (else=0 "No"), gen(married)
	lab var married "Married"
	
	* full-time employment
	recode pgemplst (1=1 "Yes") (else=0 "No"), gen(employed)
	lab var employed "Full-time employment"
	
	
* CHECKS *
	sum kids sons daughters kid son daughter ykids ysons ydaughters
	sum hleisure hhousework
	sum sex male age cage educ ceduc married employed
	sum syear
	sum iv_*
	sum lifesat famsat polint

	
* KEEP ONLY RELEVANT VARIABLES *
	keep ///
		kids sons daughters kid son daughter ykids ysons ydaughters ///
		hleisure hhousework ///
		cid pid hid parid partner psample sampreg pop ///
		sex	male age cage educ ceduc married employed ///
		lifesat famsat polint ///
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
		hleisure hhousework ///
		sex	male age cage educ ceduc married employed ///
		lifesat famsat polint

	
* FILTER - HAS A PARTNER *
	tab partner
	keep if inlist(partner, 1, 2)
	

* DYADIC DATA *

	* Create dyad-id
	sort hid pid syear
	egen a=group(hid pid parid)
	egen b=group(hid parid pid)
	egen c=rowmin(a b)

	* Generate dyad-id
	egen dyad=group(hid c)
	lab var dyad "Dyad ID"
	sort hid syear

	* Create dyad indicator
	gen dyad_ind=.
	replace dyad_ind=1 if !missing(dyad)
	replace dyad_ind=0 if missing(dyad_ind)
	lab var dyad_ind "Dyad Indicator"

	* Check
	sort dyad pid syear

	* Drop non-dyads
	drop if missing(dyad)

	* Create indicator for Actor 1 and Actor 2 in dyads
	gen dyad_nr = .
	bysort dyad syear (pid): replace dyad_nr = _n
	tab dyad_nr

	* Create partner vars
	sort dyad syear pid
	gen male_p = .
	gen age_p = .
	gen educ_p = .
	gen employed_p = .
	gen lifesat_p = .
	gen polint_p = .
	gen hleisure_p = .
	gen hhousework_p = .

	* Fill partner vars with value from actor
	bysort dyad syear: replace male_p = male[_n-1]
	bysort dyad syear: replace age_p = age[_n-1]
	bysort dyad syear: replace educ_p = educ[_n-1]
	bysort dyad syear: replace employed_p = employed[_n-1]
	bysort dyad syear: replace lifesat_p = lifesat[_n-1]
	bysort dyad syear: replace polint_p = polint[_n-1]
	bysort dyad syear: replace hleisure_p = hleisure[_n-1]
	bysort dyad syear: replace hhousework_p = hhousework[_n-1]

	* Checking
	sort dyad syear pid

	* Fill partner vars with value from partner
	gsort dyad syear -pid
	bysort dyad syear: replace male_p = male[_n-1] if !missing(male) & missing(male_p)
	bysort dyad syear: replace age_p = age[_n-1] if !missing(age) & missing(age_p)
	bysort dyad syear: replace educ_p = educ[_n-1] if !missing(educ) & missing(educ_p)
	bysort dyad syear: replace employed_p = employed[_n-1] if !missing(employed) & missing(employed_p)
	bysort dyad syear: replace lifesat_p = lifesat[_n-1] if !missing(lifesat) & missing(lifesat_p)
	bysort dyad syear: replace polint_p = polint[_n-1] if !missing(polint) & missing(polint_p)
	bysort dyad syear: replace hleisure_p = hleisure[_n-1] if !missing(hleisure) & missing(hleisure_p)
	bysort dyad syear: replace hhousework_p = hhousework[_n-1] if !missing(hhousework) & missing(hhousework_p)

	* Variable labels
	lab var male_p "Male (partner)"
	lab var age_p "Age (partner)"
	lab var educ_p "Education (years) (partner)"
	lab var employed_p "Full-time employment (partner)"
	lab var lifesat_p "Life satisfaction (0-10) (partner)"
	lab var polint_p "Interest in politics (partner)"
	lab var hleisure_p "Hours Weekday Leisure, Hobbies (partner)"
	lab var hhousework_p "Hours Weekday Housework (partner)"
	
	* Value labels
	lab val male_p male
	lab val employed_p employed
	lab val polint_p polint
	
	* Drop variables
	drop a b c dyad dyad_ind dyad_nr


* DV: # HOURS OF LEISURE / # HOURS OF LEISURE PARTNER *
	order hleisure hleisure_p hhousework hhousework_p, last
	gen leisure_equal = 100 - (abs(hleisure - hleisure_p) / (hleisure + hleisure_p) * 100)
	replace leisure_equal = 100 if hleisure == 0 & hleisure_p == 0
	sum leisure_equal
	lab var leisure_equal "Gender equality in the HH (behaviour: leisure)"

* DV: # HOURS OF HOUSEWORK / # HOURS OF LEISURE PARTNER *
	gen housework_equal = 100 - (abs(hhousework - hhousework_p) / (hhousework + hhousework_p) * 100)
	replace housework_equal = 100 if hhousework == 0 & hhousework_p == 0
	sum housework_equal
	lab var housework_equal "Gender equality in the HH (behaviour: housework)"
	
* FORMAT VARIABLES *
	format %30.0g partner psample pop
	format %20.0g sampreg 
	format %1.0g sex
	format %5.1f cage educ ceduc educ_p
	format %4.1f ykids ysons ydaughters
	format %3.1f leisure_equal hleisure hleisure_p
	format %3.1f housework_equal hhousework hhousework_p
	label drop pli0051_EN
	
	
* SAVE DATASET *
	*sort pid syear
	compress
	codebook, compact
	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h3_dataset.dta", clear


* HYPOTHESIS TEST FOR H3: Having daughters leads to more gender equality in the household (as measured by behaviors or outcomes). *

	* Model: Fixed-effects linear regression (xtreg)
	* Dependent variable = left-leaning attitudes (factorial score of plh0016 - plh002)
	* Independent variable: number of years with daughter(s)
	* Control variables: age, education, life satisfaction, political interest

	*xtset
	xtset pid syear
	xtsum age educ married employed lifesat 
	xtsum age_p educ_p employed_p lifesat_p
	xtsum leisure_equal housework_equal ydaughters
	
	version 17 // for reproducibility
	set seed 1984 // for reproducibility
	sort pid // for reproducibility
	set processors 2 // for reproducibility
	
	*model
	xtreg housework_equal ydaughters ///
			married kids age c.educ##c.educ_p employed##employed_p c.lifesat##c.lifesat_p, fe vce(robust)

	
* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[ydaughters]

		* (ii) standard error
		local se = _se[ydaughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[ydaughters] / _se[ydaughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient, se, family, residual_df, model_df, p_value, num_obs, num_participants, interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
