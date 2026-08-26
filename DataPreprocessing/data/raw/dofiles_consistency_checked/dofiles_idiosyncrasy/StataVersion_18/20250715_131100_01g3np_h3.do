********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
ssc install reghdfe
ssc install ereplace

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

	/* merge SOEP datasets */

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
	
	/* recode missings */
	ds syear pgmonth pid gebjahr sex pgfamst ///
	kidgeb* kidsex* kidmon* pli0044_h ///
	pgpartnr

	local vars `r(varlist)'

	foreach x of local vars{
		replace `x'=. if `x'<0
		di "`x'"
		// tab `x' // Commented by David Albrecht to avoid errors on 95%-sample
	}

	/* create household level dataset */
	
	preserve
	
	* save dataset containing all women
	keep if sex==2
	replace pgpartnr=pid
	drop pid
	gen hours_f = pli0044_h
	save "$DATA/h3_women.dta", replace
	
	restore

	* now keep men only & add female partners
	keep if sex==1
	gen hours_m = pli0044_h
	replace pgpartnr=-runiform() if mi(pgpartnr)
	merge 1:1 pgpartnr syear using "$DATA/h3_women.dta", keep(match)  nogen
	replace pgpartnr=. if pgpartnr<0
	
	save "$DATA/h3_dataset.dta", replace // save merged dataset

	use "$DATA/h3_dataset.dta", clear
	
* ============================
* Analysis
* ============================

/* REMINDER: H3 Having daughters leads to more gender equality in the household (as measured by behaviors or outcomes)*/

* FILTERS * (Please adapt according to your needs)
	
	* drop observations where main outcome is missing for one of the partners
	drop if missing(hours_m) | missing(hours_f)
	
	* drop observations with no child / no information on birth biography
	drop if missing(kidgeb01) 
	
	* drop people with first birth before SOEP observation period
	drop if kidgeb01 < 1985

* GENERATE VARIABLES * (Please adapt according to your needs)


/* variables on individual characteristics */

	* gen age variable
	gen gebjahr_clean = gebjahr 
	ereplace gebjahr_clean = mode(gebjahr_clean), by(pid) // ensure consistent birth year
	gen age = syear - gebjahr_clean
	label var age "Age"
	
	* gen dummy indicating whether individual is of male sex
	tab sex, m
	gen male = (sex == 1) if !missing(sex)
	label var male "Sex: Male"
	
	* gen dummy for being married
	tab pgfamstd, m
	gen married = (pgfamstd == 1 | pgfamstd == 7) if !missing(pgfamstd)
	label var married "Married (Verheiratet zus. / eingetragende gg Partnerschaft zus.)"

/* variables on parenthood/childbirth */

	describe kidgeb01
	sum kidgeb01 if !missing(kidgeb01)
	
	* gen year-month format (YYYYMM) for survey data & birth date
	* account for month of questionnaire and month of birth
	tab pgmonth // month of the interview
	gen sdate = syear * 100 + pgmonth if !missing(pgmonth)
	gen birthdate01 = kidgeb01 * 100 + kidmon01 if !missing(kidmon01)
	gen birthdate02 = kidgeb02 * 100 + kidmon02 if !missing(kidmon02)

/* generate time to birth  */

	* generate variable on time until birth in months
	gen event_time = (syear - kidgeb01) * 12 + (pgmonth - kidmon01)
	label var event_time "Time until first childbirth [Months]"
	
/* gen treatment and post indicator */

	* treated: gender of first child is female
	gen treat = (kidsex01 == 2) if !missing(kidsex01)
	label var treat "Treatment: first child has female sex"
	tab treat  
	
	* post: observation after birth of first child
	gen post = event_time >= 0 if !missing(event_time)
	label var post "Post: person is observed after birth of first child"
	tab post
	
	* interaction term
	gen interaction = treat * post 
	label var interaction "Interaction term: Treat"
	tab interaction
	
	/* variables on gender equality in terms of behaviour/outcomes */
	
	/*time use: childcare weekday */
	
	local childcare pli0044_h 		
	sum hours_m
	sum hours_f
			
	gen childcare_diff = abs(hours_m - hours_f)
	
* SAMPLE RESTRICTIONS *

	* keep only observations 1 year before & after childbirth
	keep if event_time >= -12 & event_time <= 12
	sum biokids

	* drop observations where month of childbirth = month of survey
	drop if event_time == 0
	
* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	/*
We use a simple difference-in-differences setup, where we compare households who had a girl to parents who had a boy before and after giving birth to their first child. We limit our sample to individuals surveyed within 12 months before and 12 months after giving birth to the first child because we want to separate the effect of having a daughter from the potential effects of additional children. Our main identification assumption requires that, in the absence of the treatment (i.e., having a girl), parents who had daughters and those who had sons would have shown similar trends in the outcome variable prior to birth. 
	*/

	reghdfe childcare_diff treat i.post interaction, vce(cluster hid)

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[interaction]

		* (ii) standard error
		local se = _se[interaction]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[interaction] / _se[interaction]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[interaction]
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
