********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2
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
	* biol contains data from biography questionnaires using individual-level raw data from the biographical questionnaire
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches)
	* bioparen contains generated biographical information on parents’ and respondent’s background
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset

	use "$DATA/h2_dataset.dta", clear
* ============================
* Analysis
* ============================

/*
REMINDER: H2. Having daughters leads to more left-leaning political preferences
*/


* FILTERS * (Please adapt according to your needs)

	/* recode missings */
	 foreach var of varlist syear pgmonth pid gebjahr sex pgfamst biokids kidgeb* kidsex* kidmon* plh0004{
		replace `var'=. if `var'<0
	}

	* drop observations where main outcome is missing
	drop if missing(plh0004)
	* drop observations with no child / no information on birth biography
	drop if missing(kidgeb01)


* GENERATE VARIABLES * (Please adapt according to your needs)

/* variables on individual characteristics */

	* gen age variable
	gen gebjahr_clean = gebjahr 
	ereplace gebjahr_clean = mode(gebjahr_clean), by(pid) // ensure consistent birth year
	gen age = syear - gebjahr_clean
	label var age "Age"
	
	/*
	* gen dummy indicating whether individual is of male sex
	tab sex, m
	gen male = (sex == 1) if !missing(sex)
	label var male "Sex: Male"
	
	
	* gen dummy for being married
	tab pgfamstd, m
	gen married = (pgfamstd == 1 | pgfamstd == 7) if !missing(pgfamstd)
	label var married "Married (Verheiratet zus. / eingetragende gg Partnerschaft zus.)"
	*/

/* variables on parenthood/childbirth */

	describe kidgeb01
	sum kidgeb01 if !missing(kidgeb01)
	
	* vars only available where birthbio is available
	//biokids // number of births from biography
	//sum kidgeb01 // birth year first child
	//kidmon01 // birth month first child
	//kidsex01 // sex of first child
	//kidgeb02 // birth year second child
	// ...
	sum biokids
	
	* gen year-month format (YYYYMM) for survey data & birth date
	* account for month of questionnaire and month of birth
	*tab pgmonth // month of the interview
	gen sdate = syear * 100 + pgmonth if !missing(pgmonth)
	
	global kids "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18" 

	foreach i of global kids {
	
		gen birthdate`i' = kidgeb`i' * 100 + kidmon`i' if !missing(kidmon`i')
	
	}
	
/* number of children */
	
	ds kidgeb*
	
	gen n_sons = 0 if biokids >= 0
	gen n_daughters  = 0 if biokids >= 0
	
	foreach i of global kids {
	    
		local gebvar = "birthdate`i'"
		local sexvar = "kidsex`i'"

		replace n_sons = n_sons + 1 if !missing(`gebvar') & `gebvar' <= sdate & `sexvar' == 1
		replace n_daughters  = n_daughters  + 1 if !missing(`gebvar') & `gebvar' <= sdate & `sexvar' == 2
		
		label var n_sons "Number of sons"
		label var n_sons "Number of daughters"
	
	}
	
/* number of brothers / sisters */

	gen n_brothers = n_sons
	replace n_brothers = n_sons - 1 if kidsex01 == 1
	label var n_brothers "Number of higher order brothers"
	
	gen n_sisters = n_daughters
	replace n_sisters = n_daughters - 1 if kidsex01 == 2
	label var n_sisters "Number of higher order sisters"
	
/* gen treatment indicator: first child has female sex */

	* treated: gender of first child is female
	gen d_daughter = (kidsex01 == 2) if !missing(kidsex01)
	label var d_daughter "Treatment: first child has female sex"
	*tab d_daughter  // 24,591 observations
	
	* generate variable on time until birth 
	gen event_time = ((syear - kidgeb01) * 12 + (pgmonth - kidmon01)) / 12
	label var event_time "Time until first childbirth [Years]"
	
	gen fchild_age = floor(event_time) 
	*tab fchild_age if !missing(plh0308_v1) 
	*tab fchild_age if !missing(plh0308_v2) 
	label var fchild_age "Age of first child [Years]"
	
/* variables on political preferences */
	*tab plh0004 // scale from 0 (ganz links) to 10 (ganz rechts)
	*tab syear if plh0004 >= 0 // 2000-2020; 12,602 obs
	
	* recode variable such that increase = more left
	gen left = plh0004 if !missing(plh0004)
	recode left (0=10) (1=9) (2=8) (3=7) (4=6) (5=5) (6=4) (7=3) (8=2) (9=1) (10=0)
	
	*tab event_time

* HYPOTHESIS TEST FOR H2 * 

	* Please add a verbal description of your model here:

	/*
We estimate the effect of having a daughter compared to having a son as a first child on political preferences. Yi,t represents our outcome variable, a value for political preferences between far right and far left, ranging from zero to ten. As before, variable d_daughter represents the indicator variable for having a daughter as a first child and our coefficient of interest is estimated by 1. The rest of the equation remains unchanged, Xi,t represents the same set of control variables, and we include survey wave fixed effects, represented by t to "capture" any influence common to all parents in that specific wave —like macro shocks, policy changes, or trends—thereby controlling for time-specific confounders.
We take the family as the unit that is treated and hence based on Abadie et al. (2022)cluster standard errors at the household level.
	*/
	
	reghdfe left d_daughter n_brothers n_sisters fchild_age, absorb (syear) cluster(hid)
	

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = _se[d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter] / _se[d_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
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
	file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
