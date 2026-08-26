********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
version 18
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
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
	tab syear // 2000-2020

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
	
	tab syear

	save "$DATA/h4_dataset.dta", replace // save merged dataset
	

	use "$DATA/h4_dataset.dta", clear

* ============================
* Analysis
* ============================

/*
REMINDER: H4. Having daughters leads to a larger increase in the support of gender equality for men than for women (with gender equality measured by attitudes)
*/


* FILTERS * (Please adapt according to your needs)


* GENERATE VARIABLES *

/* recode missings */
	ds syear pgmonth pid gebjahr sex pgfamst biokids ///
	kidgeb* kidmon* kidsex* plh0308_v1 plh0308_v2

	local vars `r(varlist)'

	foreach x of local vars{
		replace `x'=. if `x'<0
		di "`x'"
		// tab `x' // Commented by David Albrecht to avoid errors on 95%-sample
	}


/* variables on individual characteristics */

	* gen age variable
	gen gebjahr_clean = gebjahr 
	ereplace gebjahr_clean = mode(gebjahr_clean), by(pid) // ensure consistent birth year
	gen age = syear - gebjahr_clean
	label var age "Age"
	sum age
	
	* gen dummy indicating whether individual is of male sex
	tab sex, m
	gen father = (sex == 1) if !missing(sex)
	label var father "Sex: Male"
	sum father
	
	* gen dummy for being married
	tab pgfamstd, m
	gen married = (pgfamstd == 1 | pgfamstd == 7) if !missing(pgfamstd)
	label var married "Married (Verheiratet zus. / eingetragende gleichgeschlechtliche Partnerschaft zus.)"

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
	tab pgmonth // month of the interview
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
	tab d_daughter  // 24,591 observations
	
	* generate variable on time until birth 
	//gen event_time = sdate - birthdate01 if !missing(kidgeb01)
	gen event_time = ((syear - kidgeb01) * 12 + (pgmonth - kidmon01)) / 12
	label var event_time "Time until first childbirth [Years]"
	
	gen fchild_age = floor(event_time)
	tab fchild_age if !missing(plh0308_v1) 
	tab fchild_age if !missing(plh0308_v2) 
	label var fchild_age "Age of first child"
	
/* variables on gender norms (attitudes) */
	tab plh0308_v2 // scale from 1 (stimme überhaupt nicht zu) to 7 (stimme voll zu)
	
* SAMPLE RESTRICTIONS

		
	* keep only individuals where outcome variable is filled
	drop if missing(plh0308_v2)
	* drop if no info on birth biography / first child
	drop if missing(kidgeb01)
	* keep only observations after birth of first child
	drop if event_time <= 0
	
* HYPOTHESIS TEST FOR H4 * 

	* Please add a verbal description of your model here
	
	/* 
	
We estimate the effect of having a daughter compared to having a son as a first child on gender norms by gender of the parent. Yi represents our outcome variable. d_daughter represents the indicator variable for having a daughter as a first child. father represents the indicator variable for being a father. Our coefficient of interest is estimated by 1. Xi, represents a set of control variables: To account for the fact that having any additional child or a daughter as a subsequent child might also affect gender norms, we control for the number of brothers and for the number of sisters of the first-born child at the time of the survey.
Our approach relies on the assumption that the sex of the first child is exogenous to parents. The assumption would be violated if parents had an influence on the sex of their first born, for example through selective abortion of unborn girls or boys. §15 of the Genetic Diagnostics Act is designed to prevent parents from aborting an unborn child based on undesired sex. Until the 14th week of pregnancy, abortions are generally illegal under criminal law, but they are not prosecuted and remain exempt from punishment under certain conditions. After the 14th week of pregnancy has passed, abortions are only permitted if the pregnant woman faces a life-threatening situation or the risk of a serious impairment to her physical or mental health. We therefore think that our assumption holds for the German context.
We take the family as the unit that is treated and hence based on Abadie et al. (2022)cluster standard errors at the household level.
	
	*/
	
	reghdfe plh0308_v2 d_daughter##father n_brothers n_sisters fchild_age, absorb (syear) cluster(hid) coeflegend
	
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.d_daughter#1.father]

		* (ii) standard error
		local se = _se[1.d_daughter#1.father]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.d_daughter#1.father] / _se[1.d_daughter#1.father]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughterxfather = _b[1.d_daughter#1.father]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
