********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

cd "$PATH" 

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

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

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)

    * Keep only relevant variables
    keep kidgeb01-kidgeb19 kidsex01-kidsex19 sumkids cid pid syear sex psample ///
         gebjahr germborn sampreg partner hghinc pgbilzeit plh0004

    * Drop if no biological kids ever
    drop if kidsex01 <= 0


* GENERATE VARIABLES * (Please adapt according to your needs)

    * Left-Right political scale (DV)
    gen lrscale = plh0004
    replace lrscale = . if lrscale < 0

    * First child female dummy (treatment group)
    gen first_child_female = .
    replace first_child_female = 1 if kidsex01 == 2 & !missing(kidsex01)
    replace first_child_female = 0 if kidsex01 == 1 & !missing(kidsex01)

    * Post indicator (treatment timing)
    gen post = .
    replace post = 1 if syear >= kidgeb01 & !missing(kidgeb01)
    replace post = 0 if syear < kidgeb01 & kidgeb01 > 1900 & !missing(kidgeb01)

    * Control variables
    replace sex = 0 if sex == 1
    replace sex = 1 if sex == 2

    replace germborn = 0 if germborn == 1
    replace germborn = 1 if germborn == 2

    rename sampreg westgerm
    replace westgerm = . if westgerm <= 0
    replace westgerm = 0 if westgerm == 2

    replace partner = . if partner < 0 | partner == 5
    replace partner = 1 if partner != 0

    replace pgbilzeit = . if pgbilzeit < 0

    gen age = syear - gebjahr
    gen age_sq = age^2

    gen ln_hghinc = log(hghinc)
	
	* Dummies for number of kids
	forvalues k = 0/7 {
		gen kids_`k' = .
		replace kids_`k' = 1 if sumkids == `k'
		replace kids_`k' = 0 if sumkids != `k' & !missing(sumkids)
	}
	
	* Dummy if 8 kids or more
	gen kids_8plus = .
	replace kids_8plus = 1 if sumkids >= 8
	replace kids_8plus = 0 if sumkids <8 & !missing(sumkids)




* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

    * Hypothesis: Having a daughter increases left-wing attitudes (measured via lrscale), using DiD
    * Model: DiD regression with individual and year fixed effects (panel)
    * Controls: sex, age_sq, ln_hghinc, kids dummies, and others

    xtset pid syear

    xtreg lrscale i.first_child_female##i.post i.syear sex age age_sq germborn ///
        westgerm partner ln_hghinc pgbilzeit kids_*, fe vce(cluster pid)


* EXTRACT AND EXPORT RESULTS * (Please adapt to fit your reporting format)

    * Extract required statistics for interaction term (treatment effect)

        * (i) coefficient estimate
        local coefficient = _b[1.first_child_female#1.post]

        * (ii) standard error
        local se = _se[1.first_child_female#1.post]

        * (iii) family of the test statistic
        local test_stat_family = "t"

        * (iv) residual and model degrees of freedom
        local df_model = e(df_m)
		local df_residual = e(df_r)

        * (v) p-value
        local t_stat = _b[1.first_child_female#1.post] / _se[1.first_child_female#1.post]
        local p_value = 2 * ttail(e(df_r), abs(`t_stat'))

        * (vi) number of observations
        local num_obs = e(N)

        * (vii) number of participants
        quietly levelsof pid if e(sample), local(participants)
        local num_participants : word count `participants'

        * (viii) interpretation of directionality
        scalar coef_d_daughter = _b[1.first_child_female#1.post]
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
	file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
