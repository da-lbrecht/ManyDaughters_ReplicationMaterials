********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

// cd "$PATH" // Commented by David Albrecht for reproducibility check
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

	save "$DATA/h3_dataset.dta", replace // save merged dataset

	use "$DATA/h3_dataset.dta", clear

* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)

    * Keep only relevant variables
    keep parid pld0133 hid kidgeb01-kidgeb19 kidsex01-kidsex19 sumkids cid pid syear ///
         sex gebjahr germborn sampreg partner hghinc pgbilzeit plh0012_h plj0046 ///
         pli0040 pli0012_h pli0016_h pli0019_h pli0022_h pli0043_h pli0044_h pli0046 plc0011

    * Drop if no biological kids ever
    drop if kidsex01 <= 0

    * Drop if no partner
    drop if parid == -2

    * Drop if pid missing
    drop if pid == .

    * Keep only if respondent lives with partner
    keep if pld0133 == 1


* GENERATE VARIABLES * (Please adapt according to your needs)

    * Gender
    replace sex = 0 if sex == 1
    replace sex = 1 if sex == 2

    * Relative year to first childbirth
    gen rel_year = syear - kidgeb01 if !missing(syear, kidgeb01)
    keep if rel_year >= -3 & rel_year <= 3

    * First child female dummy (treatment group)
    gen first_child_female = .
    replace first_child_female = 1 if kidsex01 == 2 & !missing(kidsex01)
    replace first_child_female = 0 if kidsex01 == 1 & !missing(kidsex01)

    * Post indicator (treatment timing)
    gen post = .
    replace post = 1 if syear >= kidgeb01 & !missing(kidgeb01)
    replace post = 0 if syear < kidgeb01 & kidgeb01 > 1900 & !missing(kidgeb01)

    * Generate household labor measures
    replace pli0040 = . if pli0040 < 0
    gen errands_workdays = pli0040 * 5

    replace pli0012_h = . if pli0012_h < 0
    replace pli0016_h = . if pli0016_h < 0
    replace pli0019_h = . if pli0019_h < 0
    replace pli0022_h = . if pli0022_h < 0

    replace pli0043_h = . if pli0043_h < 0
    gen housework_workdays = pli0043_h * 5

    replace pli0044_h = . if pli0044_h < 0
    gen childcare_workdays = pli0044_h * 5

    replace pli0046 = . if pli0046 < 0
    gen care_workdays = pli0046 * 5

    * Sum all labor across week
    gen work_alldays = errands_workdays + pli0012_h + pli0016_h + pli0019_h + ///
                       pli0022_h + housework_workdays + childcare_workdays

    * Merge in partner’s labor
    preserve
        drop parid
        rename pid parid
        rename work_alldays partner_work_alldays
        keep parid syear partner_work_alldays
        tempfile partnerdata
        save `partnerdata'
    restore

    merge m:1 parid syear using `partnerdata'
    drop if pid == .

    * Calculate share of work done by respondent (men only)
    gen total_work_alldays_partners = work_alldays + partner_work_alldays ///
        if !missing(work_alldays) & !missing(partner_work_alldays)
    
    gen share_work_alldays_partners = work_alldays / total_work_alldays_partners ///
        if total_work_alldays_partners > 0


* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

    * Hypothesis: Men with daughters take on a greater share of household labor (gender role attitudes in practice)
    * Model: DiD fixed effects regression of share of male labor (within couple) on first_child_female × post
    * Controls: Fixed effects for individual and year (absorbed), cluster by pid

    xtset pid syear

    xtreg share_work_alldays_partners i.first_child_female##i.post i.syear ///
        if sex == 0, fe vce(cluster pid)


* EXTRACT AND EXPORT RESULTS * (Please adapt to fit your reporting format)

    * Extract required statistics for interaction term (treatment effect)

        * (i) coefficient estimate
        local coefficient = _b[1.first_child_female#1.post]

        * (ii) standard error
        local se = _se[1.first_child_female#1.post]

        * (iii) family of the test statistic
        local test_stat_family = "t"

        * (iv) degrees of freedom
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
