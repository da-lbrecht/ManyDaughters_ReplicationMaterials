********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * 
* ============================

//clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

/*
* CONFIGURE FILE PATHS * (Please do not change)
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

**Set path here 
// cd "C:\Users\MuradZ\Dropbox\Academic\Teaching\Portsmouth\Marielle\Marielle Zahra\SOEP_many_daughters\#ManyDaughters_RT_AnalysisPackage\#ManyDaughters_RT_AnalysisPackage"

	//use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	use "$IN\ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	//merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	merge m:1 pid using "$IN\biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	//merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	merge 1:1 pid syear using "$IN\pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	//merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	//merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	merge 1:1 pid syear using "$IN\pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	//merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)
	merge m:1 hid syear using "$IN\hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	//save "$DATA/h1_dataset.dta", replace // save merged dataset
	save "$DATA\h3_dataset.dta", replace // save merged dataset
	
	clear all
	use "$DATA\h3_dataset.dta"

* ============================
* Analysis
* ============================

* FILTERS *
 
	* Example: only keep participants from private households
	//keep if inlist(pop,1,2)
keep if pld0133 == 1 & biokids > 0 & syear == 2018


* GENERATE VARIABLES * 
	
*------------------------------------------------------------
* Create HasDaughter: 1 = at least one daughter (kidsex## == 2)
*                    0 = only sons
*                    . = no children (biokids == 0)
*------------------------------------------------------------

generate byte HasDaughter = 0    // default: no daughters

* Loop through kidsex01 to kidsex10
forvalues i = 1/10 {
    local varname = string(`i', "%02.0f")    // ensures 01, 02, ..., 10
    replace HasDaughter = 1 if kidsex`varname' == 2
}

* Set missing if respondent has no children
replace HasDaughter = . if biokids == 0

*-------------------------------------------------
* Clean pgisced11: set special negatives to missing for Education control
*-------------------------------------------------

clonevar pgisced11_clean = pgisced11      // keep original intact

replace pgisced11_clean = . if inrange(pgisced11_clean, -2, -1)

* quick check
tab pgisced11 pgisced11_clean if pgisced11 < 0 | pgisced11_clean==.

* pgisced11_clean will be used as the education control variable.

*-------------------------------------------------
* Clean pglabnet: set special negative values to missing for Net Income Control
*-------------------------------------------------

clonevar pglabnet_clean = pglabnet         // optional: preserve original

replace pglabnet_clean = . if pglabnet_clean == -2

* Optional: verify cleaning
tab pglabnet if pglabnet < 0
summarize pglabnet_clean

*-------------------------------------------------
* Clean religion control variable: is_religious
*-------------------------------------------------
generate byte is_religious = .

* 1 if person identifies with any religion
replace is_religious = 1 if inlist(plh0258_h, 1, 2, 3, 4, 7)

* 0 if person is non-religious (Konfessionslos)
replace is_religious = 0 if plh0258_h == 6

*-------------------------------------------------
* MarriedStatus = 1 if married or in registered partnership
*                0 if not in formal union
*                . if missing/invalid
*-------------------------------------------------
generate byte MarriedStatus = .
replace MarriedStatus = 1 if inlist(pgfamstd, 1, 5)   // married or civil partnership
replace MarriedStatus = 0 if inlist(pgfamstd, 2, 3, 4, 6)

label define married_lab1 0 "Not in formal union" 1 "In formal union"
label values MarriedStatus married_lab1

*-------------------------------------------------------------
* Compute age of youngest child in household
*-------------------------------------------------------------

* 1. Clean invalid birth years (<1900 or > survey year)
foreach i of numlist 1/10 {
    local v = string(`i', "%02.0f")
    replace kidgeb`v' = . if kidgeb`v' < 1900 | kidgeb`v' > syear
}

* 2. Get the most recent (youngest) birth year across all kids
egen YoungestBirthYear = rowmax(kidgeb01-kidgeb10)

* 3. Calculate age of youngest child at survey date
generate YoungestAge = syear - YoungestBirthYear

* 4. If no children, set to missing
replace YoungestAge = . if missing(YoungestBirthYear)

* 5. Optional: summary check
summarize YoungestBirthYear YoungestAge


*------------------------------------------------------
* Compute respondent's age based on birth year
*------------------------------------------------------
generate Age = syear - gebjahr

* Set to missing if birth year is invalid or missing
replace Age = . if gebjahr < 1900 | gebjahr > syear

*------------------------------------------------------
* Create HasPartner: 1 = has partner, 0 = does not
*------------------------------------------------------
generate byte HasPartner = .

replace HasPartner = 1 if inrange(pgpartz, 1, 3)   // has spouse or partner (including "probably")
replace HasPartner = 0 if inlist(pgpartz, 0, 4, 5) // no partner or uncertain

label define partner_lbl 0 "No partner" 1 "Has partner"
label values HasPartner partner_lbl

** Employment status dummy **
generate byte Employed = .
replace Employed = 1 if inlist(pgemplst, 1, 2, 4)   // all working statuses
replace Employed = 0 if pgemplst == 5               // not employed

/*   Assume the original variable is pgsex
     (1 = male, 2 = female in SOEP convention)
*/

generate byte male = .              // initialize as missing
replace  male = 1 if sex == 1     // male   → 1
replace  male = 0 if sex == 2     // female → 0

label define sex01 0 "Female" 1 "Male"
label values male sex01


*---------------------------------------------------------------
* Clean pli0012_h  (hours of housework on Saturday, harmonised)
*---------------------------------------------------------------

clonevar pli0012_clean = pli0012_h          // preserve original

* 1.  Turn special codes into Stata missing .
replace pli0012_clean = . if pli0012_clean < 0   // -8, -5, -3, -2, -1 → .

* 2.  Drop implausible positive outliers (>12 h)
replace pli0012_clean = . if pli0012_clean > 12

* 3.  Quick check
tab pli0012_clean, missing
summarize pli0012_clean

*---------------------------------------------------------------
* Clean pli0016_h  (hours of housework on Sunday, harmonised)
*---------------------------------------------------------------

clonevar pli0016_clean = pli0016_h          // preserve original

* 1.  Turn special codes into Stata missing .
replace pli0016_clean = . if pli0016_clean < 0   // -8, -5, -3, -2, -1 → .

* 2.  Drop implausible positive outliers (>12 h)
replace pli0016_clean = . if pli0016_clean > 12

* 3.  Quick check
tab pli0016_clean, missing
summarize pli0016_clean

*---------------------------------------------------------------
* Clean pli0043_h  (hours of housework on working week, harmonised)
*---------------------------------------------------------------

clonevar pli0043_clean = pli0043_h          // preserve original

* 1.  Turn special codes into Stata missing .
replace pli0043_clean = . if pli0043_clean < 0   // -8, -5, -3, -2, -1 → .

* 2.  Drop implausible positive outliers (>12 h)
replace pli0043_clean = . if pli0043_clean > 12

* 3.  Quick check
tab pli0043_clean, missing
summarize pli0043_clean

*total housework for all days per week
//gen byte total_housework = pli0012_clean + pli0016_clean + pli0043_clean
egen total_housework = rowtotal(pli0012_clean pli0016_clean pli0043_clean)


*---------------------------------------------------------------
* Clean pli0019_h  (hours of childcare on saturday, harmonised)
*---------------------------------------------------------------

clonevar pli0019_clean = pli0019_h          // preserve original

* 1.  Turn special codes into Stata missing .
replace pli0019_clean = . if pli0019_clean < 0   // -8, -5, -3, -2, -1 → .

* 2.  Drop implausible positive outliers (>12 h)
replace pli0019_clean = . if pli0019_clean > 12

* 3.  Quick check
tab pli0019_clean, missing
summarize pli0019_clean

*---------------------------------------------------------------
* Clean pli0022_h  (hours of childcare on sundayy, harmonised)
*---------------------------------------------------------------

clonevar pli0022_clean = pli0022_h          // preserve original

* 1.  Turn special codes into Stata missing .
replace pli0022_clean = . if pli0022_clean < 0   // -8, -5, -3, -2, -1 → .

* 2.  Drop implausible positive outliers (>12 h)
replace pli0022_clean = . if pli0022_clean > 12

* 3.  Quick check
tab pli0022_clean, missing
summarize pli0022_clean

*---------------------------------------------------------------
* Clean pli0044_h  (hours of childcare on working week, harmonised)
*---------------------------------------------------------------

clonevar pli0044_clean = pli0044_h          // preserve original

* 1.  Turn special codes into Stata missing .
replace pli0044_clean = . if pli0044_clean < 0   // -8, -5, -3, -2, -1 → .

* 2.  Drop implausible positive outliers (>12 h)
replace pli0044_clean = . if pli0044_clean > 12

* 3.  Quick check
tab pli0044_clean, missing
summarize pli0044_clean


*total childcare for all days per week
//gen byte total_childcare = pli0019_clean + pli0022_clean + pli0044_clean
egen total_childcare = rowtotal(pli0019_clean pli0022_clean pli0044_clean)

save "$DATA\h3_dataset_controls.dta", replace


*** Creating dyadic dataset for H3 ***

clear all
use "$DATA\h3_dataset_controls.dta"
   // or your current dataset

keep if pld0133 == 1 & biokids > 0 & syear == 2018

*** Creating partner look up file ***

keep pid sex total_childcare total_housework pgisced11_clean Employed pglabnet_clean Age YoungestAge biokids
rename (pid sex total_childcare total_housework pgisced11_clean Employed pglabnet_clean Age YoungestAge biokids) ///
           (pgpartnr sex_p total_childcare_p total_housework_p pgisced11_p Employed_p pglabnet_clean_p age_p YoungestAge_p biokids_p)
save "$DATA\partner_info_h3.dta", replace


*** Back to respondent dataset to merge with partner dataset ***

clear all
use "$DATA\h3_dataset_controls.dta" // your respondent file

* Merge based on partner ID (pgpartnr = pid of partner)
merge m:1 pgpartnr using "$DATA\partner_info_h3.dta"


*** Generating binary variable for both partners employed coded as 1, 0 otherwise ***

generate both_employed = .
replace both_employed = 1 if Employed == 1 & Employed_p == 1
replace both_employed = 0 if Employed != . & Employed_p != . & (Employed != 1 | Employed_p != 1)
label define emp2 0 "At least one not employed" 1 "Both employed"
label values both_employed emp2

*** Creating household income as the sum of the two partners's incomes ***
egen total_income = rowtotal(pglabnet_clean pglabnet_clean_p)

*** Clean household income from the hgen dataset ***
replace hghinc = . if hghinc < 0

*** Generating variables for DV_H3 ***

* 1. Male hours of childcare
gen male_childcare = .
replace male_childcare = total_childcare if sex == 1
replace male_childcare = total_childcare_p if sex_p == 1

* 2. Male hours of housework
gen male_housework = .
replace male_housework = total_housework if sex == 1
replace male_housework = total_housework_p if sex_p == 1

* 3. Total childcare + housework for both partners (ignoring missing components)
egen total_joint = rowtotal(total_childcare total_childcare_p total_housework total_housework_p)

* 4. Total contribution from male
egen male_total = rowtotal(male_childcare male_housework)

* 5. Male share of total labor
gen male_share = male_total / total_joint

* 6. Inequality index: absolute deviation from equal division
gen DV_H3 = abs(0.5 - male_share)


/*************************************************************************
*  Creating One row per couple                                           *
*  Assumptions:                                                          *
*    • Each row = respondent                                             *
*    • Partner vars have _p suffix                                       *
*    • pid  = respondent's own ID                                        *
*    • pgpartnr = partner's ID                                           *
*************************************************************************/

* 1.  Create a couple identifier: smaller of the two IDs
generate long couple_id = cond(pid < pgpartnr, pid, pgpartnr)

* 2.  Sort so the "anchor" row (lower pid) comes first within each couple
sort couple_id pid

* 3.  Keep only the first row of each couple_id (drops mirror row)
sort couple_id pid              // anchor = lower pid
by couple_id: keep if _n == 1

* 4.  (Optional) confirm one row per couple
duplicates report couple_id



* HYPOTHESIS TEST FOR H3 *

	* We construct an inequality index as dependent variable and name it DV_H3. DV_H3 measures the deviation from an equal division of household labor, combining housework and childcare. First, we calculate the total number of hours spent on these tasks by both partners. We then compute the male partner's share of the total labor and define the inequality index as the absolute deviation from an equal split: DV_H3 = abs(0.5 - male_share). 
	*The independent variable is HasDaughter, a dummy variable coded as 1 if the respondent has least one daughter, 0 otherwise.
	*We use a tobit regression since dependent variable is censored between 0 and 0.5
	*an additional note is that differently from the PAP we did not have any religiousness measure in this data, hence we have omitted it from control variables.

	tobit DV_H3 HasDaughter Age age_p YoungestAge both_employed pgisced11_clean pgisced11_p biokids, ll(0) ul(0.5)




* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[HasDaughter]

		* (ii) standard error
		local se = _se[HasDaughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[HasDaughter] / _se[HasDaughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
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
	//file open myfile using "out\h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
