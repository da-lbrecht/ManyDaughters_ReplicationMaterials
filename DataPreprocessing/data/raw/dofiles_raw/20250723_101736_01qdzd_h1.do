********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * 
* ============================

//clear all
version 19.5 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
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
	save "$DATA\h1_dataset.dta", replace // save merged dataset
	
	clear all
	use "$DATA\h1_dataset.dta"


* ============================
* Analysis
* ============================

* FILTERS * 
	
	* --- Apply inclusion / exclusion criteria --------------------------
* Keep only respondents from double-parent families (pld0133 == "Yes")
* and who report at least one biological child (biokids > 0)

* Restrict to double-parent families and non-child-free respondents
tab syear pld0133 if biokids > 0
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

/*****************************************************************
*  Gender-attitude scale (SOEP 2018, Q197)                       *
*  Items:                                                        *
*   plh0298_v2  Child <6 suffers if mother works   (1–7)         *
*   plh0302_v2  Child <3 suffers if mother works   (1–7)         *
*   plh0308_v2  Best if man & woman work equally   (1–7)         *
*****************************************************************/

* 1.  Keep only valid responses (1–7) for each item
foreach v in plh0298_v2 plh0302_v2 plh0308_v2 {
    replace `v' = . if !inrange(`v', 1, 7)   // recode -8, -1, etc. to missing
}

* 2.  Reverse-code the first two items (1↔7) so that higher = egalitarian
generate byte V1_rev = 8 - plh0298_v2   // Child <6
generate byte V2_rev = 8 - plh0302_v2   // Child <3
generate byte V3     = plh0308_v2       // Already egalitarian-oriented

* 3.  Construct the dependent variable: mean of the three items
egen DV_H1 = rowmean(V1_rev V2_rev V3)   // complete-case average

* 4.  Quick sanity check
summarize V1_rev V2_rev V3 DV_H1


*-------------------------------------------------
* Clean pgisced11: set special negatives to missing for Education control
*-------------------------------------------------

clonevar pgisced11_clean = pgisced11    

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

generate byte male = .              
replace  male = 1 if sex == 1     // male   → 1
replace  male = 0 if sex == 2     // female → 0

label define sex01 0 "Female" 1 "Male"
label values male sex01

save "$DATA\h1_dataset_controls.dta", replace

*** Creating Mini partner dataset ***
* Create mini lookup table from full dataset *
clear all
	use "$DATA\h1_dataset_controls.dta"

keep if pld0133 == 1 & biokids > 0 & syear == 2018

keep pid pgisced11_clean Employed  // this is the partner's row (must include pid)
rename (pid pgisced11_clean Employed) (pgpartnr pgisced11_p Employed_p)

* Save for merging
tempfile partnerinfo
save "$DATA\partnerinfo.dta", replace

* Back to respondent dataset to merge with partnerdataset*

clear all
	use "$DATA\h1_dataset_controls.dta" 

* Merge based on partner ID (pgpartnr = pid of partner)
merge m:1 pgpartnr using "$DATA\partnerinfo.dta"


* HYPOTHESIS TEST FOR H1 *

	* * For the dependent variable, we construct a gender attitudes index, DV_H1, to capture individual beliefs about gender roles in parenting and household responsibilities.This index is based on responses to three statements from the 2018 SOEP individual questionnaire: (1) "Children under 6 suffer if their mother works", (2) "Children under 3 suffer if their mother works", and (3) "It's best if men and women work equally so they can share family and household responsibilities equally." Respondents rated their agreement with each item on a scale from 1 ("Do not agree at all") to 7 ("Completely agree"). For interpretability, the first two items were reverse-coded so that higher values consistently reflect more gender-equal attitudes. We then compute DV_H1 as the average of the three items, resulting in a continuous index where higher scores represent more progressive views about gender equality in the family domain.
	
		*The independent variable is HasDaughter, a dummy variable coded as 1 if the respondent has least one daughter, 0 otherwise.
	*We use a tobit regression since dependent variable is censored between 1 and 7
	*an additional note is that differently from the PAP we did not have any religiousness measure in this data, hence we have omitted it from control variables.

	tobit DV_H1 HasDaughter male pglabnet_clean Age YoungestAge Employed_p pgisced11_clean pgisced11_p biokids MarriedStatus, ll(1) ul(7)

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

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
	file open myfile using "$OUT/h1_results.csv", write replace
	//file open myfile using "out\h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
