********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

*clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2
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

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
* Example: only keep participants from private households
keep if inlist(pop,1,2)


* GENERATE VARIABLES * 

* Example: generate dummy variable for whether participant has at least one daughter (1) or not (0)
* Create treatment: has at least one daughter
gen has_daughter = 0
replace has_daughter = 1 if inlist(kidsex01, 2) | inlist(kidsex02, 2) | ///
                         inlist(kidsex03, 2) | inlist(kidsex04, 2) | ///
                         inlist(kidsex05, 2)

* Generate some control variables
gen male = (sex == 1)
gen age = bioage
gen income = hghinc 
gen num_kids = sumkids 
gen educ = pgbilzeit  
replace educ = . if pgbilzeit < 0
gen region = birthregion if birthregion > 0


* HYPOTHESIS TEST FOR H2 * 

*Original H2 (attitudinal):“Having daughters leads to more left-leaning political preferences”
*We intended to use the variable: bjp171 (left–right political self-placement scale) — which unfortunatelynot available
*Instead of it we use the question phl0004 - "Political attitude" where values typically range from 0 (left) to 10 (right). (0 = far left; 10 = far right)

* So our H2 is still: “Having daughters leads to more left-leaning political preferences.” (Political Proxy)
*but we use a different variables has a proxy

*We are reversing the code for phl0004 so that higher values = more left-leaning

gen political_orientation_rev = 10 - plh0004
replace political_orientation_rev = . if political_orientation_rev > 10

reg political_orientation_rev has_daughter male age educ income num_kids i.region i.syear, r



* EXTRACT AND EXPORT RESULTS * 

* Extract the required statistics

* (i) coefficient estimate
local coefficient = _b[has_daughter]

* (ii) standard error
local se = _se[has_daughter]

* (iii) family of the test statistic (e.g. z, t, F, Chi²)
local test_stat_family = "t"

* (iv) residual and model degrees of freedom (DFs) if applicable
local df_residual = e(df_r)
local df_model = e(df_m)

* (v) p-value
local p_value = _b[has_daughter] / _se[has_daughter]
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
file open myfile using "$OUT/h2_results.csv", write replace
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile


********************************************************************************

exit
