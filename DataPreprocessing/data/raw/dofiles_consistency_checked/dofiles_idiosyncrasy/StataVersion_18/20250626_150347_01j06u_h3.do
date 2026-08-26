********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (kept the same as in the example)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2, replace
ssc install reghdfe, replace


// cd "C:\Users\mkeinpre\OneDrive - WU Wien\Dokumente\ManyDaughters\Analysis_Package" Commented by David Albrecht on 27. June 2025

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

* LOAD AND MERGE DATA * (kept the same as in the example)

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

* Load the dataset again
use "$DATA/h1_dataset.dta", clear 

********************************* Sample restrictions *********************************
* FILTERS *

* Keep only the relevant variables
* Define control variables
local controls pop gebjahr kidgeb* kidsex* pid parid sex bioage corigin pgbilzeit hghinc pgfamstd sampreg ple0008 syear

* Define dependent variables
local dependents pgtatzeit

* Keep only control + dependent variables
keep `controls' `dependents'
 
* Keep only participants from private households
keep if inlist(pop,1,2)

* Keep only those in the working age population (in 2020) i.e. born after 1955
keep if gebjahr > 1955

* Keep only participants with kids
keep if kidgeb01 > 0

* Keep only participants who had their first child in the sample period (2000-2020)
keep if kidgeb01 > 1999



********************************* GENERATE VARIABLES *********************************
* --- Clean and count number of children ---
*Loop through control variables and replace negative values with missing
foreach var of varlist `controls' {
    quietly replace `var' = . if `var' < 0
}

* Loop through dependent variables and replace negative values with missing
foreach var of varlist `dependents' {
    quietly replace `var' = . if `var' < 0
}


* Create dependent variable (showing the relative differences in actual hours worked)
* Step 1: Create and save partner data with unique parid-syear
preserve
keep pid syear pgtatzeit sex
rename pid parid
rename pgtatzeit pgtatzeit_partner
rename sex sex_partner

* Ensure parid+syear uniquely identify observations
bysort parid syear: keep if _n == 1

tempfile partner_data
save `partner_data'
restore

* Step 2: Merge original data with partner data
merge m:1 parid syear using `partner_data'

* Step 3: Create difference variable
gen diff_pgtatzeit = abs(pgtatzeit - pgtatzeit_partner)/(pgtatzeit + pgtatzeit_partner) if !missing(pgtatzeit) & !missing(pgtatzeit_partner)

* Step 4: Keep only opposite-sex couples
keep if sex != sex_partner

* Step 5: Create a couple ID to uniquely identify couples and allows cluster for them
*Create a consistent numeric identifier for each couple
gen long min_id = min(pid, parid)
gen long max_id = max(pid, parid)

* Combine to create unique couple ID
egen coupleid = group(min_id max_id)

* Keep one partner per couple per year (female)
gen is_female = (sex == 2)
bysort coupleid syear (is_female): keep if _n == _N


* --- Count number of children per year ---
* Replace negative values with missing
foreach i of numlist 1/13 {
    local varname = "kidgeb" + string(`i', "%02.0f")
    replace `varname' = . if `varname' < 0
}

* Time-varying count of kids up to and including current year
gen num_kids = 0
foreach i of numlist 1/13 {
    local birthvar = "kidgeb" + string(`i', "%02.0f")
    gen kid`i'_born = (`birthvar' <= syear) & !missing(`birthvar')
    replace num_kids = num_kids + kid`i'_born
}

* Drop temporary helper vars
drop kid*_born
* --- DiD variables ---
* Generate variable showing relative time to birth
gen rel_birth = .
replace rel_birth = syear - kidgeb01

* Make it a binary pre-post variable
gen post = .
replace post = 0 if rel_birth < 0
replace post = 1 if rel_birth > -1

* --- Clean marital status ---
*clean marital status (collapse all others to not married)
replace pgfamstd = 0 if pgfamstd != 1 & !missing(pgfamstd) 

* --- Assign continent codes based on country of origin (corigin) ---
gen byte birth_continent = .

* Europe
replace birth_continent = 1 if inlist(corigin, 1, 3, 4, 5, 6, 10, 11, 14, 15, 21, 22, 26, 28, 29, 31, 32, 41, 73, 74, 75, 76, 78, 113, 118, 119, 120, 121, 122, 123, 132, 140, 143, 146, 148, 152, 165, 196)

* Asia
replace birth_continent = 2 if inlist(corigin, 2, 23, 24, 38, 39, 40, 42, 43, 50, 60, 68, 79, 81, 82, 83, 85)

* Africa
replace birth_continent = 3 if inlist(corigin, 30, 47, 48, 49, 52, 54, 67, 84, 89, 98, 102, 111, 113, 125, 166)

* North America
replace birth_continent = 4 if inlist(corigin, 18, 55, 61)

* South America
replace birth_continent = 5 if inlist(corigin, 64, 117, 133)

* Oceania (Australia, etc.)
replace birth_continent = 6 if corigin == 41

* Unknown or other countries
replace birth_continent = 7 if birth_continent == .

* Define the label "continents"
label define continents 1 "Europe" 2 "Asia" 3 "Africa" 4 "North America" 5 "South America" 6 "Oceania" 7 "Unknown"

* Assign label values using the label "continents"
label values birth_continent continents

	
****************************** TREATMENT GROUP ASSIGNMENT ******************************

* Treatment group: whether first child is female (1 = male, 2 = female)
* 0 = Son (control group), 1 = Daughter (treatment group)
gen T1 = .                            // initialize with missing
replace T1 = 0 if kidsex01 == 1       // First child male
replace T1 = 1 if kidsex01 == 2       // First child female


* Define the label for T1
label define T1_labels 0 "Son" 1 "Daughter" 

* Assign label to T1
label values T1 T1_labels

************************************** H3 ***************************************
* Run the Diff-in-Diff (the higher the difference the less equality)
reghdfe diff_pgtatzeit i.post##i.T1 num_kids i.sex c.bioage##c.bioage i.birth_continent pgbilzeit hghinc i.pgfamstd i.sampreg i.ple0008, ///
    absorb(coupleid i.syear) cluster(coupleid)

****************************** EXTRACT AND EXPORT RESULTS ******************************


	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.post#1.T1]

		* (ii) standard error
		local se = _se[1.post#1.T1]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.post#1.T1] / _se[1.post#1.T1]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_daughter = _b[1.post#1.T1]
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
