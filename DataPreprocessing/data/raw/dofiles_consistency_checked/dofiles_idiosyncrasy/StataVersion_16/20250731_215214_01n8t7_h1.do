********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000


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

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* FILTER & PREPARE DATA
* ============================

* Keep only participants from private households (adjust as needed)
keep if inlist(pop, 1, 2)



* ============================
* DEPENDENT VARIABLE: GENDER-EGALITARIAN-ATTITUDES 
* ============================

* Define base item codes
local items     298 302 308
local rev_items 298 302

* Clean negative values
foreach i of local items {
    foreach v in v1 v2 {
        replace plh0`i'_`v' = . if plh0`i'_`v' < 0
    }
}

* Reverse code items (only 298 & 302)
foreach i of local rev_items {
    gen plh0`i'_v1_rev = 5 - plh0`i'_v1 if !missing(plh0`i'_v1)
    gen plh0`i'_v2_rev = 8 - plh0`i'_v2 if !missing(plh0`i'_v2)
}

* Copy item 308 into its _rev vars (no reversal)
foreach v in v1 v2 {
    gen plh0308_`v'_rev = plh0308_`v' if !missing(plh0308_`v')
}

* Standardize
foreach i of local items {
    foreach v in v1 v2 {
        egen z_plh0`i'_`v' = std(plh0`i'_`v'_rev)
    }
}

* Combine v1 and v2 into a unified variable
foreach i of local items {
    gen z_plh0`i' = z_plh0`i'_v1
    replace z_plh0`i' = z_plh0`i'_v2 if missing(z_plh0`i'_v1)
}

* Create the final index as the row mean of all available variables
egen gender_eq_index = rowmean(z_plh0308 z_plh0298 z_plh0302)






* ============================
* INDEPENDENT VARIABLE: HAS DAUGHTER
* ============================

* Generate indicator for having any children 
gen has_child = 0
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
    replace has_child = 1 if inlist(kidsex`i', 1, 2) & kidgeb`i' <= syear
}

* Filter out cases where has_child does not align with sumkids (e.g. stillbirths or children dying later in life)
drop if has_child == 0 & sumkids > 0
drop if has_child == 1 & sumkids == 0

* Count total number of children at time of survey to replace sumkids which is time-invariant
gen num_kids = 0
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
    replace num_kids = num_kids + 1 if inlist(kidsex`i', 1, 2) & kidgeb`i' <= syear
}


* Initialize has_daughter as missing
gen d_daughter = .

* Set to 0 if person has children 
replace d_daughter = 0 if has_child == 1

* Set to 1 if any daughter is found
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
    replace d_daughter = 1 if kidsex`i' == 2 & has_child == 1 & kidgeb`i' <= syear
}

* ============================
* CLEAN CONTROL VARIABLES
* ============================

***** household size
egen hhsize = count(pid), by(hid syear)

***** region (east-west divide)
replace sampreg = . if sampreg < 0
gen eastgermany = 1 if sampreg == 2
replace eastgermany = 0 if sampreg == 1

***** Age
gen age = syear - gebjahr
label variable age "Age at interview"
replace age = . if age < 0

***** Number of children
replace num_kids = . if num_kids < 0

***** Gender
replace sex = . if sex < 0
gen male = 1 if sex == 1
replace male = 0 if sex == 2

***** Education (in years)
replace pgbilzeit = . if pgbilzeit < 0

***** Income
replace hghinc = . if hghinc < 0
replace pglabgro = 0 if pglabgro < 0 & inlist(pgstib, 10, 11, 12, 13, 15) // Set unemployed people to 0 labor income
replace pglabgro = . if pglabgro < 0


***** Family status
replace pgfamstd = . if pgfamstd < 0

***** Survey year
gen y2018 = 1 if syear == 2018
replace y2018 =0 if syear == 2012


***** exclusion criteria
drop if pgfamstd > 6
drop if d_daughter == .
drop if sex == 3

* ============================
* HYPOTHESIS TEST FOR H1
* ============================

* H1: Having a daughter increases support for gender equality

* To empirically test this hypothesis, a linear Ordinary Least Squares (OLS) regression model is employed. The dependent variable is an index of gender-egalitarian attitudes (gender_eq_index), constructed by standardizing and averaging responses to survey items related to gender equality.

* The main independent variable of interest is a binary indicator (d_daughter) representing whether respondents have at least one daughter (1) or none (0). The regression analysis controls for various demographic and socioeconomic variables, including age, gender, number of children, household size, years of education, labor income, migration background, region, survey year, and family/marriage status. Standard errors are clustered at the household level to account for within-household correlations.

* The coefficient estimate for d_daughter tests directly whether respondents with daughters demonstrate significantly higher support for gender equality compared to those without daughters.

reg gender_eq_index d_daughter age i.sex num_kids hhsize pgbilzeit pglabgro i.migback eastgermany##y2018 i.pgfamstd , cluster(hid)


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
		local p_value = 2 * ttail(e(df_r), `p_value')

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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
