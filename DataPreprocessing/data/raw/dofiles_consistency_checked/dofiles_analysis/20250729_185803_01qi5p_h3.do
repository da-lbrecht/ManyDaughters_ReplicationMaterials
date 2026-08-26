********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

//clear all
* version 19 SE // indicate the version of stata you are using here *
 set maxvar 10000 // extend the maximum number of variables to 10,000 * 

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2
ssc install distinct

/*
* CONFIGURE FILE PATHS * (Please do not change)
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "C:/Users/ondra/Dropbox/Dokumenty/Věda/[REDACTED]/Many Daughters/29.5.2025 DATA SOEP UPDATE/#ManyDaughters_RT_AnalysisPackage"
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

*	use  "C:\Users\ondra\Dropbox\Dokumenty\Věda\[REDACTED]\Many Daughters\29.5.2025 DATA SOEP UPDATE\#ManyDaughters_RT_AnalysisPackage\data\derived\h3_dataset.dta" *


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
 * N/A -> would limit already low number of observations. *


* GENERATE VARIABLES * (Please adapt according to your needs)

* Number of Children *
* Generate variable counting all children with sex 1 or 2
	* Create a variable that is 1 if male or female, else 0, for each child
foreach var of varlist kidsex01-kidsex19 {
    gen temp_`var' = inlist(`var', 1, 2)
}

* Sum across all children to get the count
egen child_count = rowtotal(temp_kidsex*)

* Clean up temporary variables
drop temp_kidsex*
tab child_count
* If all kidsex variables are missing or not 1/2, child_count will be 0

 label variable child_count "Number of Children"
tab2 plb0085 child_count

* Having at least one Female Child * 

* Generate a variable indicating if at least one child is female (value 2) among kidsex01 to kidsex19
* Initialize the indicator variable as 0
gen at_least_one_female = 0

* Set to 1 if any kidsex variable is 2 (female)
foreach var of varlist kidsex01-kidsex19 {
    replace at_least_one_female = 1 if `var' == 2 & at_least_one_female == 0
}


label variable at_least_one_female "Having at least one Female Child"

tab2 kidsex01 at_least_one_female if !missing(plb0085) 
tab2 kidsex02 at_least_one_female if !missing(plb0085) 
tab2 kidsex03 at_least_one_female if !missing(plb0085) 
tab2 kidsex04 at_least_one_female if !missing(plb0085) 
tab2 kidsex05 at_least_one_female if !missing(plb0085) 
tab2 kidsex06 at_least_one_female if !missing(plb0085) 
tab2 kidsex07 at_least_one_female if !missing(plb0085) 
tab2 kidsex08 at_least_one_female if !missing(plb0085) 
tab2 kidsex09 at_least_one_female if !missing(plb0085) 
tab2 kidsex10 at_least_one_female if !missing(plb0085) 
tab2 kidsex11 at_least_one_female if !missing(plb0085) 
tab2 kidsex12 at_least_one_female if !missing(plb0085) 
tab2 kidsex13 at_least_one_female if !missing(plb0085) 
tab2 kidsex14 at_least_one_female if !missing(plb0085) 
tab2 kidsex15 at_least_one_female if !missing(plb0085) 
tab2 kidsex16 at_least_one_female if !missing(plb0085) 
tab2 kidsex17 at_least_one_female if !missing(plb0085) 
tab2 kidsex18 at_least_one_female if !missing(plb0085) 
tab2 kidsex19 at_least_one_female if !missing(plb0085) 

* Respondent gender *
tab sex
tab sex, gen(sex)
tab sex2
label variable sex2 "Female"

* VARIABLE FEMALE - RENAME *
gen d_daughter = .
replace d_daughter = at_least_one_female
label variable d_daughter "Having at least one Female Child"


* Respondent Years of Education *
tab pgbilzeit
gen education_years = .
replace education_years = pgbilzeit
replace education_years = . if inlist(pgbilzeit, -1, -2)
tab education_years
label variable education_years "Years of Education"

* Household Income *
tab hghinc if hghinc < 0
tabstat hghinc, stat(min sd max n)
gen household_income = .
replace household_income = hghinc
replace household_income = . if inlist(hghinc, -1, -2, -3)
label variable education_years "Household Income (EUR)"
tabstat household_income, stat(min sd max n)

* Respondent age *
tab bioage
gen age = .
replace age = bioage
replace age = . if inlist(bioage, -1, -2, -3)
label variable age "Respondent Age"
tabstat age, stat(min sd max n)

* Marital Status *
tab pgfamstd
replace pgfamstd = . if inlist(pgfamstd, -1, -2, -3, -4, -5, -6, -7, -8)
tab pgfamstd , gen(pgfamstd)

tab at_least_one_female if !missing(plb0085)
* For our sample, the variable is clean, i.e., there are no missing values *
* Nationality *
* Remove missings, add a set of variables for the nationality of the respondent, controlling for the national and cultural values *
tab pgnation
replace pgnation = . if pgnation == -1 
replace pgnation = . if pgnation == -2 
replace pgnation = . if pgnation == -3 
replace pgnation = . if pgnation == -4 
replace pgnation = . if pgnation == -5 
replace pgnation = . if pgnation == -6
replace pgnation = . if pgnation == -7 
replace pgnation = . if pgnation == -8 

tab pgnation , gen(pgnation)

/* Commented out by DA during consistency check
tabstat parid, stat (min max mean)
tab parid if parid < 0


tabstat pid, stat (min max mean)
tab pid if parid < 0
tab parid if parid < 0 | missing(parid)
*/


* Missing data in PARID *
replace parid = . if inlist(parid, -1, -2, -3, -4, -5, -6, -7, -8)

* ---------------------------
* Clean and prepare respondent data
* ---------------------------

* Keep only valid partner IDs (positive values only) in respondent dataset
keep if parid > 0 & !missing(parid)

* Clean respondent housework variable (pli0043_v3)
replace pli0043_v3 = . if inlist(pli0043_v3, -1, -2, -3, -4, -5, -6, -7, -8)

* Clean respondent childcare variable (pli0044_v3)
replace pli0044_v3 = . if inlist(pli0044_v3, -1, -2, -3, -4, -5, -6, -7, -8)

* ---------------------------
* Prepare partner data 
* ---------------------------

preserve

* Keep only identifier and housework/childcare variables + year for deduplication
keep pid pli0043_v3 pli0044_v3 syear

* Rename pid to parid for merging
rename pid parid

* Clean partner housework variable
replace pli0043_v3 = . if inlist(pli0043_v3, -1, -2, -3, -4, -5, -6, -7, -8)

* Clean partner childcare variable
replace pli0044_v3 = . if inlist(pli0044_v3, -1, -2, -3, -4, -5, -6, -7, -8)

* Sort by parid and descending year to keep most recent record per partner
gsort parid -syear

* Keep only the first occurrence (most recent) per partner
bysort parid: keep if _n == 1

* Rename cleaned partner variables for clarity
rename pli0043_v3 partner_hw
rename pli0044_v3 partner_cc

* Save cleaned partner dataset to tempfile
tempfile partnerdata
save `partnerdata', replace

restore

* ---------------------------
* Merge partner data back to respondent data
* ---------------------------

merge m:1 parid using `partnerdata'

* Check merge result for correctness
tab _merge

* ---------------------------
* Handle missing values before calculation
* ---------------------------

* Replace missing housework/childcare with zero if justified (assume missing = 0 hours)
replace pli0043_v3 = 0 if missing(pli0043_v3)
replace partner_hw = 0 if missing(partner_hw)

replace pli0044_v3 = 0 if missing(pli0044_v3)
replace partner_cc = 0 if missing(partner_cc)

* ---------------------------
* Compute total couple hours and shares
* ---------------------------

gen total_couple_housework = pli0043_v3 + partner_hw
gen total_couple_childcare = pli0044_v3 + partner_cc
gen total_couple_domestic = total_couple_housework + total_couple_childcare

gen share_housework = .
replace share_housework = pli0043_v3 / total_couple_housework if total_couple_housework > 0

gen share_childcare = .
replace share_childcare = pli0044_v3 / total_couple_childcare if total_couple_childcare > 0

gen share_total_domestic = .
replace share_total_domestic = (pli0043_v3 + pli0044_v3) / total_couple_domestic if total_couple_domestic > 0

* ---------------------------
* Summarize results
* ---------------------------

summarize share_housework share_childcare share_total_domestic

* Label the main share variable
label variable share_total_domestic "Domestic Work Share"

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Calculate mean of share_total_domestic for the analysis sample
	su share_total_domestic
	local mean_share_total_domestic = r(mean)
	di "Mean of share_total_domestic: `mean_share_total_domestic'"

	* As seen from the described variables, our model aims to test whether the household home work,
	* approximated as the proportion of hours spent on childcare and homework, is more equally distributed 
	* in the families with daughters. The distribution of the work at the household is constructed for each of the partners, 
	* and then the percentage of the total domestic work is calculated, serving as a dependent variable. 
	* The daughter variable is then being used as the key independent variable. 
	* Given the nature of the dependent variable, we can this time opt for simple OLS regression with household and family controls. *

	
	reg share_total_domestic d_daughter sex2 age education_years household_income i.pgnation i.pgfamstd i.syear


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
local coefficient = _b[d_daughter]

* (ii) standard error
local se = _se[d_daughter]

* (iii) test statistic family
local test_stat_family = "t"  // Assuming t-tests are appropriate

* (iv) degrees of freedom
local df_residual = e(df_r)   // Residual DF
local df_model = e(df_m)      // Model DF

* (v) p-value (corrected calculation)
local t_stat = _b[d_daughter] / _se[d_daughter]  // Calculate t-statistic first
local p_value = 2 * ttail(e(df_r), abs(`t_stat'))  // Correct two-tailed p-value

* (vi) number of observations
local num_obs = e(N)

* (vii) number of participants (improved method) --> Did not work for us, used distinct pid instead *
* qui tab pid if e(sample), nofreq  // More reliable than levelsof
* local num_participants = r(r)

distinct pid if e(sample)
local num_participants = r(ndistinct)


* (viii) interpretation of directionality // Adapted by DA during consistency check
//local interpretation "not linearly interpretable"
* Set interpretation based on mean
if `mean_share_total_domestic' > 0.5 {
local interpretation "negative coefficient is in line with hypothesis"
}
if `mean_share_total_domestic' == 0.5 {
local interpretation "not linearly interpretable"
}
else {
local interpretation "positive coefficient is in line with hypothesis"
}

// Display statistics
di "Coefficient: `coefficient'"
di "Standard Error: `se'"
di "Test Statistic Family: `test_stat_family'"
di "Residual DF: `df_residual'"
di "Model DF: `df_model'"
di "p-value: `p_value'"
di "Observations: `num_obs'"
di "Participants: `num_participants'"
di "Directionality: `interpretation'"

* Export to CSV (with error handling)
capture file close myfile  // Close any open file handles
file open myfile using "$OUT/h3_results.csv", write replace
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile ("`coefficient'") "," ("`se'") "," ("`test_stat_family'") "," ///
    ("`df_residual'") "," ("`df_model'") "," ("`p_value'") "," ///
    ("`num_obs'") "," ("`num_participants'") "," ("`interpretation'") _n
file close myfile

********************************************************************************

exit
