********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2

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


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)

*** NOTE! In our PAP, we restricted the analysis of H3 to the years mentioned below, since our reading of the SOEP data documentation suggested that these would be the only years for which information about the chidrens' gender would be available. However, in the actual data, these information are in principle avaiable in all years. Thus, we decided to run the analysis based on all years for which the information are available. If desired, you can un-comment the following line of code to restrict the analyses to those years that were indicated in the PAP as supposedly being the only years in which these information were available.
*keep if syear==2001|syear==2006|syear==2011|syear==2016


*** Only include married people living together (married, i.e., exclude married, but separated (2), singles, divorced, widowed, partner living abroad). Other than indicated in the PAP, individuals in civil union are not counted here because, as we explained in the PAP, we only include partners of different genders (which contradicts including individuals in civil union)
keep if pgfamstd >=0
gen live_together = 1 if pgfamstd==1
replace live_together = 0 if live_together==.
keep if live_together == 1


* GENERATE VARIABLES * (Please adapt according to your needs)

*** Generate treatment variable: Has exactly one child, and that child is female
*** Note: We use sumkids and daughter instead of pld0025 and bgp15307a (which we mentioned in our PAP), since these variables did not contain the desired information for all necessary years
keep if sumkids==1
gen d_daughter=1 if kidsex01==2
replace d_daughter = 0 if d_daughter==.

*** Generate numbers of hours worked on the individual level:
tab pli0043_h
tab pli0044_h
tab pli0049_h

keep if pli0043_h >=0 & pli0043_h !=.
keep if pli0044_h >=0 & pli0044_h !=.
keep if pli0049_h >=0 & pli0049_h !=.

gen total_hours = pli0043_h+pli0044_h+pli0049_h
keep pid parid cid hid sex syear d_daughter live_together total_hours pli0043_h pli0044_h pli0049_h

keep if parid!=-2

*** Match partners using parid and pid, and create new household-level data set:
preserve
	keep if sex==1
	count
	rename total_hours total_hours_male 
	rename sex sex_male
	replace parid = pid
	keep pid parid cid hid  sex_male syear d_daughter live_together total_hours_male
	tempfile males
	save `males'
restore

preserve
	keep if sex==2
	count
	keep pid parid cid hid  sex syear d_daughter live_together total_hours
	tempfile females
	save `females'
restore

use `males', clear 

merge m:m parid syear using `females'

keep if _merge==3

*** Generate outcome variable on the household level: the absolute value of difference between individual cumulated hours worked in household (childcare, chores, repair work) for partners of different genders in the same household
gen hrs_diff = total_hours_male-total_hours
gen hrs_diff_sq=hrs_diff*hrs_diff
gen hrs_diff_sq_sqrt = sqrt(hrs_diff_sq)


*** Remove respondents with missings
keep if hrs_diff_sq_sqrt!=. & hrs_diff_sq_sqrt>=0 & d_daughter!=. & d_daughter>=0


* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* We run a linear OLS regression with clustered standard errors at the household level.
	* Due to our data prep, pid identifies a household dyad now.

reg hrs_diff_sq_sqrt d_daughter, vce(cluster pid)

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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
