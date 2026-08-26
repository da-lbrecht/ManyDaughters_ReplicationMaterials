********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
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

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
*** filter for years 2001, 2006, 2011, 2016, 2005, 2009, 2014, 2019
keep if syear==2001|syear==2005|syear==2006|syear==2009|syear==2011|syear==2014|syear==2016|syear==2019


* GENERATE VARIABLES * (Please adapt according to your needs)

*** AS SPECIFIED IN THE PAP: Match treatment variables from years 2001, 2006, 2011, 2016 (pld0026) with the outcomes measured in 2005, 2009, 2014, 2019
*** Note that the data generation of the treatment variable (d_daughter) and the recoding of marital status (pgfamstd -> married) happens within each of the following code chunks.
*** Note: We use sumkids and daughter instead of pld0025 and bgp15307a (which we mentioned in our PAP), since these variables did not contain the desired information for all necessary years
*** Recode marital status: coded as one if married (1); married, but separated (2); husband/wife abroad (6); civil union living together or apart (7 and 8); coded as zero if single(3); divorced(4); widowed(5)

*** SOMEWHAT MEANINGFUL CHANGE TO WHAT WE SPECIFIED IN THE PAP: DUE TO A MISUNDERSTANDING OF THE AVAILABILITY OF THE 
*** GENDER OF CHILDREN VARIABLES, WE CAME UP WITH THE TREATMENT /LAGGED OUTCOMES DESIGN DESCRIBED ABOVE
*** WE NOW SEE THAT WE CAN ACTUALLY USE THE INFORMATION ON THE TREATMENT VARIABLE FROM THE SAME YEAR AS FOR THE OUTCOME VARIABLES
*** WE HAVE KEPT THE CODE FOR THE PAP BELOW IN COMMENTS, BUT USE THE SLIGHTLY ADAPTED CODE WITHOUT THE LAGGED OUTCOMES FOR THE ANALYSIS
*** BECAUSE IT AVOIDS POTENTIAL ISSUES DUE TO THE LAG BETWEEN TREATMENT AND OUTCOME MEASUREMENT THAT COULD BIAS OUR ESTIMATES

*preserve
*	keep if syear==2001
*	keep if sumkids==1
*	gen d_daughter=1 if kidsex01==2
*	replace d_daughter = 0 if d_daughter==.
*	count
*	keep if pgfamstd >=0
*	gen married = 0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
*	replace married = 1 if married==.
*	keep pid syear d_daughter gebjahr sex pgisced97 pgemplst married
*	tempfile t_2001
*	save `t_2001'
*restore

*preserve
*	keep if syear==2006
*	keep if sumkids==1
*	gen d_daughter=1 if kidsex01==2
*	replace d_daughter = 0 if d_daughter==.
*	count
*	keep if pgfamstd >=0
*	gen married = 0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
*	replace married = 1 if married==.
*	keep pid syear d_daughter gebjahr sex pgisced97 pgemplst married
*	tempfile t_2006
*	save `t_2006'
*restore

*preserve
*	keep if syear==2011
*	keep if sumkids==1
*	gen d_daughter=1 if kidsex01==2
*	replace d_daughter = 0 if d_daughter==.
*	count
*	keep if pgfamstd >=0
*	gen married = 0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
*	replace married = 1 if married==.
*	keep pid syear d_daughter gebjahr sex pgisced97 pgemplst married
*	tempfile t_2011
*	save `t_2011'
*restore

*preserve
*	keep if syear==2016
*	keep if sumkids==1
*	gen d_daughter=1 if kidsex01==2
*	replace d_daughter = 0 if d_daughter==.
*	count
*	keep if pgfamstd >=0
*	gen married = 0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
*	replace married = 1 if married==.
*	keep pid syear d_daughter gebjahr sex pgisced97 pgemplst married
*	tempfile t_2016
*	save `t_2016'
*restore
*

preserve
	keep if syear==2005
	keep if sumkids==1
	gen d_daughter=1 if kidsex01==2
	replace d_daughter = 0 if d_daughter==.
	count
	keep if pgfamstd >=0
	gen married = 0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
	replace married = 1 if married==.
	keep pid syear d_daughter gebjahr sex pgisced97 pgemplst married
	tempfile t_2001
	save `t_2001'
restore

preserve
	keep if syear==2009
	keep if sumkids==1
	gen d_daughter=1 if kidsex01==2
	replace d_daughter = 0 if d_daughter==.
	count
	keep if pgfamstd >=0
	gen married = 0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
	replace married = 1 if married==.
	keep pid syear d_daughter gebjahr sex pgisced97 pgemplst married
	tempfile t_2006
	save `t_2006'
restore

preserve
	keep if syear==2014
	keep if sumkids==1
	gen d_daughter=1 if kidsex01==2
	replace d_daughter = 0 if d_daughter==.
	count
	keep if pgfamstd >=0
	gen married = 0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
	replace married = 1 if married==.
	keep pid syear d_daughter gebjahr sex pgisced97 pgemplst married
	tempfile t_2011
	save `t_2011'
restore

preserve
	keep if syear==2019
	keep if sumkids==1
	gen d_daughter=1 if kidsex01==2
	replace d_daughter = 0 if d_daughter==.
	count
	keep if pgfamstd >=0
	gen married = 0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
	replace married = 1 if married==.
	keep pid syear d_daughter gebjahr sex pgisced97 pgemplst married
	tempfile t_2016
	save `t_2016'
restore



*** same for outcomes
preserve
	keep if syear==2005
	keep if sumkids==1
	keep if plh0004>=0
	gen pol_pref = 10 - plh0004
	count
	keep pid syear pol_pref 
	tempfile o_2005
	save `o_2005'
restore

preserve
	keep if syear==2009
	keep if sumkids==1
	keep if plh0004>=0
	gen pol_pref = 10 - plh0004
	count
	keep pid syear pol_pref 
	tempfile o_2009
	save `o_2009'
restore

preserve
	keep if syear==2014
	keep if sumkids==1
	keep if plh0004>=0
	gen pol_pref = 10 - plh0004
	count
	keep pid syear pol_pref 
	tempfile o_2014
	save `o_2014'
restore

preserve
	keep if syear==2019
	keep if sumkids==1
	keep if plh0004>=0
	gen pol_pref = 10 - plh0004
	count
	keep pid syear pol_pref 
	tempfile o_2019
	save `o_2019'
restore


use `t_2001', clear 
merge 1:1 pid using `o_2005'
keep if _merge==3
drop _merge
keep if pol_pref!=. & d_daughter!=. & married!=. & sex!=. & gebjahr !=. & pgemplst !=. & pgisced97 !=. & pgisced97 >=0
tempfile 2001_2005
save `2001_2005'

use `t_2006', clear 
merge 1:1 pid using `o_2009'
keep if _merge==3
drop _merge
keep if pol_pref!=. & d_daughter!=. & married!=. & sex!=. & gebjahr !=. & pgemplst !=. & pgisced97 !=. & pgisced97 >=0
tempfile 2006_2009
save `2006_2009'

use `t_2011', clear 
merge 1:1 pid using `o_2014'
keep if _merge==3
drop _merge
keep if pol_pref!=. & d_daughter!=. & married!=. & sex!=. & gebjahr !=. & pgemplst !=. & pgisced97 !=. & pgisced97 >=0
tempfile 2011_2014
save `2011_2014'

use `t_2016', clear 
merge 1:1 pid using `o_2019'
keep if _merge==3
drop _merge
keep if pol_pref!=. & d_daughter!=. & married!=. & sex!=. & gebjahr !=. & pgemplst !=. & pgisced97 !=. & pgisced97 >=0
tempfile 2016_2019
save `2016_2019'


use `2001_2005', clear
append using `2006_2009'
append using `2011_2014'
append using `2016_2019'


*** Remove respondents with missings
keep if pol_pref!=. & pol_pref>=0 & d_daughter!=. & d_daughter>=0 & married!=. & married>=0 & sex!=. & sex>=0 & gebjahr !=. & gebjahr >=0 & pgemplst !=. & pgemplst >=0 & pgisced97 !=. & pgisced97 >=0
	
	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* We run a linear OLS regression with clustered standard errors, with several control variables (year of birth, respondent's, education, employment status, and marital status). Categorical control variables are included with dummy variables.

reg pol_pref d_daughter gebjahr i.sex i.pgisced97 i.pgemplst i.married i.syear, vce(cluster pid) 


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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
