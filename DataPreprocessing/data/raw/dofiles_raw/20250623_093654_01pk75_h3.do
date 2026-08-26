********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht
	* altered by: Team 01pk75 ([REDACTED]/[REDACTED])
	
	
* ============================
* Setup * (Please adapt according to your needs)
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
* Following path commented out by David Albrecht on 24.06.2025
//global IN "/Users/[REDACTED]/Desktop/manydaughters/#ManyDaughters_RT_AnalysisPackage/data/raw" // folder, in which the original datasets are stored
//global DATA "/Users/[REDACTED]/Desktop/manydaughters/#ManyDaughters_RT_AnalysisPackage/data/derived" // folder for datasets
//global OUT "/Users/[REDACTED]/Desktop/manydaughters/#ManyDaughters_RT_AnalysisPackage/out" // folder for output (results, tables)
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
 
* FILTERS * (Please adapt according to your needs)
 
	* keep only participants from private households
	keep if inlist(pop,1,2)
	
	* apply age restriction: 25-65
	gen age = syear - gebjahr
	drop if age < 25 |  age > 65  
	
	* restrict to all waves since reunification
	keep if syear >=1990
	* NOTE: in ou PAP we restricted the data to the time period 1990-2022, the provided dataset, however, only includes observations for the years 2000-2020


* GENERATE VARIABLES * (Please adapt according to your needs)

* IV: DUMMY FOR HAVING AT LEAST ONE DAUGHTER: generate dummy variable for whether participant has at least one daughter (1) or not (0) in a specfic year 

gen first_daughter_year = .

 foreach num  in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
 	
	    replace first_daughter_year = kidgeb`num' if kidsex`num' == 2 & kidgeb`num' >0 &  first_daughter_year == . 
 }
 


* gen age variable of age of first daughter
gen firstdaughterage = syear - first_daughter_year

* gen dummy that indicates 
gen d_daughter = (firstdaughterage >= 1) if !missing(firstdaughterage)
replace d_daughter = 0 if missing(firstdaughterage)

* DV Housework in hours per working day harmonised
clonevar hhworkhours = pli0043_h 
recode hhworkhours -5=. -3=. -2=. -1=.


* CONTROLS
	* sex of the respondent: # NOTE we specified in PAP to use pequiv/d11102ll to derive the sex variable. because we do not have access to pequiv, we use the variable sex 
	* we generate a dummy that is 1 if female and 0 if male
	gen male =.
	recode male .=1 if sex ==1
	recode male .=0 if sex ==2
	
	* # of children in household: pequiv/d11107 # NOTE we specified in PAP to use pequiv/d11107 to derive the number of children in household. because we do not have access to pequiv, we use the variable sex 

		* calculate age of children 
foreach var of varlist  kidgeb01- kidgeb19 {
    gen kid_age_`var' = syear - `var' if `var' > 0  
}

		* there are individuals who do not have a daughter in syear under consideration but will get a daughter in the years after, we will put those respondents in the control group
foreach var of varlist  kid_age_kidgeb01- kid_age_kidgeb19 {
	replace d_daughter = 0 if `var' < 0
}

		* make children >18 to missings because likely not to live in hh anymore
foreach var of varlist  kid_age_kidgeb01- kid_age_kidgeb19 {
    replace `var' = . if `var' > 18
}

		* count how many children in each observation
egen childreninhh = rownonmiss(kid_age_kidgeb01-kid_age_kidgeb19)
	

	* Employment status of respondent: pgen/emplst // pgemplst 
clonevar emplst = pgemplst
recode emplst (-7/-1=.)



* we specify a twoway-fixed effects model using hhworkhours as a proxy for gender egalitarian hhwork as DV and an interaction of having the first daugther (time-varying) dummy and gender as IV assuming that on average, females do more household work than males as shown by past research (e.g., Nitsche & Grunow, 2016); we would therefore expect that the effect of transitioning into having a daughter on hours spent with household work is more positive for males than for females.
	
	
* xtset the data
xtset pid syear

* xtreg FE model
xtreg  hhworkhours i.d_daughter##i.male i.childreninhh i.emplst i.syear, fe cluster(pid)


* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	* As an example, and for illustrative purposes we consider a: linear OLS regression




* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.d_daughter#1.male]

		* (ii) standard error
		local se = _se[1.d_daughter#1.male]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.d_daughter#1.male] / _se[1.d_daughter#1.male]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
