********************************************************************************
******************Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* ============================
* Setup * 
* ============================

* clear all
version 18
set maxvar 10000 

* INSTALL PACKAGES * 
ssc install outreg2

* CONFIGURE FILE PATHS * (Please do not change)
/*
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

* FILTERS * 
 
	* Keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES * 

	* generate dummy variable for whether participant has at least one daughter (1) or not (0)

gen has_daughter = 0
foreach k of varlist kidsex01-kidsex19 {
    replace has_daughter = 1 if `k' == 2 & has_daughter == 0
}

	* generate dependent variable capturing left-leaning political preference (1) or not (0)
gen ideology = .
replace ideology = 1 if inlist(plh0012_h, 1, 5, 6, 9, 16, 17, 24, 19, 26) // Left-leaning
replace ideology = 2 if inlist(plh0012_h, 2, 3, 4, 10, 11, 13, 14, 15, 20, 22, 23) // Center-Right leaning
replace ideology = 3 if inlist(plh0012_h, 7, 27, 12, 18, 21, 25, 30, 31) // Right-leaning
label define ideol 1 "Left" 2 "Center" 3 "Right"
label values ideology ideol
gen left=0 if ideology==2|ideology==3
replace left=1 if ideology==1

	* recode controls (marital status, education, employment type, income, satisfaction with life, gender, year of the survey, born in germany, East or West Germany, Migration background) to replace negative values by missing values

foreach var in pgfamstd pgpsbil pgemplst hghinc plh0182 sex syear germborn sampreg migback {
    replace `var' = . if `var' < 0
}

	* generate the number of kids

gen nb_kids= 0
foreach k of varlist kidsex01-kidsex19 {
    replace nb_kids = nb_kids + 1 if inrange(`k', 1, 2)
}

	* generate age & age squared
	
gen age=syear-gebjahr
 gen age2=c.age#c.age
 
	

	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Please add a verbal description of your model here
	
	
	*This regression model investigates whether having a daughter (has_daughter) is associated with their political ideology (left). It controls for the total number of children (nb_kid), age (age), and age squared (age2) to account for non-linear age effects. Socio-economic controls include current life satisfaction (plh0182) and household income (hghinc). The model also includes dummies for family status (pgfamstd), education level (pgpsbil), employment status (pgemplst), survey year (syear), sex (sex), federal state (sampreg), and migration background (migback). The analysis is limited to individuals older than 15 (if age > 15), and standard errors are clustered by individual ID (cluster(pid)) to account for repeated observations over time. Robust standard errors are used to correct for heteroskedasticity.

	* As an example, and for illustrative purposes we consider a linear OLS regression
	
	
	 reg left has_daughter nb_kid age age2  plh0182 hghinc i.(pgfamstd pgpsbil pgemplst syear sex sampreg migback) if age>15, cluster(pid) robu




* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

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
		scalar coef_d_daughter = _b[has_daughter]
		local interpretation "positive coefficient is in line with hypothesis"

	* Display the saved statistics
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
