********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

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

	* generate dependent variables capturing attitude towards gender equality (plh series: attitudes towards gender / pjl series: attitudes towards women in society)

	
foreach var in plh0395i01 plh0395i02 plh0395i03 plh0395i04 plh0395i05 plh0395i06 {
    replace `var' = . if `var' < 0
}

egen gender_eq_mean1 = rowmean(plh0395i01 plh0395i02 plh0395i03 plh0395i04 plh0395i05 plh0395i06)


foreach var in plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 {
    replace `var' = . if `var' < 0
}

*three questions have reversed scales (e.g. "totally agree" means having negative attitudes towards gender equality)

foreach var in plj0623 plj0624 plj0625 {
    replace `var' = 8 - `var'
}

egen gender_eq_mean2 = rowmean(plj0620 plj0621 plj0622 plj0623 plj0624 plj0625)


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
 
	


* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	*I use an OLS regression model to examine how having at least one daughter (has_daughter) relates to the average gender equality attitude score (gender_eq_mean1, gender_eq_mean2), controlling for the total number of children (nb_kid), age and its squared term (age, age2), current life satisfaction (plh0182), household income (hghinc), family standard classification (pgfamstd), educational attainment (pgpsbil), employment status (pgemplst), gender (sex), sampling region (sampreg), and migration background (migback). The analysis includes only respondents aged over 15 and uses robust standard errors. The model aims to isolate the association between having daughters and progressive gender equality attitudes, while adjusting for demographic and socioeconomic factors.
	
reg gender_eq_mean1 has_daughter nb_kid age age2  plh0182 hghinc i.(pgfamstd pgpsbil pgemplst syear sex sampreg migback) if age>15, r

*reg gender_eq_mean2 has_daughter nb_kid age age2  plh0182 hghinc i.(pgfamstd pgpsbil pgemplst syear sex sampreg migback) if age>15, r


*I will keep the results from the first equation because the panel is larger


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
