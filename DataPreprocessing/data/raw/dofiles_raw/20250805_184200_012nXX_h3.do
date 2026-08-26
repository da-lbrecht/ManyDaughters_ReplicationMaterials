********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************


* ============================
* Setup * 
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

	save "$DATA/h3_dataset.dta", replace // save merged dataset

	*generate the dependent variable measuring gender equality within households (relative time spent on chores and childcare)

	gen chores_mean = (pli0043_h*5 + pli0012_h + pli0016_h) / 7
	gen childcare_mean = (pli0044_h*5 + pli0019_h + pli0022_h) / 7

		*Keep couples
keep if !missing(parid)

		*Identify couples
gen couple_id = cond(pid < parid, pid, parid)

		*downsize the dataset
gen obs_order = _n
bysort couple_id syear sex (obs_order): keep if _n == 1
keep couple_id pid sex childcare_mean chores_mean syear

	*Reshape to compute male-female difference
	drop if sex<0
reshape wide childcare_mean chores_mean  pid, i(couple_id syear) j(sex)

	*Compute female-male differences
 gen diff_childcare = childcare_mean2 - childcare_mean1
 gen diff_chores = chores_mean2- chores_mean1
 
 save "$DATA/pid.dta", replace // save merger dataset
 
 clear
use "$DATA/h3_dataset.dta" 
gen couple_id = cond(pid < parid, pid, parid)

merge m:1 couple_id syear using  "$DATA/pid.dta"

replace diff_chores=-diff_chores
replace diff_childcare=-diff_childcare
save "$DATA/h3_dataset.dta", replace // save merged dataset
* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)


*Compute average means, e.g. mean_diff_chore is the annual average of the difference in time spent on household chores between partners within couples, calculated for each survey year (syear). It serves as a benchmark to capture the average trend for each year.
egen mean_diff_chore = mean(diff_chores), by(syear)
egen mean_diff_childcare = mean(diff_childcare), by(syear)

*The variable perf represents the individual couple's difference relative to this annual average. In other words, perf2 shows whether a given couple's gap in chore time is above or below the average observed that year.
gen perf=diff_childcare-mean_diff_childcare
gen perf2=diff_chores-mean_diff_chore

	* generate dummy variable for whether participant has at least one daughter (1) or not (0)

gen has_daughter = 0
foreach k of varlist kidsex01-kidsex19 {
    replace has_daughter = 1 if `k' == 2 & has_daughter == 0
}


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
 


* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Please add a verbal description of your model here
	
	*We estimate two linear regression models to assess whether having daughters is associated with greater gender equality in household behaviors. The dependent variables are perf2 and perf, which measure the within-couple difference in time spent on housework and childcare relatively to the annual average, with positive values indicating greater involvement of men relative to other couples. The key independent variable is has_daughter, a binary indicator equal to 1 if the household has at least one daughter. We control for the number of children (nb_kid), respondent's age and its square (age, age2), income (hghinc), satisfaction with life (plh0182), and a set of individual-level covariates including family status (pgfamstd), employment status (pgemplst), and education level (pgpsbil). Year fixed effects (syear), gender (sex), region (sampreg), and migration background (migback) are also included. Standard errors are clustered at the individual level (pid) and robust to heteroskedasticity.

	* The following models are tested
	
*reg perf2 has_daughter nb_kid age age2  plh0182 hghinc i.(pgfamstd pgpsbil pgemplst syear sex sampreg migback) if age>15 , cluster(pid) r

reg perf has_daughter nb_kid age age2  plh0182 hghinc i.(pgfamstd pgpsbil pgemplst syear sex sampreg migback) if age>15 & nb_kid>0, cluster(pid) r

*I will keep the results on childcare as illustrative of the test of the hypothesis


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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
