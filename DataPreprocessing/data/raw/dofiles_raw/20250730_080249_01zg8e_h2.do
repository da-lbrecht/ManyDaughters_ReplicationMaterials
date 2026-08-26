					/* ManyDaughters master file  */
********************************************************************************

* Authors: [REDACTED]


*************** SETUP ***************

* clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

/*
* Configure file paths * (Please do not change)
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

********************************************************************************
				/* Create dataset for testing H1-H4  */
********************************************************************************



* Load and merge data * 

use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

keep pid hid syear parid partner sex gebjahr

* Merge ppathl with other datasets.

* biobirth contains information on fertility history
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) ///
	keepusing(sumkids kidgeb* kidsex*)
	
/* drop those who don't have children (sumkids and all variables related to 
	children seem to be updated at the end of the sample periods, 
	so sumkids are kids until 2020) */
drop if sumkids == 0
* drop those for whom we don't know the gender of the first child
drop if inlist(kidsex01, -2, -1)
		
* pl contains all variables from the individual questionnaire 
* pli* is time use for H3
* plj* and plh* are gender attitudes + some plh* are topics of disagreement 
	* with partner for H1 and H4
* plh0004 is political orientation, plh0012_h is party preference, 
	* plh0013_h is preference intensity for H2
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
	keepusing(pli0010 pli0011 pli0012_h pli0016_h pli0019_h pli0022_h pli0024_h ///
	pli0028_h pli0031_h pli0036 pli0038_h pli0040 pli0043_h pli0044_h ///
	pli0046 pli0049_h pli0051 pli0052 pli0055 pli0057 pli0059 pli0060 ///
	pli0159 pli0162 pli0163 pli0164 pli0178 pli0175 pli0176 pli0177 ///
	plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 plh0298_v1 plh0298_v2 ///
	plh0301 plh0302_v1 plh0302_v2 plh0303 plh0304 plh0308_v1 plh0308_v2 ///
	plh0309 plh0307 plh0322 plh0323 plh0324 plh0329 plh0004 plh0012_h plh0013_h)
	
* hgen contains generated variables on household level from the household interview; use for household net income
merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) ///
	keepusing(hid syear hghinc)

	
label language EN // change language of the labels to English

compress
save "$DATA/hypotheses-dataset.dta", replace // save merged dataset

********************************************************************************
				/* Create and recode variables for H1-H4  */
********************************************************************************


* Generate age
gen age = syear - gebjahr

** First child is a girl

gen first_daughter = kidsex01 == 2 if inlist(kidsex01, 1, 2)
rename kidgeb01 first_byear
	


** H2: left-leaning political preferences **
gen polattitude = plh0004
replace polattitude = . if inlist(plh0004, -8, -5, -1)

gen sample_h2 = inlist(first_byear, 2004, 2008, 2013, 2018)

preserve
keep if sample_h2 == 1

replace polattitude = . if syear != first_byear + 1

save "$DATA/h2_dataset.dta", replace


********************************************************************************
 /*  H2: Having daughters leads to more left-leaning political preferences.  */
********************************************************************************

* Authors: [REDACTED]

use "$DATA/h2_dataset.dta", clear


* ============================
* Analysis
* ============================


* HYPOTHESIS TEST FOR H2 *

	/* We regress political attitudes (0: entirely left, 10: entirely right, 
	measured in the year after the first child was born) on the first child 
	being a girl, age of the parent and survey year.  We cluster the standard 
	errors on the household level. */

	reg polattitude i.first_daughter age i.syear, cluster(hid)


* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.first_daughter]

		* (ii) standard error
		local se = _se[1.first_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.first_daughter] / _se[1.first_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_first_daughter = _b[1.first_daughter]
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
