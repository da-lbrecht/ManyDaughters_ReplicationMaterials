********************************************************************************
****************** Do-File H4: #ManyDaughters Project **************************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* RT: [REDACTED], [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
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

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
use "$DATA/h4_dataset.dta", clear
keep sumkids kidsex01 plh0308_v1 plh0308_v2 pid syear sex

* FILTERS * 
 
	* Keep participants with exactly 1 kid
	keep if sumkids == 1

* GENERATE VARIABLES 

	* We  measure support for gender equality by a single item availlable in two waves 
	* "men and women should work the same amount so they can share responsibillities": plh0308_v1, plh0308_v2
	* However, while writing the PAP we assumed that both variables are on the same scale
	* Since both variables are on different scales in the two waves we standardize them
		mvdecode plh0308_v1 plh0308_v2, mv(-9/0)
		tab1 plh0308_v1 plh0308_v2
	
			egen egal = std(plh0308_v1*-1) // variable in 2012 wave is reverse coded
			egen egal2 = std(plh0308_v2)
			replace egal = egal2 if egal == . 
				tab egal plh0308_v1, nol
				tab egal plh0308_v2

	* Generating the dependent variable
	* 	As explained in our PAP we consider the treatment and the control to be "having one daughter" and "having one son" respectively. 
	* 	This ensures that the treatment is randomly assgined and a causal interpretation is warranted.
	tab kidsex01
		gen daughter = (kidsex01==2) if kidsex01 >= 0
		lab def daughter 1 "daughter" 0 "son"
		lab val daughter daughter
		tab daughter

	
* HYPOTHESIS TEST FOR H4 * 

	* Please add a verbal description of your model here

	*	Our operationalization ensures ranodmization of the treatment and hence warrants a causal interpretation. 
	*	Thus, we do not employ further controls.
	*	The only exception are survey waves to control for general trends in gender equality and trends in birth rates. 
	*	We use a random effects model with observations clustered at the individual level. We xtset our data. We do not use an OLS since a RE model is more efficient. 
	
	* The hypothesis is tested by specifying an interaction between daughter and sex.
		
	xtset pid syear
	* H4
	xtreg egal 1.daughter##1.sex i.syear, cluster(pid) re
	* Set son and female as reference categories to make sure the extracted coefficient reflects formulation of the hypothesis
	* I.e., positive interaction coefficiet = greater effect of daughters for men
	matrix list r(table) // needed to extract p-value - original code did not work for me
		ereturn list // "Residual Degrees of Freedom" (e(df_r)) are not available after xtreg, re 

	
	
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = r(table)[1,3] // adjusted code

		* (ii) standard error
		local se = r(table)[2,3] // adjusted code

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z" // xtreg, re uses z test statistic

	
	* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r) // see line 106
		local df_model = e(df_m)

		* (v) p-value
		local p_value = r(table)[4,3] // adjusted code

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_daughter = _b[1.daughter#1.sex] 
		local interpretation "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'" // see line 106
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
