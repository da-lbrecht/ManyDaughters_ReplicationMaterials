********************************************************************************
****************** Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* ============================
* Setup * 
* ============================

* clear all
version 17 // indicate the version of stata you are using here
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

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Generate Variables
* ============================


* FILTERS * (Please adapt according to your needs)
 
* only keep participants from private households
keep if inlist(pop,1,2)

*------------
* GENERATE CONTROLS
* compute age
gen age =  syear - gebjahr
gen age2 =  age^2

* male
gen male = 0
replace male = 1 if sex == 1


*------------
* GENERATE TREATMENT VARIABLE

* generate variable to describe if individual has a daughter and how many
gen num_daughters = 0
foreach i of numlist 1/9 {
    replace num_daughters = num_daughters + 1 if kidsex0`i' == 2 // https://paneldata.org/soep-core/datasets/biobirth/kidsex01
}
foreach i of numlist 0/9 {
    replace num_daughters = num_daughters + 1 if kidsex1`i' == 2
}

* generate variable to describe if individual has a son and how many
gen num_sons = 0
foreach i of numlist 1/9 {
    replace num_sons = num_sons + 1 if kidsex0`i' == 1
}
foreach i of numlist 0/9 {
    replace num_sons = num_sons + 1 if kidsex1`i' == 1
}

* generate variables
gen num_children = num_sons + num_daughters
gen d_daughter = 0
replace d_daughter = 1 if num_daughters > 0 & num_daughters != . 
gen d_son = 0
replace d_son = 1 if num_sons > 0 & num_sons != .

* Quality Check: the two variables almost perfectly align. For consistency, we stick to our own variable
tab sumkids num_children // https://paneldata.org/soep-core/datasets/biobirth/

* interaction variable
gen male_x_num_daughters = num_daughters * male

*------------
* GENERATE OUTCOME VARIABLE

* WE COULD BUILD AN INDEX. BASED ON FOUR VARIABLES ON ATTITUDES TOWARD GENDER EQUALITY

* re-scale variables scaled from 0 (more gender equality) to 4 (less gender equality)
gen gender_attitude1 = (plh0308_v1 + plh0303) - 2 // substract 3 to start values at 1
replace gender_attitude1 = . if plh0308_v1 < 0 | plh0303 < 0 // remove missing data
replace gender_attitude1 = gender_attitude1 / 6 // normalize to 0 to 1
replace gender_attitude1 = 1- gender_attitude1 // rescale such that a value of 1 means higher gender equality attitudes
 
* re-scale variables scaled from 0 (less gender equality) to 4 (more gender equality)
gen gender_attitude2 = (plh0301 + plh0307) - 2 // substract 2 to start values at 0
replace gender_attitude2 = . if plh0301 < 0 | plh0307 < 0 // remove missing data
replace gender_attitude2 = gender_attitude2 / 6 // normalize to 0 to 1
 
* merge the variables:
gen gender_attitude = 1/2 * gender_attitude1 + 1/2 * gender_attitude2 // create one index variable of gender attitudes
// gender attitudes are scaled from 0 to 1 with 1 meaning more gender equality supporting attitudes




	
* ============================
* Analysis
* ============================


* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)

	* here, we regress the index of gender attitudes on an interaction between having daughters and 
	* the respondent being male in addition to the two separate variables and controls. We do so in an 
	* OLS regression, given that the data are only available for one cross-section. A positive value of the 
	* interaction term suggests that men more strongly develop pro-equality gender attitudes than women
	* when having daughters.

	reg gender_attitude male_x_num_daughters num_daughters male age age2 i.num_children if num_children > 0, vce(robust)

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[male_x_num_daughters]

		* (ii) standard error
		local se = _se[male_x_num_daughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[male_x_num_daughters] / _se[male_x_num_daughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_male_x_num_daughters = _b[male_x_num_daughters]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile




********************************************************************************

exit
