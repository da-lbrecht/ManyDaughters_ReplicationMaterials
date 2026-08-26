********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
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
 
	* Only keep household with children
keep if sumkids >0
keep if parid>0

* GENERATE VARIABLES * (Please adapt according to your needs)

*generate leisure variable
gen hhours= pli0051 if pli0051>=0&pli0051<.
gen phours= pli0162 if pli0162>=0&pli0162<.
gen leisure=hhour+phours

preserve 
keep cid pid syear leisure sex
rename pid parid
rename leisure par_leisure
rename sex parsex
tempfile partner
save `partner'
restore
merge 1:1 cid syear parid using `partner'
keep if _merge==3

gen GEoutcome = abs(leisure-par_leisure)
gen d_daughter= (kidsex01==2) if kidsex01>0
gen male = (sex==1) if sex!=.
 forvalue i = 1/9 {
replace d_daughter=1 if kidsex0`i'==2
}
forvalue i = 0/9 {
replace d_daughter=1 if kidsex1`i'==2
}

gen age= syear -  gebjahr
gen kid1age=syear- kidgeb01 if  kidgeb01>0
gen edu=pgpsbil if pgpsbil>0
* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	*We meisure leisure as hours spent on physical activity and hobbies (based on variables pli0051 and pli0162). 
	*Our measure of gender equaliy (GEoutcome) is the absolute value of the difference between the man and the woman in leisure time. 
	*Key idea - having similar amount of hours for leisure is a proxy for gender equality. 

	*To test if having a daugher increases gender equality, we estimate an OLS regression with GEoutcome as dependent variable. 
	*The main independent variable of interest is d_daughters which is 1 if the person has at least one 
	*d_daughter and 0 if the person has only sons (missing if no children). We include other control variables.

	*Note that this analysis differs from the pre-analysis plan. The outcome is different. 

	reg GEoutcome d_daughter age kid1age male germborn i.edu i.syear if sex!=parsex,cluster (hid)


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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
