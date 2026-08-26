*******************************************************************************
****************** Actual Do-File H1: #ManyDaughters Project ******************
*******************************************************************************

* H1: Having daughters leads to stronger support for gender equality (as measured by attitudes).

*X var: DV for have 1 or more daughters
*Y var: attitudes for gender equality

*3 hours

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16.1 // indicate the version of stata you are using here
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

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* only keep participants from private households
	keep if inlist(pop,1,2)

* GENERATE VARIABLES * (Please adapt according to your needs)

	* generate dummy variable for whether participant has at least one daughter (1) or not (0)
	gen d_daughter = (kidsex01==2|kidsex02==2|kidsex03==2|kidsex04==2|kidsex05==2|kidsex06==2|kidsex07==2|kidsex08==2|kidsex09==2|kidsex10==2|kidsex11==2|kidsex12==2|kidsex13==2|kidsex14==2|kidsex15==2|kidsex16==2|kidsex17==2|kidsex18==2|kidsex19==2)

	*Proxy for support for gender equality: agreement with: Schools and preschools should promote non-gender-typical activities that are not typical for children’s gender
	gen gender_att = plh0395i05

	*generate control variables
	*male DV
	gen male=sex==1
	*age=survey year-birth year
	gen age=syear-gebjahr
	*immigrant status DV =1 if born overseas, 0 if born in germany or immig <1950
	gen immig_status=germborn==2
	*log net household income
	gen log_income=log(hghinc)
	*total number of years of education
	gen edu=pgbilzeit

	global controls "age male immig_status log_income edu"


* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Please add a verbal description of your model here: linear OLS regression
	* Using plh0395i05 as measure of support for gender equality (agreement with: Schools and preschools should promote non-gender-typical activities that are not typical for children’s gender).
	* Accordingly, we run a linear OLS regression analysis with plh0395i05 as dependent variable and the dummy variable d_daughter as independent variable

	reg gender_att d_daughter $controls if edu>=0 & gender_att>0 & gender_att!=., r


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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
