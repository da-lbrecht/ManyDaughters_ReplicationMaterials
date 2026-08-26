********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2
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
*/

* ============================
* Data Preparation * 
* ============================

	* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/biol.dta", clear // use biol as a starting dataset

	* Merge biol with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* pl contains individual information 
	merge m:1 pid hid syear using "$IN/pl.dta", nogen keep(master matches)
	
	* bio_birth contains biographic information 
	merge m:1 pid cid using "$IN/biobirth.dta", nogen keep(master matches)
	
	keep pid syear hid cid pnr lb0288_v1 lb0290_h lb0287_h lb0290_h plj0620 plj0624 plj0625 gebjahr sex lb0063 plh0258_v10 plh0258_v11 plb0022_h plj0022 plj0023_v1 plb0471_h plh0004
	
	label language EN // change language of the labels to English
	
	save "$DATA/h2_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	* keep if inlist(pop,1,2)

* GENERATE VARIABLES * (Please adapt according to your needs)

	* We use the variable lb0288_v1 to identify the sex of the firstborn child. To ensure comparability across respondents, we limit our analysis to individuals with exactly one child. This restriction is based on the variable lb0290_h, which indicates whether the respondent has a second child. A value of -2 for lb0290_h signifies that the question is not applicable, which we interpret as the respondent not having a second child. Therefore, we retain only those respondents for whom lb0290_h = -2, ensuring the sample includes only individuals with a single child.
	
	gen d_daughter = (lb0288_v1 == 2) if (lb0290_h == -2)&(lb0288_v1 >= 0)
	label variable d_daughter "1 if the responder has only one daughter and zero if has only a son"
	
	tab d_daughter
	
	* Following the approach outlined in our pre-analysis plan (PAP), we construct a variable to identify which cases are included in the analysis. Our objective is to examine the parenting effect associated with raising a daughter. We assume that this effect requires sufficient exposure to the parenting experience, meaning that the firstborn daughter must be at least one year old at the time the gender equality attitudes are measured. To operationalize this, we create a variable named "sample", which takes the value 1 for individuals meeting this exposure condition. Specifically, we define the year of birth of the firstborn child—ensuring the respondent has no additional children—and compare it to the year the political preference item was answered. Respondents whose firstborn child was born at least one year prior to the measurement of attitudes are included in the analysis (i.e., sample == 1).
	
	gen year_first_born = lb0287_h if (lb0290_h == - 2) & (lb0287_h >= 0)
    label variable year_first_born "Year of the firstborn when the individual has only one child"
	
	* The variable sample is defined as the difference between the year in which the dependent variable (political preferences) is measured and the year of birth of the firstborn child. We include in the analysis only cases where this difference is at least one year, ensuring that the respondent has been exposed to parenting their firstborn child for a minimum of one year. This restriction helps capture a meaningful level of parenting experience with that child. 
	
	gen sample = (syear - year_first_born >= 1) if!missing(year_first_born)
	label variable sample "1 if the individual's one child firstborn has at least one year wrt the dep. var.'"

* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* The analysis follows the same approach used to test H1. As outlined in our PAP, we use the variable plh0004 as a measure of respondents' political preferences. This variable is measured on an 11-point scale, ranging from 0 (extreme left) to 10 (extreme right). Consistent with our previous strategy, we estimate an OLS regression model in which the dependent variable is political preference, and the main independent variable is a binary indicator for whether the respondent with only one child has a firstborn daughter. Although the dependent variable is observed in scattered survey years, we include year fixed effects to account for temporal variation. This adjustment was not explicitly mentioned in the PAP, but we believe it does not alter the original intent of the planned analysis. We use the same identification strategy as the one described in the test of H1 and in our PAP.
	
	reg plh0004 d_daughter i.syear if plh0004 >=0 & sample == 1, robust	
	
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
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
