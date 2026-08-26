********************************************************************************
*********************  Do-File H1: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

*clear all
version 19 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000
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

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgnuts1_ew)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * 
 
* We focus on respondents with kids in 2019 

	keep if sumkids > 0 & syear == 2019 & inrange(kidgeb01, 1900, 2019)
		
* GENERATE VARIABLES * 

	* Generate the dependent variable (plh0395avg). 
	* This is the average to the six questions in Attitudes towards gender. 
	egen plh0395avg = rowmean(plh0395i01 plh0395i02 plh0395i03 plh0395i04 plh0395i05 plh0395i06) if min(plh0395i01, plh0395i02, plh0395i03, plh0395i04, plh0395i05, plh0395i06) >= 0 
	
	* Generate a dummy variable for whether participant has at least one daughter (1) or not (0)
	
	gen d_daughter = 0

	forval i = 1/19 {
		local sexvar = "kidsex" + string(`i', "%02.0f")
		local yearvar = "kidgeb" + string(`i', "%02.0f")
		replace d_daughter = 1 if `sexvar' == 2 & `yearvar' <= 2019
	}

	* Controls to be included in the regression analysis
	
	* Gender (dummy for female respondent)
	
	gen 	female = 0
	replace	female = 1 if sex == 2
	
	* Age: We use year of birth to generate age 
	gen 	age  = 2019 - gebjahr
	gen		age2 = (age * age)/100

	* Replace missing values for non-valid responses to control variables 
	* We replace values for level of in education, marital status, religion, income...
	* We do not need to replace values for employment status (pgemplst)
	replace pgisced11 = . 	if 	pgisced11 <= 0
	replace pgfamstd = . 	if 	pgfamstd <= 0
	replace plh0258_h = . 	if  plh0258_h <= 0
	replace pglabnet = . 	if  pglabnet  <= 0
	
	* Dummy variable if the respondent is German nationality
	gen		Germany = 0		
	replace Germany = 1 	if 	pgnation == 1
	
	* Transformation of labour income into logs 
	gen		log_pglabnet = ln(pglabnet)
	
	
* HYPOTHESIS TEST FOR H1 * 

	* Using OLS, we regress attitudes towards gender equality on "having at least one daughter"
	* Our specification includes controls for the respondent's gender (female), age, age2/100, 
	* level of education, marital status, employment status, income, religion, East Germany, 
	* and emigrant. The dependent variable in the analysis is plh0395avg as defined above.
	* We include robust standard errors, as detailed in our pre-analysis plan. 

	reg plh0395avg d_daughter female age age2 i.pgisced11 i.pgfamstd i.pgemplst i.plh0258_h i.hgnuts1_ew  Germany log_pglabnet, robust

	* NOTE: When we performed our analysis for H1 we conducted a series of robustness checks to examine whether having "at least one daughter" (d_daughter) influences attitudes towards gender equality (plh0395avg) with different specifications. We noted that the estimates for d_daughter are always significant except when we include the variable log_pglabgro, which stands for the current net labor income. If this variable is omitted or replaced by the monthly household net income (hghinc) the estimates for d_daughter become negative and significantly different from 0 (p-values < 0.05). 

* EXTRACT AND EXPORT RESULTS * 

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
		local interpretation "positive coeffiecient is in line with hypothesis"
		
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
