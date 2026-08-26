********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
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
ssc install outreg2

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
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Example: generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* For illustrative purposes, we will generate the variable randomly:
	

	
	gen d_daughter =0 
forvalues i=1(1)9{
	replace d_daughter = 1 if kidsex0`i'==2
}
forvalues i=10(1)19{
	replace d_daughter = 1 if kidsex`i'==2
}	



* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	* As an example, and for illustrative purposes we consider a: linear OLS regression

	* For illustrative purposes, let's further assume that having a job that requires hard manual labor (plb0085) is a good measure of support for gender equality (hypothesis 1), 
	* if having a job with less manual labor, the respondent is more likely to support gender equality.
	* We want to test whether the having daughters leads to a larger increase in the support of gender equality for men than for women (hypothesis 4).
	* Accordingly, we run a linear OLS regression analysis with plb0085 as dependent variable and the dummy variable d_daughter as independent variable


qui g equality=plh0308_v2	
qui replace equality=. if plh0308_v2<0
	
.........................................
//Gender: females
//................................................................................................................................
qui g female=1 if sex==2
qui replace female=0 if sex==1


.........................................
//Age: 
//................................................................................................................................
qui g age=bioage 
qui replace age=. if bioage<0

qui g age2=age*age




//Employment Status:
//................................................................................................................................
qui g emp_stat= pgemplst
qui replace emp_stat=. if pgemplst<0


//Marital Status
//................................................................................................................................
qui gen mar_stat=pgfamstd
qui replace mar_stat=. if pgfamstd<0


.........................................
//Education using ISCED-2011-Classification
//................................................................................................................................

 qui g education= pgisced11 
 qui replace education=. if pgisced11<0
 
 
.........................................
// Labour income net
......................................... 
qui g wageinc= pglabnet
qui replace wageinc=. if wageinc<0
qui replace wageinc=1 if wageinc==0
qui g lwage_i=log(wageinc)


	
...............................
qui g dougher_female = d_daughter*female
	
	
	xi: oprobit equality d_daughter female dougher_female age age2  lwage_i i.mar_stat i.education i.emp_stat , r



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[dougher_female]

		* (ii) standard error
		local se = _se[dougher_female]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		matrix results = r(table)
		local col = colnumb(results, "d_daughter")
		local p_value = results[4, `col']
		* local p_value = _b[d_daughter] / _se[d_daughter]
		* local p_value = 2 * ttail(e(df_r), abs(`p_value'))
		
		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local interpretation "negative coefficient is in line with hypothesis"


		* (ix) Hypothesis test
		

		test d_daughter + dougher_female = 0 
		local hypothesis = r(chi2)		
		local hypothesis_pvalue = r(p)
		
		
	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Hypothesis-Test:" `hypothesis'
	di "Hypothesis-Test p-value:" `hypothesis_pvalue'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient, se, family, residual_df, model_df, p_value, hypothesis, hypothesis_pvalue, num_obs, num_participants, interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value', `hypothesis' , `hypothesis_pvalue', `num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
