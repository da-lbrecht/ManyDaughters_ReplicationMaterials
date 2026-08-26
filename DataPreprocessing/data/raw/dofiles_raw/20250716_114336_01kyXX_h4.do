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
*ssc install outreg2

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

	*save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	*keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)

	set seed 123

	* Indicator that respondent has only one biological child:
	generate have_one_child = biokids==1
	
	* Indicator that first biological child is a daughter:
	generate d_daughter = .
	replace d_daughter = 1 if kidsex01==2	
	replace d_daughter = 0 if kidsex01==1

	* Male respondent indicator
	generate male_respondent = .
	replace male_respondent = 1 if sex==1
	replace male_respondent = 0 if sex==2
	
	
* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)


	* Test of H4: 
	* If we assume that disagreeing with the statements:
	* "For parents, vocational training or higher education for their sons should be more important than vocational training or higher education for their daughters" (plj0624)
	* and
	* "At home, the husband should have the final say" (plj0625) 
	* is a good measure of support for gender equality,
	* Then, accordingly, we run a linear OLS regression analysis with the average response to plj0624* and plj0625* as the dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* where "*" indicates I will reverse-code the item so that ">0" average is interpreted as in line with the hypothesis
	* and where taking the average is valid since both items are on a scale 1-7 
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	* Generate average outcome
	local i=1  // test number within hypothesis
	local outcome`i' = "avg_6_7"
	foreach v of varlist plj0624 plj0625 {
		generate `v'_reverse = 8-`v'
		
	}
	egen `outcome`i'' = rowmean(plj0624_reverse plj0625_reverse)
	
	* Regression
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (sex==1 | sex==2) & (plj0624>=1 & plj0624<=7) & (plj0625>=1 & plj0625<=7) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation_`i' = "positive coefficient is in line with hypothesis"

		
	// Display the saved statistics
	forvalues i=1/1 {
		di "Coefficient: `coefficient_`i''"
		di "Standard Error: `se_`i''"
		di "Test Statistic Family: `test_stat_family_`i''"
		di "Residual Degrees of Freedom: `df_residual_`i''"
		di "Model Degrees of Freedom: `df_model_`i''"
		di "p-value: " `p_value_`i''
		di "Number of observations: " `num_obs_`i''
		di	"Number of participants: " `num_participants_`i''
		di "Interpretation" "`interpretation`i''"
	}	

	* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	forvalues i=1/1 {
		file write myfile "`coefficient_`i'', `se_`i'', `test_stat_family_`i'', `df_residual_`i'', `df_model_`i'',`p_value_`i'',`num_obs_`i'',`num_participants_`i'',`interpretation_`i''" _n
	}
	file close myfile

********************************************************************************

exit


********************************************************************************
********************************************************************************
* BELOW IS MY ORIGINAL SUBMISSION COMMENTED OUT
********************************************************************************
********************************************************************************
/*
********************************************************************************
* PRIMARY
********************************************************************************

********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2

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

	*save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	*keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)

	set seed 123

	* Indicator that first biological child is a daughter:
	generate have_one_child = biokids==1
	generate d_daughter = .
	replace d_daughter = 1 if kidsex01==2	
	replace d_daughter = 0 if kidsex01==1

	* Male respondent indicator
	generate male_respondent = 2-sex

	
	
* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)


	* Test 1 of H4: 
	* If we assume that disagreeing with the statement that women should care about family rather than their career (plh0301) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0301 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=1  // test number within hypothesis
	local outcome`i' = "plh0301"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=1 & `outcome`i''<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		

	* Test 2 of H4: 
	* If we assume that agreeing with the statement that men should be invovled in housework (plh0303) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0303 as dependent variable
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=2  // test number within hypothesis
	local outcome`i' = "plh0303"

	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=1 & `outcome`i''<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		

	* Test 3 of H4: 
	* If we assume that agreeing with the statement that children suffer when the father is focused on his career (plh0304) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0304 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=3  // test number within hypothesis
	local outcome`i' = "plh0304"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=1 & `outcome`i''<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		
		
	* Test 4 of H4: 
	* If we assume that agreeing with the statement that it is best if man and woman work the same amount so they can share the household and family responsibility (plh0308_v2) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0308_v2 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=4  // test number within hypothesis
	local outcome`i' = "plh0308_v2"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=1 & `outcome`i''<=7) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		
		
	* Test 5 of H4: 
	* If we assume that agreeing with the statement that women should have the same rights as men in a democracy (plj0620) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plj0620 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=5  // test number within hypothesis
	local outcome`i' = "plj0620"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=0 & `outcome`i''<=10) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		

	* Test 6 of H4: 
	* If we assume that disagreeing with the statement that For parents, vocational training or higher education for their sons should be more important than vocational training or higher education for their daughters
	* (plj0624) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plj0624 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=6  // test number within hypothesis
	local outcome`i' = "plj0624"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=1 & `outcome`i''<=7) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		

	* Test 7 of H4: 
	* If we assume that disagreeing with the statement that At home, the husband should have the final say (plj0625) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plj0625 as dependent variable
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=7  // test number within hypothesis
	local outcome`i' = "plj0625"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=1 & `outcome`i''<=7) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"


	* Test 8 of H4: 
	* If we assume that agreeing with the statement that a working mother can give as much emotional warmth (plh0309) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0309 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=8  // test number within hypothesis
	local outcome`i' = "plh0309"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=1 & `outcome`i''<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"

		
	* Test 9 of H4: 
	* Average all items whose answers are given on a 1-4 scale (plh0301 plh0303* plh0304* plh0309*), where "*" indicates I will reverse-code the item so that ">0" average is interpreted as in line with the hypothesis
	* Then, accordingly, we run a linear OLS regression analysis with the average as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	* Generate average outcome
	local i=9  // test number within hypothesis
	local outcome`i' = "avg_1_2_3_8"
	foreach v of varlist plh0303 plh0304 plh0309 {
		generate `v'_reverse = 5-`v'
		
	}
	egen `outcome`i'' = rowmean(plh0301 plh0303_reverse plh0304_reverse plh0309_reverse)
	
	* Regression
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (plh0301>=1 & plh0301<=4) & (plh0303>=1 & plh0303<=4) & (plh0304>=1 & plh0304<=4) & (plh0309>=1 & plh0309<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"

		
	* Test 10 of H4: 
	* Average all items whose answers are given on a 1-7 scale administered in the same year (plj0624* plj0625*), where "*" indicates I will reverse-code the item so that ">0" average is interpreted as in line with the hypothesis
	* Then, accordingly, we run a linear OLS regression analysis with the average as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	* Generate average outcome
	local i=10  // test number within hypothesis
	local outcome`i' = "avg_6_7"
	foreach v of varlist plj0624 plj0625 {
		generate `v'_reverse = 8-`v'
		
	}
	egen `outcome`i'' = rowmean(plj0624_reverse plj0625_reverse)
	
	* Regression
	regress `outcome`i'' i.d_daughter##i.male_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (plj0624>=1 & plj0624<=7) & (plj0625>=1 & plj0625<=7) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"

		
	// Display the saved statistics
	forvalues i=1/10 {
		di "Outcome var: `outcome`i''"
		di "Coefficient: `coefficient_`i''"
		di "Standard Error: `se_`i''"
		di "Test Statistic Family: `test_stat_family_`i''"
		di "Residual Degrees of Freedom: `df_residual_`i''"
		di "Model Degrees of Freedom: `df_model_`i''"
		di "p-value: " `p_value_`i''
		di "Number of observations: " `num_obs_`i''
		di	"Number of participants: " `num_participants_`i''
		di "Interpretation of directionality: " "`interpretation_`i''"
	}	

	* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "analysis_type,test_number,outcome_var,coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	forvalues i=1/10 {
		file write myfile "primary,`i',`outcome`i'',`coefficient_`i'', `se_`i'', `test_stat_family_`i'', `df_residual_`i'', `df_model_`i'',`p_value_`i'',`num_obs_`i'',`num_participants_`i'',`interpretation_`i''" _n
	}
	file close myfile


********************************************************************************

********************************************************************************
* SECONDARY
********************************************************************************

********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2

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

	*save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	*keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)

	set seed 123

	* Indicator that first biological child is a daughter:
	*generate have_one_child = biokids==1
	*generate d_daughter = .
	*replace d_daughter = 1 if kidsex01==2	
	*replace d_daughter = 0 if kidsex01==1
	
	* Number of children for the respondent
	generate num_children = biokids
	
	* Number of daughters for the respondent
	egen num_daughters = anycount(kidsex*), values(2)
	replace num_daughters = 6 if num_daughters>6 & num_daughters!=.
	
	* Indicator at least one daughter
	generate d_daughter = .
	replace d_daughter = 1 if num_daughters>0 & num_daughters!=.
	replace d_daughter = 0 if num_daughters==0

	* Male respondent indicator
	generate male_respondent = 2-sex

	
	
* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)


	* Test 1 of H4: 
	* If we assume that disagreeing with the statement that women should care about family rather than their career (plh0301) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0301 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=1  // test number within hypothesis
	local outcome`i' = "plh0301"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=1 & `outcome`i''<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		

	* Test 2 of H4: 
	* If we assume that agreeing with the statement that men should be invovled in housework (plh0303) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0303 as dependent variable
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=2  // test number within hypothesis
	local outcome`i' = "plh0303"

	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=1 & `outcome`i''<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		

	* Test 3 of H4: 
	* If we assume that agreeing with the statement that children suffer when the father is focused on his career (plh0304) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0304 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=3  // test number within hypothesis
	local outcome`i' = "plh0304"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=1 & `outcome`i''<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		
		
	* Test 4 of H4: 
	* If we assume that agreeing with the statement that it is best if man and woman work the same amount so they can share the household and family responsibility (plh0308_v2) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0308_v2 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=4  // test number within hypothesis
	local outcome`i' = "plh0308_v2"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=1 & `outcome`i''<=7) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		
		
	* Test 5 of H4: 
	* If we assume that agreeing with the statement that women should have the same rights as men in a democracy (plj0620) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plj0620 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=5  // test number within hypothesis
	local outcome`i' = "plj0620"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=0 & `outcome`i''<=10) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		

	* Test 6 of H4: 
	* If we assume that disagreeing with the statement that For parents, vocational training or higher education for their sons should be more important than vocational training or higher education for their daughters
	* (plj0624) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plj0624 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=6  // test number within hypothesis
	local outcome`i' = "plj0624"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=1 & `outcome`i''<=7) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"
		

	* Test 7 of H4: 
	* If we assume that disagreeing with the statement that At home, the husband should have the final say (plj0625) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plj0625 as dependent variable
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=7  // test number within hypothesis
	local outcome`i' = "plj0625"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=1 & `outcome`i''<=7) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"


	* Test 8 of H4: 
	* If we assume that agreeing with the statement that a working mother can give as much emotional warmth (plh0309) is a good measure of support for gender equality.
	* Then, accordingly, we run a linear OLS regression analysis with plh0309 as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=8  // test number within hypothesis
	local outcome`i' = "plh0309"
	
	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=1 & `outcome`i''<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"

		
	* Test 9 of H4: 
	* Average all items whose answers are given on a 1-4 scale (plh0301 plh0303* plh0304* plh0309*), where "*" indicates I will reverse-code the item so that ">0" average is interpreted as in line with the hypothesis
	* Then, accordingly, we run a linear OLS regression analysis with the average as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	* Generate average outcome
	local i=9  // test number within hypothesis
	local outcome`i' = "avg_1_2_3_8"
	foreach v of varlist plh0303 plh0304 plh0309 {
		generate `v'_reverse = 5-`v'
		
	}
	egen `outcome`i'' = rowmean(plh0301 plh0303_reverse plh0304_reverse plh0309_reverse)
	
	* Regression
	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (plh0301>=1 & plh0301<=4) & (plh0303>=1 & plh0303<=4) & (plh0304>=1 & plh0304<=4) & (plh0309>=1 & plh0309<=4) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"

		
	* Test 10 of H4: 
	* Average all items whose answers are given on a 1-7 scale administered in the same year (plj0624* plj0625*), where "*" indicates I will reverse-code the item so that ">0" average is interpreted as in line with the hypothesis
	* Then, accordingly, we run a linear OLS regression analysis with the average as dependent variable 
	* and the interaction of a dummy variable d_daughter and a dummy variable male_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	* Generate average outcome
	local i=10  // test number within hypothesis
	local outcome`i' = "avg_6_7"
	foreach v of varlist plj0624 plj0625 {
		generate `v'_reverse = 8-`v'
		
	}
	egen `outcome`i'' = rowmean(plj0624_reverse plj0625_reverse)
	
	* Regression
	regress `outcome`i'' i.d_daughter##i.male_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (plj0624>=1 & plj0624<=7) & (plj0625>=1 & plj0625<=7) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,8]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,8]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_`i' = "t"
		
		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_`i' = e(df_r)
		local df_model_`i' = e(df_m)

		* (v) p-value
		local p_value_`i' = `coefficient_`i'' / `se_`i''
		local p_value_`i' = 2 * ttail(e(df_r), abs(`p_value_`i''))
		
		* (vi) number of observations
		local num_obs_`i' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants_`i')
		local num_participants_`i' : word count `participants_`i''
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter_`i' = `coefficient_`i''
		local interpretation "positive coefficient is in line with hypothesis"

		
	// Display the saved statistics
	forvalues i=1/10 {
		di "Outcome var: `outcome`i''"
		di "Coefficient: `coefficient_`i''"
		di "Standard Error: `se_`i''"
		di "Test Statistic Family: `test_stat_family_`i''"
		di "Residual Degrees of Freedom: `df_residual_`i''"
		di "Model Degrees of Freedom: `df_model_`i''"
		di "p-value: " `p_value_`i''
		di "Number of observations: " `num_obs_`i''
		di	"Number of participants: " `num_participants_`i''
		di "Interpretation of directionality: " "`interpretation_`i''"
	}	

	* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write append
	file write myfile "analysis_type,test_number,outcome_var,coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	forvalues i=1/10 {
		file write myfile "secondary,`i',`outcome`i'',`coefficient_`i'', `se_`i'', `test_stat_family_`i'', `df_residual_`i'', `df_model_`i'',`p_value_`i'',`num_obs_`i'',`num_participants_`i'',`interpretation_`i''" _n
	}
	file close myfile


********************************************************************************

exit
