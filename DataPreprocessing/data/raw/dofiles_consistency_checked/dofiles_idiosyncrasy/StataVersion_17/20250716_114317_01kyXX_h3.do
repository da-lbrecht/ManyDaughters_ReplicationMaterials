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

	*save "$DATA/h3_dataset.dta", replace // save merged dataset


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
	
	* Employed full time indicator based on plb0022_h
	generate employed_ft_v1 = .
	replace employed_ft_v1 = 1 if plb0022_h==1
	replace employed_ft_v1 = 0 if plb0022_h>=2 & plb0022_h<=12
	

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	
	* Test 2 H3: 
	* If we assume that a mother being empoyed full time (employed_ft_v1) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with employed_ft_v1 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=1  // test number within hypothesis
	local outcome`i' = "employed_ft_v1"
	
	regress `outcome`i'' i.d_daughter c.bioage##c.bioage hghinc pgisced97 i.syear if sex==2 & have_one_child==1 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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
		local interpretation_`i' "positive coefficient is in line with hypothesis"



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
	file open myfile using "$OUT/h3_results.csv", write replace
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
****************** Example Do-File H3: #ManyDaughters Project ******************
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

	*save "$DATA/h3_dataset.dta", replace // save merged dataset


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
	
	* Hours per week childcaring
	foreach v of varlist pli0019_h pli0022_h pli0044_h {
		generate `v'_mod = `v'
		replace `v'_mod = . if `v'<0 | `v'>24
	}
	generate hrs_week_child = pli0019_h_mod + pli0022_h_mod + pli0044_h_mod 

	* Female respondent indicator
	generate female_respondent = sex-1
	
	* Employed full time indicator based on plb0022_h
	generate employed_ft_v1 = .
	replace employed_ft_v1 = 1 if plb0022_h==1
	replace employed_ft_v1 = 0 if plb0022_h>=2 & plb0022_h<=12
	
	* Unemployed indicator based on plb0022_h
	generate unemployed_v1 = .
	replace unemployed_v1 = 1 if plb0022_h==9
	replace unemployed_v1 = 0 if (plb0022_h>=1 & plb0022_h<=8) | (plb0022_h>=10 & plb0022_h<=12)

	* Employed full time indicator based on pgemplst
	generate employed_ft_v2 = .
	replace employed_ft_v2 = 1 if pgemplst==1
	replace employed_ft_v2 = 0 if pgemplst>=2 & pgemplst<=7
	
	* Unemployed indicator based on pgemplst
	generate unemployed_v2 = .
	replace unemployed_v2 = 1 if pgemplst==5
	replace unemployed_v2 = 0 if (pgemplst>=1 & pgemplst<=4) | (pgemplst>=6 & pgemplst<=7)
	

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Test 1 of H3: 
	* The preregistered test considered  the share of hours spent on childcare by the father as a measure of gender equality in the household.
	* However I could not construct this var because there may be multiple individuals in the household doing childcare beside parents, or the may be single-parent households
	* Therefore, instead I examine whether the number of hours spent on childcare by an individual (regardless of parental status) is larger for males than females when they have a daughter in the household.
	* If we assume that more hours spent on childcare by males relative to females (negative interaction term) is a measure of gender equality in the household
	* Then, accordingly, we run a linear OLS regression analysis with the total hourse per week spent on childcare by the individual (hrs_week_child) as dependent variable and the
	* interaction of a dummy variable d_daughter and a dummy variable female_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=1  // test number within hypothesis
	local outcome`i' = "hrs_week_child"

	regress `outcome`i'' i.d_daughter##i.female_respondent c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

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
	
	
	* Test 2 of H3: 
	* If we assume that a mother being empoyed full time (employed_ft_v1) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with employed_ft_v1 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=2  // test number within hypothesis
	local outcome`i' = "employed_ft_v1"
	
	regress `outcome`i'' i.d_daughter c.bioage##c.bioage hghinc pgisced97 i.syear if sex==2 & have_one_child==1 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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


	* Test 3 of H3: 
	* If we assume that a mother being empoyed full time (employed_ft_v2) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with employed_ft_v2 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=3  // test number within hypothesis
	local outcome`i' = "employed_ft_v2"
	
	regress `outcome`i'' i.d_daughter c.bioage##c.bioage hghinc pgisced97 i.syear if sex==2 & have_one_child==1 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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
		

		
	* Test 4 of H3: 
	* If we assume that a mother being not being unempoyed (unemployed_v1) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with unemployed_v1 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=4  // test number within hypothesis
	local outcome`i' = "unemployed_v1"
	
	regress `outcome`i'' i.d_daughter c.bioage##c.bioage hghinc pgisced97 i.syear if sex==2 & have_one_child==1 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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

		
	* Test 5 of H3: 
	* If we assume that a mother being not being unempoyed (unemployed_v2) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with unemployed_v2 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=5  // test number within hypothesis
	local outcome`i' = "unemployed_v2"
	
	regress `outcome`i'' i.d_daughter c.bioage##c.bioage hghinc pgisced97 i.syear if sex==2 & have_one_child==1 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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
		

	* Test 6 of H3: 
	* If we assume that the number of hours worked per week by a mother (plb0186_h) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with plb0186_h as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=6  // test number within hypothesis
	local outcome`i' = "plb0186_h"
	
	regress `outcome`i'' i.d_daughter c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>0 & `outcome`i''!=.) & sex==2 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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
		

	* Test 7 of H3: 
	* If we assume that the mother's self-reported health status (ple0008) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with ple0008 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=7  // test number within hypothesis
	local outcome`i' = "ple0008"
	
	regress `outcome`i'' i.d_daughter c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=1 & `outcome`i''<=5) & sex==2 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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
	forvalues i=1/7 {
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "analysis_type,test_number,outcome_var,coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	forvalues i=1/7 {
		file write myfile "primary,`i',`outcome`i'',`coefficient_`i'', `se_`i'', `test_stat_family_`i'', `df_residual_`i'', `df_model_`i'',`p_value_`i'',`num_obs_`i'',`num_participants_`i'',`interpretation_`i''" _n
	}
	file close myfile


********************************************************************************

********************************************************************************
* SECONDARY
********************************************************************************

********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
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

	*save "$DATA/h3_dataset.dta", replace // save merged dataset


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
	
	* Hours per week childcaring
	foreach v of varlist pli0019_h pli0022_h pli0044_h {
		generate `v'_mod = `v'
		replace `v'_mod = . if `v'<0 | `v'>24
	}
	generate hrs_week_child = pli0019_h_mod + pli0022_h_mod + pli0044_h_mod 

	* Female respondent indicator
	generate female_respondent = sex-1
	
	* Employed full time indicator based on plb0022_h
	generate employed_ft_v1 = .
	replace employed_ft_v1 = 1 if plb0022_h==1
	replace employed_ft_v1 = 0 if plb0022_h>=2 & plb0022_h<=12
	
	* Unemployed indicator based on plb0022_h
	generate unemployed_v1 = .
	replace unemployed_v1 = 1 if plb0022_h==9
	replace unemployed_v1 = 0 if (plb0022_h>=1 & plb0022_h<=8) | (plb0022_h>=10 & plb0022_h<=12)

	* Employed full time indicator based on pgemplst
	generate employed_ft_v2 = .
	replace employed_ft_v2 = 1 if pgemplst==1
	replace employed_ft_v2 = 0 if pgemplst>=2 & pgemplst<=7
	
	* Unemployed indicator based on pgemplst
	generate unemployed_v2 = .
	replace unemployed_v2 = 1 if pgemplst==5
	replace unemployed_v2 = 0 if (pgemplst>=1 & pgemplst<=4) | (pgemplst>=6 & pgemplst<=7)
	

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Test 1 of H3: 
	* The preregistered test considered the share of hours spent on childcare by the father as a measure of gender equality in the household.
	* However I could not construct this var because there may be multiple individuals in the household doing childcare beside parents, or the may be single-parent households
	* Therefore, instead I examine whether the number of hours spent on childcare by an individual (regardless of parental status) is larger for males than females when they have at least one daughter in the household.
	* If we assume that more hours spent on childcare by males relative to females (negative interaction term) is a measure of gender equality in the household
	* Then, accordingly, we run a linear OLS regression analysis with the total hourse per week spent on childcare by the individual (hrs_week_child) as dependent variable and the
	* interaction of a dummy variable d_daughter and a dummy variable female_respondent as independent variable of interest.
	* We restrict the sample to individuals with valid answers to the variables included in the regression
	
	local i=1  // test number within hypothesis
	local outcome`i' = "hrs_week_child"

	regress `outcome`i'' i.d_daughter##i.female_respondent i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

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
	
	
	* Test 2 of H3: 
	* If we assume that a mother being empoyed full time (employed_ft_v1) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with employed_ft_v1 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=2  // test number within hypothesis
	local outcome`i' = "employed_ft_v1"
	
	regress `outcome`i'' i.d_daughter i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if sex==2 & num_children>0 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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


	* Test 3 of H3: 
	* If we assume that a mother being empoyed full time (employed_ft_v2) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with employed_ft_v2 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=3  // test number within hypothesis
	local outcome`i' = "employed_ft_v2"
	
	regress `outcome`i'' i.d_daughter i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if sex==2 & num_children>0 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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
		

		
	* Test 4 of H3: 
	* If we assume that a mother being not being unempoyed (unemployed_v1) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with unemployed_v1 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=4  // test number within hypothesis
	local outcome`i' = "unemployed_v1"
	
	regress `outcome`i'' i.d_daughter i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if sex==2 & num_children>0 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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

		
	* Test 5 of H3: 
	* If we assume that a mother being not being unempoyed (unemployed_v2) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with unemployed_v2 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=5  // test number within hypothesis
	local outcome`i' = "unemployed_v2"
	
	regress `outcome`i'' i.d_daughter i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if sex==2 & num_children>0 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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
		

	* Test 6 of H3: 
	* If we assume that the number of hours worked per week by a mother (plb0186_h) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with plb0186_h as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=6  // test number within hypothesis
	local outcome`i' = "plb0186_h"
	
	regress `outcome`i'' i.d_daughter i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>0 & `outcome`i''!=.) & sex==2 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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
		

	* Test 7 of H3: 
	* If we assume that the mother's self-reported health status (ple0008) is a good measure of gender equality in the household.
	* Then, accordingly, we run a linear OLS regression analysis with ple0008 as dependent variable and the dummy variable d_daughter as independent variable.
	* We restrict the sample to mothers and individuals with valid answers to the variables included in the regression
	
	local i=7  // test number within hypothesis
	local outcome`i' = "ple0008"
	
	regress `outcome`i'' i.d_daughter i.num_children c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=1 & `outcome`i''<=5) & sex==2 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)
	

	* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	
		matrix a_`i' = r(table)
		
		* (i) coefficient estimate
		local coefficient_`i' = a_`i'[1,2]
		
		* (ii) standard error
		local se_`i' = a_`i'[2,2]
		
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
	forvalues i=1/7 {
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
	file open myfile using "$OUT/h3_results.csv", write append
	file write myfile "analysis_type,test_number,outcome_var,coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	forvalues i=1/7 {
		file write myfile "secondary,`i',`outcome`i'',`coefficient_`i'', `se_`i'', `test_stat_family_`i'', `df_residual_`i'', `df_model_`i'',`p_value_`i'',`num_obs_`i'',`num_participants_`i'',`interpretation_`i''" _n
	}
	file close myfile


********************************************************************************

exit
