********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
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

	*save "$DATA/h2_dataset.dta", replace // save merged dataset


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
	
	* Indicator of not a right-wing party based on plh0012_h:
	generate left_wing_party = .
	replace left_wing_party = 1 if plh0012_h==1 | plh0012_h==5 | plh0012_h==6 | plh0012_h==9 | plh0012_h==16 | plh0012_h==17
	replace left_wing_party = 0 if plh0012_h==2 | plh0012_h==3 | plh0012_h==4 | plh0012_h==7 | plh0012_h==8 | plh0012_h==13 | plh0012_h==14 | plh0012_h==21 | plh0012_h==22 | plh0012_h==25 | plh0012_h==27 | plh0012_h==30
	

	
	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Test 1 H2:
	* If we assume that preference for a not right-wing party as defined by var left_wing_party is a good measure of left-leaning political preferences.
	* Then, accordingly, we run a linear OLS regression analysis with left_wing_party as dependent variable and the dummy variable d_daughter as independent variable
	* We restrict the sample to individuals with valid answers to the variables included in the regression

	local i=1  // test number within hypothesis
	local outcome`i' = "left_wing_party"
	
	regress `outcome`i'' i.d_daughter i.sex c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (sex==1 | sex==2) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)

	
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
		di "Interpretation" "`interpretation_`i''"
	}	

	
	* Export results to csv 
	file open myfile using "$OUT/h2_results.csv", write replace
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
****************** Example Do-File H2: #ManyDaughters Project ******************
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

	*save "$DATA/h2_dataset.dta", replace // save merged dataset


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
	
	* Indicator of not a right-wing party based on plh0012_h:
	generate left_wing_party = .
	replace left_wing_party = 1 if plh0012_h==1 | plh0012_h==5 | plh0012_h==6 | plh0012_h==9 | plh0012_h==16 | plh0012_h==17
	replace left_wing_party = 0 if plh0012_h==2 | plh0012_h==3 | plh0012_h==4 | plh0012_h==7 | plh0012_h==8 | plh0012_h==13 | plh0012_h==14 | plh0012_h==21 | plh0012_h==22 | plh0012_h==25 | plh0012_h==27 | plh0012_h==30
	
	* Indicator of strength of left-leaning preference 
	generate strength_left = .
	replace strength_left=1 if left_wing_party==0 & plh0013_v2==1
	replace strength_left=2 if left_wing_party==0 & plh0013_v2==2
	replace strength_left=3 if left_wing_party==0 & plh0013_v2==3
	replace strength_left=4 if left_wing_party==0 & plh0013_v2==4
	replace strength_left=5 if left_wing_party==0 & plh0013_v2==5
	replace strength_left=6 if left_wing_party==1 & plh0013_v2==5
	replace strength_left=7 if left_wing_party==1 & plh0013_v2==4
	replace strength_left=8 if left_wing_party==1 & plh0013_v2==3
	replace strength_left=9 if left_wing_party==1 & plh0013_v2==2
	replace strength_left=10 if left_wing_party==1 & plh0013_v2==1

	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Test 1 of H2:
	* If we assume that preference for a not right-wing party as defined by var left_wing_party is a good measure of left-leaning political preferences.
	* Then, accordingly, we run a linear OLS regression analysis with left_wing_party as dependent variable and the dummy variable d_daughter as independent variable

	local i=1  // test number within hypothesis
	local outcome`i' = "left_wing_party"
	
	regress `outcome`i'' i.d_daughter i.sex c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)

	
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


	* Test 2 of H2:
	* If we assume that the strength of preference for a not right-wing party as defined by var strength_left is a good measure of left-leaning political preferences.
	* Then, accordingly, we run a linear OLS regression analysis with strength_left as dependent variable and the dummy variable d_daughter as independent variable

	local i=2  // test number within hypothesis
	local outcome`i' = "strength_left"
	
	regress `outcome`i'' i.d_daughter i.sex c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)

	
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
		

	* Test 3 of H2:
	* If we assume that left-leaning political attitude (plh0004) is a good measure of left-leaning political preferences.
	* Then, accordingly, we run a linear OLS regression analysis with plh0004 as dependent variable and the dummy variable d_daughter as independent variable

	local i=3  // test number within hypothesis
	local outcome`i' = "plh0004"
	
	regress `outcome`i'' i.d_daughter i.sex c.bioage##c.bioage hghinc pgisced97 i.syear if have_one_child==1 & (`outcome`i''>=0 & `outcome`i''<=10) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)

	
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
	forvalues i=1/3 {
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "analysis_type,test_number,outcome_var,coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	forvalues i=1/3 {
		file write myfile "primary,`i',`outcome`i'',`coefficient_`i'', `se_`i'', `test_stat_family_`i'', `df_residual_`i'', `df_model_`i'',`p_value_`i'',`num_obs_`i'',`num_participants_`i'',`interpretation_`i''" _n
	}
	file close myfile


********************************************************************************

********************************************************************************
* SECONDARY
********************************************************************************

********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
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

	*save "$DATA/h2_dataset.dta", replace // save merged dataset


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
	
	* Indicator of not a right-wing party based on plh0012_h:
	generate left_wing_party = .
	replace left_wing_party = 1 if plh0012_h==1 | plh0012_h==5 | plh0012_h==6 | plh0012_h==9 | plh0012_h==16 | plh0012_h==17
	replace left_wing_party = 0 if plh0012_h==2 | plh0012_h==3 | plh0012_h==4 | plh0012_h==7 | plh0012_h==8 | plh0012_h==13 | plh0012_h==14 | plh0012_h==21 | plh0012_h==22 | plh0012_h==25 | plh0012_h==27 | plh0012_h==30
	
	* Indicator of strength of left-leaning preference 
	generate strength_left = .
	replace strength_left=1 if left_wing_party==0 & plh0013_v2==1
	replace strength_left=2 if left_wing_party==0 & plh0013_v2==2
	replace strength_left=3 if left_wing_party==0 & plh0013_v2==3
	replace strength_left=4 if left_wing_party==0 & plh0013_v2==4
	replace strength_left=5 if left_wing_party==0 & plh0013_v2==5
	replace strength_left=6 if left_wing_party==1 & plh0013_v2==5
	replace strength_left=7 if left_wing_party==1 & plh0013_v2==4
	replace strength_left=8 if left_wing_party==1 & plh0013_v2==3
	replace strength_left=9 if left_wing_party==1 & plh0013_v2==2
	replace strength_left=10 if left_wing_party==1 & plh0013_v2==1

	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Test 1 of H2:
	* If we assume that preference for a not right-wing party as defined by var left_wing_party is a good measure of left-leaning political preferences.
	* Then, accordingly, we run a linear OLS regression analysis with left_wing_party as dependent variable and the dummy variable d_daughter as independent variable

	local i=1  // test number within hypothesis
	local outcome`i' = "left_wing_party"
	
	regress `outcome`i'' i.d_daughter i.num_children i.sex c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)

	
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


	* Test 2 of H2:
	* If we assume that the strength of preference for a not right-wing party as defined by var strength_left is a good measure of left-leaning political preferences.
	* Then, accordingly, we run a linear OLS regression analysis with strength_left as dependent variable and the dummy variable d_daughter as independent variable

	local i=2  // test number within hypothesis
	local outcome`i' = "strength_left"
	
	regress `outcome`i'' i.d_daughter i.num_children i.sex c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)

	
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
		

	* Test 3 of H2:
	* If we assume that left-leaning political attitude (plh0004) is a good measure of left-leaning political preferences.
	* Then, accordingly, we run a linear OLS regression analysis with plh0004 as dependent variable and the dummy variable d_daughter as independent variable

	local i=3  // test number within hypothesis
	local outcome`i' = "plh0004"
	
	regress `outcome`i'' i.d_daughter i.num_children i.sex c.bioage##c.bioage hghinc pgisced97 i.syear if num_children>0 & (`outcome`i''>=0 & `outcome`i''<=10) & (hghinc>0 & hghinc!=.) & (pgisced97>=0 & pgisced97!=.), vce(robust)

	
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
	forvalues i=1/3 {
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
	file open myfile using "$OUT/h2_results.csv", write append
	file write myfile "analysis_type,test_number,outcome_var,coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	forvalues i=1/3 {
		file write myfile "secondary,`i',`outcome`i'',`coefficient_`i'', `se_`i'', `test_stat_family_`i'', `df_residual_`i'', `df_model_`i'',`p_value_`i'',`num_obs_`i'',`num_participants_`i'',`interpretation_`i''" _n
	}
	file close myfile


********************************************************************************

exit
