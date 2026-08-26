********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 14 // indicate the version of stata you are using here
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
	* biol contains all variables from the biography questionnaires; use for keeping the month that the survey was carried out
	merge 1:1 pid syear using "$IN/biol.dta", nolabel nogen keep(master matches) keepusing(hid syear imonth)
	
	keep plh0395i05 pgisced11 pgfamstd kidsex01-kidsex19 kidgeb01-kidgeb19 biovalid kidmon01-kidmon19 imonth sex gebjahr corigin eintritt hid pid

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset
	
	
* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
 	* Only keep participants with non missing values for the main dependent variable ( plh0395i05)
	keep if  plh0395i05>0
	
	* Drop observations with missing values in other variables
	drop if pgisced11<0
	drop if pgfamstd<0
	local number "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19"
			foreach x in `number' {
			drop if kidsex`x'== -1 
			drop if kidgeb`x'==-1
			}
	
	* Only keep participants with birth biography available
	keep if biovalid==30| biovalid==31
	
	* Only keep participants with valid country if origin	
	keep if corigin>=0 //Added after correction 1 (running on the 95%-dataset)
	
* GENERATE VARIABLES * (Please adapt according to your needs)

	* Generate dummy variable for whether participant has at least one daughter (1) or not (0) before the interview
	gen has_daughters = 0
	
	local number "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19"

	foreach x in `number' {
    replace has_daughters = 1 if kidgeb`x'<2019 & kidgeb`x'>0 & kidsex`x'==2
	replace has_daughters = 1 if kidgeb`x'==2019 & kidgeb`x'>0 & kidgeb`x'>0 & kidmon`x'<imonth & kidsex`x'==2
}

	* Generate dummy variable for whether the participant has at least one kid (1) or not (0) before the interview
	gen has_kids = 0
	replace has_kids = 1 if kidgeb01<2019 & kidgeb01>0 
	replace has_kids = 1 if kidgeb01==2019 & kidgeb01>0 & kidmon01<imonth
	
	* Generate dummy variable for whether the participant is female (0) or male (1)
	gen male=0 if sex==2
	replace male=1 if sex==1
	
	* Generate dummy variable for whether participant is male and has at least one daughter (1) or not (0) before the interview
	gen male_daughters=male*has_daughters
	
	* Generate age of the interviewed in 2019
	gen age=2019-gebjahr
	
		
	

	
* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)

	* The item plh0395i05: ``Schools and preschools should promote non-gender-typical activities that are not typical for children's gender'', emerges as a strong proxy 
	* for attitudes toward gender equality in the general population. This item, clearly aligns with the core objective of challenging  gender stereotypes and promoting an environment in 
	* which individuals can realize their potential free from traditional gender constraints. This rejection of normative assumptions based on gender, actually  reflects a foundational principle 
	* of gender equality. Moreover, this item captures a proactive stance toward reshaping socialization processes and represents a progressive, preventive approach to addressing the root causes of gender inequality. 
	* Accordingly, I run a linear OLS regression analysis with plh0395i05 as dependent variable and the dummy variable has_daughters interacted with the male dummy as independent variable. The coefficient of interest 
	*is the interacted term. Additionally, standard errors are clustered at the hid (Current Wave Household Number) level, since it is possible that attitudes toward gender equality are correlated within the household 
	* itself due to unobservable factors and/or common influences.
	
	reg plh0395i05 has_daughters has_kids male male_daughters age i.pgisced11 i.pgfamstd i.corigin i.eintritt, cluster(hid)




* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[male_daughters]

		* (ii) standard error
		local se = _se[male_daughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[male_daughters] / _se[male_daughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[male_daughters]
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
