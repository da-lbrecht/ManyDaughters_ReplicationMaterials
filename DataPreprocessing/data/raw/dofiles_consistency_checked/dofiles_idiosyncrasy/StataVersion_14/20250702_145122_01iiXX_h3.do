********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
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
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgnuts1)
	* biol contains all variables from the biography questionnaires; use for keeping the month that the survey was carried out
	merge 1:1 pid syear using "$IN/biol.dta", nolabel nogen keep(master matches) keepusing(hid syear imonth)
	
	label language EN // change language of the labels to English

	
* ============================
* Analysis
* ============================


* FILTERS * (Please adapt according to your needs)
 
 	* Only keep participants with non missing values for the main dependent variable (constructed through pli0043_h and pli0044_h)
	keep if  pli0043_h>=0
	keep if  pli0044_h>=0
	
	* Only keep participants with birth biography available
	keep if biovalid==30| biovalid==31
	
	* Drop respondents without co-habiting partner (or with missed information)
	drop if pgpartnr==-2
	drop if pgpartnr==.
	
	* Drop observations with missing values in other variables
	drop if pgisced11<0
	drop if pgfamstd<0
	local number "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19"
			foreach x in `number' {
			drop if kidsex`x'== -1 
			drop if kidgeb`x'==-1
			}
	
	
* GENERATE VARIABLES * (Please adapt according to your needs)

	* Generate dummy variable for whether participant has at least one daughter (1) or not (0) before the interview at time t
	local year 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
	local number "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19"

	gen has_daughters=0
	gen has_kids = 0
	
	foreach y in `year' {
	foreach x in `number' {
    replace has_daughters= 1 if kidgeb`x'<`y' & kidgeb`x'>0 & kidsex`x'==2 & syear==`y'
	replace has_daughters= 1 if kidgeb`x'==`y' & kidgeb`x'>0 & kidgeb`x'>0 & kidmon`x'<imonth & kidsex`x'==2 & syear==`y'
	
	* Generate dummy variable for whether the participant has at least one kid (1) or not (0) before the interview at time t
	replace has_kids = 1 if kidgeb01<`y' & kidgeb01>0 
	replace has_kids = 1 if kidgeb01==`y' & kidgeb01>0 & kidmon01<imonth
}
}

	* Generate dummy variable for whether the participant is female (0) or male (1)
	gen male=0 if sex==2
	replace male=1 if sex==1
	
	* Generate age of the interviewed at time t
	gen age=syear-gebjahr

	save "$DATA/h3_dataset.dta", replace // save dataset
		
		
*INCORPORATING PARTNER INFO

	*Creating an auxiliar dataset with the partner information in which pid=pgpartnr (such that pid could be used as matching variable later on) 
	replace pid=pgpartnr 
	local partner "has_daughters has_kids age pgisced11 pgfamstd pgemplst pli0043_h pli0044_h male"
	
	foreach z in `partner'{
	rename `z' `z'_part
	}
	
	keep pid has_daughters has_kids_part age_part pgisced11_part pgfamstd_part pgemplst_part syear pli0043_h_part pli0044_h_part male_part
	drop if pid==. 
	save "$DATA/h3_dataset_aux.dta", replace
	
	*Recover original dataset
	use "$DATA/h3_dataset.dta", clear 

	* Merging datasets to include, for each (pid syear) combination, the corresponding partner data
	merge 1:1 pid syear using "$DATA/h3_dataset_aux.dta", nolabel nogen keep(master matches)
	
	
* CLEANING AND ORGANIZING DATASET
	
	*Keeping only heterosexual couples
	keep if male+male_part==1
	
	*Generating new variables to have identified women's and men's characteristics
	local variables "has_kids age pgisced11 pgfamstd pgemplst pli0043_h pli0044_h"
	
			foreach w in `variables' {
			gen `w'_w=`w'*(1-male)+`w'_part*(1-male_part)
			}
			
			foreach w in `variables' {
			gen `w'_m=`w'*male+`w'_part*male_part
			}
	
	*Generating the dependent variable
	gen equality=-abs(0.5-(pli0043_h_w+pli0044_h_w)/(pli0043_h_w+pli0044_h_w+pli0043_h_m+pli0044_h_m))
	
	*Generating the independent variable of interest
	gen has_daughters_h=has_daughters+has_daughters_part
	
	*To avoid double counting, delete the observation where male==1 (Makes one observation per couple per year)
	keep if male==0
	
	*Delete cases where several couples co-habit in the same houshold (Makes one observation per couple per year)
	egen n_couples = count(hid), by(hid syear)  //Added after correction 1 (running on the 95%-dataset)
	drop if n_couples>1  //Added after correction 1 (running on the 95%-dataset)
	
	*Saving the dataset
	save "$DATA/h3_dataset.dta", replace	
	
* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	/*
	To test this hypothesis, a dataset has been structured as a longitudinal panel at the household level, with repeated observations for the same household 
	across different survey waves (from 2000 to 2020). A household is defined as a unique and stable pairing of individuals $i$ and $j$. That is, for a given 
	household, the composition in terms of individuals $i$ and $j$ remains constant over time, although any given individual may belong to different households
	across waves (never more than one in a given wave).
	
	For consistency and interpretability, household $h$ is defined as comprising a woman $i$ and a man $j$. While the gender composition does not affect the symmetry 
	of the equality index, this allows us to consistently control for potential gender differences in the effects of personal characteristics (e.g., education) on 
	the variable of interest.
	
	In line with this data structure, a fixed-effects regression model will be employed to account for unobserved time-invariant household characteristics that may be 
	correlated with gender equality.
	*/

	xtset hid syear
	xtreg equality has_daughters_h has_kids_w age_w i.pgisced11_w i.pgfamstd_w i.pgemplst_w has_kids_m age_m i.pgisced11_m i.pgfamstd_m i.pgemplst_m hghinc i.hgnuts1 i.syear, fe cluster(hid)



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[has_daughters_h]

		* (ii) standard error
		local se = _se[has_daughters_h]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[has_daughters_h] / _se[has_daughters_h]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[has_daughters_h]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
