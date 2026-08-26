********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

*Run Time 23 sec on MacBookAir M2 2022 16GB with 5% of data


* clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install csdid
* ssc install revrs
* ssc install drdid 

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

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	//keep if inlist(pop,1,2)

	drop if plh0004 <0		
	drop if plh0004==. //only keep people that have answered the political attitudes question
	drop if kidgeb01 <=2005 // need to drop all with kids before 2005 (but now we only keep people that have children problem?
	
* GENERATE VARIABLES * (Please adapt according to your needs)

	
	revrs plh0004 //plh0004 plotical pref reverese coded such that left higher
	
	
	gen has_daughter = 0   //generate indicator if person has a at least one daughter =1 if daughter
	forvalues i = 1/19 {
    local varname = "kidsex" + string(`i', "%02.0f")
    replace has_daughter = 1 if `varname' == 2
	}


	gen age=syear-gebjahr

	gen log_hghinc=log(hghinc)
	
	gen num_daughters=0
	
	forvalues i=1/9{
	replace num_daughters=num_daughters+1 if kidsex0`i'==2 & kidgeb0`i'<=syear
	replace num_daughters=num_daughters+1 if kidsex1`i'==2 & kidgeb1`i'<=syear
	}
	
	gen num_sons=0
	
	forvalues i=1/9{
	replace num_sons=num_sons+1 if kidsex0`i'==1 & kidgeb0`i'<=syear
	replace num_sons=num_sons+1 if kidsex1`i'==1 & kidgeb1`i'<=syear
	}	


	// tab has_daughter 1009 people have a daughter in our sample overall  1,515
	// tab kidsex01  670 have a daughter as first child
	
	gen year = . //generate years such that csdid can use them
	replace year=1 if syear==2005 
	replace year=2 if syear==2009
	replace year=3 if syear==2014
	replace year=4 if syear==2019  
	
	
	sort  pid year
	by pid, sort: gen event=1 if kidsex01==2 &  kidgeb01 < syear // generate indicator that is 1 if the person got a daughter as first child and stays 1 after that 
	
	
	* Create a variable to with the first year of treatment (getting a first born daughter)
	gen treat_start_year = .

	* Mark the first year where treatment == 1
	bysort pid (year): replace treat_start_year = 1 if event == 1 & event[_n-1] == .

	gen event_year=year if treat_start_year==1

	sort pid event_year

	replace event_year=event_year[_n-1] if pid==pid[_n-1]

	replace event_year=0 if event_year==.
	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	* We estimate the average treatment effect of the treated with the treatment procedure of Callway St'Anna which compares the self stated ppolitical leaning (revplh0004) 
	* before and after receiving the treatment. We thereby estimate hte efffects of having a first born daughter and how this evolves over time after having the daughter. 
	* We then aggregate the group specific ATTs to one variable. 


	//csdid  political_attitude num_children, ivar(hh_id) time(year) gvar(event_year) method(dripw)
	//csdid revplh0004, ivar(pid) time(year) gvar(event_year) method(dripw)
	csdid revplh0004 log_hghinc age sex num_daughters num_sons pgpsbil pgemplst, ivar(pid) time(year) gvar(event_year) method(dripw)

	estat simple //agreagte the treatment effects.
	 
	// estat group
	 


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		matrix b = r(table)
		scalar att_coef = b[1,1]
		
		local coefficient = b[1,1]

		* (ii) standard error
		scalar se_att_coef = b[2,1]
		local se = se_att_coef

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		scalar p_val= b[4,1]
		local p_value = p_val
		

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar att_coef = b[1,1]
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
