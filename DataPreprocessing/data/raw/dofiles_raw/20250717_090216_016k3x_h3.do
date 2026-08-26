********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

*Run Time 1min 26 sec on MacBookAir M2 2022 16GB with 5% of data



* ============================
* Setup * (Please adapt according to your needs)
* ============================

//clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

/*
* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install csdid
ssc install drdid

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
	
	save "$DATA/temp.dta", replace
	
	* drop plb*
	drop plc*
	drop plg*
	drop plj*
	drop plm*
	
	save "$DATA/h3_between.dta", replace
	
	
	
	
	/*
	FROM HERE: Merging household data and calc. share of household work
	*/
	
	
	
	set more off
	
	* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 


	
	* keep: 1) only participants with at least 1 child 2) relevant variables
	keep if kidsex01>=1
	keep hid pid sex syear sumkids kidpnr01 kidsex01 kidgeb01 parid
	
	* drop participants without a partner (as we want to calculate the division of housework between 2 partners)
	drop if parid==-2
	
	* only keep first woman/man of each household
	sort hid pid sex syear
	duplicates drop hid sex, force
	
	* only keep households with 2 unique participants throughout observation period, ignore change of partners etc. 
	drop if kidpnr01<0 // kipnr01 ensures that the two participants have the same first child
	bysort hid kidpnr01: gen num_unique_persons = _N
	keep if num_unique_persons == 2
	
	* another check whether gender is uniquely represented for each household (should drop 0)
	sort hid sex
	quietly by hid sex: gen dup = cond(_N==1,0,_n)
	drop if dup>0
		
	/* check if participants are part of different households over observation period
	sort pid
	quietly by pid: gen dup2 = cond(_N==1,0,_n)
	drop if dup2>0
	*/
	
	* mark households relevant for analysis and flag male/female household members
	sort hid sex
	gen keep_hid=1
	by hid: gen man_in_hid = -1*(_n - 2)
	
	* save the dataset with relevant household ids and man/woman in household dummy
	save temp.dta, replace
	
	* merge with original dataset based on participant id
	use "$DATA/h3_between.dta"
	merge m:m pid using temp.dta
	
	* keep only the flagged participants/households
	drop _merge
	drop if man_in_hid==.
	drop if keep_hid==.
	drop if pli0043_h<0
	
	* save dataset of relevant households with 1 observation per male/female member
	save temp_panel.dta, replace
	
	/*
	* add Universitätsabschluss as highest education over Abitur 						//ADDED: FROM HERE
	replace pgpsbil=0 if pgpsbil==5
	replace pgpsbil=5 if pgpbbil02>=1													//ADDED: TO HERE
	*/
	
	* keep only male members and rename relevant variables
	keep if man_in_hid==1
	keep hid pli0043_h pid parid syear kidpnr01 pgemplst pgpsbil pgpbbil02 gebjahr	
	rename pli0043_h pli0043_h_man
	rename pid pid_man
	rename parid parid_man
	rename pgemplst pgemplst_man														
	rename pgpsbil pgpsbil_man															
	rename pgpbbil02 pgpbbil02_man														
	rename gebjahr gebjahr_man															
			
	* save temporary dataset containing male household members
	save temp_man.dta, replace
	
	* use temporary dataset and keep only female household members
	use temp_panel.dta, clear
	keep if man_in_hid==0
	
	* merge with male household members dataset and ensure that pairs/partners are identified
	merge m:m hid syear using temp_man.dta
	drop _merge
	keep if pid==parid_man

	* keep only observations with information on household work
	drop if pli0043_h==. | pli0043_h_man==.
	
	* calculate the share of household work as difference from equality (0 - equal, <0 - unequal)
	gen housework_unequal=-abs(pli0043_h-pli0043_h_man)
	sort hid syear
	save household_share.dta, replace
	
	
	* for each household id capture the observation (min and max) period
	bysort hid: egen min_year = min(syear)
	bysort hid: egen max_year = max(syear)	
	
	* keep only households that had child within observation period and at least one wave without child
	keep if max_year >= kidgeb01 & kidgeb01 > min_year
	
	* last check that no household-syears appear more than once (should drop 0)
	quietly by hid syear: gen dup3 = cond(_N==1,0,_n)
	drop if dup3>0
	
	
	
* GENERATE VARIABLES * (Please adapt according to your needs)
	

	
	* calc. no. of daughters/sons per household								
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
	
	* employment status for man and woman per household					
	
	gen woman_full=1 if pgemplst==1
	replace woman_full=0 if woman_full==.
	gen woman_part=1 if pgemplst==2
	replace woman_part=0 if woman_part==.
	gen woman_unempl=1 if pgemplst==5
	replace woman_unempl=0 if woman_unempl==.
	gen man_full=1 if pgemplst_man==1
	replace man_full=0 if man_full==.
	gen man_part=1 if pgemplst_man==2
	replace man_part=0 if man_part==.
	gen man_unempl=1 if pgemplst_man==5
	replace man_unempl=0 if man_unempl==.			
	drop man_in_hid																			
	
	* age of man and woman per survey wave								
	
	gen age=syear-gebjahr
	gen age_man=syear-gebjahr_man										
	
	
	
	
	gen year = syear-1999 //generate years such that csdid can use them
	
	sort hid year
	
	by hid, sort: gen event=1 if kidsex01==2 &  kidgeb01 < syear // generate indicator that is 1 if the person got a daughter as first child and stays 1 after that 

	* Create a variable to with the first year of treatment (getting a first born daughter)
	gen treat_start_year = .
	gen treat_start_year_delay3 = .
	gen treat_start_year_delay7 = .
	gen treat_start_year_delay12 = .

	* Mark the first year where treatment == 1
	bysort hid (year): replace treat_start_year = 1 if event == 1 & event[_n-1] == .
	bysort hid (year): replace treat_start_year_delay3 = 1 if event[_n-3] == 1 & event[_n-4] == .
	bysort hid (year): replace treat_start_year_delay7 = 1 if event[_n-7] == 1 & event[_n-8] == .
	bysort hid (year): replace treat_start_year_delay12 = 1 if event[_n-12] == 1 & event[_n-13] == .

	gen event_year=year if treat_start_year==1
	gen event_year_delay3=year if treat_start_year_delay3==1
	gen event_year_delay7=year if treat_start_year_delay7==1
	gen event_year_delay12=year if treat_start_year_delay12==1

	sort hid event_year
	replace event_year=event_year[_n-1] if pid==pid[_n-1]
	replace event_year=0 if event_year==.
	
	sort hid event_year_delay3
	replace event_year_delay3=event_year_delay3[_n-1] if pid==pid[_n-1]
	replace event_year_delay3=0 if event_year_delay3==.
	
	sort hid event_year_delay7
	replace event_year_delay7=event_year_delay7[_n-1] if pid==pid[_n-1]
	replace event_year_delay7=0 if event_year_delay7==.	
	
	sort hid event_year_delay12
	replace event_year_delay12=event_year_delay12[_n-1] if pid==pid[_n-1]
	replace event_year_delay12=0 if event_year_delay12==.
	
	sort hid syear
	
	gen log_hghinc=log(hghinc)

	save "$DATA/h3_dataset.dta", replace // save merged dataset



* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	* As an example, and for illustrative purposes we consider a: linear OLS regression

	* We estimate the causal effect of having a first daughter on the unequal division of housework within households using the Callaway and Sant’Anna (2021) difference-in-differences estimator.
	*The outcome variable housework_unequal captures the extent to which housework is unequally divided between partners and treatmetn assignment staggered across households over time.
	*We use the doubly robust inverse probability weighting method (dripw) to estimate group-time average treatment effects, comparing each treatment cohort (defined by the timing of the first daughter) to an appropriate control group of not-yet-treated households.
	csdid housework_unequal log_hghinc age age_man num_daughters num_sons pgpsbil pgpsbil_man pgemplst pgemplst_man, ivar(hid) time(year) gvar(event_year) method(dripw)
	// we do not have enough data to estimate the full model with all the controls, hence opposite to our pre-analysis plan we added the line below estimating the model without controls.
	
	//csdid housework_unequal, ivar(hid) time(year) gvar(event_year) method(dripw)
	estat simple 
	
	






* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)
		matrix h3 = r(table)
	* Extract the required statistics
		
		
		
		
		* (i) coefficient estimate
		scalar att_coef = h3[1,1]

		local coefficient =att_coef

		* (ii) standard error
		local se = h3[2,1]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = h3[4,1]

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof hid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local interpretation "not linearly interpretable"

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


	rm "$DATA/h3_between.dta"  //delet data that is used inbetween for merging purposes
	
exit
