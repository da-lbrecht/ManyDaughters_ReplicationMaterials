********************************************************************************
********************** Do-File H2: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

*clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
* install all packages you need here, for example:
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

* LOAD AND MERGE DATA *

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis.

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex* kidgeb* sumkids)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgemplst pgbilzeit)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0012_h plh0013_h)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgtyp2hh)

	label language EN // change language of the labels to English

	mvdecode _all, mv(-1=. \ -2=. \ -3=. \ -4=. \ -5=. \ -6=. \ -7=. \ -8=.)
	
	xtset pid syear
	
	save "$DATA/h2_dataset.dta", replace // save merged dataset
	
	use "$DATA/h2_dataset.dta", clear


* ============================
* Analysis
* ============================

* FILTERS *
	
	* drop years pre 1995
	drop if syear<1995
	
	* keep only individuals with children
	drop if sumkids == 0
	
	* only keep participants from private households
	keep if inlist(pop,1,2)
	
	* drop observations with missings in the outcome variable
	drop if missing(plh0012_h,plh0013_h)
	

* GENERATE & RECODE VARIABLES *

	* recode variables for political leanings variables: classify political preferences as left-leaning (1), right-leaning (0), inconclusive and across the center line (missing)
	
	recode plh0012_h (1 5 6 9 16 17 26 = 1) (2 3 4 7 13 14 21 22 25 27 30 = 0) (8 10 11 12 15 18 19 20 23 24 28 29 31 = .)
	
	label define leftright 1 "[1] left-leaning" 0 "[0] right-leaning"
	label val plh0012_h leftright

	
	* generate categorical variable that captures direction and intensity of political leaning
	gen intensity_leaning = .
	
	replace intensity_leaning = 1 if plh0012_h == 0 & inlist(plh0013_h,1,2)
	replace intensity_leaning = 2 if plh0012_h == 0 & inlist(plh0013_h,3,4,5)
	replace intensity_leaning = 3 if plh0012_h == 1 & inlist(plh0013_h,3,4,5)
	replace intensity_leaning = 4 if plh0012_h == 1 & inlist(plh0013_h,1,2)	

    * create event time and eventtime dummies
	
	* Year of birth of first child
	gen event_year = kidgeb01
	
	* time relative to the event
	gen event_time = syear - event_year
	
	* create event time dummies
    foreach k of numlist 0/25 {
    
	gen event_f`k' = (event_time == `k')    
	gen event_daughter_f`k' = (event_time == `k' & kidsex01 == 2)
	}
	
    foreach k of numlist 0/10 {
    
	gen event_l`k' = (event_time == -`k')    
	gen event_daughter_l`k' = (event_time == -`k' & kidsex01 == 2)
	}	
	replace event_l1 = 0          // base year
	replace event_daughter_l1 = 0 // base year

	* Global with event dummies
	global event_time "event_l10 event_l9 event_l8 event_l7 event_l6 event_l5 event_l4 event_l3 event_l2 event_l1 event_f0 event_f1  event_f2  event_f3  event_f4  event_f5  event_f6  event_f7  event_f8  event_f9  event_f10"
	
	global event_daughter_time "event_daughter_l10 event_daughter_l9 event_daughter_l8 event_daughter_l7 event_daughter_l6 event_daughter_l5 event_daughter_l4 event_daughter_l3 event_daughter_l2 event_daughter_l1 event_daughter_f0 event_daughter_f1  event_daughter_f2  event_daughter_f3  event_daughter_f4  event_daughter_f5  event_daughter_f6  event_daughter_f7  event_daughter_f8  event_daughter_f9  event_daughter_f10"	
	
	
	
	* Generate dummy variable for whether participant has at least one daughter (1) or not (0) at the time of survey
	
	forvalues i = 1/9 {
		
		* dummy for sex of individual child (set to missing if child not yet born at time of survey)
		gen d_daughter0`i' = (kidsex0`i' == 2)
		replace d_daughter0`i' = . if kidgeb0`i'> syear
		gen d_son0`i' = (kidsex0`i' == 1)
		replace d_son0`i' = . if kidgeb0`i'> syear
	}
	
    forvalues i = 0/2 {
		
		* dummy for sex of individual child (set to missing if child not born at time of survey) 
		gen d_daughter1`i' = (kidsex1`i' == 2)
		replace d_daughter1`i' = . if kidgeb1`i'> syear
		gen d_son1`i' = (kidsex1`i' == 1)
		replace d_son1`i' = . if kidgeb1`i'> syear
	}	
	

	gen d_daughter = .

		forvalues k=1/9 {
			replace d_daughter = 1 if d_daughter0`k' == 1
			replace d_daughter = 0 if (d_son0`k' == 1 & d_daughter != 1) 
			
		}

	* generate  variables for number of daughters and sons and kids
	egen no_daughter = rowtotal(d_daughter01 d_daughter02 d_daughter03 d_daughter04 d_daughter05 d_daughter06 d_daughter07 d_daughter08 d_daughter09 d_daughter10 d_daughter11 d_daughter12)

	egen no_son = rowtotal(d_son01 d_son02 d_son03 d_son04 d_son05 d_son06 d_son07 d_son08 d_son09 d_son10 d_son11 d_son12)	
	
	egen no_kids = rowtotal(no_daughter no_son)
	
	* employment status (fulltime, parttime/marginal/vocational training, unemployed)
	gen     empl = .
	replace empl = 1 if pgemplst == 1
	replace empl = 2 if inlist(pgemplst,2,3,4)
	replace empl = 3 if pgemplst == 5

	* years of education squared
	gen educ_sq = pgbilzeit*pgbilzeit
	gen educ = pgbilzeit
	
	* migration background: code yes/no
	recode migback (1 = 0) (2 3 = 1)
	
	save "$DATA/h2_dataset_temp.dta", replace

	* check whether children live in same household
	* merge together temp dataset that contains parent-children connectione in the same household:
	
	use "$IN/ppathl.dta", clear
	
	* use child tracking data
	merge m:1 pid using "$IN/bioparen.dta", keep(match master) keepusing(mnr fnr)
	drop _merge
	
	rename pid kid_pid
	rename hid kid_hid
	rename mnr pid
	
	* merge hid of mother
	merge m:1 pid syear using "$IN/ppathl.dta", keep(match master) keepusing(pid syear hid)
	drop _merge
	
	rename pid m_pid
	rename hid m_hid
	
	rename fnr pid
	
	* merge hid of father
	merge m:1 pid syear using "$IN/ppathl.dta", keep(match master) keepusing(pid syear hid)
	drop _merge
	
	rename pid f_pid
	rename hid f_hid
	
	* check whether same hid
	gen m_same_hh = (m_hid == kid_hid)
	gen f_same_hh = (f_hid == kid_hid)
	
	keep f_pid m_pid kid_pid syear m_same_hh f_same_hh
	
	preserve 
	
	drop f_same_hh f_pid kid_pid
	drop if m_same_hh==0
	duplicates drop
	
	* save dataset with mother-child connections
	save "$DATA/temp_m.dta", replace
	
	restore
	
	drop m_same_hh m_pid kid_pid
	drop if f_same_hh==0
	duplicates drop
	
	* save dataset with father-child connections
	save "$DATA/temp_f.dta", replace	

 	
	* back to main dataset: merge connections data
	use "$DATA/h2_dataset_temp.dta", clear
	
	* merge temp dataset with connection of kids, parents pid and household identifiers
	rename pid m_pid
	
	merge 1:1 m_pid syear using "$DATA/temp_m.dta", keep(match master) keepusing(m_pid syear m_same_hh)
	drop _merge
	
	rename m_pid pid
	rename pid f_pid

	merge 1:1 f_pid syear using "$DATA/temp_f.dta", keep(match master) keepusing(f_pid syear f_same_hh)
	drop _merge
	
	rename f_pid pid
	
	* dummy for own child living in the same household
	gen same_hh = (m_same_hh==1 | f_same_hh==1)
	drop m_same_hh f_same_hh

	
* HYPOTHESIS TEST FOR H2 *

	* Verbal description of our model: Event-study regression with year FE

	* Hypothesis 2. Having daughters leads to more left-leaning political preferences

	* Accordingly, we run a event-study regression with the leaning intensity variable as dependent variables. The event is the birth of a daughter as a first child compared to a son. We control for survey wave (syear), the employment status (empl), years of education (educ), years of education squared (educ_sq), migration background (migback) and sample region (sampreg), sex (sex), whether a child is still living in the same household (same_hh) and number of kids (no_kids) as independent variables
	
	* the identifying assumption is that conditional on giving birth, the gender of the child is exogenous. We believe this assumption is fulfilled, since it is illegal to tell the parents about the sex of an embryo before abortion can no longer be performed without risking legal punishment in Germany. Furthermore, there is some evidence that female embryos seem to have a higher chance of survival in stressful pregnancies (e.g. Hamoudi & Nobles, 2014) but this does not seem to meaningfully impact sex ratios at birth in Germany. Therefore, we believe the identifying assumption to be fulfilled.


	xtreg intensity_leaning $event_time $event_daughter_time i.syear i.empl educ educ_sq i.migback i.sampreg i.sex i.no_daughter i.no_son, fe vce(robust)

	
* test whether the linear combination of all post-event coefficients is equal to zero
	
lincom event_daughter_f0 + event_daughter_f1 + event_daughter_f2 + event_daughter_f3 + event_daughter_f4 + event_daughter_f5 + event_daughter_f6 + event_daughter_f7 + event_daughter_f8 + event_daughter_f9 + event_daughter_f10

		 
* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = r(estimate)

		* (ii) standard error
		local se = r(se)

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = r(p)

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
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
