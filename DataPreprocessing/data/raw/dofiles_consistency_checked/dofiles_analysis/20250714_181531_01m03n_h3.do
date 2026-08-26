********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht
* Revised by Research Team RT01m03n: [REDACTED], [REDACTED]


* ============================
* Setup * 
* ============================

* clear all
version 17.0 // indicate the version of stata used here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* ssc install outreg2
* ssc install ftools
* ssc install reghdfe, replace

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

* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, relevant for the analysis. 

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)
	
	
	** Merge also with biol data to gather the remaining original variables needed.
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches)


	label language EN // change language of the labels to English
	
	
* Reduce dataset to retain only relevant variables.
keep pid hid syear ///
     parid ///
     kidsex* kidgeb* ///
	 plb0176_h ///
	 gebjahr sumkids pgpartz 
	

	save "$DATA/h3_dataset_RT01m03n.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS *

	* 1. Generate flags per child if that child is a daughter born before 2000. 
gen byte daughter_pre2000 = 0

forvalues i = 1/19 {
    local k = string(`i', "%02.0f")  // formats 1 as "01", 2 as "02", ..., 19 as "19"
    quietly replace daughter_pre2000 = 1 if kidsex`k' == 2 & kidgeb`k' < 2000
}

	* 2. Drop participants with daughters born before 2000
drop if daughter_pre2000 == 1


* GENERATE VARIABLES * 

	* 1. Generate main predictor: binary variable to measure whether a given respondent has (1) one or multiple daughters or (0) no children or one or multiple sons born by the current year.
	** Using variables kidgeb* kidsex*. Binary assessed at all panel waves (2000-2020)
	*** Note: The answer "Does not apply (coded as -2)" in var kidgeb* is assumed to measure absence of children, thus coded as 0

gen byte daughter_any = 0

forvalues i = 1/19 {
    local k = string(`i', "%02.0f")
    quietly replace daughter_any = 1 if kidsex`k' == 2 & kidgeb`k' <= syear
    quietly replace daughter_any = . if kidgeb`k' == -3 | kidgeb`k' == -1
}


	* 2. Generate main outcome: index of unequal participation in the labor market (i.e., the difference in the number of working hours between partners). 
	** Using the following items: plb0176_h measuring the agreed weekly working hours (harmonized and assessed from 2000 to 2020).
	*** Note: Differently from what indicated in the pre-analysis plan (PAP), we did not impute values for 2020, as the variable was also assessed in 2020. 
	
	* 2.1 Recode missing values to Stata system missing (.) and generate the variable age that we need in step 2.2.2 to build the covariate.
gen weekly_hours = plb0176_h
	replace weekly_hours = . if inlist(weekly_hours, -5, -3, -2)

gen age = syear - gebjahr


	* 2.2 Constructing the difference score measuring inequality among the dyad
	
	** Using the following items: pid, syear, parid (partner person number).
	*** Note: Differently from what we indicated in the PAP, we do not use the variables sex and hid to characterize dyads. Rather, we use the variable parid because, as stated in the PAP, we do not exclude same-sex partners. Our measurement allows us to "capture" inequality among partners, regardless of their sexual orientation.  
	
	* 2.2.1 Create a new frame that holds the lookup table of pid + working hours (create & copy current data to "partner lookup")
	
frame copy default partner_lookup

	* 2.2.2 In the lookup frame (dataset copy), keep relevant variables (partner's age - to generate the covariate - and partner's working hours - for the outcome). We generated the age variable before.
	
frame partner_lookup {
    keep pid syear weekly_hours age
    rename pid parid         // this is then used to match the parid in the main data and create dyads
    rename weekly_hours hours_partner
	rename age age_partner
}

	* 2.2.3 Link partner to the default frame. This matches the observations in default.dta to those in partner_lookup.dta based on equal values of variable parid and syear. 
	
frame default {
    frlink m:1 parid syear, frame(partner_lookup)
	// Using many-to-one linkage becuse multiple people may refer to the same partner in the same year or there may be duplicates
    frget hours_partner age_partner, from(partner_lookup) 
	// the frget command copies the variables and their associated metadata needed from the data in the linked frame (partner_lookup.dta) to the data in the current frame (default.dta)
    
    // Create the difference score - outcome
    gen hour_gap = weekly_hours - hours_partner
    label var hour_gap "Working hours gap: self - partner"
    // Create the mean age of parents in the household - covariate
	egen mean_age = rowmean(age age_partner)
}


	
* FURTHER FILTER *: Exclude also couples that experienced a dissolution. 

	* 1. Generate partnership status: using the pgpartz variable to assess the respondent's partnership status, distinguishing between (0) no partner and (1) spouse/registered partner, partner, probably spouse/registered partner, probably partner.
gen partner_status = .
	replace partner_status = 0 if pgpartz == 0
	replace partner_status = 1 if inlist(pgpartz, 1, 2, 3, 4)
	label define partner_lbl 0 "No partner" 1 "Has partner"
	label values partner_status partner_lbl

	
	* 2. Generate a variable that tracks whether couples experienced a dissolution of the partnership. This may include couples that did not register another partner after the dissolution, those that registered the same partner as before, and those that changed it. 
by pid (syear), sort: gen partner_status_lag = partner_status[_n-1]
gen dissolution = partner_status_lag == 1 & partner_status == 0 

	* 3. Drop couples that experienced a dissolution after the birth of a daughter. Note that the couples that changed partners within one year but did not register a dissolution are retained.
	*** Note: In contrast to what we specified in the PAP, we only exclude couples that experienced a dissolution 'after' the daughter was born.
drop if dissolution==1 & daughter_any==1



* HYPOTHESIS TEST FOR H3 * 

	* Fixed effects model.

	** We assume that the measure unequal participation in the labor market is an appropriate measure to "capture" gender inequality in the household (hypothesis 3). At higher values, the respondent is more likely to experience more gender equality in the household.

	** We run a fixed effects model with hour_gap as the dependent variable and the dummy variable daughter_any as the main independent variable. 
	
	** set data as panel so we can use Stata's panel data xt commands and the time series operators. 
xtset pid syear
		
	** We predict respondents' gender inequality in the household as a function of whether they had one or multiple daughters in the preceding panel wave. The model controls for parents' mean age and sumkids (number of children) also measured at the preceding panel waves. To include in the model all the predictors assessed in the preceding panel wave, we transformed them before running the model using the time series operator L1.
foreach var in daughter_any mean_age sumkids {
    gen `var'_L1 = L1.`var'
}
	
	** We estimate cluster-robust standard errors at the household level (using the hid variable). 
	
reghdfe hour_gap daughter_any_L1 ///
        mean_age_L1 sumkids_L1 /// 
  , absorb(hid) /// fixed effects option at the household level
  vce(cluster hid)
  
 

* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[daughter_any_L1]

		* (ii) standard error
		local se = _se[daughter_any_L1]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[daughter_any_L1] / _se[daughter_any_L1]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof hid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_daughter_any = _b[daughter_any_L1]
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
	file open myfile using "$OUT/h3_results.csv", write replace // Renamed to original naming convention by David Albrecht on 15.07.2025
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
