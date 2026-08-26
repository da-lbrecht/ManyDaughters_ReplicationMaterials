********************************************************************************
************************** H4: #ManyDaughters Project **************************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 16.1 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
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

* LOAD AND MERGE DATA *

	use "$IN/pl.dta", clear // use pl as a starting dataset 

	* Merge pl with biobirth, which contains information on fertility history	
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex01 kidgeb01 kidpnr01) 
	
	* Merge with pgen, which contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgstib pgpartnr pgtatzeit) 
	
	label language EN // change language of the labels to English
		
	save "$DATA/h3_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================
/* REMARKS: 

1. We deviate from our pre-specified outcome variable in the following way. We pre-specified to use (roughly speaking) the share of female vs. male working hours for the parents in the household and calculate its absolute devation from 1. This outcome variable does not allow producing a meaningful estimate for two main reasons. First, it implies that the same allocation of working hours leads to a different measure of gender equality depending on whether the female or male has the higher number of working hours. Second, the measure is infinity/missing once the male has zero working hours. Unfortunately, this issue only became clear to us shortly after submitting the PAP. We therefore deviate from our pre-analysis plan: We construct the working hours exactly as pre-specified but instead of focusing on the share of female vs. male working hours, we use the absolute difference in female vs. male working hours as our outcome variable. 

2. The provided data set pl does not include the variable pla0009_h although it was listed on paneldata.org as being part of the data set. We use variables pla0009_v2 and pla0009_v3 to construct pla0009_h ourselves. 

3. Our pre-analysis plan did not specify how we deal with cases where individuals provided inconsistent responses for their year of birth or sex. In this code, we set the corresponding variable to missing if an individual provided inconsistent responses across the years. 

*/

* GENERATE VARIABLES * 

	* Generate dummy variable for whether participant has a first-born daughter (1) or first-born son (0)
	gen d_daughter = kidsex01==2
	replace d_daughter=. if kidsex01<1 | kidsex01==.  // set invalid responses to missing
	
	* Generate variable for individual being male based on plh0009 (considering sex observations across years)
	gen pla0009_h = pla0009_v2
	replace pla0009_v3 = -2 if  pla0009_v3==3
	replace pla0009_h = pla0009_v3 if syear>=2019
	replace pla0009_h=. if pla0009_h<0 // set invalid responses to missing
	bysort pid: egen sex_pla = mean(pla0009_h)
	gen male = sex_pla==1
	replace male = . if sex_pla!=1&sex_pla!=2 // replace with missings if inconsistent responses across years 
	
	* Generate variable for individual working hours (helpful for variables created in the following)
	gen pgtatzeit_adjusted = pgtatzeit
	replace pgtatzeit_adjusted=. if pgtatzeit<0 // set invalid response to missing
	replace pgtatzeit_adjusted=0 if pgtatzeit_adjusted==. & pgstib>=10 & pgstib<=15 // set to zero for individuals with a missing value who are not employed
	
	* Generate variable for year of first observation of working hours after first child was born
	gen after = syear if syear>kidgeb01 & kidgeb01>0 & pgtatzeit_adjusted!=.
	bysort pid: egen first = min(after)
	drop after
	
	* Generate categorical variable for age in year of first relevant observation (considering year of birth observations across years)
	replace ple0010_h=. if ple0010_h<0
	bysort pid: egen yearofbirth_min = min(ple0010_h)
	bysort pid: egen yearofbirth_max = max(ple0010_h)
	gen yearofbirth = yearofbirth_max if yearofbirth_min==yearofbirth_max // results in missing if inconsistent responses across years
	gen age_first = first-yearofbirth
	gen age_cat = 1 if age_first>=18 & age_first<=24
	replace age_cat = 2 if age_first>=25 & age_first<=30
	replace age_cat = 3 if age_first>=31 & age_first<=40
	replace age_cat = 4 if age_first>=41 & age_first<=50
	replace age_cat = 5 if age_first>=51 & age_first<=64
	replace age_cat = 6 if age_first>=65 & age_first!=.
	
	* Adjust some of the variables to simplify their use later one
	foreach var in kidsex01 kidgeb01 kidpnr01 pgpartnr pgstib{ 
		replace `var'=. if `var'<0 // set to missing if no valid response
	}
	
	* Generate identifiers for relevant household observations as specified in pre-analysis plan
	
		* (1) Add important characteristics of partner in a given survey year (kidpnr01,kidsex01,kidgeb01,hid)
		tempfile datatemp1
		save `datatemp1'
		drop if pgpartnr==.
		keep pid hid kidpnr01 kidsex01 kidgeb01 pgpartnr syear male
		ren * partner*
		rename partnerpid pgpartnr
		rename partnersyear syear
		merge 1:m pgpartnr syear using `datatemp1', nogen keep(match using) 

		* (2) Calculate the number of couples that live in a household (based on hid) in a given year
		gen hhcouple = partnerhid==hid & hid!=. & pid==partnerpgpartnr // set to 1 if the partner has the same household id and both individuals have indicated the other one as their partner 
		bysort hid syear: egen couples=total(hhcouple), missing
		replace couples = couples/2 // since counted each individual with couple match within household
		
		* (3) Check for each household whether the first observation is potentially relevant (partner lives in same household, is of different sex, has same first born child)
		gen select=0
		replace select=1 if syear==first & partnerhid==hid & male!=partnermale & ((partnerkidpnr01==kidpnr01 & kidpnr01!=.) | (partnerkidsex01==kidsex01 & partnerkidgeb01==kidgeb01 & kidsex01!=. & kidgeb01!=.)) & male!=. & partnermale!=. & first!=. & hid!=. 
		
		* (4) Identify and assign the partner of the potentially relevant first observation across all years
		gen fixedpartner=pgpartnr if select==1 
		bysort pid (fixedpartner): replace fixedpartner = fixedpartner[_n-1] if missing(fixedpartner) & select==0 
		
		* (5) Add characteristics of fixed partner 
		tempfile datatemp2
		save `datatemp2' 
		drop if pgpartnr==.
		keep pid hid pgstib pgtatzeit_adjusted syear male age_cat
		ren * fixedp*
		rename fixedppid fixedpartner
		rename fixedpsyear syear
		merge 1:m fixedpartner syear using `datatemp2', nogen keep(match using) 

		* (6) Identify all relevant observations, order as below: (observation after birth of first child) &  (parents still live as a couple in same household) & (both parents are not pensioned) & (no other couple in the household) & (working hours are available)
		replace select=0
		replace select=1 if (syear>=first & syear!=.) & (fixedpartner==pgpartnr & fixedpartner!=. & fixedphid==hid & hid!=.) & (pgstib!=13 & fixedppgstib!=13 & pgstib!=. & fixedppgstib!=.) & (couples==1) & (pgtatzeit_adjusted!=. & fixedppgtatzeit_adjusted!=.)
		bysort pid (syear): replace select = select[_n-1] if syear>first & syear!=. & select==1 // Once criteria are violated, set select to zero for the correspondig year and all following years. 
	
	* Age categorical variable for mother and father	
	gen age_cat_dad = age_cat if male==1
	replace age_cat_dad = fixedpage_cat if male==0
	gen age_cat_mum = age_cat if male==0
	replace age_cat_mum = fixedpage_cat if male==1	
		
	* Calculate the average working hours across the relevant observations for individual and partner
	bysort pid: egen workinghours = mean(pgtatzeit_adjusted) if select==1
	bysort pid: egen partner_workinghours = mean(fixedppgtatzeit_adjusted) if select==1 

	* Calculate absolute difference in working hours
	gen difference_wh = abs(workinghours-partner_workinghours) if select==1
	
	
* FILTERS * 
	
	* Only keep one observation per household that is identified according to pre-analysis plan
	drop if select!=1     
	duplicates drop pid, force 
	duplicates drop hid, force 
	
	* Exclude participants with missing values in regressors (which also excludes individuals below 18 based on age variable and individuals without children)
	keep if d_daughter!=. & age_cat_dad!=. & age_cat_mum!=. & first!=.


* HYPOTHESIS TEST FOR H3 * 

	* Description of model: We conduct an OLS regression, where we regress the outcome variable (absolute difference in working hours between mother and farther during period after the first-born child where they live together as a couple in a household and for both the workings hours are observed) on a dummy for whether the first-born child is female. We additionally include a separate categorical variable for the age of each parent in the survey year of the first considered observation of their working hours, and a dummy for the year of the first considered observation of their working hours as regressors. We use heteroskedasticity robust standard errors. To test the hypothesis, we test whether we can reject that the coefficient of the dummy for the first-born child being female is greater than or equal to zero at the 5% significance level (based on a one-sided t-test). 

	reg difference_wh d_daughter i.age_cat_mum i.age_cat_dad i.first, vce(robust)

* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = _se[d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter] / _se[d_daughter]
		local p_value = 1-ttail(e(df_r), `p_value')

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
		local interpretation "negative coefficient is in line with hypothesis"
		
		* (iv) check that sample size does not fall below pre-specified minimum 
		if `num_participants' < 1500 {
			local remark "DO NOT INTERPRET RESULTS SINCE EFFECTIVE SAMPLE SIZE IS BELOW PRE-SPECIFIED MINIMUM (1500)"
		} 
		else {
			local remark ""
		}
		
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
	di "Remark: `remark'"

	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation, remark" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation',`remark'" _n
	file close myfile


********************************************************************************

exit
