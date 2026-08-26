********************************************************************************
********************** Do-File H2: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

//clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

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

	save "$DATA/manydaughters_h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * 

/* 
sample: private households, with kid(s), first kid born before 2005
*/
 
	* only keep participants from private households
	keep if inlist(pop,1,2)
		// (4,698 observations deleted)
	
	* only keep participants with children
	keep if sumkids != 0
		// (8,946 observations deleted)
	
	* only keep if first child is born before 2005
	keep if kidgeb01 < 2005
		// (4,103 observations deleted)

	
		* only keep if also partner is present in dataset
	drop if parid == -2
		//(13,329 observations deleted)
		
	* only keep respondents in heterosexual relationships
		* sort data by person id
		sort pid

		* Create a variable with partner's sex by merging it from own sex using parid
		*keep a dataset of pid and sex
		preserve
			keep pid sex syear
			rename pid parid  // rename to match partner id 
			rename sex partner_sex
			tempfile partners
			save `partners'
		restore

		* merge partner sex into original data by parid and syear
		merge m:1 parid syear using `partners', keep(match master) nogenerate

		* keep only couples: where sex != partner_sex
		keep if sex != partner_sex
			// (28 observations deleted)
			

********************************************************************************

* GENERATE VARIABLES * INDEPENDENT

	* daughter
		* dummy: 1 if respondent has at least 1 daughter
		* 0 if respondent has sons only
		* childless respondents are already dropped and not part of our sample

	gen daughter = 0
		forval i = 1/18 {
			replace daughter = 1 if kidsex`=string(`i', "%02.0f")' == 2
		}
		
	label var daughter "At least one daughter"
	label def daughter 1 "min 1 daughter" 0 "sons only"
	label val daughter daughter 
	
********************************************************************************

* GENERATE VARIABLES * DEPENDENT

	* H2: left leaning political preference
	ta plh0004 // political attitude
			   // 0 competely left, 10 completely right
	ta syear plh0004
		* we use plh0004 for 2019, to keep the results comparable to the 
		* other hypotheses for which DV's are present only for 2016-2018
		
	* replace as missing if values are negative
		replace plh0004 = . if plh0004 < 0
		
	* reverse variable so that higher value means more left-leaning attitude
	revrs plh0004 
		* new variable generated: revplh0004
	

********************************************************************************
	
* GENERATE VARIABLES * CONTROLS 

	* sisters of respondents
	gen sister = 0

	forval i = 17(3)44 {
		replace sister = 1 if pla00`i' == 2
	}
	
	

	label var sister "has at least 1 sister"
	label define sister 1 "yes" 0 "no"
	label val sister sister
		** however, sister as sibling only measured in 2013
		** so we omit it from analysis

	* year of birth
	ta gebjahr // use as i.gebjahr
	
	* income (personal, net)
	ta pglabnet
		* replace negative values = .
		gen pglabnet_mi = pglabnet
		replace pglabnet_mi = . if pglabnet < 0 
		* income is often skewed, check for that
		summarize pglabnet_mi // right skewed --> use log income instaed
		* log transformation
		gen log_netincome = log(pglabnet_mi) // use as c.log_netincome

	* education
	ta pgisced97
		* replace negative values = .
		gen pgisced97_mi = pgisced97
		replace pgisced97_mi = . if pgisced97 == -1
	
	ta pgisced97_mi // use as i.pgisced97_mi 

	* marital status survey year
	ta pgfamstd
		* replace negative values = .
		gen pgfamstd_mi = pgfamstd
		replace pgfamstd_mi = . if pgfamstd == -1 | pgfamstd == -2 | pgfamstd == -3
		
	ta pgfamstd_mi // use as i.pgfamstd_mi
	
	* migration background
	ta migback // i.migback
	
	* male
	gen male = . 
	replace male = 1 if sex == 1
	replace male = 0 if sex == 2

	* count person id
	sort pid syear 
	by pid: gen pid_count = _n
********************************************************************************
********************************************************************************

* START  ANALYSIS 

********************************************************************************
********************************************************************************

* HYPOTHESIS TEST FOR H2 * 

	* having daughters --> more left leaning political preferences 
	* OLS regression
	* robust standard errors to account for heteroskedasticity 
		* (possibility that error term is not constant across respondents)
	
	reg revplh0004 daughter 													///
		i.gebjahr c.log_netincome i.pgisced97_mi i.pgfamstd_mi i.migback male	///
		if syear == 2019 & pid_count == 1, 										///
		vce(robust)
/*
clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2


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

* LOAD AND MERGE DATA * 

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

	save "$DATA/manydaughters_h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * 

/* 
sample: private households, with kid(s), first kid born before 2005
*/
 
	* only keep participants from private households
	keep if inlist(pop,1,2)
		// (4,698 observations deleted)
	
	* only keep participants with children
	keep if sumkids != 0
		// (8,946 observations deleted)
	
	* only keep if first child is born before 2005
	keep if kidgeb01 < 2005
		// (4,103 observations deleted)

	
		* only keep if also partner is present in dataset
	drop if parid == -2
		//(13,329 observations deleted)
		
	* only keep respondents in heterosexual relationships
		* sort data by person id
		sort pid

		* Create a variable with partner's sex by merging it from own sex using parid
		*keep a dataset of pid and sex
		preserve
			keep pid sex syear
			rename pid parid  // rename to match partner id 
			rename sex partner_sex
			tempfile partners
			save `partners'
		restore

		* merge partner sex into original data by parid and syear
		merge m:1 parid syear using `partners', keep(match master) nogenerate

		* keep only couples: where sex != partner_sex
		keep if sex != partner_sex
			// (28 observations deleted)
			

********************************************************************************

* GENERATE VARIABLES * INDEPENDENT

	* daughter
		* dummy: 1 if respondent has at least 1 daughter
		* 0 if respondent has sons only
		* childless respondents are already dropped and not part of our sample

	gen daughter = 0
		forval i = 1/18 {
			replace daughter = 1 if kidsex`=string(`i', "%02.0f")' == 2
		}
		
	label var daughter "At least one daughter"
	label def daughter 1 "min 1 daughter" 0 "sons only"
	label val daughter daughter 
	
********************************************************************************

* GENERATE VARIABLES * DEPENDENT

	* H2: left leaning political preference
	ta plh0004 // political attitude
			   // 0 competely left, 10 completely right
	ta syear plh0004
		* we use plh0004 for 2019, to keep the results comparable to the 
		* other hypotheses for which DV's are present only for 2016-2018
		
	* replace as missing if values are negative
		replace plh0004 = . if plh0004 < 0
		
	* reverse variable so that higher value means more left-leaning attitude
	revrs plh0004 
		* new variable generated: revplh0004
	

********************************************************************************
	
* GENERATE VARIABLES * CONTROLS 

	* sisters of respondents
	gen sister = 0

	forval i = 17(3)44 {
		replace sister = 1 if pla00`i' == 2
	}
	
	

	label var sister "has at least 1 sister"
	label define sister 1 "yes" 0 "no"
	label val sister sister
		** however, sister as sibling only measured in 2013
		** so we omit it from analysis

	* year of birth
	ta gebjahr // use as i.gebjahr
	
	* income (personal, net)
	ta pglabnet
		* replace negative values = .
		gen pglabnet_mi = pglabnet
		replace pglabnet_mi = . if pglabnet < 0 
		* income is often skewed, check for that
		summarize pglabnet_mi // right skewed --> use log income instaed
		* log transformation
		gen log_netincome = log(pglabnet_mi) // use as c.log_netincome

	* education
	ta pgisced97
		* replace negative values = .
		gen pgisced97_mi = pgisced97
		replace pgisced97_mi = . if pgisced97 == -1
	
	ta pgisced97_mi // use as i.pgisced97_mi 

	* marital status survey year
	ta pgfamstd
		* replace negative values = .
		gen pgfamstd_mi = pgfamstd
		replace pgfamstd_mi = . if pgfamstd == -1 | pgfamstd == -2 | pgfamstd == -3
		
	ta pgfamstd_mi // use as i.pgfamstd_mi
	
	* migration background
	ta migback // i.migback
	
	* male
	gen male = . 
	replace male = 1 if sex == 1
	replace male = 0 if sex == 2

	* count person id
	sort pid syear 
	by pid: gen pid_count = _n
	*/
********************************************************************************
********************************************************************************

* START  ANALYSIS 

********************************************************************************
********************************************************************************

* HYPOTHESIS TEST FOR H2 * 

	* having daughters --> more left leaning political preferences 
	* OLS regression
	* robust standard errors to account for heteroskedasticity 
		* (possibility that error term is not constant across respondents)
	
	reg revplh0004 daughter 													///
		i.gebjahr c.log_netincome i.pgisced97_mi i.pgfamstd_mi i.migback male	///
		if syear == 2019 & pid_count == 1, 										///
		vce(robust)


********************************************************************************
********************************************************************************

* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[daughter]

		* (ii) standard error
		local se = _se[daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[daughter] / _se[daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

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
*/
exit
