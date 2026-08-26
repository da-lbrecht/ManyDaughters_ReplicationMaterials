********************************************************************************
********************** Do-File H4: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

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

	save "$DATA/manydaughters_h4_dataset.dta", replace // save merged dataset


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
			// (0 observations deleted)
		
		* keep only years 2016, 2017, 2018
		keep if syear == 2016 | syear == 2017 | syear == 2018
			// (11,984 observations deleted)

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
	
	
	* male
	gen male = . 
	replace male = 1 if sex == 1
	replace male = 0 if sex == 2
	
********************************************************************************

* GENERATE VARIABLES * DEPENDENT

	* H4: gender equality as measured in attitudes
	
	ta plj0620 // women same rights as men 
			   // this variable should range from 
				   // 0: Should definitely not happen in a democracy
				   // 10: Should definitely happen in a democracy
			   // but given our sample restrictions it ranges from 4 to 10,
			   // so we do not use it
			   
	ta plj0621 // working makes women independent 
			   // 1: disagree completely
			   // 7: agree completely 
		* replace as missing if values are negative
		replace plj0621 = . if plj0621 <1
			   
	ta plj0622 // working makes married women independent 
			   // 1: disagree completely
			   // 7: agree completely 
		* replace as missing if values are negative
		replace plj0622 = . if plj0622 <1
			   
	ta plj0623 // women earns more than partner, problem 
			   // 1: disagree completely
			   // 7: agree completely 
		* replace as missing if values are negative
		replace plj0623 = . if plj0623 <1
						* --> needs to be reversed 
						revrs plj0623
			   
	ta plj0624 // education of son more important than daughter 
			   // 1: disagree completely
			   // 7: agree completely 
		* replace as missing if values are negative
		replace plj0624 = . if plj0624 <1
						* --> needs to be reversed 
			   			revrs plj0624

	ta plj0625 // husband should have final say 
			   // 1: disagree completely
			   // 7: agree completely 
		* replace as missing if values are negative
		replace plj0625 = . if plj0625 <1
						* --> needs to be reversed 
			   			revrs plj0625

	* We want to combine the above variables on attitudes into one variable 
	* test if scales can be combined
		alpha  plj0621 plj0622 revplj0623 revplj0624 revplj0625, item

		/*
		Test scale = mean(unstandardized items)
		Reversed item:  revplj0624

		Average interitem covariance:     .2701443
		Number of items in the scale:            5
		Scale reliability coefficient:      0.2928
		--> poor scale reliability (threshold 0.7)
		*/

		alpha plj0621 plj0622

		/*
		Test scale = mean(unstandardized items)

		Average interitem covariance:     1.809256
		Number of items in the scale:            2
		Scale reliability coefficient:      0.9008
		
		--> we use this
		*/
			
	
	* summarize attitude values
	//egen combined_attitudes = rowmean(plj0620_7pt plj0621 plj0622)
	egen combined_attitudes = rowmean(plj0621 plj0622)
	
				* the combined variable (combined_attitudes) contains too 
				* many small categories 
				* to ease the interpretation of the analysis we create a variable
				* with 6 categories 
				gen combined_attitudes_round = round(combined_attitudes, 1)
					/*
					. ta combined_attitudes_round

					combined_at |
					titudes_rou |
							 nd |      Freq.     Percent        Cum.
					------------+-----------------------------------
							  2 |          1        1.41        1.41
							  3 |          4        5.63        7.04
							  4 |          8       11.27       18.31
							  5 |          2        2.82       21.13
							  6 |          8       11.27       32.39
							  7 |         48       67.61      100.00
					------------+-----------------------------------
						  Total |         71      100.00
					*/
					* --> scale goes from 2 to 7, we want it from 1 to 6 instead
				* round
				replace combined_attitudes_round = (round(combined_attitudes, 1))-1
				
				
			* generate average of attitudes combined for 2016, 2017, 2018
			egen att_mean_pid = mean(combined_attitudes_round), by(pid)
			replace combined_attitudes_round = att_mean_pid if missing(combined_attitudes_round)
			drop att_mean_pid

			*rename final dependent variable: 
			rename combined_attitudes_round  genderatt_1618

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
		** however, migration background is always == 2 for all respondents 
		** who answered attitudinal questions
		** so we omit it from analysis		
		
	* count variable per pid, to ensure that every respondent is in analysis once
	sort pid syear 
	by pid: gen pid_count = _n
	
	
********************************************************************************
********************************************************************************

* START  ANALYSIS 

********************************************************************************
********************************************************************************

* HYPOTHESIS TEST FOR H4 * 

	* having daughters --> stronger support for gender equality (attitudes), more for men
	* OLS regression
	* interaction between having daughter and being male 
	* robust standard errors to account for heteroskedasticity 
		* (possibility that error term is not constant across respondents)
/*	
	reg genderatt_1618 daughter##male 											///
		i.gebjahr c.log_netincome i.pgisced97_mi i.pgfamstd_mi 				 	///
		if syear == 2016 | syear == 2017 | syear == 2018 & pid_count == 1, 		///
		vce(robust)

		note:
		the N of the 5% sample is too small to run the above regression. in the 
		following, we omit the control for income, to make the regression work
		on the 5% sample. if you run the code on the full sample, please try
		the above regression code
*/

	reg genderatt_1618 daughter##male 											///
		i.gebjahr i.pgisced97_mi i.pgfamstd_mi 				 					///
		if syear == 2016 | syear == 2017 | syear == 2018 & pid_count == 1, 		///
		vce(robust)
		
********************************************************************************
********************************************************************************

* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coef_interaction = _b[1.daughter#1.male]

		* (ii) standard error
		local se = _se[1.daughter#1.male]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.daughter#1.male] / _se[1.daughter#1.male]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.daughter#1.male]
		local interpretation "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coef_interaction'"
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
	file write myfile "`coef_interaction', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************
*/
exit
