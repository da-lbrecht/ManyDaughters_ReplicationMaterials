********************************************************************************
****************** 	    Do-File H3: #ManyDaughters Project    ******************
********************************************************************************

* Authors: Research Team 01o7ua
* We consider this variant of analysis as our first choice, because we find it more interesting than longitudinal analysis. Additionally, the larger sample size should give more reliable results.

* ============================
* Setup * 
* ============================


* clear all
version 18 // indicate the version of stata you are using here
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

*	I kept your code as requested, but below I adjusted the folders to my needs
// global IN "C:\Source - Data - ManyDaughters\data\raw" // Commented out by David Albrecht on 08.07.2025
// global DATA "C:\Source - Data - ManyDaughters\data\derived" // Commented out by David Albrecht on 08.07.2025
// global OUT "C:\Users\mmikucka\Dropbox\Health_Inequalities\ManyDaughters\Analysis\out" // Commented out by David Albrecht on 08.07.2025
*/

* ============================
* Data Preparation * 
* ============================



* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	keep pid hid syear 		parid partner 		sex gebjahr      sexor sexorinfo
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(pid biokids sumkids kidpnr* kidgeb* kidsex*  )
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pid syear pgfamstd)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pid syear 	pli0038_h  pli0003_h  pli0007_h ///
			pli0047_v1  pli0024_h  pli0028_h pli0040 pli0054 pli0011 pli0043_h pli0012_h pli0016_h pli0044_h ///
			pli0019_h pli0022_h pli0046 pli0055 pli0057 pli0049_h pli0031_h pli0034_v4 ///
			pld0133 )

	label language EN // change language of the labels to English

	compress
	save "$DATA/h3_dataset_quasiexperimental.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h3_dataset_quasiexperimental.dta", clear




* FILTERS * 
 
	*  keep respondents 18+ years old
	gen age = syear-gebjahr
	keep if age>=18 
	
	
	* keep heterosexual respondents
	keep if sexor!=2


	* keep odd years 2001-2019 (i.e. 2001, 2003, 2005,.. 2019)
	keep if syear==2001 | syear==2003 | syear==2005 | syear==2007 | syear==2009 | syear==2011 | syear==2013 | syear==2015 | syear==2017 | syear==2019

	
	* keep respondents with at least 1 child
	gen nr_children = 0
	levelsof(syear), loc(year)
	foreach y in `year' {
		foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
			replace nr_children = nr_children + 1 if (kidgeb`i'<=`y' & kidgeb`i'>1900) & syear==`y'
		}	
	}
	keep if nr_children>=1
		
	
	* keep couples where both parthers were observed
	gen str40 partn_IDstr = strofreal(pid, "%15.0f") + "_" + strofreal(parid, "%15.0f") if sex==1 & parid!=-2
	replace partn_IDstr = strofreal(parid, "%15.0f") + "_" + strofreal(pid, "%15.0f") if sex==2 & parid!=-2
	encode partn_IDstr, gen(partn_ID)
	keep if partn_ID < .
	
	
	

* GENERATE VARIABLES * 

	* DEPENDENT VARIABLE: Similarity in time allocation between partners
	* -----------------------------------------

	* (1) Weekly hours spent on paid work (for each partner, each time point)
	recode pli0038_h pli0003_h pli0007_h pli0047_v1 pli0024_h pli0028_h (-2 = 0) (-8 -7 -6 -5 -4 -3 -1 = .)
	d pli0038_h pli0003_h pli0007_h pli0047_v1 pli0024_h pli0028_h
	gen h_w = 5 * pli0038_h + pli0003_h + pli0007_h
	gen h_edu = 5 * pli0047_v1 + pli0024_h + pli0028_h
	gen h_paid = h_w + h_edu

	* (2) Weekly hours spent on unpaid work (for each partner, each time point)
	recode 	pli0040 pli0054 pli0011 pli0043_h pli0012_h pli0016_h pli0044_h pli0019_h ///
			pli0022_h pli0046 pli0055 pli0057 pli0049_h pli0031_h pli0034_v4 (-2 = 0) (-8 -7 -6 -5 -4 -3 -1 = .)
	d pli0040 pli0054 pli0011 pli0043_h pli0012_h pli0016_h pli0044_h pli0019_h ///
			pli0022_h pli0046 pli0055 pli0057 pli0049_h pli0031_h pli0034_v4
	gen h_errands = 5 * pli0040 + pli0054 + pli0011
	gen h_hw = 5 * pli0043_h + pli0012_h + pli0016_h
	gen h_chld = 5* pli0044_h + pli0019_h + pli0022_h
	gen h_care = 5 * pli0046 + pli0055 + pli0057
	gen h_repar = 5 * pli0049_h + pli0031_h + pli0034_v4
	gen h_unp = h_errands + h_hw + h_chld + h_care + h_repar
	
	* (3) Weekly RESIDUAL hours (for each partner, each time point)
	gen h_other = 24 * 7 - h_paid - h_unp 
	
	* (4) Correct the data for overlapping activities 
	replace h_paid = . if h_other < 7  /* recode to mising if paid and unpaid work sum to more than 23h per day on average */
	replace h_unp = . if h_other < 7 
	replace h_other = . if h_other < 7 

	* (5) define all time-use variables at the couple level
	foreach var in paid unp other {
		* values for male partners
		gen tmp = h_`var' if sex==1
		egen mh_`var' = mean(tmp) , by(syear partn_ID)
		drop tmp
		* values for female partners
		gen tmp = h_`var' if sex==2
		egen fh_`var' = mean(tmp) , by(syear partn_ID)
		drop tmp
	}
	
	* (6) calculate the dissimilarity index
	gen DissInd = (abs(mh_paid/168 - fh_paid/168) + abs(mh_unp/168 - fh_unp/168) + abs(mh_other/168 - fh_other/168)) / 2
	
	* (7) reverse the index and rescale to 0-100 (index of similarity)
	gen Similarity = 100 * (1-DissInd)
	
	
	
	
	* MAIN PREDICTOR: gender of the 1st child 
	* -----------------------------------------
	gen first_daugh = kidsex01==2 if kidsex01>=1 
	
	
	
	* CONTROL VARIABLES
	* -----------------------------------------
	* Control variable: year of birth of the 1st child
	
	gen brth_y_first = kidgeb01 if kidgeb01>=1900
	sum brth_y_first
	gen Cbrth_y_first = brth_y_first - r(mean) /* variable centred at grand mean */
	
	
	* FOR THE DELAYED EFFECTS VARIANT: age of the 1st daughter
	gen fd_age = syear - kidgeb01 if first_daugh==1 & kidgeb01>=1900
	
	gen first_daugh05 = first_daugh==1 & fd_age>=0 & fd_age<=5
	gen first_daugh614 = first_daugh==1 & fd_age>=6 & fd_age<=14
	gen first_daugh15plus = first_daugh==1 & fd_age>=15
	
	* age of the 1st child
	gen fch_age = syear - kidgeb01 if kidgeb01>=1900
	
	* Control variabe: woman's age
	gen tmp = age if sex==2
	egen f_age = mean(tmp), by(pid syear)
	drop tmp
	sum f_age
	gen Cf_age = f_age-r(mean)
	
	
	

	
* FILTERS * (continued, because depend on the generated variables)

	* keep observations when both partners have valid observations
	egen both = count(sex) if Similarity<. , by(partn_ID syear)
	keep if both==2
 
	* drop observations when parents differ in reproductive biographies (= drop couples with step children); ONLY CONSIDERS THE CHILDREN BORN BEFORE LAST OBSERVATION
	egen last_obs = max(syear) if  Similarity<., by(partn_ID)
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		gen tmp = kidgeb`i' if sex==2 & kidgeb`i'<=last_obs
		egen f_ch_by`i' = max(tmp), by(partn_ID)
		drop tmp
		gen tmp = kidgeb`i' if sex==1 & kidgeb`i'<=last_obs
		egen m_ch_by`i' = max(tmp), by(partn_ID)
		drop tmp
		*
		gen tmp = kidsex`i' if sex==2
		egen f_ch_sex`i' = max(tmp), by(partn_ID)
		drop tmp
		gen tmp = kidsex`i' if sex==1
		egen m_ch_sex`i' = max(tmp), by(partn_ID)
		drop tmp
	}	
	gen discrepancies = 0
	foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
		replace discrepancies=1 if f_ch_by`i'!=m_ch_by`i' | f_ch_sex`i'!=m_ch_sex`i'
	}	
	egen Discrepancies = max(discrepancies), by(partn_ID)
	keep if Discrepancies == 0
	
	* keep one observation per couple
	egen tag = tag(partn_ID)
	keep if tag==0

	
* HYPOTHESIS TEST FOR H3 * 

	* We test whether people having a daughter as the first child have ore egalitarian time allocation than those having son as their first child
	* We run a random effects model with:
	* Dependent var: similarity in time allocation
	* Main predictor: first_daughter
	* Controls: year of birth of the 1st child (centred on the grand mean)
	
	xtset pid syear
	xtreg Similarity first_daugh c.Cbrth_y_first##c.Cbrth_y_first , re cluster(pid)

	/* DELAYED EFFECTS VARIANT	
	xtreg Similarity first_daugh05 first_daugh614 first_daugh15plus c.Cbrth_y_first##c.Cbrth_y_first c.Cf_age##c.Cf_age, re cluster(pid)
	*/
	
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[first_daugh]

		* (ii) standard error
		local se = _se[first_daugh]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[first_daugh] / _se[first_daugh]
		* local p_value = 2 * ttail(e(df_r), abs(`p_value'))
		local p_value = 2 * 2*(1-normal(abs(`p_value')))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_daughter = _b[first_daugh]
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
	// file open myfile using "$OUT/h3_results_quasiexperiment.csv", write replace // Restored original naming convention David Albrecht on 09.07.2025
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
