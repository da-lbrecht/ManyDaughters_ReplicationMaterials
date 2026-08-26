********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup 
* ============================

* clear all
version 14 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

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
	keep pid hid syear sex gebjahr pop
	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(pid sumkids kidgeb* kidsex*)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pid syear pgfamstd pgisced97 pglfs pgpartnr pgtatzeit pglabgro)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pid syear pli0043_h pli0040 pli0044_h pli0046 pli0049_h)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h3_temp.dta", replace
	
	keep pid syear sex pgpartnr pgtatzeit pglabgro pli0043_h pli0040 pli0044_h pli0046 pli0049_h
	rename pgpartnr pgpartnr_backup
	rename pid pgpartnr
	foreach x in sex pgpartnr_backup pgtatzeit pglabgro pli0043_h pli0040 pli0044_h pli0046 pli0049_h {
	rename `x' `x'_partner
	}
	
	save "$DATA/h3_partner.dta", replace
	
	use "$DATA/h3_temp.dta", clear
	merge m:1 pgpartnr syear using "$DATA/h3_partner.dta", nogen keep(master matches)
	
	keep if pid==pgpartnr_backup_partner
	
	save "$DATA/h3_dataset.dta", replace // save merged dataset
	
	erase "$DATA/h3_temp.dta"
	erase "$DATA/h3_partner.dta"
	
* ============================
* Analysis
* ============================

use "$DATA/h3_dataset.dta", clear

	* Recoding working time and income (Not working is treated as zero hours / labor income)
	recode pgtatzeit pgtatzeit_partner  (-2 = 0)
	recode pglabgro pglabgro_partner (-2 = 0)
	
	* setting all missing values to missing
	qui mvdecode _all, mv(-1/-10)

* FILTERS * (Please adapt according to your needs)
 
	* only keep participants from private households
	keep if inlist(pop,1,2)
	
	* only participants with complete information on own and partner's labor market participation and time use	
	foreach x in pgtatzeit pglabgro pli0043_h pli0040 pli0044_h pli0046 pli0049_h sex_partner pgtatzeit_partner ///
	pglabgro_partner pli0043_h_partner pli0040_partner pli0044_h_partner pli0046_partner pli0049_h_partner {
		drop if `x'==.
	}
	
	* keep only couples living within a heterosexual partnership
	keep if (sex==1 & sex_partner==2) | (sex==2 & sex_partner==1)
	

* GENERATE VARIABLES * (Please adapt according to your needs)
	
	* generate a variable capturing the number of children
	
	forvalues x=1/9 {
		rename kidgeb0`x' kidgeb`x'
		}
		
	gen nchildren=0 if sumkids==0
	replace nchildren=0 if syear<kidgeb1
	forvalues x=1/19 {
		replace nchildren=`x' if syear>=kidgeb`x'
		}
	drop kidgeb*

	* generate a variable capturing the number of daughters (Key explanatory variable)
	
	forvalues x=1/9 {
		rename kidsex0`x' kidsex`x'
		}
		
	label define label_kidsex 0 "male" 1 "female"
	forvalues x=1/19 {
		replace kidsex`x' = kidsex`x'-1
		label values kidsex`x' label_kidsex
		}
	
	forvalues x=1/19 {
		replace kidsex`x' = . if nchildren<`x'
		}
	
	egen ndaughters = rowtotal(kidsex*), missing
	replace ndaughters = 0 if nchildren==0
	
	drop kidsex*
	
	* generate a variable capturing gender equality in the household (Outcome variable)
	
	** Work Time 
	
	gen wt_male = pgtatzeit if sex==1
	replace wt_male = pgtatzeit_partner if sex_partner==1
	
	gen wt_female = pgtatzeit if sex==2
	replace wt_female = pgtatzeit_partner if sex_partner==2
	
	drop pgtatzeit pgtatzeit_partner
	
	gen E_wt = 1 - 2 * abs((wt_female / (wt_male + wt_female)) - 0.5)
	
	egen z_wt = std(E_wt)
	
	* Labor Income 
	
	gen ly_male = pglabgro  if sex==1
	replace ly_male = pglabgro_partner if sex_partner==1
	
	gen ly_female = pglabgro if sex==2
	replace ly_female = pglabgro_partner if sex_partner==2
	
	drop pglabgro pglabgro_partner
	
	gen E_ly = 1 - 2 * abs((ly_female / (ly_male + ly_female)) - 0.5)
	
	egen z_ly = std(E_ly)
	
	** Involvement in household chores
	
	egen domtime = rowtotal(pli0043_h pli0040 pli0044_h pli0046 pli0049_h), missing
	egen domtime_partner = rowtotal(pli0043_h_partner pli0040_partner pli0044_h_partner pli0046_partner pli0049_h_partner), missing
	
	drop pli0043_h pli0040 pli0044_h pli0046 pli0049_h pli0043_h_partner pli0040_partner pli0044_h_partner pli0046_partner pli0049_h_partner
	
	gen domtime_male = domtime if sex==1
	replace domtime_male = domtime_partner if sex_partner==1
	
	gen domtime_female = domtime if sex==2
	replace domtime_female = domtime_partner if sex_partner==2
	
	drop domtime domtime_partner
	
	gen E_domtime = 1 - 2 * abs((domtime_female / (domtime_male + domtime_female)) - 0.5)
	
	egen z_domtime = std(E_domtime)
	
	** Composite measure
	
	egen geneq = rowmean(z_wt z_ly z_domtime)
	
	
	* Control Variables
	*******************
	
	* Marital status
	label define label_famst 1 "married" 2 "single" 3 "seperated/divorced" 4 "widowed"
	recode pgfamstd (1 6 7 = 1) (3 = 2) (2 4 8 = 3) (5 = 4)	
	label values pgfamstd label_famst
	
	* Education
	label define label_ed 1 "less than secondary" 2 "secondary" 3 "higher"	
	recode pgisced97 (0/2 = 1) (3/4 = 2) (5/6 = 3)
	label values pgisced97 label_ed
	
	* Labor force status
	label define label_lfs 1 "employed" 2 "not employed" 3 "unemployed"
	recode pglfs (min/0 = .)  (1/5 8/10 13 = 2) (6 = 3) (11 12 = 1)
	label values pglfs label_lfs
	
	* Age and age squared 
	gen age = syear - gebjahr
	gen age_sq = age^2
	
	* Household size
	bys hid syear: egen hhsize = count(pid) 

* HYPOTHESIS TEST FOR H3

	* We estimate a linear model with our constructed measure of gender equality (geneq) as the dependent variable on the number of daughters (ndaughters)
	* as the treatment variable and further control variables (number of children, marital status, sex, age, age squared, household income, household size education
	* labor force status and the survey year). To account for individual heterogeneity, we include individual fixed effects.
	* A positive coefficient on the number of daughters (with a p-value smaller than 0.1) empirically supports H1.
	
	xtset pid syear
	
	xtreg geneq ndaughters i.nchildren i.pgfamstd i.sex age age_sq hghinc hhsize i.pgisced97 i.pglfs i.syear, fe


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[ndaughters]

		* (ii) standard error
		local se = _se[ndaughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[ndaughters] / _se[ndaughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[ndaughters]
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
