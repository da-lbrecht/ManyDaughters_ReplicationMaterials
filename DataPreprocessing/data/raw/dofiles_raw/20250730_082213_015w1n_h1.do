********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2
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

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	
	replace bioage = bioage- (2020-syear)
	
	foreach v of varlist kidsex* {
		gen no =  regexs(1) if regexm("`v'", "([0-9]+)")
		local no = no[1]
		drop no
		di "`no'"
		replace `v' = . if kidgeb`no' > syear
	}
	
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	* keep if inlist(pop,1,2)
	
	* obs: 	35228
	* •	Respondents who participated in the individual questionnaire (pl) in any year from 2016 to 2018. 
	gen y = 1 if syear  == 2016	| syear  == 2017 | syear  == 2018
	bysort pid: egen sum_y = sum(y)
	by pid: egen min_sum_y = min(sum_y)
	drop if min_sum_y == 0
	drop min_sum_y sum_y y

	* obs: 23531
	* •	Respondents without any biological children 

		drop if kidsex01 != 1 & kidsex01 != 2
	
	/*gen y = 1 if kidsex01 == 1 | kidsex01 == 2
	bysort pid syear: egen sum_y = sum(y)
	by pid syear: egen min_sum_y = min(sum_y)
	drop if min_sum_y == 0
	drop min_sum_y sum_y y*/
	
	* obs: 17042
	* •	Respondents with fewer than 2 answered gender attitude variables
	gen number_of_answered_gen_att = 0 
	foreach v of varlist plj0625 plj0624  plj0621 plj0622 plj0623 {
		replace number_of_answered_gen_att = number_of_answered_gen_att + 1 if `v' >= 1 & `v' <= 7
	}
	keep if number_of_answered_gen_att >= 2
	* obs: 264

	* •	Respondents with missing values for the key controls 
	drop if pglabnet == . /* income */
	keep if pgfamstd >= 1 & pgfamstd <= 8 
	keep if bioage >= 1 & bioage < .

* GENERATE VARIABLES * (Please adapt according to your needs)

	* Example: generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* For illustrative purposes, we will generate the variable randomly:
	set seed 123
	
	* To ensure consistency across all survey items, we standardize the direction of responses so that higher values always indicate stronger support for gender equality.
	
	* Variables that need to be inverted:
	
	foreach v of varlist plj0625 plj0624 plj0623 {  
		gen `v'_trans = 7 if `v' == 1
		replace `v'_trans = 6 if `v' == 2
		replace `v'_trans = 5 if `v' == 3
		replace `v'_trans = 4 if `v' == 4
		replace `v'_trans = 3 if `v' == 5
		replace `v'_trans = 2 if `v' == 6
		replace `v'_trans = 1 if `v' == 7
	}
	
	* Variables that need no transformation except negative values being set to missing
	
	foreach v of varlist plj0621 plj0622 {
		gen `v'_trans = `v' if `v' >= 1 & `v' <= 7
	}
	
	* 1.	Standardization: 
    * For each item j and year, compute the z-score:    z_ij = (x_ij - μ_j) / σ_j 
	
	foreach v of varlist *_trans{
		bysort syear: egen `v'_std = std(`v')
	}
	
	* 2.	Index construction: 
	* Compute the mean of the standardized responses for each individual: 
    * X_i = (1 / n_i) * ∑ z_ij over j in J_i, where J_i is the set of items answered by respondent i. 
	
	egen Sigma_z_ij = rownonmiss(*_std)
	egen X = rowtotal(*_std) 
		replace X = X/Sigma_z_ij
		drop Sigma_z_ij
	
	gen At_Least_1_Girl = 0
	
	foreach v of varlist kidsex* {
		replace At_Least_1_Girl = 1 if `v' == 2
	}
	
	* controls
	
	replace bioage = ln(bioage)
	
	gen sex_female = 1 if sex == 2
		replace sex_female= 0 if sex == 1
	
	gen no_of_kids = 0
	foreach v of varlist kidsex* {
		replace no_of_kids =  no_of_kids + 1 if `v' > 0 & `v' < 3
		
	}
	tab no_of_kids, gen(kidsex01kidsex19_)
	
	gen pgfamstd_married = 1 if pgfamstd == 1 | pgfamstd == 6
		replace pgfamstd_married = 0 if pgfamstd_married == . 
		
	gen pgfamstd_married_bs = 1 if pgfamstd == 2
		replace pgfamstd_married_bs = 0 if pgfamstd_married_bs == . 
	
	gen pgfamstd_divorced = 1 if pgfamstd == 4
		replace pgfamstd_divorced = 0 if pgfamstd_divorced == . 	
		
	gen pgfamstd_widowed = 1 if pgfamstd == 5
		replace pgfamstd_widowed = 0 if pgfamstd_widowed == . 	
		
	replace pglabnet = ln(pglabnet)
	
	gen migback_dummy = 1 if migback == 2 | migback == 3
		replace migback_dummy = 0 if migback == 1

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	* As an example, and for illustrative purposes we consider a: linear OLS regression

	* For illustrative purposes, let's further assume that having a job that requires hard manual labor (plb0085) is a good measure of support for gender equality (hypothesis 1), 
	* if having a job with less manual labor, the respondent is more likely to support gender equality.
	* Accordingly, we run a linear OLS regression analysis with plb0085 as dependent variable and the dummy variable d_daughter as independent variable
	
	tab syear, gen(__YEAR)
	*tab pid, gen (__PID)

	*reg X At_Least_1_Girl bioage sex_female kidsex01kidsex19_* pgfamstd_married pgfamstd_married_bs pgfamstd_divorced pgfamstd_widowed  pglabnet migback_dummy __YEAR* __PID*, robust
	
	areg X At_Least_1_Girl bioage sex_female kidsex01kidsex19_* pgfamstd_married pgfamstd_married_bs pgfamstd_divorced pgfamstd_widowed  pglabnet migback_dummy __YEAR* , robust absorb(pid)

	*reghdfe X At_Least_1_Girl bioage sex kidsex01kidsex19_* pgfamstd_married pgfamstd_married_bs pgfamstd_divorced pgfamstd_widowed   migback_dummy, absorb(syear pid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[At_Least_1_Girl]

		* (ii) standard error
		local se = _se[At_Least_1_Girl ]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[At_Least_1_Girl ] / _se[At_Least_1_Girl ]
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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
