********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
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

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	* keep if inlist(pop,1,2)
	
	* •	Respondents who participated in the individual questionnaire (pl) in any year from 1984 to 2022. 
	gen y = 1 if syear  >= 1984	& syear <= 2022
	bysort pid: egen sum_y = sum(y)
	by pid: egen min_sum_y = min(sum_y)
	drop if min_sum_y == 0
	drop min_sum_y sum_y y

	* obs: 35226 (full set)
	* •	Respondents without any biological children 
	
	drop if kidsex01 != 1 & kidsex01 != 2
	
	/*gen y = 1 if kidsex01 == 1 | kidsex01 == 2
	bysort pid syear: egen sum_y = sum(y)
	by pid syear: egen min_sum_y = min(sum_y)
	drop if min_sum_y == 0
	drop min_sum_y sum_y y*/
	
	* obs: 23631
	
	drop if pli0043_h < 0
	
	drop if pglabnet == . /* income */
	keep if pgfamstd >= 1 & pgfamstd <= 8 
	keep if bioage >= 1 & bioage < .
	
	* •	Respondents who cannot be assigned to a household in pairs  
	gen sex_pid_male  = 1 if sex == 1
		replace sex_pid_male  = 0 if sex == 2
	gen sex_pid_female  = 1 if sex == 2
		replace sex_pid_female  = 0 if sex == 1
	bysort syear hid: egen males_per_hid = sum(sex_pid_male) 
	bysort syear hid: egen females_per_hid = sum(sex_pid_female) 
	
	keep if males_per_hid == 1 & females_per_hid == 1
	
	
* Transforming person-based dataset to household-based one	
	
	keep pli0043_h pid hid syear bioage  pglabnet sex kidsex* migback 
	
	gen no_of_kids = 0
	foreach v of varlist kidsex* {
		replace no_of_kids =  no_of_kids + 1 if `v' > 0 & `v' < 3		
	}
	
	gen At_Least_1_Girl = 0
	
	foreach v of varlist kidsex* {
		replace At_Least_1_Girl = 1 if `v' == 2
	}
	
	drop kidsex*
	
	gen male_pli0043_h  = pli0043_h if sex == 1
	bysort hid syear: egen min_male_pli0043_h = min(male_pli0043_h)
	
	gen female_pli0043_h  = pli0043_h if sex == 2
	bysort hid syear: egen min_female_pli0043_h = min(female_pli0043_h)

	
	gen X = 1-abs(min_male_pli0043_h - min_female_pli0043_h) / (min_male_pli0043_h + min_female_pli0043_h)
	
	drop pid sex *pli0043_h* 
	
	order hid syear
	
	collapse migback-X, by(syear hid)
	
	* 1.	Standardization: 
    * For each item j and year, compute the z-score:    z_ij = (x_ij - μ_j) / σ_j 
	
	foreach v of varlist X{
		bysort syear: egen `v'_std = std(`v')
	}
	
	* 2.	Index construction: 
	* Compute the mean of the standardized responses for each individual: 
    * X_i = (1 / n_i) * ∑ z_ij over j in J_i, where J_i is the set of items answered by respondent i. 
	
	egen Sigma_z_ij = rownonmiss(*_std)
	egen X_rowtotal = rowtotal(*_std) 
		replace X = X_rowtotal/Sigma_z_ij
		drop Sigma_z_ij
	
* GENERATE VARIABLES * (Please adapt according to your needs)

	* Example: generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* For illustrative purposes, we will generate the variable randomly:
	set seed 123
	* gen d_daughter = runiformint(0,1)

	* controls
	
	replace bioage = ln(bioage)
	
	tab no_of_kids, gen(kidsex01kidsex19_)
			
	replace pglabnet = ln(pglabnet*2)
	
	gen migback_dummy = 1 if migback > 1 & migback != .
		replace migback_dummy = 0 if migback == 1

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	* As an example, and for illustrative purposes we consider a: linear OLS regression

	* For illustrative purposes, let's further assume that having a job that requires hard manual labor (plb0085) is a good measure of gender equality in the household (hypothesis 3),
	* if having a job with less manual labor, the respondent is more likely to experience more gender equality in the household.
	* Accordingly, we run a linear OLS regression analysis with plb0085 as dependent variable and the dummy variable d_daughter as independent variable

	tab syear, gen(__YEAR)
	*tab hid, gen (__HID)

	*reg X At_Least_1_Girl bioage  kidsex01kidsex19_*  pglabnet migback_dummy __YEAR* __HID*, robust
	
	areg X At_Least_1_Girl bioage  kidsex01kidsex19_*  pglabnet migback_dummy __YEAR* , robust absorb(hid)



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[At_Least_1_Girl]

		* (ii) standard error
		local se = _se[At_Least_1_Girl]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[At_Least_1_Girl] / _se[At_Least_1_Girl]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		* quietly levelsof pid if e(sample), local(participants)
		quietly levelsof hid if e(sample), local(participants)

		local num_participants : word count `participants'
		
		scalar participants2 = `num_participants' * 2 /* our analysis is based on couple households */
		local num_participants = participants2

		* (viii) interpretation of directionality
		scalar coef_At_Least_1_Girl = _b[At_Least_1_Girl]
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
