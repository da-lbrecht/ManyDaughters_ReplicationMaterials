********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2
ssc install ivreg2

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

* GENERATE VARIABLES * (Please adapt according to your needs)
	
	* Identification variables
		gen has_daughter = 0
		forvalues i = 1/19 {
		local varname : display %02.0f `i'
		replace has_daughter = 1 if kidsex`varname' == 2
		}
		replace has_daughter = . if inrange(kidsex01,-3,-1)
		
		gen first_daughter = .
		replace first_daughter = 1 if kidsex01 == 2
		replace first_daughter = 0 if kidsex01 == 1
		
	
	* Control variables
	
		gen number_kids = 0
		forvalues i = 1/19 {
		local varname : display %02.0f `i'
		replace number_kids = number_kids +1 if inrange(kidsex`varname',1,2)
		}
		
		gen male_parent = .
		replace male_parent = 1 if sex == 1
		replace male_parent = 0 if sex == 2
		
		gen region = sampreg
		replace region = . if sampreg ==-2
		
		gen year = syear
		gen age  = year - gebjahr 
		
		gen parental_age  = year - kidgeb01 
		replace parental_age =. if inrange(kidgeb01,-3,-1)
		
		gen year_edu = pgbilzeit
		replace year_edu =. if inrange(kidgeb01,-2,-1)
		
		* Create quintiles of hghinc within each year
		gen labor_inc 	   = pglabgro
		gen secondary_inc  = pgsndjob
		
		foreach var in labor_inc secondary_inc {
		replace `var' = 0 if inrange(`var',-10,-1)
		}

		gen income = labor_inc+secondary_inc
			
		gen income_groups = .
		gen income_year = .
		
		levelsof year, local(years)
		foreach y of local years {
		count if year == `y'
		if r(N) >= 5 {
		quietly {
			replace income_year   = .
			replace income_year   = income       if year == `y'
			xtile   temp_group    = income_year  if year == `y', nq(4)
			replace income_groups = temp_group   if year == `y'
			drop temp_group
				}
			}
		}	
		
		global controls male_parent i.year age parental_age year_edu i.income_groups i.region number_kids
	
	
	* Dependent variable
		gen plj0620_clean = .
		replace plj0620_clean = plj0620 if inrange(plj0620, 0, 10)

		foreach i in plj0621 plj0622 plj0623 plj0624 plj0625 {
		gen `i'_clean = .
		replace `i'_clean = `i' if inrange(`i', 1, 7)
		}

		* change label: higher label should be more gender equality
		foreach i in plj0623 plj0624 plj0625 {
		replace `i'_clean = 1 if `i' == 7
		replace `i'_clean = 2 if `i' == 6
		replace `i'_clean = 3 if `i' == 5
		replace `i'_clean = 4 if `i' == 4
		replace `i'_clean = 5 if `i' == 3
		replace `i'_clean = 6 if `i' == 2
		replace `i'_clean = 7 if `i' == 1
		}
		
		* normalize to zero mean and unit variance
		foreach var in plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 {
		su `var'_clean
		gen `var'_z = (`var'_clean - r(mean)) / r(sd)
		replace `var'_z = . if `var' == .
		}
		
		egen GenderAttitude = rowmean(plj0620_z plj0621_z plj0622_z plj0623_z plj0624_z plj0625_z)
	
	
	

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Please add a verbal description of your model here
	
	* We consider the variable GenderAttitude a suitable measure of support for 
	* gender equality, where higher values indicate greater support. To estimate 
	* the effect of having at least one daughter on gender attitudes, we conduct 
	* an instrumental variable (IV) estimation using linear OLS regression. 
	* Specifically, we instrument the variable HasDaughter with first_daughter 
	* that indicates if the first born child is a daughter.

	ivreg2 GenderAttitude (has_daughter = first_daughter) $controls if parental_age >= 10, robust
	
	mat b = e(b)    // Store coefficients
	mat V = e(V)    // Variance-covariance matrix


* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = b[1,"has_daughter"]

		* (ii) standard error
		local se = sqrt(V[rownumb(V, "has_daughter"), colnumb(V, "has_daughter")])

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(N)-e(df_m)-1
		local df_model = e(df_m)

		* (v) p-value
		local p_value = `coefficient' / `se'
		local p_value = 2 * ttail(`df_residual', abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[has_daughter]
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
	file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
