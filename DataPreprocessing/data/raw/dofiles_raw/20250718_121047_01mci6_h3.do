********************************************************************************
********************** Do-File H3: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

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
* Data Preparation 			 * 
* ============================

* LOAD AND MERGE DATA *

	use "$IN/ppathl.dta", clear
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)

	label language EN // change language of the labels to English
	
	save "$DATA/h3_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* FILTERS *
 
	* Reduce sample to private households
	keep if inlist(pop,1,2)
	drop if netto != 10 
	
	bys pid: gen nowaves = _N
	tab nowaves
	drop if nowaves == 1
	drop if partner == 0 | partner == 5 // drop respondents without partner
	
* GENERATE VARIABLES *


	* GENERATE DAUGHTER VARIABLE *	
			
		foreach v in kidgeb01 kidgeb02 kidgeb03 kidgeb04 kidgeb05 kidgeb06 ///
		kidgeb07 kidgeb08 kidgeb09 kidgeb10 kidgeb11 kidgeb12 kidgeb13 kidgeb14 ///
		kidgeb15 kidgeb16 kidgeb17 kidgeb18 kidgeb19 {
			gen cnt_`v' = 1 if `v' <= syear & `v' > 0
		}
				
		foreach var of varlist kidsex01-kidsex19 {	
			recode `var' (-2/1=0) (2=1), into(daughter`var')
		}
		
		foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
			replace cnt_kidgeb`x' = cnt_kidgeb`x' * daughterkidsex`x' 
		}

		egen daughter = rowmean(cnt_kidgeb01-cnt_kidgeb19)
		recode daughter (0 = 0) (0.01/1 = 1)
		recode daughter (. = 0) 

		
	*TIME USE (AVAILABLE IN ALL YEARS)*
	recode pli0040 pli0046 pli0051 pli0043_h pli0049_h pli0044_h plb0186_h (-100/-1 = .)

	gen dailyemp = plb0186_h/5
	tab1 pli0040 pli0046 pli0043_h pli0049_h dailyemp, m // no answers > 24 h
	
	egen timeuse = rowtotal(pli0040 pli0046 pli0043_h pli0049_h pli0044_h plb0186_h) // sum of hours in household + employment
	tab timeuse
	
	* CONTROLS *

	gen age = syear - gebjahr 
	replace age = . if age < 18
	
	recode sex (1=1) (2=0), into(male)
	
	*REDUCE DATASET
	keep pid parid syear male age daughter timeuse phrf
	
	duplicates report pid syear
	duplicates drop pid syear, force
	
	* GENERATE COUPLE ID
	tostring pid, g(pid_s)	
	tostring parid, g(parid_s)	
	
	egen copkey = concat(pid_s parid_s syear)
	egen copkey_m = concat(parid_s pid_s syear) if male == 1
	replace copkey = copkey_m if male == 1
	
	bys copkey: gen couplecount = _N
	drop if couplecount == 1
	
	drop couplecount
	
	save "$DATA/h3_dataset_full.dta", replace // save merged dataset
	
	*** females
	use "$DATA/h3_dataset_full.dta", replace
	drop if male == 1
	drop if parid == -2
           	 
	foreach x of var * {
		rename `x' `x'_women
	}

	rename pid_women pid
	rename copkey_women copkey 
	rename syear_women syear
	rename phrf_women phrf
	rename parid_women parid

	save "$DATA/h3_dataset_women.dta", replace
	
	*** males
	use "$DATA/h3_dataset_full.dta", replace
	drop if male == 0
	drop if parid == -2
           	 
	foreach x of var * {
		rename `x' `x'_men
	}

	rename pid_men pid
	rename copkey_men copkey 
	rename syear_men syear
	rename phrf_men phrf
	rename parid_men parid	
		
	save "$DATA/h3_dataset_men.dta", replace

	*** merge partner
	use "$DATA/h3_dataset_women.dta", replace
	merge 1:1 copkey using "$DATA/h3_dataset_men.dta" 
	drop if _merge != 3
	drop _merge
	
	save "$DATA/h3_dataset_analysis.dta", replace // save merged dataset

	*TIME USE (dyadic)*

	gen tuhelp = timeuse_women - timeuse_men
	clonevar timeuse_orig = tuhelp
	replace timeuse_orig = abs(tuhelp)
	gen timeuse = timeuse_orig*(-1)
	
	drop if timeuse == .
	bys pid: gen nowaves = _N
	tab nowaves
	drop if nowaves == 1
	
	sort pid syear
	bys pid: gen timeusediff = timeuse[_n] - timeuse[_n-1]
	
	gen daughter = 0
	replace daughter = 1 if daughter_women == 1 | daughter_men == 1
	
* HYPOTHESIS TEST FOR H3 *

	****************************************************************************
	*Verbal description of the model:	
	* The gender gap in total labor contributions in the household 
	* (measured as the absolute difference in reported hours spent on paid and 
	* unpaid work between male and female partners, sum of the variables: 
	* pli0040 pli0046 pli0051 pli0043_h pli0049_h pli0044_h plb0186_h) 
	* is used as a proxy for gender equality in the household (hypothesis 3). 
	* Smaller time-use gaps (higher values) between women and men indicate a 
	* more equal sharing of responsibilities.
	
	* Accordingly, we run a linear OLS regression analysis with the change score 
	* of the gender time-use gap between survey waves (variable: timeusediff) 
	* as the dependent variable, and the dummy variable daughter as the 
	* independent variable. 
	* We further control for age of both partners (variables: age_women age_men) 
	* and survey year (variable: syear) in the model. 
	* We estimate cluster-robust standard errors (cluster variable: pid). 
	* The regression is weighted (weight: phrf).
	****************************************************************************

	reg timeusediff i.daughter age_women age_men i.syear [aw=phrf], cluster(pid)

* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.daughter]

		* (ii) standard error
		local se = _se[1.daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.daughter] / _se[1.daughter]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
