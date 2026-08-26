********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht
* Team: 010cqb

* ============================
* Setup *
* ============================

* clear all
version 18 // indicate the version of stata you are using here
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

* LOAD AND MERGE DATA * 

	* use  pl dataset as starting point
	use "$IN/pl.dta", clear // 
	
	* keep only those of the survey years 2005, 2009, 2014 and 2019 (i.e., where dependent variable was elicited)
	keep if syear == 2001 | syear == 2003 | syear == 2005 | syear == 2007 | syear == 2009 | syear == 2011 | syear == 2013 | syear == 2015 | syear == 2017 | syear == 2019 
	
	* merge with biobirth
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	
	* merge with pgen to obtain highest obtained level of education
	merge m:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	
	
	* Now we create covariates, if necessary 
	
		* Number of daughters
		
			* Create new variables, number of daughter and children up until survey year
			gen num_daughters = 0
			gen num_children = 0
			
			* Loop over child slots 1 to 19
			forval i = 1/19 {
				local sexvar = "kidsex" + string(`i', "%02.0f")
				local yearvar = "kidgeb" + string(`i', "%02.0f")
			
				* Check that both variables exist
				capture confirm variable `sexvar'
				if !_rc {
					capture confirm variable `yearvar'
					if !_rc {
						* Only include child if birth year ≤ ssyear
						replace num_children = num_children + ( `sexvar' > 0 & `yearvar' <= syear)
						replace num_daughters = num_daughters + (`sexvar' == 2 & `yearvar' <= syear)
					}
				}
			}
			
			
			* Count members per household
			bysort hid syear: gen members = _N
			keep if members == 2
			
	bysort hid syear (pid): gen person_num = _n
			
	* Keep only necessary variables
	keep pid pla0009_v2 num_daughters num_children plb0022_h pli0019_h pli0022_h hid syear

	* Drop individuals with no children
	drop if num_children <= 0

	* drop individuals where gender is not elicited
	* Deviation from PAP: we state to use pla0009_h. However, this is not present in the pl dataset.
	* Therefore we use pla0009_v2.
	drop if pla0009_v2 == -5

	* Sort so that females come first (female = 2, so reverse the order)
	gen sort_gender = -pla0009_v2  // female (2) → -2, male (1) → -1

	* Generate person number (so female = person_num 1, male = person_num 2)
	bysort hid syear (sort_gender pid): gen person_num = _n

	* Drop helper variable
	drop sort_gender

	* Reshape so person_num 1 = female, 2 = male
	reshape wide pid num_children num_daughters pli0019_h pli0022_h pla0009_v2 plb0022_h, ///
		i(hid syear) j(person_num)
		
		
	
	* merge with biobirth
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) keepusing(hlc0005_h)
	
	* drop missings
	drop if missing(pid1)
	drop if missing(pid2)
	
	* remove same sex houesholds
	drop if pla0009_v21 == pla0009_v22
			
	* additionally check if same number of children and daugthers within household	
	gen check_children = (num_children1 == num_children2)
	gen check_daughters = (num_daughters1 == num_daughters2)
	
	* drop households without children
	drop if check_children == 0
	drop if check_daughters == 0
	

	
	* drop those with no income reported
	drop if hlc0005_h == -1 
	
	
	
	* calculate share of childcare taken over by fathers on weekends; suffix 2 denotes fathers share 
	gen mother_hours = (pli0019_h1 + pli0022_h1)
	gen father_hours = (pli0019_h2 + pli0022_h2)
	
	gen father_share = father_hours / (mother_hours + father_hours)
	
	* drop if they are missing as might be that parents indicate 0 hours on weekends
	drop if missing(father_share)
	
	drop num_children2 num_daughters2
	
	
	* rename 
	rename num_children1 num_children
	rename num_daughters1 num_daughters
	
	rename plb0022_h1 employment_mother
	rename plb0022_h2 employment_father
	
	
	* dave dataset
	save "$DATA/h3_dataset.dta", replace // save preprocessed dataset
	

* ============================
* Analysis
* ============================
 
	* Filter
	* drop where only one observations, can't use fixed effects
	bysort hid: gen n_obs = _N
	gen singleton = n_obs == 1
	tab singleton
	drop if singleton == 1 
	

* GENERATE VARIABLES *
	

* HYPOTHESIS TEST FOR H3 * (

	* Description:
	* Again, we assume that nature randomly assigns the gender of children and based on the subset
	* of households who became parents (unobserved variables might drive the decision to become a parent
	* in the first place), we estimate the effect of one additional daughter on the share of childcare
	* fathers take over on weekends (i.e., Staturday and Sunday).
	* Additionally, we include control variables for income and employment status of mother and father, respectively, as well as household fixed effects. Finally,
	* we use cluster robust standard errors on the household level.


	
	xtset hid syear
	
	xtreg father_share num_daughters i.num_children i.employment_mother i.employment_father hlc0005_h  i.syear, fe cluster(hid)



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[num_daughters]

		* (ii) standard error
		local se = _se[num_daughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		* Note that accroding to our PAP we would define p < 0.005 to indicate significance
		local p_value = _b[num_daughters] / _se[num_daughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof hid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[num_daughters]
		local interpretation "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di	"Number of households: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
