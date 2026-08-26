********************************************************************************
********************** Do-File H2: #ManyDaughters Project *********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* 	clear all
	version 17
	set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
	foreach package in reghdfe estout ftools {
		ssc install `package', replace
	}

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

	* Merge ppathl with all datasets relevant for analysis
	
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) ///
		keepusing(pid sex gebjahr sumkids kidgeb* kidmon* kidsex*)
		

	* pgen contains user-friendly data on the individual level
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) ///
		keepusing(pid syear pgisced97 pgemplst pglabgro pgmonth)
    
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
		keepusing(pid syear plh0004 plh0007)
		
	
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN

	* Negative values typically indicate "Don't know", "Refused to answer",
	* "Not applicable",or "Question not asked in survey year"
	* Hence, replace negative values with missing
	ds, has(type numeric)
	foreach v of varlist `r(varlist)' {
		replace `v' = . if `v' < 0
	}
	
* (PRE-)FILTERS *
	* Keep only private households
	keep if inlist(pop,1,2)

	save "$DATA/h2_dataset.dta", replace

* ============================
* Pre-Analysis
* ============================

	use "$DATA/h2_dataset.dta", clear

* GENERATE VARIABLES

	* Outcome: Political left-right scale (plh0004)
	* Original scale: 0 = far left, 10 = far right
	* Reverse code so higher values = more left-leaning
	gen left_right_scale = 10 - plh0004 if !missing(plh0004)
	label variable left_right_scale "Political orientation (1=far right, 10=far left)"

	* Treatment: First child is daughter
	gen first_child_daughter = (kidsex01 == 2) if !missing(kidsex01)
	label variable first_child_daughter "First child is daughter"
	label define first_child_lab 0 "First child son" 1 "First child daughter"
	label values first_child_daughter first_child_lab

	* Age at survey year
	gen age_at_survey = syear - gebjahr
	label variable age_at_survey "Age at survey year"

	* Month-level dates for correct timing
	gen interview_date = ym(syear, pgmonth) if !missing(pgmonth)
	replace interview_date = ym(syear, 1) if missing(pgmonth)  // Assume January if month missing
	gen interview_month_imputed = missing(pgmonth)
	label variable interview_month_imputed "Interview month was imputed"

	* Month-level first child sex knowledge date 
	* (birth date -5 because of ultrasound child sex checks anticipation)
	gen first_know_sex = ym(kidgeb01, kidmon01) - 5 if !missing(kidmon01) & !missing(kidgeb01)
	replace first_know_sex = ym(kidgeb01, 6) - 5 if missing(kidmon01) & !missing(kidgeb01)  // Assume June birth
	gen know_sex_month_imputed = missing(kidmon01) & !missing(kidgeb01)
	label variable first_know_sex "Date of first child sex knowledge (birth - 5 months)"
	label variable know_sex_month_imputed "Sex knowledge month was imputed"

	* Time since first child sex knowledge
	gen months_since_know_sex = interview_date - first_know_sex if !missing(first_know_sex)
	gen years_since_know_sex = months_since_know_sex / 12
	label variable months_since_know_sex "Months since first child sex was known"
	label variable years_since_know_sex "Years since first child sex was known"

	* CONTROL VARIABLES

	* Pre-treatment

	* Sex
	gen male = (sex == 1) if !missing(sex)
	label variable male "Biological sex of respondent is male"
	label define male_lab 0 "Female" 1 "Male"
	label values male male_lab
	
	* Post-treatment (only for robustness)

	* Education
	rename pgisced97 education 

	* Income variables
	gen log_labor_income = ln(pglabgro + 1) if !missing(pglabgro)
	label variable log_labor_income "Log of labor income (plus 1)"
	gen log_household_income = ln(hghinc + 1) if !missing(hghinc)
	label variable log_household_income "Log of household income (plus 1)"


	* Employment status
	gen employed = (pgemplst == 1) if !missing(pgemplst)
	gen part_time = (pgemplst == 2) if !missing(pgemplst)
	gen unemployed = (pgemplst == 4 | pgemplst == 5) if !missing(pgemplst)
	gen not_in_labor_force = (pgemplst == 3 | pgemplst == 6) if !missing(pgemplst)
	label variable employed "Employed full-time"
	label variable part_time "Employed part-time"
	label variable unemployed "Unemployed"
	label variable not_in_labor_force "Not in labor force"
	
	* Has any daughter - For robustness check
    gen any_daughter = 0
    forvalues i = 1/19 {
        local ii = string(`i', "%02.0f")
        gen temp_know_sex`i' = ym(kidgeb`ii', kidmon`ii') - 5 if !missing(kidgeb`ii') & !missing(kidmon`ii')
        replace temp_know_sex`i' = ym(kidgeb`ii', 6) - 5 if !missing(kidgeb`ii') ///
			& missing(kidmon`ii') // Assume June if month missing
        replace any_daughter = 1 if kidsex`ii' == 2 & !missing(kidsex`ii') ///
            & temp_know_sex`i' <= interview_date & !missing(temp_know_sex`i')
        * Clean up temporary variable
        drop temp_know_sex`i'
    }
    label variable any_daughter "Has any daughter (based on sex knowledge date)"
    label define any_daughter_lab 0 "No daughters" 1 "Has at least one daughter"
    label values any_daughter any_daughter_lab

	* Age at first birth
	gen age_at_first_birth = kidgeb01 - gebjahr if !missing(kidgeb01) & !missing(gebjahr)
	label variable age_at_first_birth "Age at first child birth"

* SAMPLE RESTRICTIONS

	* Create matrix to store sample sizes
	matrix sample_sizes = J(4,2,.)
	matrix rownames sample_sizes = "Initial" "Parents_with_kids" "Has_political_data" ///
		"Age_16_+60"
	matrix colnames sample_sizes = "N" "Percent_remaining"

	local row = 1
	count
	local initial_n = r(N)
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = 100
	di "Initial sample: " r(N)

	* Keep only parents with at least one child and known first child sex
	local row = `row' + 1
	keep if !missing(first_child_daughter) & !missing(kidgeb01)
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After keeping only parents with known first child sex: " r(N)
	
	* Keep only observations with political scale data
	local row = `row' + 1
	keep if !missing(left_right_scale)
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After keeping only observations with political data: " r(N)

	* Age restriction
	local row = `row' + 1
	keep if age_at_survey >= 16 & age_at_first_birth < 60 & !missing(age_at_first_birth)
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After age restriction (>16 & realistic first child birth date): " r(N)

	* Display sample size table
	matlist sample_sizes, format(%12.0fc %6.2f)

	* Save analysis dataset
	save "$DATA/h2_dataset.dta", replace

* BALANCE TESTS

	* Check for panel structure
	tab syear
	sort pid syear
	by pid: gen n_observations = _N
	tab n_observations
	gen multiple_obs = (n_observations > 1)
	label variable multiple_obs "Individual observed multiple times"
	label variable n_observations "Number of observations per individual"
	
	* Summary statistics by treatment status
	estpost tabstat age_at_survey sumkids years_since_know_sex  ///
		n_observations multiple_obs male, by(first_child_daughter) ///
		statistics(mean sd count) columns(statistics)
	tab birthregion first_child_daughter, chi2 col
	* (Potentially post-treatment)
	tab education first_child_daughter, chi2 col
	tab employed first_child_daughter, chi2 col
	tab part_time first_child_daughter, chi2 col
	tab unemployed first_child_daughter, chi2 col
	tab not_in_labor_force first_child_daughter, chi2 col

	* Univariate check
	local balance_vars "gebjahr birthregion male age_at_first_birth"  
	foreach var of local balance_vars {
		quietly reg `var' first_child_daughter if !missing(`var'), robust
		local coef = _b[first_child_daughter]
		local se = _se[first_child_daughter] 
		local pval = 2*ttail(e(df_r), abs(`coef'/`se'))
		di "Balance test - `var': coef = " %6.4f `coef' ", SE = " %6.4f `se' ", p-value = " %6.4f `pval'
	}

	* Joint significance test
	reg first_child_daughter gebjahr birthregion male age_at_first_birth if !missing(gebjahr) & !missing(birthregion) & !missing(male), robust
	test gebjahr birthregion male age_at_first_birth
	local joint_p = r(p)
	di "Joint balance test p-value: " %6.4f `joint_p'

	* Test if first child sex predicts family size (differential stopping) - We will use any_daughter in robustness
	reg sumkids first_child_daughter, robust
	di "Fertility stopping test:"
	test first_child_daughter
	local stop_p = r(p)

	* Test if first child sex predicts survey participation
	reg n_observations first_child_daughter, robust
	di "Survey participation test:"
	test first_child_daughter
	local participation_p = r(p)
	
	* Summary of balance tests
	di _newline "=== BALANCE TEST SUMMARY ==="
	di "Joint balance test p-value: " %6.4f `joint_p'
	di "Differential stopping p-value: " %6.4f `stop_p'
	di "Survey participation p-value: " %6.4f `participation_p'

* ============================
* Analysis
* ============================

* HYPOTHESIS TEST FOR H2

	* For consistency of approach between all different hypotheses and given data
	* availability of outcome variables (in particular of gender attitudes for H1
	* and H4), we use a cross-sectional approach (OLS w/fixed effects) exploiting the
	* random assignment of gender of the first child, conditional on becoming a parent.
	* Hence, we prioritize causality over perfect adherence to the hypothesis, and we
	* test the effect of having a daughter instead of a son as a first child.
	* See the PAP for a further discussion on the empirical approach.
	
	* In our main specification, we only include pre-treatment controls
	* (i.e., sex of the parent and years since the first child was born + FEs for 
	* birth region and age of the parent and survey year).
	* Accordingly, we run a linear OLS regression analysis with fixed effects with the
	* reversed plh0004 question of the survey (political attitude - renamed left_right_scale) 
	* as dependent variable and the dummy variable first_child_daughter as independent variable. 
	* To limit our degrees of freedom, we use plh0004 instead of any proxy 
	* (say, converting the party preferences into right-left scale).
	* See the PAP for a further discussion on the empirical approach.
	
	* Model 1: Basic specification
	reghdfe left_right_scale first_child_daughter, absorb(syear) vce(cluster pid)
	est store h2_basic

	* Model 2: Full controls
	reghdfe left_right_scale first_child_daughter years_since_know_sex male, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h2_main

* EXTRACT AND EXPORT RESULTS * 

	* Restore main specification
	est restore h2_main

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[first_child_daughter]

		* (ii) standard error
		local se = _se[first_child_daughter]

		* (iii) family of the test statistic
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs)
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[first_child_daughter] / _se[first_child_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_first_child_daughter = _b[first_child_daughter]
		local interpretation "positive coefficient is in line with hypothesis"

	* Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"
	
	* Export to CSV
	file open results using "$OUT/h2_results.csv", write replace
	file write results "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write results "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close results

* ROBUSTNESS CHECKS

	* R1: Including (potential) post-treatment controls
	reghdfe left_right_scale first_child_daughter years_since_know_sex male ///
		education log_labor_income log_household_income, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h2_post_treat

	* R2: Single child families only
	reghdfe left_right_scale first_child_daughter years_since_know_sex male ///
		if sumkids == 1, absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h2_single_child

	* R3: Has any daughter - Here the years_since_know_sex indicates the sex of the firstborn,
	* hence not necessarily a daughter
	tab any_daughter
	reghdfe left_right_scale any_daughter years_since_know_sex male, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h2_any_daughter
	
	* R4: Exclude observation with imputed months
	reghdfe left_right_scale first_child_daughter years_since_know_sex male if ///
		interview_month_imputed == 0 & know_sex_month_imputed == 0, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h2_not_imputed
