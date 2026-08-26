********************************************************************************
********************** Do-File H1: #ManyDaughters Project *********************
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
		keepusing(pid syear plh0308_v2 plh0298_v2 plh0302_v2)
	
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

	save "$DATA/h1_dataset.dta", replace

* ============================
* Pre-Analysis
* ============================

	use "$DATA/h1_dataset.dta", clear

* GENERATE VARIABLES

	* Outcome: Index (Anderson, 2008) of gender attitudes (from plh0302_v2, plh0298_v2, and plh0308_v2)
	
	* Attitudes towards maternal employment (plh0302_v2 & plh0298_v2)
	* Original scale: 1 = Strongly in favour, 7 = Strongly against
	* Reverse code so higher values = more left-leaning
	gen child_3_suffer = 8 - plh0302_v2 if !missing(plh0302_v2)
	gen child_6_suffer = 8 - plh0298_v2 if !missing(plh0298_v2)
										
	* Attitudes towards work-family role sharing equality (plh0308_v2)
	* 1 = Strongly disagree, 7 = Strongly agree
	rename plh0308_v2 work_family_equal

	* Anderson (2008) index of gender attitudes
	* Standardise each item (mean 0, sd 1)   
	foreach v in child_3_suffer child_6_suffer work_family_equal {
		egen z_`v' = std(`v')
		label var z_`v' "`: var label `v'' (z-score)"
	}
	correlate z_child_3_suffer z_child_6_suffer z_work_family_equal
	matrix C   = r(C)
	matrix iC  = syminv(C)
	matrix one = J(3,1,1)
	matrix tmp = one' * iC * one       // 1×1 matrix
	scalar denom = tmp[1,1]            // extract the single entry
	matrix w = (iC * one) / denom
	matrix list w                      // check weights
	* Build the weighted index
	local varlist z_child_3_suffer z_child_6_suffer z_work_family_equal
	generate double anderson_raw = 0
	local i = 1
	foreach v of local varlist {
		replace anderson_raw = anderson_raw + w[`i',1] * `v'
		local ++i
	}
	* Standardise the index
	egen gender_attitudes = std(anderson_raw)
	label var gender_attitudes "Gender-attitude index (Anderson weighting, std.)"

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
	matrix rownames sample_sizes = "Initial" "Parents_with_kids" "Has_gender_attitudes_data" ///
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

	* Keep only observations with gender attitudes data
	local row = `row' + 1
	keep if !missing(gender_attitudes)
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After keeping only observations with gender attitudes data: " r(N)

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
	save "$DATA/h1_dataset.dta", replace

* BALANCE TESTS

	* Check for panel structure
	tab syear
	
	* Summary statistics by treatment status
	estpost tabstat age_at_survey sumkids years_since_know_sex  ///
		male, by(first_child_daughter) statistics(mean sd count) columns(statistics)
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
	
	* Summary of balance tests
	di _newline "=== BALANCE TEST SUMMARY ==="
	di "Joint balance test p-value: " %6.4f `joint_p'
	di "Differential stopping p-value: " %6.4f `stop_p'

* ============================
* Analysis
* ============================

* HYPOTHESIS TEST FOR H1

	* For consistency of approach between all different hypotheses and given data
	* availability of outcome variables (in particular of gender attitudes for H1
	* and H4), we use a cross-sectional approach (OLS w/fixed effects) exploiting the
	* random assignment of gender of the first child, conditional on becoming a parent.
	* Hence, we prioritize causality over perfect adherence to the hypothesis, and we
	* test the effect of having a daughter instead of a son as a first child.
	* See the PAP for a further discussion on the empirical approach.
	
	* Given the - to our understanding - limited availability of measurements of 
	* gender attitudes in the SOEP-Core data, we use the variables that seem to capture
	* most closely two of the dimensions we are interested in: attitudes towards 
	* equality between genders in work-family balance (plh0308_v2) and attitudes 
	* towards mothers who stay in the workforce (plh0298_v2 and plh0302_v2). To 
	* obtain only one coefficient, we create a standardized index as in Anderson (2008)
	* out of the three variables. In the robustness checks, we also test the hypothesis
	* using each variable singularly.
	
	* In our main specification, we only include pre-treatment controls
	* (i.e., sex of the parent and years since the first child was born + FEs for 
	* birth region and age of the parent and survey year).
	* Accordingly, we run a linear OLS regression analysis with fixed effects with the
	* gender attitudes index variable (generated using plh0298_v2, plh0302_v2, and plh0308_v2) 
	* as dependent variable and the dummy variable first_child_daughter as independent variable. 
	* See the PAP for a further discussion on the empirical approach.

	* Model 1: Basic specification
	reghdfe gender_attitudes first_child_daughter, absorb(syear) vce(cluster pid)
	est store h1_basic

	* Model 2: Full controls
	reghdfe gender_attitudes first_child_daughter years_since_know_sex male, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h1_main

* EXTRACT AND EXPORT RESULTS * 

	* Restore main specification
	est restore h1_main

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
	file open results using "$OUT/h1_results.csv", write replace
	file write results "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write results "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close results

* ROBUSTNESS CHECKS

	* R1: Including (potential) post-treatment controls
	reghdfe gender_attitudes first_child_daughter years_since_know_sex male ///
		education log_labor_income log_household_income, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h1_post_treat

	* R2: Single child families only
	reghdfe gender_attitudes first_child_daughter years_since_know_sex male ///
		if sumkids == 1, absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h1_single_child

	* R3: Has any daughter
	reghdfe gender_attitudes any_daughter years_since_know_sex male, ///
	absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h1_any_daughter
	
	* R4_1: child_3_suffer
	reghdfe child_3_suffer first_child_daughter years_since_know_sex male, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h1_child_3_suffer
	
	* R4_2: child_6_suffer
	reghdfe child_6_suffer first_child_daughter years_since_know_sex male, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h1_child_6_suffer
	
	* R4_3: work_family_equal
	reghdfe work_family_equal first_child_daughter years_since_know_sex male, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h1_work_family_equal
	
	* R5: Exclude observation with imputed months
	reghdfe gender_attitudes first_child_daughter years_since_know_sex male if ///
		interview_month_imputed == 0 & know_sex_month_imputed == 0, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h1_not_imputed
