********************************************************************************
********************** Do-File H3: #ManyDaughters Project *********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

	//clear all
	version 17
	set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
	foreach package in reghdfe estout ftools {
		ssc install `package', replace
	}

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
		keepusing(pid syear pli0043_h pli0012_h pli0016_h)
		
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

	save "$DATA/h3_dataset.dta", replace

* ============================
* Pre-Analysis
* ============================

	use "$DATA/h3_dataset.dta", clear

* GENERATE INDIVIDUAL-LEVEL VARIABLES

	* Create total weekly housework hours
	* Total = weekday*5 + Saturday + Sunday
	gen weekly_housework = pli0043_h * 5 + pli0012_h + pli0016_h if !missing(pli0043_h) & !missing(pli0012_h) & !missing(pli0016_h)
	label variable weekly_housework "Total weekly housework hours"

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

	* Employment status and work hours
	gen employed = (pgemplst == 1) if !missing(pgemplst)
	gen part_time = (pgemplst == 2) if !missing(pgemplst)
	gen unemployed = (pgemplst == 4 | pgemplst == 5) if !missing(pgemplst)
	gen not_in_labor_force = (pgemplst == 3 | pgemplst == 6) if !missing(pgemplst)
	label variable employed "Employed full-time"
	label variable part_time "Employed part-time"
	label variable unemployed "Unemployed"
	label variable not_in_labor_force "Not in labor force"
		
	* Has any daughter (month-precise based on sex knowledge date)
	gen any_daughter = 0
	forvalues i = 1/19 {
		local ii = string(`i', "%02.0f")
		gen temp_know_sex_`i' = ym(kidgeb`ii', kidmon`ii') - 5 if !missing(kidgeb`ii') & !missing(kidmon`ii')
		replace temp_know_sex_`i' = ym(kidgeb`ii', 6) - 5 if !missing(kidgeb`ii') & missing(kidmon`ii')
		* Count as daughter if sex is female AND sex was known before interview
		replace any_daughter = 1 if kidsex`ii' == 2 & !missing(kidsex`ii') ///
			& temp_know_sex_`i' <= interview_date & !missing(temp_know_sex_`i')
		* Clean up temporary variable
		drop temp_know_sex_`i'
	}
	label variable any_daughter "Has any daughter (based on sex knowledge date)"
	label define any_daughter_lab 0 "No daughters" 1 "Has at least one daughter"
	label values any_daughter any_daughter_lab
	
	* Age at first birth
	gen age_at_first_birth = kidgeb01 - gebjahr if !missing(kidgeb01) & !missing(gebjahr)
	label variable age_at_first_birth "Age at first child birth"
	
	* Save individual level data
	save "$DATA/h3_individual.dta", replace

* CREATE COUPLE-LEVEL ANALYSIS DATA

	use "$DATA/h3_individual.dta", clear
	
	* Count initial sample
	count
	local initial_n = r(N)
	
	* Keep only individuals currently with partners
	keep if !missing(parid)
	
	* Create a dataset of partners' housework and income info
	preserve
		keep pid syear weekly_housework sex first_child_daughter any_daughter years_since_know_sex
		rename pid partner_pid
		rename weekly_housework partner_housework
		rename sex partner_sex
		rename first_child_daughter partner_first_daughter
		rename any_daughter partner_any_daughter
		rename years_since_know_sex partner_years_since
		save "$DATA/h3_partner_info.dta", replace
	restore
	
	* Check for duplicate partner-year combinations before merge
	rename parid partner_pid
	duplicates report partner_pid syear
	
	* Merge with partner's housework and income
	merge 1:1 partner_pid syear using "$DATA/h3_partner_info.dta"
	count if _merge==1
	di r(N) " partner IDs are not matched to personal IDs, likely due to 5% sample slice"
	keep if _merge == 3
	drop _merge
	
	* Identify couple type
	gen opposite_sex_couple = (male == 0 & partner_sex == 1) | (male == 1 & partner_sex == 2) if !missing(partner_sex)
	label variable opposite_sex_couple "In opposite-sex relationship"
	
* OUTCOME: Female share of housework (for opposite-sex couples only)
	gen female_housework = cond(male == 0, weekly_housework, partner_housework) if opposite_sex_couple == 1
	gen male_housework = cond(male == 1, weekly_housework, partner_housework) if opposite_sex_couple == 1
	gen female_housework_share = female_housework / (female_housework + male_housework) ///
		if !missing(female_housework) & !missing(male_housework) & (female_housework + male_housework) > 0
	label variable female_housework_share "Female share of housework (opposite-sex couples)"
	* Check average imbalance (so that decrease in female share is an increase in gender equality)
	summ female_housework_share
	di "Average female share of housework is: " round(r(mean)*100,0.1) "%"
	
	* Additional robustness variables:
	* Both partners are treated
	gen both_daughter = (first_child_daughter==1 & partner_first_daughter==1)
	* Either partner was treated
	gen either_daughter = (first_child_daughter==1 | partner_first_daughter==1)

* SAMPLE RESTRICTIONS

	* Create matrix to store sample sizes
	matrix sample_sizes = J(7,2,.)
	matrix rownames sample_sizes = "Initial" "Has_partner" "Has_first_child" "Has_housework_data" ///
		"Age_16_+60" "Only Opposite-Sex Couples" "Only Female"
	matrix colnames sample_sizes = "N" "Percent_remaining"

	local row = 1
	matrix sample_sizes[`row',1] = `initial_n'
	matrix sample_sizes[`row',2] = 100
	di "Initial sample: " `initial_n'

	* Already filtered for having partners
	local row = `row' + 1
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After keeping only individuals with partners: " r(N)

	* Keep only parents with known first child sex
	local row = `row' + 1
	keep if !missing(first_child_daughter) & !missing(kidgeb01)
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After keeping only parents with known first child sex: " r(N)

	* Keep only observations with housework data for both partners
	local row = `row' + 1
	keep if !missing(female_housework_share)
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After keeping only observations with complete housework data: " r(N)

	* Age restriction
	local row = `row' + 1
	keep if age_at_survey >= 16 & age_at_first_birth < 60 & !missing(age_at_first_birth)
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After age restriction: " r(N)
	
	* Keep only opposite-sex couples for more straightforward test of gender equality in household
	local row = `row' + 1
	keep if opposite_sex_couple == 1
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After keeping only opposite-sex couples: " r(N)
	
	* Keep only female partners - as in each couple both female and male partners
	* share the same female_housework_share
	local row = `row' + 1
	keep if male == 0
	count
	matrix sample_sizes[`row',1] = r(N)
	matrix sample_sizes[`row',2] = (r(N)/`initial_n')*100
	di "After keeping only female partners: " r(N)

	* Display sample size table
	matlist sample_sizes, format(%12.0fc %6.2f)

	* Save analysis dataset
	save "$DATA/h3_analysis.dta", replace

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
		n_observations multiple_obs, by(first_child_daughter) ///
		statistics(mean sd count) columns(statistics)
	tab birthregion first_child_daughter, chi2 col
	* (Potentially post-treatment)
	tab education first_child_daughter, chi2 col
	tab employed first_child_daughter, chi2 col
	tab part_time first_child_daughter, chi2 col
	tab unemployed first_child_daughter, chi2 col
	tab not_in_labor_force first_child_daughter, chi2 col

	* Univariate check
	local balance_vars "gebjahr birthregion age_at_first_birth"  
	foreach var of local balance_vars {
		quietly reg `var' first_child_daughter if !missing(`var'), robust
		local coef = _b[first_child_daughter]
		local se = _se[first_child_daughter] 
		local pval = 2*ttail(e(df_r), abs(`coef'/`se'))
		di "Balance test - `var': coef = " %6.4f `coef' ", SE = " %6.4f `se' ", p-value = " %6.4f `pval'
	}

	* Joint significance test
	reg first_child_daughter gebjahr birthregion age_at_first_birth if !missing(gebjahr) & !missing(birthregion), robust
	test gebjahr birthregion age_at_first_birth
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

* HYPOTHESIS TEST FOR H3

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
	* Accordingly, we run a linear OLS regression analysis with fixed effects with a
	* measure of female share of weekly housework (generated using pli0043_h, pli0012_h, 
	* and pli0016_h)in opposite-sex couples (identified using pid and parid) as dependent 
	* variable  and the dummy variable first_child_daughter as independent variable. 
	* We do not track partners over time by creating permanent couple IDs, but we 
	* only focus on partners identified for each specific survey year. This choice is to
	* keep the focus on individuals as in the other hypotheses testings, and we can do so
	* because the treatment is permanent (i.e., once an individual becomes parent of a daughter
	* as a first child, our independent variable becomes 1 and is carried over to any new
	* couples that the individual enters or exites).
	* See the PAP for a further discussion on the empirical approach.
	
	* Model 1: Basic specification
	reghdfe female_housework_share first_child_daughter, absorb(syear) vce(cluster pid)
	est store h3_basic

	* Model 2: Full controls
	reghdfe female_housework_share first_child_daughter years_since_know_sex, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h3_main

* EXTRACT AND EXPORT RESULTS * 

	* Restore main specification
	est restore h3_main

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
		* For female housework share: negative coefficient = more equality
		local interpretation "not linearly interpretable"
	

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
	file open results using "$OUT/h3_results.csv", write replace
	file write results "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write results "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close results

* ROBUSTNESS CHECKS

	* R1: Including (potential) post-treatment controls
	reghdfe female_housework_share first_child_daughter years_since_know_sex ///
		education log_labor_income log_household_income, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h3_post_treat

	* R2: Single child families only
	reghdfe female_housework_share first_child_daughter years_since_know_sex ///
		if sumkids == 1, absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h3_single_child

	* R3: Has any daughter
	reghdfe female_housework_share any_daughter years_since_know_sex, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h3_any_daughter
	
	* R4: Partner's treatment
	reghdfe female_housework_share first_child_daughter partner_first_daughter years_since_know_sex, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h3_partner_treat
	
	* R5: Both partners have daughters
	reghdfe female_housework_share both_daughter years_since_know_sex, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h3_both_daughter
	
	* R6: Either partner has daughter
	reghdfe female_housework_share either_daughter years_since_know_sex, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
	est store h3_either_daughter
	
	* R7: Exclude observation with imputed months
	reghdfe female_housework_share first_child_daughter years_since_know_sex if ///
		interview_month_imputed == 0 & know_sex_month_imputed == 0, ///
		absorb(syear gebjahr birthregion) vce(cluster pid)
