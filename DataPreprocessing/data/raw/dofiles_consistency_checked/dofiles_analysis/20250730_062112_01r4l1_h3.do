********************************************************************************
******************       Do-File H4: #ManyDaughters Project   ******************
********************************************************************************

* Authors: 

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 19 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2
* ssc install drdid
* ssc install csdid

* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}

di "$ROOT"

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
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgisced11 pgfamstd pgemplst pgpartnr)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pli0019_h pli0012_h pli0022_h pli0016_h pli0044_h pli0043_h pla0009_v2)
	
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hgnuts1 hgnuts1_ew)

	* hl contains variables on household level from the household interview;
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) keepusing(hid syear hlc0005_h hlf0019_h hlk0006)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
	
	**** Create Heterosexual couples ***
	* Gender
	gen male = (pla0009_v2 == 1)
	
	* Find Partners in the data*
	
	* Clean up partner variable nad keep only of partner recorded
	mvdecode pgpartnr, mv(-9/-1 = .)

	* Step 2: Drop people without a partner ID
	drop if missing(pgpartnr)
	
	* Step 3: hholds with two partners
	egen hh_size = count(pid), by(hid syear)
	keep if hh_size == 2
		
	* Step 4: Sort so both people are together
	sort hid syear pid

	* Step 5: Check mutual partnership
	gen mutual_partner = 0
	bysort hid syear (pid): ///
	    replace mutual_partner = 1 if pgpartnr[1] == pid[2] & pgpartnr[2] == pid[1]

	* Step 6: Keep only mutual couples
	bysort hid syear (pid): egen mutual_couple = max(mutual_partner)
	keep if mutual_couple == 1
	
	* Step 7: Check that each couple has exactly one male and one female
	egen males_in_couple = total(male == 1), by(hid syear)
	egen females_in_couple = total(male == 0), by(hid syear)

	* Drop households that are not 1M + 1F
	drop if males_in_couple != 1 | females_in_couple != 1

	
	**** CREATE INPUTS to the final OUTCOME VARIABLE ****	

	mvdecode pli0019_h pli0012_h pli0022_h pli0016_h pli0044_h pli0043_h, mv(-9/-1 = .)
	drop if missing(pli0019_h, pli0012_h, pli0022_h, pli0016_h, pli0044_h, pli0043_h)
	
	* 1. Create weekly time use variables
	gen childcare_week = 5 * pli0044_h + pli0019_h + pli0022_h
	gen chores_week    = 5 * pli0043_h + pli0012_h + pli0016_h
	gen childcare_plus_chores = childcare_week + chores_week

	* 2. Drop implausible cases (>168 hours/week)
	gen flag_too_many_hours = (childcare_plus_chores > 168)
	drop if flag_too_many_hours

	* 3. Assign male and female hours
	gen male_hours   = .
	gen female_hours = .

	replace male_hours   = childcare_plus_chores if male == 1
	replace female_hours = childcare_plus_chores if male == 0

	*** CREATE TREATMENT VARIABLE ***
	* Does a row have a daughter
	gen d_daughter = 0
	foreach var of varlist kidsex01-kidsex19 {
	    replace d_daughter = 1 if `var' == 2
	}

	* Does have kids?
	gen has_kids = (sumkids > 0)

	*** CREATE CONTROL VARIABLES

	* employment status
	mvdecode pgemplst, mv(-9/-1 = .)
	gen labor_force_status = .
	label define labor_lbl 1 "Full-Time" 2 "Part-Time" 3 "Not in Labor Force"

	replace labor_force_status = 1 if pgemplst == 1
	replace labor_force_status = 2 if inlist(pgemplst, 2, 3, 4)
	replace labor_force_status = 3 if inlist(pgemplst, 5, 6)
	
	* Separate labor status for male and female
	gen male_labor_status   = .
	gen female_labor_status = .

	replace male_labor_status   = labor_force_status if male == 1
	replace female_labor_status = labor_force_status if male == 0

	* income category
	gen income_cat = .

	replace income_cat = 1 if hlc0005_h < 1000
	replace income_cat = 2 if inrange(hlc0005_h, 1000, 1999)
	replace income_cat = 3 if inrange(hlc0005_h, 2000, 2999)
	replace income_cat = 4 if inrange(hlc0005_h, 3000, 3999)
	replace income_cat = 5 if inrange(hlc0005_h, 4000, 4999)
	replace income_cat = 6 if inrange(hlc0005_h, 5000, 5999)
	replace income_cat = 7 if inrange(hlc0005_h, 6000, 6999)
	replace income_cat = 8 if inrange(hlc0005_h, 7000, 7999)
	replace income_cat = 9 if inrange(hlc0005_h, 8000, 8999)
	replace income_cat = 10 if inrange(hlc0005_h, 9000, 9999)
	replace income_cat = 11 if hlc0005_h >= 10000


	* East/West Germany
	mvdecode hgnuts1_ew, mv(-9/-1 = .)
	
	* Number of people in household
	* We are approx-ing this with hlk0006 (highest person number) since ///
	  hgnrpers is not included in the data 
	 mvdecode hlk0006, mv(-9/-1 = .)  
	
	* Now prepare data for Callaway & Sant'Anna style estimation with repeated cross section
	* Step 1: Create a variable for birth year of each daughter
	forvalues i = 1/19 {
	    local j = string(`i', "%02.0f")
	    gen dtr`i' = kidgeb`j' if kidsex`j' == 2 & kidgeb`j' > 0
	}
	egen first_daughter_year = rowmin(dtr1-dtr19)

	* Step 2: Replace missing with 0 to indicate never treated
	replace first_daughter_year = 0 if missing(first_daughter_year)

	tab first_daughter_year

	* Optional: Clean up intermediate variables
	drop dtr1-dtr19
	
	* Only private households before collapse
	keep if pop==1 | pop==2
	
	* --------------------------------------------
	* Collapse to one row per household-year
	* (since each hid-year has only one heterosexual couple)
	* --------------------------------------------
	collapse (mean) male_hours female_hours hlf0019_h hlk0006 ///
         (max)  male_labor_status female_labor_status ///
                d_daughter has_kids income_cat hgnuts1_ew first_daughter_year, ///
         by(hid syear)
	 
	count if missing(male_hours, female_hours, male_labor_status, female_labor_status, hlf0019_h, ///
                 hlk0006, d_daughter, has_kids, income_cat, hgnuts1_ew, first_daughter_year)
	
	drop if missing(male_hours, female_hours, male_labor_status, female_labor_status, hlf0019_h, ///
                hlk0006, d_daughter, has_kids, income_cat, hgnuts1_ew, first_daughter_year)

	
	* Create Joint Labor Status Categories
	gen joint_labor_combo = .

	* 1. Both Full-Time
	replace joint_labor_combo = 1 if male_labor_status == 1 & female_labor_status == 1

	* 2. Male FT only
	replace joint_labor_combo = 2 if male_labor_status == 1 & inlist(female_labor_status, 2, 3)

	* 3. Female FT only
	replace joint_labor_combo = 3 if female_labor_status == 1 & inlist(male_labor_status, 2, 3)

	* 4. Both in LF, neither FT (i.e., both PT)
	replace joint_labor_combo = 4 if male_labor_status == 2 & female_labor_status == 2

	* 5. Male not in LF only
	replace joint_labor_combo = 5 if male_labor_status == 3 & female_labor_status != 3

	* 6. Female not in LF only
	replace joint_labor_combo = 6 if female_labor_status == 3 & male_labor_status != 3

	* 7. Both not in LF
	replace joint_labor_combo = 7 if male_labor_status == 3 & female_labor_status == 3

	label define joint_labor_combo_lbl ///
	    1 "Both FT" ///
	    2 "Male FT only" ///
	    3 "Female FT only" ///
	    4 "Both PT" ///
	    5 "Male not in LF only" ///
	    6 "Female not in LF only" ///
	    7 "Both not in LF"

	label values joint_labor_combo joint_labor_combo_lbl

	* Construct Outcome Variable: Household Gender Equality Score

	* 1. Share of household chores/childcare done by the woman
	gen share_women = female_hours / (female_hours + male_hours)

	* 2. Absolute deviation from equality (0.5 means equal split)
	gen abs_distance_from_equality = abs(share_women - 0.5)

	* 3. Multiply by -1 so higher values = more equality
	gen household_equality = -1 * abs_distance_from_equality

	* 5. Label for clarity
	label var share_women "Household share of time done by woman"
	label var abs_distance_from_equality "|share_women - 0.5|"
	label var household_equality "Gender equality score (-1 × |share_women - 0.5|)"

	
* FILTERS *
	* Drop 99th percentile families size
	tabulate hlk0006
	** Drop 99th percentile
	* Step 1: Get the 99th percentile
	summarize hlk0006, detail
	* Step 2: Use the percentile cutoff to drop values above it
	drop if hlk0006 > r(p99)
	
	* Drop 99th percentile hhold living size
	tabulate hlf0019_h
	** Drop 99th percentile
	* Step 1: Get the 99th percentile
	summarize hlf0019_h, detail
	* Step 2: Use the percentile cutoff to drop values above it
	drop if hlf0019_h > r(p99)
	
	* Drop child-less families 
	drop if has_kids == 0


* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)
* ------------------------------------------------------------------------------
* MODEL DESCRIPTION:
* This model estimates the causal effect of having a first-born daughter on 
* gender attitudes in the household as measured by time spent on chores and childcare.
*
* We use the csdid command for Difference-in-Differences estimation in a 
* repeated cross-section design, following Callaway and Sant'Anna (2021).
* 
* Treatment timing is defined by `first_daughter_year`, the year in which 
* an individual first had a daughter. The outcome is observed in survey year 
* `syear`. Never-treated individuals (those who never had a daughter) are 
* coded with `first_daughter_year = 0`.
*
* Estimation uses the doubly robust IPW approach (`method(dripw)`),
*  and computes group-time 
* average treatment effects (ATT(g,t)). We aggregate these to a single ATT
*
* Identification relies on a conditional parallel trends assumption: 
* in the absence of treatment, the outcome trajectories of treated and 
* never-treated units would have evolved similarly, conditional on covariates.
*
* Covariates include: income category,
* labor force status of both members of the couple, region (East/West Germany), and household size.
*
* Standard errors are clustered at cohort of first daughter birth year
* ------------------------------------------------------------------------------
	
	csdid household_equality ///
	    hlf0019_h hlk0006 i.income_cat  ///
	    i.hgnuts1_ew i.joint_labor_combo, ///
	    gvar(first_daughter_year) ///
	    time(syear) ///
	    method(dripw) ///
	    rseed(42) ///
	    vce(cluster first_daughter_year)

	estat simple

	    
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
	* `r(table)` columns: [1] coef, [2] se, [3] z, [4] p, [5] ci_low, [6] ci_high
	matrix result = r(table)

	scalar coef_d_daughter = result[1,1]
	scalar se_d_daughter   = result[2,1]
	scalar z_value         = result[3,1]
	scalar p_value         = result[4,1]

	* Define locals for output (mirroring your original export structure)
	local coefficient = coef_d_daughter
	local se = se_d_daughter
	local test_stat_family = "z"
	local p_value = p_value

	* csdid doesn't return model/residual DF → set as missing
	local df_residual = .
	local df_model = .

	
	* (vi) number of observations
	local num_obs = .
	* Observation count
	* Not available from csdid package
	
	* (vii) number of participants
	* not available from csdid as it find treatment cohorts but does not 
	* return who is vs isnt included in post estimation commands
	*quietly levelsof pid if !missing(left_leaning), local(participants)
	*local num_participants : word count `participants'
	local num_participants = .

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
