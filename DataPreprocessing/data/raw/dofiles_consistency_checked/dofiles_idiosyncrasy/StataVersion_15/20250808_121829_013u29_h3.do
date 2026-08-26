********************************************************************************
****************** Do-File H3: Behavioral Equality ******************
********************************************************************************

* Authors: [REDACTED]


* ============================
* Setup * 
* ============================

* clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
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
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset

	use "$DATA/h3_dataset.dta", clear

* ============================
* Analysis 
* ============================

* STEP 0: Generate control variables
* Birthregion
gen birthregion_clean = birthregion
* Assign "Outside of Germany" = 17 to -1 (No answer) and -2 (Does not apply)
replace birthregion_clean = 17 if inlist(birthregion, -1, -2)
* Treat -5 as missing
replace birthregion_clean = . if birthregion == -5
replace birthregion_cl = 0 if birthregion_clean ==.

* Label the new version
label define birthregion_lbl ///
    0  "Missing" ///
    1  "Schleswig-Holstein" ///
    2  "Hamburg" ///
    3  "Lower Saxony" ///
    4  "Bremen" ///
    5  "North Rhine-Westphalia" ///
    6  "Hesse" ///
    7  "Rhineland-Palatinate" ///
    8  "Baden-Wuerttemberg" ///
    9  "Bavaria" ///
    10 "Saarland" ///
    11 "Berlin" ///
    12 "Brandenburg" ///
    13 "Mecklenburg-West Pomerania" ///
    14 "Saxony" ///
    15 "Saxony-Anhalt" ///
    16 "Thuringia" ///
    17 "Outside of Germany"
label values birthregion_clean birthregion_lbl

* Income is defined later

* Age
gen age = syear - gebjahr

* Gender: 0 is male, 1 is female
gen gender = 0 if pla0009_v2 ==1
replace gender = 1 if pla0009_v2 ==2
replace gender = 0 if pla0009_v3 ==1
replace gender = 1 if pla0009_v3 ==2

egen tag = tag(pid gender)
bysort pid (gender): gen n_unique_status = .
bysort pid (gender): replace n_unique_status = sum(tag)
bysort pid (gender): replace n_unique_status = n_unique_status[_N]
tab n_unique_st
sort pid gender
list pid gender if n_unique_st ==2
egen max_value = max(n_unique_st), by(pid)
* drop individuals that change gender
drop if  max_value ==2
* sometimes individuals have missing values, replace by the values we know
egen newgender = max(gender), by(pid)
replace gender = newgender
drop newgender

* Education
gen years_schooling = pgbilzeit
replace years_schooling = . if pgbilzeit<0

global control_variables "age gender years_schooling" /* xxx */

* STEP 1: 
* Keep only relevant variables
keep pid parid syear hid pli0043_h pli0044_h plc0014_h ///
     pgbilzeit sex gebjahr kidgeb01 kidsex01 biokids pld0026 ///
	 birthregion_c $control_variables hghinc

* Drop invalid response codes for outcomes
foreach var in pli0043_h pli0044_h plc0014_h {
    replace `var' = . if `var' < 0
}

* Generate total domestic hours
gen total_domestic_hours = pli0043_h + pli0044_h

* STEP 2: Tag gender
gen is_female = (sex == 2)
gen is_male   = (sex == 1)

* STEP 3: Prepare female respondent file
preserve
keep if is_female
rename pid pid_f
rename parid merge_key
rename pli0043_h housework_f
rename pli0044_h childcare_f
rename plc0014_h income_f
rename total_domestic_hours domhours_f
rename pgbilzeit educ_f
rename age age_f
rename gebjahr birthyear_f
rename kidgeb01 kidgeb01_f
rename kidsex01 kidsex01_f
rename biokids biokids_f
rename pld0026 pld0026_f
rename birthregion_clean birthregion_clean_f
rename hghinc hghinc_f

drop if merge_key <0
save temp_female.dta, replace
restore


* STEP 4: Prepare male respondent file
keep if is_male

rename pid pid_m
rename parid parid_m
rename pli0043_h housework_m
rename pli0044_h childcare_m
rename plc0014_h income_m
rename total_domestic_hours domhours_m
rename pgbilzeit educ_m
rename age age_m
rename gebjahr birthyear_m
rename kidgeb01 kidgeb01_m
rename kidsex01 kidsex01_m
rename biokids biokids_m
rename pld0026 pld0026_m
rename birthregion_clean birthregion_clean_m
rename hghinc hghinc_m

gen merge_key = pid_m
drop if merge_key<0
* Ensure one record per male partner per household-year
bysort hid syear merge_key (pid_m): keep if _n == 1
save temp_male_key.dta, replace

* STEP 5: Merge female and male partners
use temp_female.dta, clear
merge 1:1 hid syear merge_key using temp_male_key.dta, keep(match) nogen

* STEP 6: Further prepare for analysis

* Drop households with missing outcome components
drop if missing(domhours_m, domhours_f)
drop if missing(income_m, income_f)

* Drop if first-born child gender is missing or non-binary
drop if missing(kidsex01_m)
drop if !inlist(kidsex01_m, 1, 2)

* Construct treatment: first_child_female from whichever partner has valid data
gen first_child_female = .
replace first_child_female = 1 if kidsex01_f == 2
replace first_child_female = 0 if kidsex01_f == 1

replace first_child_female = 1 if missing(first_child_female) & kidsex01_m == 2
replace first_child_female = 0 if missing(first_child_female) & kidsex01_m == 1

* Only compute when denominator is defined (i.e., no missing)
gen father_share_domestic = .
replace father_share_domestic = domhours_m / (domhours_m + domhours_f) ///
    if !missing(domhours_m, domhours_f) & (domhours_m + domhours_f > 0)
replace father_share_domestic = 0.5 if father_share_domestic == . & (domhours_m == domhours_f)

gen father_share_income = .
replace father_share_income = income_m / (income_m + income_f) ///
    if !missing(income_m, income_f) & (income_m + income_f > 0)
	
* Construct education difference (father minus mother)
gen educ_diff = .
replace educ_diff = educ_m - educ_f if !missing(educ_m, educ_f)

gen total_income = income_m + income_f
gen log_income = log(total_income) if total_income > 0

gen prop_daughters = .
replace biokids_m = . if biokids_m <1
replace pld0026_m = 0 if pld0026_m<1
replace prop_daughters = pld0026_m / biokids_m ///
    if biokids_m > 0 & !missing(pld0026_m, biokids_m)
replace prop_daughters = pld0026_f / biokids_f ///
    if prop_daughters ==. & (biokids_f > 0 & !missing(pld0026_f, biokids_f))

***************************************************************
* STEP 7: Prepare for Regressions
***************************************************************
* Create treatment year (first child's birth year)
gen treat_year = .
replace treat_year = kidgeb01_f if !missing(kidgeb01_f)
replace treat_year = kidgeb01_m if missing(treat_year) & !missing(kidgeb01_m)
drop if kidgeb01_f < 0
drop if kidgeb01_m < 0

	
***************************************************************
* STEP 7: Prepare for DiD
***************************************************************
* Use only out-of-window treatment dates 
gen in_sample_birth = inrange(treat_year, 2000, 2020)

* Flag post-treatment observations
gen post = .
replace post = 0 if (syear < treat_year) & (in_sample_birth==1)
replace post = 1 if (syear >= treat_year) & (in_sample_birth==1)

* Step 1: Distance from treatment year
gen rel_year = syear - treat_year 

* STEP 5: Define valid pre/post obs for diff-in-diff regression
gen pre1 = (rel_year < 0)
gen post1 = (rel_year > 0)

* tag each individual that has at least one valid pre and post obs
egen ever_pre1 = max(pre1), by(hid)
egen ever_post1 = max(post1), by(hid)

* tag and keep only individuals who meet both
gen include_in_did = (ever_pre == 1 & ever_post == 1)

* Create rel_year only for pre- and post-birth valid obs
gen rel_pre = rel_year if rel_year < 0 & include_in_did==1
gen rel_post = rel_year if rel_year > 0 & include_in_did==1

* For each individual, find closest pre and latest post
egen closest_pre = max(rel_pre), by(hid)
egen latest_post = max(rel_post), by(hid)

* STEP 7: check if we have individuals for whom we have one pre and one post observation
gen keep_pre = (rel_year == closest_pre & rel_year < 0 & include_in_did==1)
gen keep_post = (rel_year == latest_post & rel_year > 0 & include_in_did==1)
* Final tag: keep only one pre and one post observation
gen keep_obs = .
replace keep_obs = 1 if keep_pre
replace keep_obs = 2 if keep_post
* Use only selected observations, 2 per individual
gen observations_for_dif = 1 if inlist(keep_obs, 1, 2)
* check whether we have enough observations for diff-in-diff
sum observations_for_dif
scalar num_two_obs = r(N)
dis num_two_obs


* STEP 8: generate interaction terms
generate interaction = post*first_child_female

save "$DATA/temp_hypotheses.dta", replace 

* ------------------
* ANALYSIS FIRST MEASURE (father_share_income)
* ------------------
use "$DATA/temp_hypotheses.dta", clear

* STEP 9: Depending on our number of observations, either run first or second specification for our first measure
* cross section
gen check = 0
local num_two_obs1 = num_two_obs
while `num_two_obs1' < 550 {
    display "num_two_obs1 = `num_two_obs1'"
	* reg h1_index post1 first_child_female interaction if post1 == 1, cluster(pid)
    
bysort hid (syear): gen cs_sample = _n == _N
rename first_child_female treatment
reg father_share_income treatment ///
        age_f age_m educ_m educ_diff prop_daughters biokids_m log_income i.syear i.birthregion_clean_m ///
		if  cs_sample ==1
			
	local num_two_obs1 = `num_two_obs1' + 550
    display "num_two_obs1 = `num_two_obs1'"
	
	gen number_obs = e(N)
	replace check = 1 if number_obs>=550
rename treatment first_child_female

}
sum check

*  STEP 10: fixed effects model with year fixed effects, diff-in-diff structure
local num_two_obs2 = num_two_obs
while `num_two_obs2' >= 550 {
display "num_two_obs2 = `num_two_obs2'"
drop if observations_for_dif !=1

rename interaction treatment
bysort hid syear: keep if _n == 1
xtset hid syear
xtreg father_share_income post treatment ///
    if observations_for_dif == 1, fe cluster(hid)

local num_two_obs2 = 0 
display "num_two_obs2 = `num_two_obs2'"
	
gen number_obs2 = e(N)
replace check = 1 if number_obs2>=550
rename treatment interaction
}
sum check

********************************************************************************************************************************************************************************************************
* EXTRACT AND EXPORT RESULTS MEASURE 1 * 
* Extract the required statistics
		* (i) coefficient estimate
		local coefficient_a = _b[treatment]

		* (ii) standard error
		local se_a = _se[treatment]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_a = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_a = e(df_r)
		local df_model_a = e(df_m)

		* (v) p-value
		local p_value_a = _b[treatment] / _se[treatment]
		local p_value_a = 2 * ttail(e(df_r), abs(`p_value_a'))

		* (vi) number of observations
		local num_obs_a = e(N)

		* (vii) number of participants
		local num_participants_a = e(N)*2

		* (viii) interpretation of directionality
		scalar coef_d_daughter_a = _b[treatment]
		local interpretation1_a "negative coefficient is in line with hypothesis"
		local interpretation1_b "negative coefficient is in line with hypothesis"
		* (ix) do we have enough number of observations? ATTENTION ALSO HERE WE HAVE TO SPECIFY THE NUMBER!
		scalar a = sum(check)
		if a ==1 {
		    local interpretation2_a "We have enough observations. Results for the first measure (father_share_income) are reported below:"
} 
		else {
		    local interpretation2_a "We do not have enough observations. Results for the first measure (father_share_income) should be disregarded."
}
				
	// Display the saved statistics
	di "Coefficient: `coefficient_a'"
	di "Standard Error: `se_a'"
	di "Test Statistic Family: `test_stat_family_a'"
	di "Residual Degrees of Freedom: `df_residual_a'"
	di "Model Degrees of Freedom: `df_model_a'"
	di "p-value: " `p_value_a'
	di "Number of observations: " `num_obs_a'
	di	"Number of participants: " `num_participants_a'
	di "Interpretation of directionality: " "`interpretation1_a'"
	di "Interpretation of directionality: " "`interpretation2_a'"
	
*******************************************************************************************************************************************************	
	

* ------------------
* ANALYSIS SECOND MEASURE (father_share_domestic)
* ------------------
use "$DATA/temp_hypotheses.dta", clear

* STEP 9: Depending on our number of observations, either run first or second specification for our first measure
* cross section
gen check = 0
local num_two_obs3 = num_two_obs
while `num_two_obs3' < 550 {
    display "num_two_obs3 = `num_two_obs3'"
	* reg h1_index post1 first_child_female interaction if post1 == 1, cluster(pid)
    
bysort hid (syear): gen cs_sample = _n == _N
rename first_child_female treatment
reg father_share_domestic treatment ///
        age_f age_m educ_m educ_diff prop_daughters biokids_m log_income i.syear i.birthregion_clean_m ///
		if  cs_sample ==1
			
	local num_two_obs3 = `num_two_obs3' + 550
    display "num_two_obs3 = `num_two_obs3'"
	
	gen number_obs = e(N)
	replace check = 1 if number_obs>=550
rename treatment first_child_female

}
sum check

*  STEP 10: fixed effects model with year fixed effects, diff-in-diff structure
local num_two_obs4 = num_two_obs
while `num_two_obs4' >= 550 {
display "num_two_obs4 = `num_two_obs4'"
drop if observations_for_dif !=1

rename interaction treatment
bysort hid syear: keep if _n == 1
xtset hid syear

xtreg father_share_domestic post treatment ///
    if observations_for_dif == 1, fe cluster(hid)
	
local num_two_obs4 = 0 
display "num_two_obs4 = `num_two_obs4'"
	
gen number_obs2 = e(N)
replace check = 1 if number_obs2>=550
rename treatment interaction
}
sum check

********************************************************************************************************************************************************************************************************
* EXTRACT AND EXPORT RESULTS * 
* Extract the required statistics
		* (i) coefficient estimate
		local coefficient_b = _b[treatment]

		* (ii) standard error
		local se_b = _se[treatment]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_b = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_b = e(df_r)
		local df_model_b = e(df_m)

		* (v) p-value
		local p_value_b = _b[treatment] / _se[treatment]
		local p_value_b = 2 * ttail(e(df_r), abs(`p_value_b'))

		* (vi) number of observations
		local num_obs_b = e(N)

		* (vii) number of participants
		local num_participants_b = e(N)*2

		* (viii) interpretation of directionality
		scalar coef_d_daughter_b = _b[treatment]
		local interpretation "positive coefficient is in line with hypothesis"

		* (ix) do we have enough number of observations? ATTENTION ALSO HERE WE HAVE TO SPECIFY THE NUMBER!
		scalar a = sum(check)
		if a ==1 {
		    local interpretation2_b "We have enough observations. Results for the second measure (father_share_domestic) are reported below:"
} 
		else {
		    local interpretation2_b "We do not have enough observations. Results for the second measure (father_share_domestic) should be disregarded."
}
				
	// Display the saved statistics
	di "Coefficient: `coefficient_b'"
	di "Standard Error: `se_b'"
	di "Test Statistic Family: `test_stat_family_b'"
	di "Residual Degrees of Freedom: `df_residual_b'"
	di "Model Degrees of Freedom: `df_model_b'"
	di "p-value: " `p_value_b'
	di "Number of observations: " `num_obs_b'
	di	"Number of participants: " `num_participants_b'
	di "Interpretation of directionality: " "`interpretation1_b'"
	di "Interpretation of directionality: " "`interpretation2_b'"
	

* ============================================
* Take only one of the two measures
* ============================================

if a >= 1{
local modeltype         = "`modeltype_a'"
local coefficient       = "`coefficient_a'"    
local se                = "`se_a'"              
local test_stat_family  = "`test_stat_family_a'"
local df_residual       = "`df_residual_a'"     
local df_model          = "`df_model_a'"        
local t_stat            = "`t_stat_a'"          
local p_value           = "`p_value_a'"         
local num_obs           = "`num_obs_a'"         
local num_participants  = "`num_participants_a'"
local interpretation1   = "`interpretation1_a'"
}
else {
local modeltype         = "`modeltype_b'"
local coefficient       = "`coefficient_b'"     
local se                = "`se_b'"              
local test_stat_family  = "`test_stat_family_b'"
local df_residual       = "`df_residual_b'"     
local df_model          = "`df_model_b'"        
local t_stat            = "`t_stat_b'"         
local p_value           = "`p_value_b'"         
local num_obs           = "`num_obs_b'"         
local num_participants  = "`num_participants_b'"
local interpretation1   = "`interpretation1_b'" 
}
	
* ============================================
* EXPORT RESULTS TO CSV
* ============================================

file open myfile using "$OUT/h3_results.csv", write replace
*file write myfile "First output measure (h1_index)" _n
*file write myfile "`interpretation2_a'" _n
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation1'" _n
*file write myfile "Second output measure (gender_sharing_norm)" _n
*file write myfile "`interpretation2_b'" _n
*file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
*file write myfile "`coefficient_b', `se_b', `test_stat_family_b', `df_residual_b', `df_model_b',`p_value_b',`num_obs_b',`num_participants_b',`interpretation1_b'" _n
file close myfile
