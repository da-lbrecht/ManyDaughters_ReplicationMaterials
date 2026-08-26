********************************************************************************
****************** Do-File H1: Gender-Related Attitudes ******************
********************************************************************************
* Authors: [REDACTED]

* ============================================
* STEP 1: Setup
* ============================================

*clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES* 
*ssc install outreg2
/*
* CONFIGURE FILE PATHS *
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
* ============================================
* STEP 2: Load and Merge Data
* ============================================

use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

label language EN // change language of the labels to English

save "$DATA/h1_dataset.dta", replace // save merged dataset


use "$DATA/h1_dataset.dta", clear

* ============================================
* STEP 3: Sample Restrictions and Treatment Definition
* ============================================

drop if missing(kidsex01) | !inlist(kidsex01, 1, 2)

* Treatment indicator
gen first_child_female = (kidsex01 == 2)
label var first_child_female "1 if first child is female"

* Treatment timing
drop if kidgeb01 < 0
gen treat_year = kidgeb01
gen in_sample_birth = inrange(treat_year, 2000, 2020)

gen rel_year = syear - treat_year
gen post = .01
replace post = 0 if syear < treat_year & in_sample_birth == 1
replace post = 1 if syear >= treat_year & in_sample_birth == 1

* ============================================
* STEP 4: Construct Dependent Variables
* ============================================

foreach var in plj0620 plj0621 plj0622 plj0623 plj0625 {
    replace `var' = . if `var' < 0
}

egen h1_valid_items = rownonmiss(plj0620 plj0621 plj0622 plj0623 plj0625)
egen h1_index = rowmean(plj0620 plj0621 plj0622 plj0623 plj0625)
replace h1_index = . if h1_valid_items < 3
label var h1_index "Gender attitudes index (0-4)"

replace plh0308_v1 = . if plh0308_v1 < 0
replace plh0308_v2 = . if plh0308_v2 < 0
replace plh0308_v1 = 5 - plh0308_v1 

gen gender_sharing_norm = (plh0308_v1 - 1)/3 if syear == 2012
replace gender_sharing_norm = (plh0308_v2 - 1)/6 if syear == 2018
label var gender_sharing_norm "Egalitarian norm on income sharing (0-1)"


* ============================================
* STEP 5: Control Variables
* ============================================

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

* Income
gen log_income = log(plc0014_h) if plc0014_h > 0
gen log_income_hid = log(hghinc) if hghinc > 0
replace log_income_hid  = log_income if log_income_hid ==.

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

* Number of children
foreach x in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
gen num_girls_`x' = 1 if  kidsex`x' == 2
replace num_girls_`x' = 0 if num_girls_`x' !=1
gen num_boys_`x' = 1 if  kidsex`x' == 1
replace num_boys_`x' = 0 if num_boys_`x' !=1
tab num_boys_`x', missing
tab num_girls_`x', missing
}

order num_*, alphabetic
egen totalboys = rowtotal(num_boys_01 - num_boys_19)
egen totalgirls = rowtotal(num_girls_01 - num_girls_19)
tab totalboys
tab totalgirls
gen num_children = totalboys + totalgirls

replace biokids = 0 if biokids ==2
gen test = 1 if biokids == num_children
tab test, missing

* Proportion daughters
gen perc_girls = totalgirls/num_children if num_children !=0
tab perc_girls, missing
replace perc_girls = 0 if num_children == 0 
tab perc_girls

global control_variables "age gender log_income_hid num_children perc_girls years_schooling" /* xxx */

* ============================================
* STEP 6: Prepare for Cross-Section
* ============================================

gen pre = (rel_year < 0)
gen post1 = (rel_year > 0)
egen ever_pre = max(pre), by(pid)
egen ever_post = max(post1), by(pid)
gen include_in_did = (ever_pre == 1 & ever_post == 1)

* For h1_index
gen relevant_post_h1 = rel_year if rel_year > 0 & !missing(h1_index)
egen latest_post_h1 = max(relevant_post_h1), by(pid)
gen relevant_for_cs_h1 = (latest_post_h1 == rel_year)

* For gender_sharing_norm
gen relevant_post_norm = rel_year if rel_year > 0 & !missing(gender_sharing_norm)
egen latest_post_norm = max(relevant_post_norm), by(pid)
gen relevant_for_cs_norm = (latest_post_norm == rel_year)

* ============================================
* STEP 7: Prepare for Difference-in-Differences Estimation
* ============================================
* A) DiD flags for h1_index

gen rel_pre_h1 = rel_year if rel_year < 0 & include_in_did == 1 & !missing(h1_index)
gen rel_post_h1 = rel_year if rel_year > 0 & include_in_did == 1 & !missing(h1_index)
egen closest_pre_h1 = max(rel_pre_h1), by(pid)

gen keep_pre_h1 = (rel_year == closest_pre_h1 & rel_year < 0 & include_in_did == 1)
gen keep_post_h1 = (rel_year == latest_post_h1 & rel_year > 0 & include_in_did == 1)
gen observations_for_dif_h1 = inlist(1, keep_pre_h1, keep_post_h1)
sum observations_for_dif_h1 if observations_for_dif_h1 == 1
scalar num_two_obs_A = r(N)
display "num_two_obs_A = " num_two_obs_A


* B) DiD flags for gender_sharing_norm

gen rel_pre_norm = rel_year if rel_year < 0 & include_in_did == 1 & !missing(gender_sharing_norm)
gen rel_post_norm = rel_year if rel_year > 0 & include_in_did == 1 & !missing(gender_sharing_norm)
egen closest_pre_norm = max(rel_pre_norm), by(pid)

gen keep_pre_norm = (rel_year == closest_pre_norm & rel_year < 0 & include_in_did == 1)
gen keep_post_norm = (rel_year == latest_post_norm & rel_year > 0 & include_in_did == 1)
gen observations_for_dif_norm = inlist(1, keep_pre_norm, keep_post_norm)
sum observations_for_dif_norm if observations_for_dif_norm == 1
scalar num_two_obs_B = r(N)
display "num_two_obs_B = " num_two_obs_B


* ============================================
* STEP 8: Interaction term
* ============================================
gen interaction = post * first_child_female

* ============================================
* STEP 9: Analysis - First Measure (h1_index)
* ============================================

gen check_did = 0
gen check_cs  = 0

if num_two_obs_A >= 550 {
    display "Attempting fixed effects DiD for h1_index"
    rename interaction treatment
    xtset pid syear
    xtreg h1_index post treatment ///
        age years_schooling log_income_hid ///
        if observations_for_dif_h1 == 1, fe cluster(pid)
    replace check_did = (e(N) >= 550)
    rename treatment interaction
}

if check_did == 0 {
    display "Fallback: Running cross-sectional regression for h1_index"
    rename first_child_female treatment
    reg h1_index treatment ///
        gender age years_schooling num_children perc_girls log_income_hid ///
        if relevant_for_cs_h1 == 1
    replace check_cs = (e(N) >= 550)
    rename treatment first_child_female
}

* ============================================
* STEP 10: Results Extraction - First Measure
* ============================================

local coefficient_a       = _b[treatment]
local se_a                = _se[treatment]
local test_stat_family_a  = "t"
local df_residual_a       = e(df_r)
local df_model_a          = e(df_m)
local t_stat_a            = `coefficient_a' / `se_a'
local p_value_a           = 2 * ttail(`df_residual_a', abs(`t_stat_a'))
local num_obs_a           = e(N)
quietly levelsof pid if e(sample), local(participants_a)
local num_participants_a  : word count `participants_a'


local interpretation1_a "positive coefficient is in line with hypothesis"


summarize check_cs
scalar sum_check_cs = r(sum)

summarize check_did
scalar sum_check_did = r(sum)

scalar a = sum_check_cs + sum_check_did

if sum_check_did >= 1 {
    local modeltype_a "Model: DiD Estimation"
}
else if sum_check_cs >= 1 {
    local modeltype_a "Model: Cross-Sectional Estimation"
}
else {
    local modeltype_a "Model: Not estimated"
}

if a >= 1 {
    local interpretation2_a "We have enough observations. Results for the first measure (h1_index) are reported below:"
}
else {
    local interpretation2_a "We do not have enough observations. Results for the first measure (h1_index) are not reported."
}

di "--------------------------------------------------------"
di "`modeltype_a'"
di "`interpretation2_a'"
di "Coefficient:             `coefficient_a'"
di "Standard Error:          `se_a'"
di "Test Statistic Family:   `test_stat_family_a'"
di "Residual DF:             `df_residual_a'"
di "Model DF:                `df_model_a'"
di "p-value:                 `p_value_a'"
di "Number of observations:  `num_obs_a'"
di "Number of participants:  `num_participants_a'"
di "Interpretation:          `interpretation1_a'"
di "--------------------------------------------------------"

* ============================================
* STEP 11: Analysis - Second Measure (gender_sharing_norm)
* ============================================

gen check_did_b = 0
gen check_cs_b  = 0

if num_two_obs_B >= 550 {
    display "Attempting fixed effects DiD for gender_sharing_norm"
    rename interaction treatment
    xtset pid syear
    xtreg gender_sharing_norm post treatment ///
        age years_schooling log_income_hid ///
        if observations_for_dif_norm == 1, fe cluster(pid)
    replace check_did_b = (e(N) >= 550)
    rename treatment interaction
}

if check_did_b == 0 {
    display "Fallback: Running cross-sectional regression for gender_sharing_norm"
    rename first_child_female treatment
    reg gender_sharing_norm treatment ///
        gender age years_schooling num_children perc_girls log_income_hid ///
        if relevant_for_cs_norm == 1
    replace check_cs_b = (e(N) >= 550)
    rename treatment first_child_female
}

* ============================================
* STEP 12: Results Extraction - Second Measure
* ============================================

local coefficient_b       = _b[treatment]
local se_b                = _se[treatment]
local test_stat_family_b  = "t"
local df_residual_b       = e(df_r)
local df_model_b          = e(df_m)
local t_stat_b            = `coefficient_b' / `se_b'
local p_value_b           = 2 * ttail(`df_residual_b', abs(`t_stat_b'))
local num_obs_b           = e(N)
quietly levelsof pid if e(sample), local(participants_b)
local num_participants_b  : word count `participants_b'

local interpretation1_b "positive coefficient is in line with hypothesis"


summarize check_cs_b
scalar sum_check_cs_b = r(sum)

summarize check_did_b
scalar sum_check_did_b = r(sum)

scalar b = sum_check_cs_b + sum_check_did_b

if sum_check_did_b >= 1 {
    local modeltype_b "Model: DiD Estimation"
}
else if sum_check_cs_b >= 1 {
    local modeltype_b "Model: Cross-Sectional Estimation"
}
else {
    local modeltype_b "Model: Not estimated"
}

if b >= 1 {
    local interpretation2_b "We have enough observations. Results for the second measure (gender_sharing_norm) are reported below:"
}
else {
    local interpretation2_b "We do not have enough observations. Results for the second measure (gender_sharing_norm) are not reported."
}

di "--------------------------------------------------------"
di "`modeltype_b1'"
di "`interpretation2_b'"
di "Coefficient:             `coefficient_b'"
di "Standard Error:          `se_b'"
di "Test Statistic Family:   `test_stat_family_b'"
di "Residual DF:             `df_residual_b'"
di "Model DF:                `df_model_b'"
di "p-value:                 `p_value_b'"
di "Number of observations:  `num_obs_b'"
di "Number of participants:  `num_participants_b'"
di "Interpretation:          `interpretation1_b'"
di "--------------------------------------------------------"


* ============================================
* STEP 13a: take only one of the two measures
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
* STEP 13b: Export Results to CSV
* ============================================

file open myfile using "$OUT/h1_results.csv", write replace
*file write myfile "First output measure (h1_index)" _n
*file write myfile "`interpretation2_a'" _n
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation1'" _n
*file write myfile "Second output measure (gender_sharing_norm)" _n
*file write myfile "`interpretation2_b'" _n
*file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
*file write myfile "`coefficient_b', `se_b', `test_stat_family_b', `df_residual_b', `df_model_b',`p_value_b',`num_obs_b',`num_participants_b',`interpretation1_b'" _n
file close myfile
