********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]
* Team: RT 0148y1

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18.5 // indicate the version of stata you are using here
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

* LOAD AND MERGE DATA * 

    
use "${IN}/pgen.dta", clear
keep pid hid syear pgpartnr
drop if pgpartnr<0
drop if pgpartnr==.

preserve
keep pid hid syear
rename pid partner_id
save "${DATA}/partner.dta", replace
restore

rename pgpartnr partner_id

duplicates tag partner_id hid syear, gen(tag)
tab tag
drop if tag>0
drop tag

merge 1:1 partner_id hid syear using "${DATA}/partner.dta"
sort hid syear pid
keep if _merge==3
drop _merge

duplicates tag hid syear, gen(tag)
tab tag
drop if tag==0 | tag==2 | tag==4 | tag==6 | tag==8 | tag==10 //There are households where 2 or more couples live together. Tag cannot be an odd number.
drop tag 

gen D_couple_in_HH=1

save "${DATA}/sample_couple.dta", replace


// Generate couple ID

use "${DATA}/sample_couple.dta", clear
keep pid partner_id
duplicates drop

gen long min_id = min(pid, partner_id)
gen long max_id = max(pid, partner_id)

egen couple_id = group(min_id max_id)
keep pid partner_id couple_id
label variable couple_id "Couple ID"

save "${DATA}/couple_id.dta", replace

// Find out households with only one child or periods when the household had one child

use "${DATA}/sample_couple.dta", clear
merge m:1 pid partner_id using "${DATA}/couple_id.dta",
drop _merge
merge m:1 pid using "${IN}/biobirth.dta"
drop if _merge==2
drop _merge

sort hid syear pid
order hid syear pid
keep hid pid syear partner_id couple_id D_couple_in_HH sumkids

**Both spouses have the same information

bysort couple_id: egen kid_max=max(sumkids)
bysort couple_id: egen kid_min=min(sumkids)
drop if kid_max~=kid_min
drop kid_max kid_min

gen D_bio_parent=1
keep pid hid syear D_bio_parent couple_id

// Find out period with one child

merge m:1 pid using "${IN}/biobirth.dta"
drop if _merge==2
drop _merge

sort hid syear pid
keep hid syear pid couple_id D_bio_parent sumkids kidgeb01 kidsex01 kidgeb02 

**Drop observations if survey year>=birth year of the second child
**Drop households without children

drop if sumkids==0

foreach var of varlist kidgeb01 kidsex01 kidgeb02{
	replace `var'=. if `var'<0
	bysort couple_id: egen `var'_min=min(`var')
}

drop if syear>=kidgeb02_min & sumkids>1

drop kidgeb02 kidgeb02_min

// Generate child variables

gen d_daughter=1 if kidsex01_min==2
replace d_daughter=0 if kidsex01_min==1
drop kidsex01 kidsex01_min
label variable d_daughter "1st child is a daughter."

gen child_age=syear-kidgeb01_min
label variable child_age "1st child age"
drop if child_age<0 // Drop years before the birth of the first child.
drop kidgeb01

rename kidgeb01_min child_by 
label variable child_by "1st child birth year"


save "${DATA}/sample_basic.dta", replace


*****************************************************************************************

// Read in datasets

use pid hid syear ///
plh0298_v2 plh0302_v2 plh0308_v2 ///
plh0004 plh0011_h plh0012_h plh0333 ///
pli0043_h pli0044_h ///
using "${IN}/pl.dta", clear

save "${DATA}/pl_temp.dta", replace

use pid hid syear ///
gebjahr sex migback ///
using "${IN}/ppathl.dta", clear

save "${DATA}/ppathl_temp.dta", replace

use pid hid syear ///
pgbilzeit pglfs ///
using "${IN}/pgen.dta", clear

save "${DATA}/pgen_temp.dta", replace


// Merge datasets

use "${DATA}/sample_basic.dta", clear
merge 1:1 pid hid syear using "${DATA}/pl_temp.dta"
keep if _merge==3
drop _merge

merge 1:1 pid hid syear using "${DATA}/ppathl_temp.dta"
keep if _merge==3
drop _merge

merge 1:1 pid hid syear using "${DATA}/pgen_temp.dta"
keep if _merge==3
drop _merge


// Control variables

gen age=syear-gebjahr
label variable age "Age"

gen male=1 if sex==1
replace male=0 if sex==2
label variable male "=1 if male, =0 if female"

gen migration=migback if inrange(migback,1,3)
label variable migration "=1 if no, =2 if direct, =3 if indirect"

gen educ=pgbilzeit if pgbilzeit>=0 & pgbilzeit<.
label variable educ "years of schooling"

gen working=1 if inlist(pglfs,11,12)
replace working=0 if !inlist(pglfs,11,12) & pglfs>0 & pglfs<.
label variable working "=1 if working, =0 otherwise"

/*
In our pre-analysis plan (PAP), we stated that we intended to use the variable bula_h to generate dummy variables for the 16 federal states in Germany.
However, we were unable to locate this variable in the datasets provided.

Based on the information available online (https://paneldata.org/soep-core/datasets/hbrutto/bula_h), it appears that we can generate a variable for the federal state of residence using the following code:

gen state=bula_h if inrange(bula_h,1,16)
label variable state "Federal states"
*/

save "${DATA}/Final_Sample.dta", replace


* ============================
* Analysis
* ============================


// Outcome variables

use "${DATA}/Final_Sample.dta", clear


****H4: Gender difference in the support for gender equality

/*
Initially, we prepared several outcome variables based on three survey questions in the SOEP.
However, we have ultimately decided to use only one of these variables to test the hypothesis.
The variables not used in the final analysis have been commented out in the code.

gen suffer_c6=plh0298_v2 if inrange(plh0298_v2,1,7)
recode suffer_c6 (1=7) (2=6) (3=5) (5=3) (6=2) (7=1)

gen suffer_c6_D=1 if inrange(suffer_c6,5,7)
replace suffer_c6_D=0 if inrange(suffer_c6,1,4)

gen suffer_c3=plh0302_v2 if inrange(plh0302_v2,1,7)
recode suffer_c3 (1=7) (2=6) (3=5) (5=3) (6=2) (7=1)

gen suffer_c3_D=1 if inrange(suffer_c3,5,7)
replace suffer_c3_D=0 if inrange(suffer_c3,1,4)
*/

gen work_same=plh0308_v2 if inrange(plh0308_v2,1,7)

gen work_same_D=1 if inrange(work_same,5,7) 
replace work_same_D=0 if inrange(work_same,1,4) 


// Regression

global control age /*i.male*/ i.migration educ i.working child_age i.syear /*i.state*/

reg work_same_D i.d_daughter##i.male $control, robust



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.d_daughter#1.male]

		* (ii) standard error
		local se = _se[1.d_daughter#1.male]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.d_daughter#1.male] / _se[1.d_daughter#1.male]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.d_daughter#1.male]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
