***************************************************************
****************** Many Daughters Project - H1  ***************
***************************************************************

* ============================
* Setup
* ============================
* clear all
version 19
set maxvar 10000


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
* Data Preparation
* ============================

* Our dependent variable is from 2018 survey.
* Variables: agreement_share (plh0308_v2),
use "$IN/pl.dta", clear
keep if syear == 2018
keep pid plh0308_v2
rename plh0308_v2 agreement_sharework
keep if agreement_sharework > 0 & !missing(agreement_sharework)
save "$DATA/h1_sharework.dta", replace

* Partner information, sex, birth year, migration background
* Variables: pid, syear, hid, cid, parid, partner
use "$IN/ppathl.dta", clear
keep if syear == 2018
keep pid hid parid partner sex gebjahr migback
replace parid = . if parid < 0
replace partner = . if partner < 0
keep if (!missing(pid)) & (!missing(hid)) & (!missing(partner))
save "$DATA/h1_partner_migration.dta", replace

* First child information
* Variables: pid, kidsex01, kidgeb01
* Generated variable: firstkid_female
use "$IN/biobirth.dta", clear
keep pid kidsex01 kidgeb01
keep if kidsex01 > 0
keep if kidgeb01 > 0
gen firstkid_female = kidsex01 == 2
rename kidgeb01 firstkid_birthyear
rename kidsex01 firstkid_sex
save "$DATA/h1_firstkid.dta", replace


* Education and income 
use "$IN/pgen.dta", clear
keep if syear == 2018
keep pid pgbilzeit pglabnet
replace pgbilzeit = . if pgbilzeit < 0
replace pglabnet = . if pglabnet < 0
keep if (!missing(pid)) 
rename pglabnet incomelabor_net
rename pgbilzeit education_totalyears
save "$DATA/h1_education_income.dta", replace

use "$IN/biobirth.dta", clear
drop if kidsex01 < 0 & kidgeb01 < 0

* Remove kids born after 2018
forvalues i = 1/19 {
    local k = string(`i', "%02.0f")

    * Replace both sex and birth year with missing if born after 2018
    quietly replace kidsex`k' = . if kidsex`k' < 0
	quietly replace kidgeb`k' = . if kidgeb`k' < 0
	
	quietly replace kidsex`k' = . if kidgeb`k' > 2018
    quietly replace kidgeb`k' = . if kidgeb`k' > 2018
}


gen num_sons = 0
gen num_daughters = 0

forvalues i = 1/19 {
    local k = string(`i', "%02.0f")
    quietly replace num_sons = num_sons + 1 if kidsex`k' == 1
    quietly replace num_daughters = num_daughters + 1 if kidsex`k' == 2
}

gen num_biochildren = num_sons + num_daughters
gen has_children = num_biochildren > 0
gen has_daughters = num_daughters > 0


gen lastkid_birthyear = .
forvalues i = 1/19 {
    local k = string(`i', "%02.0f")
    quietly replace lastkid_birthyear = max(lastkid_birthyear, kidgeb`k') ///
        if !missing(kidgeb`k') & (missing(lastkid_birthyear) | kidgeb`k' > lastkid_birthyear)
}


keep pid num_biochildren num_sons num_daughters has_children has_daughters lastkid_birthyear

save "$DATA/h1_nkids.dta", replace


* ============================
* Merge All Data
* ============================
use "$DATA/h1_firstkid.dta", clear
merge 1:1 pid using "$DATA/h1_nkids.dta", nogen
merge 1:1 pid using "$DATA/h1_education_income.dta", nogen
merge 1:1 pid using "$DATA/h1_sharework.dta", nogen
merge 1:1 pid using "$DATA/h1_partner_migration.dta", nogen


replace education_totalyears = . if education_totalyears < 0
replace agreement_sharework = . if agreement_sharework < 0


keep if !missing(sex) & !missing(firstkid_birthyear) & !missing(firstkid_sex) & !missing(agreement_sharework) & !missing(gebjahr)
keep if num_biochildren > 0

* ============================
* Generate additional vars
* ============================

gen male = sex == 1
gen years_since_lastkid = 2018 - lastkid_birthyear
gen years_since_firstkid = 2018 - firstkid_birthyear
gen age = 2018 - gebjahr
gen logincome = log(incomelabor_net)

keep if years_since_firstkid >= 1
save "$DATA/h1_dataset.dta", replace


* ============================
* Regression
* ============================

ivregress 2sls agreement_sharework (has_daughters = firstkid_female) ///
    num_biochildren male age education_totalyears logincome i.migback i.partner years_since_firstkid, cluster(hid)


* ============================
* Collect parameters
* ============================

* (i) Coefficient 
local coefficient = _b[has_daughters]

* (ii) Standard error
local se = _se[has_daughters]

* (iii) Test statistic family
local test_stat_family = "z"  

* (iv) Degrees of freedom

* Residual d.f. = nclusters - 1
local num_clusters = e(N_clust)
local df_residual = `num_clusters' - 1

* Model d.f.
local df_model = e(df_m)

`* (v) p-value '
local col = colnumb(r(table), "has_daughters")
matrix b = r(table)
local p_value = b[4, `col']

* (vi) Number of observations 
local num_obs = e(N)

* (vii) participants
local num_participants = e(N)


* (viii) interpretation of directionality
scalar coef_has_daughters = _b[has_daughters]
		local interpretation "positive coefficient is in line with hypothesis"

* ============================
* Write to CSV
* ============================

file open myfile using "$OUT/h1_results.csv", write replace
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile ///
    "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile

exit
