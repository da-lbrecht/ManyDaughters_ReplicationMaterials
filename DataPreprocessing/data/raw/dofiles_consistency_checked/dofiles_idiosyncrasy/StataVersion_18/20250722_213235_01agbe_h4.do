**************************************************************************
* H4: Gender heterogeneity – Triple‐Difference DiD Analysis               *
* Authors: [REDACTED]
* Based on pre-analysis plan ([REDACTED] & Román, July 14, 2025)                 *
* Hypothesis: Having daughters increases support for gender equality more   *
* for women than men (Triple Difference)                                   *
**************************************************************************
* CHANGE LOG (July 2025):
* Following the request from the study organizers, this version reports 
* a single regression-based result for Hypothesis 4. 
* 
* We use the variable `id_gnorm_eq`, capturing agreement with the 
* statement: "Best if man and woman work the same amount so they can 
* share the responsibility".
* 
* This item is the most general and directly interpretable measure of 
* gender egalitarianism in roles, making it the most conceptually 
* appropriate outcome to test whether daughters shift beliefs more in 
* men than women.
**************************************************************************/
* clear all
version 18
set maxvar 10000


*----------------------------------------------------------------------------
* INSTALL PACKAGES * (Please adapt according to your needs)
*----------------------------------------------------------------------------
ssc install mdesc

*-----------------------------
* CONFIGURE FILE PATHS  (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
*-----------------------------

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

*----------------------------------------------------------------------------
* LOAD AND MERGE DATA * (Please adapt according to your needs)
*----------------------------------------------------------------------------

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

save "$DATA/h4_dataset.dta", replace // save merged dataset
*-----------------------------
*  Construct outcome
*     id_gnorm_eq
*-----------------------------
*......................................
* Dependent variables 
*......................................

mvdecode plh0308_v1 plh0308_v2, mv(-10/-1) // missing values coded as negative to missing


* Transform variables such that the lowest value is most in accordance with traditional gender norms, i.e. disagreeing on an egalitarian taks distribution 
replace plh0308_v1 = 5 - plh0308_v1
gen id_gnorm_eq = plh0308_v1 > 2 & plh0308_v1 != .
replace id_gnorm_eq = 1 if id_gnorm_eq == 0 & plh0308_v2 > 4 & plh0308_v2 != .
label variable id_gnorm_eq "Agree with: Best if man and woman work the same amount"



*  Construct DiD regressors 
*-----------------------------
bysort pid: egen byte Daughter = max(kidsex01==2)
bysort pid: egen byte Son      = max(kidsex01==1)
bysort pid (syear): assert Daughter==Daughter[1]
gen int   yearFirstChild = kidgeb01
gen byte  Post             = (syear >= kidgeb01) if Daughter==1 | Son==1
gen byte  Man              = (sex==1)

*-----------------------------
* Controls (PaP Table 4)
*-----------------------------
mdesc sumkids pgfamstd plh0258_h sampreg birthregion_ew bioage migback pgbilzeit hghinc pgemplst pgexpft

gen nchild = sumkids
replace nchild = 5 if nchild > 5

gen mastat = pgfamstd == 1 | pgfamstd == 6 | pgfamstd == 7 | pgfamstd == 8
replace mastat = . if pgfamstd == .
label define mastat 0 "Unmarried" 1 "Married"
label values mastat mastat

replace sampreg = sampreg - 1
replace birthregion_ew = birthregion_ew - 1
label define ew 0 "West" 1 "East"
label values sampreg ew
label values birthregion_ew ew

sort pid piyear
gen age = syear - gebjahr
gen age_sq = age^2

gen exp = pgexpft
gen exp_sq = exp^2

gen byte emplyd = inlist(pgemplst,1,2)
label define emp 0 "Not employed" 1 "Employed"
label values emplyd emp

label variable hghinc "Household net income"
global controls "i.nchild i.mastat i.sampreg age age_sq exp exp_sq i.migback pgbilzeit hghinc i.emplyd"

*-----------------------------
* Sample Restrictions
*-----------------------------
keep if inlist(pop,1,2)
keep if sampreg >= 0 & inrange(age,18,50)
keep if yearFirstChild > 0
keep if inlist(syear, 2012, 2018)
keep if inrange(yearFirstChild, 2013, 2017)

bysort pid: egen byte has_eq_pre = max(!missing(id_gnorm_eq) & !Post & syear==2012)
bysort pid: egen byte has_eq_post = max(!missing(id_gnorm_eq) & Post & syear==2018)


*-----------------------------
* HYPOTHESIS TEST FOR H4 *
*-----------------------------
* Model Description:
* We test whether having a firstborn daughter increases support for gender equality 
* more for men than for women using a triple-difference design.
* 
* Model: y = β₀ + β₁Man + β₂Daughter + β₃Post + β₄Man×Daughter + β₅Man×Post + 
*            β₆Daughter×Post + β₇Man×Daughter×Post + γX + ε
*
* Key hypothesis: H₀: β₄ + β₇ = 0 vs H₁: β₄ + β₇ > 0
* This tests whether the effect of having a daughter is larger for men than women.

* Create results file
    

keep if has_eq_pre & has_eq_post

* Run triple-difference regression
reg id_gnorm_eq i.Man##i.Daughter##i.Post $controls, vce(cluster cid)

* Extract all coefficients and SEs
local b1 = _b[1.Man]
local s1 = _se[1.Man]
local b2 = _b[1.Daughter]
local s2 = _se[1.Daughter]
local b3 = _b[1.Post]
local s3 = _se[1.Post]
local b4 = _b[1.Man#1.Daughter]
local s4 = _se[1.Man#1.Daughter]
local b5 = _b[1.Man#1.Post]
local s5 = _se[1.Man#1.Post]
local b6 = _b[1.Daughter#1.Post]
local s6 = _se[1.Daughter#1.Post]
local b7 = _b[1.Man#1.Daughter#1.Post]
local s7 = _se[1.Man#1.Daughter#1.Post]




* Hypothesis test: beta4 + beta7
lincom 1.Man#1.Daughter + 1.Man#1.Daughter#1.Post
local bsum = r(estimate)
local ssum = r(se)
local tsum = `bsum'/`ssum'
local pvalsum = ttail(e(df_r), `tsum')
local interp = "positive coefficient is in line with hypothesis"


* Summary stats
local df_m = e(df_m)
local df_r = e(df_r)
local Nobs = e(N)
quietly levelsof pid if e(sample), local(ids)
local Nind: word count `ids'
local R2 = e(r2)
local test_stat_family = "t"


* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`bsum', `ssum', `test_stat_family', `df_r', `df_m',`pvalsum',`Nobs',`Nind', `interp'" _n
	file close myfile
**************************************************************************
* End of H4 Triple‐Difference Analysis                                     *
**************************************************************************