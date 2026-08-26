/**************************************************************************
* H1: Gender Role Attitudes – Final Version                              *
* Authors: [REDACTED]
**************************************************************************
* CHANGE LOG (July 2025):
* Following feedback, we revised the H1 analysis to produce 
* a single testable result for H1. We retained only `id_gnorm_eq` as our 
* outcome, which captures agreement with the statement:
* "Best if man and woman work the same amount so they can share the responsibility".
* 
* This variable offers the most direct and generalizable measure of 
* support for gender equality in household roles. It avoids parenting-specific 
* framings and best reflects the conceptual aim of H1.
**************************************************************************/
* clear all
version 18
set maxvar 10000


*----------------------------------------------------------------------------
* INSTALL PACKAGES * (Please adapt according to your needs)
*----------------------------------------------------------------------------
ssc install mdesc

*-----------------------------
* CONFIGURE FILE PATHS
/*
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

*===============================================================================
**# Data Preparation * 
*===============================================================================

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
************************************
save "$DATA/h1_dataset.dta", replace // save merged dataset
************************************

*----------------------------------------------------------------------------
* PREPARE VARIABLES * 
*----------------------------------------------------------------------------
use "$DATA/h1_dataset.dta", clear

*-----------------------------
* Construct Outcome Variables
*-----------------------------
global gnorm_raw plh0302_v1 plh0302_v2 plh0308_v1 plh0308_v2 plh0298_v1 plh0298_v2
mvdecode ${gnorm_raw} piyear, mv(-10/-1)

replace plh0308_v1 = 5 - plh0308_v1
gen id_gnorm_eq = plh0308_v1 > 2 & plh0308_v1 != .
replace id_gnorm_eq = 1 if id_gnorm_eq == 0 & plh0308_v2 > 4 & plh0308_v2 != .
label variable id_gnorm_eq "Agree with: Best if man and woman work the same amount"

*-----------------------------
* Construct Regressors
*-----------------------------
bysort pid: egen byte Daughter = max(kidsex01==2)
bysort pid: egen byte Son = max(kidsex01==1)
bysort pid (syear): assert Daughter==Daughter[1]
gen int yearFirstChild = kidgeb01
gen byte Post = (syear >= kidgeb01) if (Daughter==1 | Son==1) & inrange(yearFirstChild, 2013, 2017)
gen byte Man = (sex==1)

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


*===============================================================================
*H1 Analysis
*===============================================================================

*-----------------------------
* DiD regressions  
*Regression : y_i,t = β_0 + β_1*Daughter_i + β_2*Post_i,t + β_3*Daughter_i × Post_i,t + \gamma*X_i,t + \varepsilon_i,t
* The study employs a difference-in-differences (DiD) design to test whether having daughters leads to more left-leaning political preferences. The approach leverages the quasi-random assignment of child sex to compare individuals whose first child is a girl (treatment group) versus those whose first child is a boy (control group), examining changes in support for gender equality.
*-----------------------------

keep if has_eq_pre & has_eq_post
xtset pid syear
xtreg id_gnorm_eq i.Daughter##i.Post i.Man $controls, re vce(cluster cid)

* Extract coefficients and SEs
local b1 = _b[1.Daughter]
local s1 = _se[1.Daughter]
local b2 = _b[1.Post]
local s2 = _se[1.Post]
local b3 = _b[1.Daughter#1.Post]
local s3 = _se[1.Daughter#1.Post]

* Compute test for β3 > 0
local df_m = e(df_m)
local df_r = e(N_clust) - 1
local t3 = `b3'/`s3'
local pval3 = ttail(`df_r', `t3')
local Nobs = e(N)
quietly levelsof pid if e(sample), local(ids)
local Nind: word count `ids'
local R2 = e(r2_o)
local interp = "positive coefficient is in line with hypothesis"
* (iii) family of the test statistic (e.g. z, t, F, Chi²)

local test_stat_family = "z"


	* Export results to csv 
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`b3',`s3',`test_stat_family',`df_r',`df_m',`pval3',`Nobs',`Nind',`interp'" _n
	file close myfile

**************************************************************************
* End of H1 Gender Role Attitudes DiD Analysis                           *
**************************************************************************