**************************************************************************
* H2: Political preferences – Analysis                      *
* Authors: [REDACTED]
/********************************************************************
* CHANGE LOG (July 2025):
* Following feedback, we revised the H2 
* analysis to ensure it yields one single testable coefficient per 
* hypothesis. Specifically, we:
* - Focused exclusively on the ideological self-placement variable 
*   (`zLeftRight`) as the outcome for H2.
* - Removed the secondary outcome based on political party affiliation 
*   (`plh0012_h`, previously recoded into `LeftParty5`).
*
* This change aligns the analysis with the pre-analysis plan's DiD 
* specification (Equation 1), and guarantees comparability across 
* hypotheses.
********************************************************************/
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
save "$DATA/h2_dataset.dta", replace // save merged dataset
************************************

*----------------------------------------------------------------------------
* PREPARE VARIABLES * 
*----------------------------------------------------------------------------
use "$DATA/h2_dataset.dta", clear

*-----------------------------
* Construct Outcome Variables
*-----------------------------

*–– 3.1 Clean raw items; set negatives to missing
foreach v in plh0004 plh0012_h {
    replace `v' = . if `v' < 0
}

*–– 3.2 Ideological self‐placement (plh0004)
*    Flip so higher = more left, then standardize within wave
gen LeftRight = 10 - plh0004
bysort syear: egen mean_LR = mean(LeftRight) if !missing(LeftRight)
bysort syear: egen sd_LR   = sd(LeftRight)   if !missing(LeftRight)
gen zLeftRight = (LeftRight - mean_LR) / sd_LR
drop mean_LR sd_LR

*-----------------------------
* 4.  Construct key regressors
*    – Daughter_i  (firstborn=girl)
*    – Post_i,t    (post‐birth periods)
*-----------------------------
* identify ever‐had a daughter/son (first child)
bysort pid: egen byte Daughter = max(kidsex01==2)
bysort pid: egen byte Son      = max(kidsex01==1)
bysort pid (syear): assert Daughter==Daughter[1]    // ensure time‐invariant

* timing of birth
gen yearFirstChild = kidgeb01
gen byte Post = (syear >= kidgeb01) 

* gender dummy (for H4)
gen byte Man = (sex==1)

*-----------------------------
* 5.  Controls (Table 4 PaP) 
*-----------------------------
mdesc sumkids pgfamstd plh0258_h sampreg birthregion_ew bioage migback pgbilzeit hghinc pgemplst pgexpft

* number of children (pool 5+)
gen nchild = sumkids
replace nchild = 5 if nchild>5

* marital status
gen mastat = pgfamstd == 1 | pgfamstd == 6 | pgfamstd == 7 | pgfamstd == 8
replace mastat = . if pgfamstd == .
label define mastat 0 "Unmarried" 1 "Married"
label values mastat mastat

* region of interview & origin
replace sampreg      = sampreg - 1                   // 0=West,1=East
replace birthregion_ew = birthregion_ew - 1           // 0=West,1=East
label define ew 0 "West" 1 "East"
label values sampreg ew
label values birthregion_ew ew

* age & squared
sort pid piyear
gen age    = syear - gebjahr
gen age_sq = age^2

* work experience & squared
gen exp    = pgexpft
gen exp_sq = exp^2

* employment indicator
gen byte emplyd = inlist(pgemplst,1,2)
label define emp 0 "Not employed" 1 "Employed"
label values emplyd emp

* rename household income
label variable hghinc "Household net income"
local controls "i.nchild i.mastat i.sampreg age age_sq exp exp_sq i.migback pgbilzeit hghinc i.emplyd"
global controls "`controls'"

*-----------------------------
*   Sample restrictions (Section 4 PaP)
*    – first‐time parents
*    – outcome observed both before & after
*-----------------------------
keep if inlist(pop,1,2) // only keeping participants from private households and with complete information 

* restrict to plausible ages and valid region
keep if sampreg>=0 & inrange(age,18,50)

* must have become first‐time parent within window
keep if yearFirstChild>0 

* Flags for zLeftRight availability pre/post
bysort pid: egen byte has_zLR_pre  = max(!missing(zLeftRight) & !Post)
bysort pid: egen byte has_zLR_post = max(!missing(zLeftRight) & Post)


*===============================================================================
* H2 Analysis
*===============================================================================


*-----------------------------
* DiD regressions  
*Regression : y_i,t = β_0 + β_1*Daughter_i + β_2*Post_i,t + β_3*Daughter_i × Post_i,t + \gamma*X_i,t + \varepsilon_i,t
* The study employs a difference-in-differences (DiD) design to test whether having daughters leads to more left-leaning political preferences. The approach leverages the quasi-random assignment of child sex to compare individuals whose first child is a girl (treatment group) versus those whose first child is a boy (control group), examining changes in political attitudes before and after childbirth.
*-----------------------------

keep if has_zLR_pre & has_zLR_post
xtset pid syear
xtreg zLeftRight i.Daughter##i.Post i.Man $controls, vce(cluster cid)
    
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
local test_stat_family = "t"

* Export results to csv 
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`b3',`s3',`test_stat_family',`df_r',`df_m',`pval3',`Nobs',`Nind',`interp'" _n
	file close myfile


**************************************************************************
* End of H2 Political Preferences DiD Analysis                           *
**************************************************************************