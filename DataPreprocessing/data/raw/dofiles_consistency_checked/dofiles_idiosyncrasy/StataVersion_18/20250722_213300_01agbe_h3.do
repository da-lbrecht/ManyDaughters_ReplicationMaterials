**************************************************************************
* H3: Income pooling & WorkShare – Improved DiD Analysis             *
* Authors: [REDACTED]

/********************************************************************
* CHANGE LOG (July 2025):
* Based on feedback, we revised the H3 
* analysis to produce a single result per hypothesis. Specifically:
*
* - We dropped the "Joint financial decision" outcome and the 
*   "AbsDev_Work" and "AbsDev_Total" time-use measures.
* - We retained only "AbsDev_Home" (deviation from equal housework 
*   distribution) as the outcome for H3.
*
* This ensures one consistent DiD result aligned with our pre-analysis 
* plan (see Table 1 and Section 6).
* This variable captures the extent to which partners share 
* responsibility for essential, recurring household tasks that directly 
* support family functioning (e.g., cleaning, cooking, laundry). 
* Among the available time-use outcomes, housework most directly reflects 
* negotiated task division and shared labor within the home.
*
* We view this as the most conceptually grounded behavioral measure for 
* testing H3: whether having daughters leads to greater gender equality 
* in the household. 
********************************************************************/                               *
**************************************************************************
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
save "$DATA/h3_dataset.dta", replace // save merged dataset
************************************

*----------------------------------------------------------------------------
* PREPARE VARIABLES * 
*----------------------------------------------------------------------------
use "$DATA/h3_dataset.dta", clear

*============================
* Construct outcomes                                          *
*============================

* 3.1 Joint financial decision making
gen byte JointDecision = (plc0011 == 3)
label variable JointDecision "Joint financial decision equal say"

*============================
* Construct DiD regressors                                     *
*============================
* identify first‐child sex and timing
bysort pid: egen byte Daughter = max(kidsex01==2)
bysort pid: egen byte Son      = max(kidsex01==1)
bysort pid (syear): assert Daughter==Daughter[1]

gen int   yearFirstChild = kidgeb01
gen byte Post            = (syear >= kidgeb01) if Daughter==1 | Son==1
gen byte Man             = (sex==1)

*============================
*  Controls (PaP Table 4)                                               *
*============================
mdesc sumkids pgfamstd plh0258_h sampreg bioage migback pgbilzeit hghinc pgemplst pgexpft

* number of children (pool 5+)
gen nchild = sumkids
replace nchild = 5 if nchild > 5

* marital status
gen mastat = pgfamstd == 1 | pgfamstd == 6 | pgfamstd == 7 | pgfamstd == 8
replace mastat = . if pgfamstd == .
label define mastat 0 "Unmarried" 1 "Married"
label values mastat mastat

* region of residence/origin
replace sampreg        = sampreg - 1
replace birthregion_ew = birthregion_ew - 1
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

label variable hghinc "Household net income"

* controls list (PaP Table 4)
local controls2 "i.nchild i.mastat i.sampreg age age_sq exp exp_sq i.migback pgbilzeit hghinc i.emplyd"
global controls "`controls2'"

*============================
*  Sample restrictions (PaP §4)                                         *
*============================
keep if inlist(pop,1,2) // only keeping participants from private households and with complete information 
* plausible ages & valid region
keep if sampreg>=0 & inrange(age,18,50)
* must have become first‐time parent
keep if yearFirstChild>0

*============================
*  Compute Time‐Use shares                                      *
*============================
* restrict to wives & couples
keep if parid>0

* merge partner time‐use data
tempfile partner
preserve
    keep pid syear pli0012_h pli0016_h pli0043_h pli0007_h pli0003_h pli0038_h
    rename pid parid
    rename pli0012_h HomeSat_p
    rename pli0016_h HomeSun_p
    rename pli0043_h HomeWkday_p
   
    save `partner', replace
restore
merge 1:1 parid syear using "`partner'", nogenerate keep(match)

* recode negatives to missing
foreach v in pli0012_h pli0016_h pli0043_h  {
    replace `v' = . if `v' < 0
}
foreach v in HomeSat_p HomeSun_p HomeWkday_p {
    replace `v' = . if `v' < 0
}

* compute weekly totals
gen HomeWeek_r  = pli0012_h + pli0016_h + 5*pli0043_h
gen HomeWeek_p  = HomeSat_p + HomeSun_p + 5*HomeWkday_p

* keep only complete couples
keep if !missing(HomeWeek_r, HomeWeek_p) 

* allocate to spouses
gen wife_HomeWeek    = cond(sex==2, HomeWeek_r, HomeWeek_p)
gen husband_HomeWeek = cond(sex==1, HomeWeek_r, HomeWeek_p)

* compute wife shares
gen HomeShare  = wife_HomeWeek / (wife_HomeWeek + husband_HomeWeek)

* deviations from equality
gen AbsDev_Home  = abs(HomeShare - 0.5)
label variable AbsDev_Home "Absolute deviation from equal distribution of housework"

* * Flags for AbsDev_Home availability pre/post
bysort pid: egen byte has_AbsDev_Home_pre = max(!missing(`y') & !Post)
bysort pid: egen byte has_AbsDev_Home_post = max(!missing(`y') & Post)



*============================
*  DiD regression: TotalShare       
*Regression : y_i,t = β_0 + β_1*Daughter_i + β_2*Post_i,t + β_3*Daughter_i × Post_i,t + \gamma*X_i,t + \varepsilon_i,t
* The study employs a difference-in-differences (DiD) design to test whether having daughters leads to moreEqual Share of Hours. The approach leverages the quasi-random assignment of child sex to compare individuals whose first child is a girl (treatment group) versus those whose first child is a boy (control group), examining changes in Hours Division before and after childbirth.                                
*============================

keep if has_AbsDev_Home_pre & has_AbsDev_Home_post & Man == 0
xtset pid syear
xtreg AbsDev_Home i.Daughter##i.Post $controls, vce(cluster cid)

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
local interp = "negative coefficient is in line with hypothesis"
local test_stat_family = "t"


* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`b3', `s3', `test_stat_family', `df_r', `df_m',`pval3',`Nobs',`Nind',`interp'" _n
	file close myfile

**************************************************************************
* End of H3 Analysis                                     
**************************************************************************
