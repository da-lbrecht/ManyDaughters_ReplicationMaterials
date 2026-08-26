********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

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

*---------------------------------------------------------------------------
* 1.  Make CLEAN copies of the original birth-year and sex variables
*     so that the raw SOEP variables remain untouched
*---------------------------------------------------------------------------
foreach v of varlist kidgeb01-kidgeb19 {
    gen  `v'_c = `v'               // copy birth year
    replace `v'_c = . if `v'_c < 0 // turn SOEP's negative missings into .
}

foreach v of varlist kidsex01-kidsex19 {
    gen  `v'_c = `v'               // copy child's sex (1 = boy, 2 = girl)
    replace `v'_c = . if `v'_c < 0 // convert negative missings to .
}	
	
*---------------------------------------------------------------------------
* 2.  Compute running totals by survey year (syear)
*     – number of children already born
*     – number of girls among them
*---------------------------------------------------------------------------
gen numkids_sofar       = 0        // children born up to and incl. syear
gen numdaughters_sofar  = 0        // girls born up to and incl. syear

forvalues i = 1/19 {
    local g = string(`i',"%02.0f") // pads 1→"01", 2→"02", … 19→"19"

    * add 1 child if birth year exists and is ≤ survey year
    replace numkids_sofar = numkids_sofar + ///
        ( kidgeb`g'_c <= syear & !missing(kidgeb`g'_c) )

    * add 1 daughter under the same condition AND sex = 2 (female)
    replace numdaughters_sofar = numdaughters_sofar + ///
        ( kidgeb`g'_c <= syear & !missing(kidgeb`g'_c) & kidsex`g'_c == 2 )
}

gen anydaughter_sofar = numdaughters_sofar > 0   // 1 if at least one daughter

label var numkids_sofar       "Children born up to survey year"
label var numdaughters_sofar  "Daughters born up to survey year"
label var anydaughter_sofar   "Has ≥1 daughter by survey year (0/1)"


/***********************************************************************
* 3. Y – Left-leaning political preference (z-score)
*        Source : plh0004   (0 = far left … 10 = far right)
*        Steps  : 1) clean missings   2) reverse scale   3) standardise
***********************************************************************/

* Treat SOEP negative codes as missing
replace plh0004 = . if plh0004 < 0           // -1, -2, -8 …

* Reverse so that higher = more left
gen Y_raw = 10 - plh0004
label var Y_raw "Reversed left–right scale (0–10; higher = more left)"

* Standardise (z-score: mean 0, SD 1) over the full sample in memory
summarize Y_raw	
gen Y = (Y_raw - r(mean)) / r(sd)
label var Y "Left-leaning preference (z-score; ↑ = more left)"


*---------------------------------------------------------------------------
* 4. Create the key control variables 
*  – create AGE, NATIONALITY, 
*  – drop rows with no valid info for control variables
*---------------------------------------------------------------------------

* 1)  AGE  = survey year – year of birth
gen age = syear - gebjahr
label var age "Age in survey year"

* 2)  EDUCATION  (here we use pgisced11; 
*     alternative variables: pgcasmin or pgbilzeit if preferred)
*     convert SOEP negative codes to missing
*     drop observations with no valid education info
replace pgisced11 = . if pgisced11 < 0
drop if missing(pgisced11)

* 3)  NATIONALITY  – create a dummy: 1 = German citizen, 0 = other
*     (SOEP codebook: pgnation == 1  → German nationality)
replace pgnation = . if pgnation < 0            // clean missing codes
gen german = pgnation == 1 if !missing(pgnation)
label var german "German nationality (1 = German, 0 = non-German)"


* 4)  Calculate age at first birth
*     (missing if respondent has no children or missing dates)
*.    - remove those without information
gen age1stb = kidgeb01_c - gebjahr if !missing(kidgeb01_c, gebjahr)
label var age1stb "Age at first birth (years)"
drop if missing(age1stb)

* 5)  Labour-force status  (pglfs)
*     – treat <0 as missing
*     – "working" = employed (code 1)
replace pglfs = . if pglfs < 0
gen working = (pglfs == 11 | pglfs == 12)  if !missing(pglfs)
label var working "Working (1 = Working (employed), 0 = Non-working)"

* 6)  Net personal labour income  (pglabnet)
*     – negative codes are SOEP missings → set to .
replace pglabnet = . if pglabnet < 0
replace pglabnet = 0 if pglabnet ==. & working ==0
label var pglabnet "Net monthly labour income (€)"

* 7)  Marital status  (pgfamstd)
*     – recode into four broad groups
*       1 married,  						→ "married"
*       2 married-living apart, 4 divorced	→ "divorced / separated"
*       3 single                          	→ "single"
*       5, 6, 7, 8                         	→ "other"
*.    - remove those without information

replace pgfamstd = . if pgfamstd < 0
recode pgfamstd (1   = 1 "Married") ///
                (2 4 = 2 "Divorced/Separated") ///
                (3   = 3 "Single") ///
                (5 6 7 8   = 4 "Other"), gen(marstat)
label var marstat "Marital status (4 categories)"
drop if missing(marstat)

* 8) Household net income (hghinc)
*     – negative codes are SOEP missings → set to .
*     - remove those without information
replace hghinc = . if hghinc < 0
drop if missing(hghinc)
label var hghinc "Household net income (€)"

* 9) Male (sex)
*     – create the dummy for male
gen male = (sex == 1)

/***********************************************************************
*  SUMMARY NOTE – KEY CONTROL VARIABLES IN THE ANALYSIS DATASET
*  (You can add any important variables for controls)
*
*  ▸ Time-INVARIANT (drop out automatically in FE models)
*    ------------------------------------------------------------------
*      male         : Respondent's biological sex (male=1 / female=0)
*      pgisced11    : Highest educational attainment (ISCED-2011)
*      pgcasmin     : Highest educational attainment (CASMIN)   [alt.]
*      pgbilzeit    : Years of schooling                        [alt.]
*      german       : German nationality dummy (1 German / 0 non-German)
*      age1stb      : Age at first birth (years)
*      sumkids      : Total number of births
*
*  ▸ Time-VARIANT (must be included explicitly on RHS of FE regressions)
*    ------------------------------------------------------------------
*      age          : Current age  (syear − gebjahr)
*      working      : Labour-force status (working/non-working)
*      pglabnet     : Net personal labour income (€)
*      marstat      : Marital status (category variable)
*      hghinc       : Net household income (€)
*      numkids_sofar: Number of kids so far 
*
*  Outcome & Treatment (already in dataset)
*      Y                   : z-standardised Left-leaning political preference index
*      anydaughter_sofar   : 1 if respondent has ≥1 daughter by this year
***********************************************************************/


*---------------------------------------------------------------------------
* 5. FILTERS 
*    - Keep only observations with a valid composite attitude score  Y
*    - Exclude observations where the respondent has no children
*    - KEEP only variables needed for analysis
*--------------------=------------------------------------------------------
keep if !missing(Y)      				// retains rows where Y is not missing
keep if numkids_sofar > 0               // drops the pre-parenthood rows

keep pid syear                             /// identifiers
     Y                                     /// outcome (left-leaning preference z-mean)
     anydaughter_sofar                     /// treatment
     age working pglabnet marstat          /// time-variant variables
	 hghinc numkids_sofar                  ///
     male pgisced11 german age1stb sumkids /// time-invariant variables

order pid syear                             /// identifiers
      Y                                     /// outcome (left-leaning preference z-mean)
      anydaughter_sofar                     /// treatment
      age working pglabnet marstat          /// time-variant variables
	  hghinc numkids_sofar                  ///
      male pgisced11 german age1stb sumkids /// time-invariant variables

	save "$DATA/h2_dataset.dta", replace // save merged dataset



* ============================
* Analysis
* ============================

	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

/***********************************************************************
*  ANALYSIS ROADMAP  –  Two-Way Fixed Effects Model
*
*  Outcome variable      :  Y                   (Left-leaning political preference index)
*  Treatment dummy       :  anydaughter_sofar   (1 = ≥1 daughter by wave)
*  Control-variable set  :  W                   (age, income, education, etc.)
*
*  ────────────────────────────────────────────────────────────────────
*  MODEL    Two-way Fixed Effects (panel)
*  ────────────────────────────────────────────────────────────────────
*      Y_it = β0 + β1·anydaughter_sofar_it  +  W_it·β  +
*              γ_t  +  α_i  +  ε_it
*
*      • γ_t : year fixed effects
*      • α_i : individual fixed effects
*      Estimation:  xtreg , fe   (clustered SE at pid)
*	   => H1 implies β1>0
*
***********************************************************************/

/***********************************************************************
*  MODEL  –  Eight regressions exported with outreg2
*  Outcome   : Y
*  Treatment : anydaughter_sofar 
*  Controls  : time-variant  (age working pglabnet marstat hghinc numkids_sofar)
*              time-invariant (sex pgisced11 german age1stb sumkids)
*
*  Column layout
*    1  OLS  no controls
*    2  OLS  + time-invariant controls
*    3  OLS  + all controls
*    4  FE   + time-variant controls                 (individual FE only)
*    5  OLS  + year FE
*    6  OLS  + time-invariant controls + year FE
*    7  OLS  + all controls + year FE
*    8  FE   + time-variant controls + year FE       (two-way FE)
*
*  outreg2 will include two information rows:
*      – Individual FE   (Yes / No)
*      – Year FE         (Yes / No)
***********************************************************************/

* panel declaration
xtset pid syear

* control lists
global TV  age working pglabnet i.marstat hghinc numkids_sofar
global INV male i.pgisced11 german age1stb sumkids

* file path
local outfile "$OUT/h2_model_results.xls"

* ---------------- Column 1  ----------------
reg Y anydaughter_sofar, vce(cluster pid)
outreg2 using "`outfile'", replace dec(3)                                   ///
        addtext("Individual FE","No","Year FE","No")

* ---------------- Column 2  ----------------
reg Y anydaughter_sofar $INV, vce(cluster pid)
outreg2 using "`outfile'", append dec(3)                                    ///
        addtext("Individual FE","No","Year FE","No")

* ---------------- Column 3  ----------------
reg Y anydaughter_sofar $INV $TV, vce(cluster pid)
outreg2 using "`outfile'", append dec(3)                                    ///
        addtext("Individual FE","No","Year FE","No")

* ---------------- Column 4  ----------------
xtreg Y anydaughter_sofar $TV, fe vce(cluster pid)
outreg2 using "`outfile'", append dec(3)                                    ///
        drop(i.pid)                                                         ///
        addtext("Individual FE","Yes","Year FE","No")

* ---------------- Column 5  ----------------
reg Y anydaughter_sofar i.syear, vce(cluster pid)
outreg2 using "`outfile'", append dec(3)                                    ///
        drop(i.syear)                                                       ///
        addtext("Individual FE","No","Year FE","Yes")

* ---------------- Column 6  ----------------
reg Y anydaughter_sofar $INV i.syear, vce(cluster pid)
outreg2 using "`outfile'", append dec(3)                                    ///
        drop(i.syear)                                                       ///
        addtext("Individual FE","No","Year FE","Yes")

* ---------------- Column 7  ----------------
reg Y anydaughter_sofar $INV $TV i.syear, vce(cluster pid)
outreg2 using "`outfile'", append dec(3)                                    ///
        drop(i.syear)                                                       ///
        addtext("Individual FE","No","Year FE","Yes")

* ---------------- Column 8  ----------------
xtreg Y anydaughter_sofar $TV i.syear, fe vce(cluster pid)
outreg2 using "`outfile'", append dec(3)                                    ///
        drop(i.pid i.syear 2016o.syear 2017o.syear 2019o.syear)             ///
        addtext("Individual FE","Yes","Year FE","Yes")

/***********************************************************************
*  The spreadsheet  $OUT/h2_model_results.xls  now contains eight
*  columns with coefficients ± cluster-robust SEs and two info rows
*  indicating whether individual and/or year fixed effects were used.
***********************************************************************/

/***********************************************************************
*  EXTRACT & EXPORT  –  Column 8 results (FE + TV + Year FE)
*  (Our preferred specification for the two-way fixed effects model)
***********************************************************************/

*--------------------------------------------------
* 1.  Pull raw statistics
*--------------------------------------------------
local coef  = _b[anydaughter_sofar]
local se    = _se[anydaughter_sofar]
local tstat = `coef' / `se'

* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

* p-value (use df_r if available, else normal)
capture local df = e(df_r)
if missing(`df') {
    local pval = 2*normal(-abs(`tstat'))
}
else {
    local pval = 2*ttail(`df',abs(`tstat'))
}

local nobs = e(N)

*--------------------------------------------------
* 2.  Conclusion
*  Rule: if the treatment coefficient is statistically insignificant
*        (p ≥ .05), treat it as zero and mark as "not in line".
*--------------------------------------------------

* interpretation
local interpretation = "positive coefficient is in line with hypothesis"

* (iii) family of the test statistic (e.g. z, t, F, Chi²) // Added by DA for consistency
local test_stat_family = "t"

*--------------------------------------------------
* 3.  Display for quick check
*--------------------------------------------------
di "Coefficient (adjusted) : " `coef'
di "Standard Error         : " `se'
di "p-value                : " `pval'
di "Number of observations : " `nobs'
di "Interpretation         : " "`interpretation'"
di "Family                 : " "`test_stat_family'"

*--------------------------------------------------
* 4.  Export to CSV
*--------------------------------------------------
file open myfile using "$OUT/h2_results.csv", write replace
//file write myfile "coefficient,se,p_value,df_residual,df_model,num_obs,interpretation" _n
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n //Adapted by DA to ensure consistent export of results.
file write myfile "`coef',`se',`test_stat_family',`df_residual',`df_model',`pval',`nobs',,`interpretation'" _n
file close myfile


********************************************************************************

exit
