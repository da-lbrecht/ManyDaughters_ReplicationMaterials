********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************
* Authors: 01m2XX (11 Jul 2025)

* ============================
* Setup
* ============================
* clear all
version 19
set maxvar 10000

* Install user packages if missing
ssc install outreg2, replace

* CONFIGURE FILE PATHS
/*
if strpos("`c(pwd)'", "code") > 0 {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global IN   "$ROOT/data/raw"
global DATA "$ROOT/data/derived"
global OUT  "$ROOT/out"
*/

* ============================
* Data Preparation
* ============================
use "$IN/ppathl.dta", clear                     
keep if inlist(syear, 2018, 2019)               

* Fertility biography (for child sex vars)
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master match)

* Demographics & weights
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master match)

* Individual questionnaire (attitudes)
merge 1:1 pid syear using "$IN/pl.dta",  nogen keep(master match)

* Household income
merge m:1 hid syear using "$IN/hgen.dta", ///
          nogen keep(master match) keepusing(hid syear hghinc)

label language EN
save "$DATA/h1_dataset.dta", replace

* ============================
* Analysis
* ============================
use "$DATA/h1_dataset.dta", clear

* -------- Filters --------------------------------------------------
keep if inlist(pop, 1, 2)                       // private households
gen  age = syear - gebjahr
keep if age >= 40                               // completed fertility

* -------- Treatment & family size (kidsex## wide vars) ------------
ds kidsex*, has(type numeric)
local kidvars `r(varlist)'

gen daughters = 0
foreach v of local kidvars {
    replace daughters = daughters + (`v' == 2)
}

gen kids = 0
foreach v of local kidvars {
    replace kids = kids + (!missing(`v') & `v' > 0)
}

label var daughters "Number of daughters"
label var kids      "Total number of children"

bysort pid syear: keep if _n == 1               // 1 row per parent-year

* -------- Outcome: 7-point "position of women" index --------------

* non-reverse items
foreach v in plj0621 plj0622 {
    recode `v' (-1 -2 -3 -5 = .)                // set missings to .
    gen   r_`v' = `v'                           // reverse: here not necessary
    egen  z_r_`v' = std(r_`v')
}

* reverse items
foreach v in plj0623 plj0624 plj0625 {
    recode `v' (-1 -2 -3 -5 = .)                // set missings to .
    gen   r_`v' = 7 - `v'                      // reverse: high = egalitarian
    egen  z_r_`v' = std(r_`v')
}

egen geq_index = rowmean(z_r_plj0621 z_r_plj0622 z_r_plj0623 z_r_plj0624 z_r_plj0625)

* set to missing if all items missing
replace geq_index = . if missing(z_r_plj0621) & missing(z_r_plj0622) & missing(z_r_plj0623) & missing(z_r_plj0624) & missing(z_r_plj0625)
label var geq_index "Gender-equality index"

* -------- Covariates ----------------------------------------------
gen agesq   = age^2
label var agesq "Age Square"
gen lnhhinc = log(hghinc)
label var lnhhinc "Household Income"
label var pgbilzeit "Education Years"
label var pgemplst "Employment Status"
recode pgbilzeit pgemplst (-1 -2 -3 -5 = .)
gen migrant = inlist(migback, 2, 3)
label var migrant "Migration background =1"
gen east = (loc1989 == 2)
label var east "East Germany (DDR) =1"

* -------- Main regression -----------------------------------------
assert !missing(phrf)                            // person weight exists
reg geq_index daughters agesq age i.kids lnhhinc pgbilzeit i.pgemplst migrant east i.syear [pw = phrf], cluster(pid)
	
* -------- Extract & export organiser statistics -------------------
local coefficient       = _b[daughters]
local se                = _se[daughters]
local test_stat_family  = "t"
local df_residual       = e(df_r)
local df_model          = e(df_m)
local p_value           = 2 * ttail(`df_residual', abs(`coefficient'/`se'))
local num_obs           = e(N)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'
local interpretation "positive coefficient is in line with hypothesis"

file open myfile using "$OUT/h1_results.csv", write replace
file write myfile ///
"coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile ///
"`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile

display "=== H1 analysis completed – results saved to $OUT/h1_results.csv ==="
exit
