********************************************************************************
********************** Do-File H3: #ManyDaughters Project (RT 0168sb) **********
********************************************************************************

* Authors: [REDACTED]
* (based on the example do file by Tina Wang and David Albrecht)

* ============================
* Setup *
* ============================

//clear all
version 16.1               // indicate the version of Stata you are using here
set maxvar 10000           // extend the maximum number of variables to 10,000
set type double

/*
* CONFIGURE FILE PATHS * (Please do not change)
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global IN   "$ROOT/data/raw"     // folder with the original datasets
global DATA "$ROOT/data/derived" // folder for derived datasets
global OUT  "$ROOT/out"          // folder for output (results, tables)
*/

* ============================
* Data Preparation * 
* ============================

use "$IN/ppathl.dta", clear                                // start with ppathl
keep cid pid hid syear parid partner sex gebjahr sampreg pop todjahr immiyear ///
     germborn corigin migback miginfo sexor birthregion birthregion_ew arefback phrf ///
     phrf0 phrf1 piyear

merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)

drop pgstib pgjobch pgisco88 pgisco08 pgisei08 pgisei88 pgkldb92 pgkldb2010 ///
     pgmps08 pgmps92 pgsiops88 pgsiops08 pgegp88 pgegp08 pgerljob pgausb ///
     pgerwzeit pgbetr pgjobend pgisced97 pgisced11 pgcasmin pgbilzeit ///
     pgpsbilo pgpbbilo pgpsbila pgpbbila pgfield pgdegree pgtrainb pgtraina ///
     pgtraind pgtrainc pgfdt_f pgbilztch pgbilztev pgpicoriga pgpicorigb ///
     pgpicorigc pgsndtyp1 pgsndtyp2 pghonor1 pghonor2 pghonor3 ///
     pgimpsnd3 pgimpsnd2 pgsndtyp3 pgsndjob2 pgsndjob3 pgimpsnd1 ///
     pgsndjob1 pgmode inputdataset

merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) ///
    keepusing(hid syear hghinc)

merge m:1 pid using "$IN/bioparen.dta",   nogen keep(master matches)
merge m:1 hid syear using "$IN/hl.dta",    nogen keep(master matches)
merge 1:1 pid syear using "$IN/biol.dta",  nogen keep(master matches)
merge m:1 pid using "$IN/biobirth.dta",   nogen keep(master matches)
merge 1:1 pid syear using "$IN/pl.dta",    nogen keep(master matches)

* Recode ‘not applicable’ (-2) in the child vars as valid non-missing
forvalues i = 1/19 {
    local yy : display %02.0f `i'
    recode kidsex`yy'  (-2 = 3)
    recode kidgeb`yy'  (-2 = 1)
}

label language EN

save "$DATA/h3_dataset.dta", replace


* ============================
* Data manipulation *
* ============================

* Age at interview in 2018
gen byte age_at_int = cond(gebjahr > 0, 2018 - gebjahr, .)

* Only keep respondent's at least 18 years old at time of interview
keep if age_at_int >= 18

* Private households only
keep if inlist(pop, 1, 2)
drop pop

* Parents with at least one biological child
keep if sumkids > 0

* Flag having at least one daughter born before the interview year
gen byte having_daughter = 0
forvalues i = 1/19 {
    local yy: display %02.0f `i'
    replace having_daughter = 1 if kidsex`yy' == 2 & kidgeb`yy' < syear
}
drop kidsex* kidgeb*

* Household chore attitudes scale: recode negatives to missing
replace plh0308_v2 = . if plh0308_v2 < 0

* Recode marital status to missing if invalid (<0)
replace pgfamstd = . if pgfamstd < 0

* Impute 2018 religious_affil from other years if matching 2019
tempfile tmp
preserve

keep plh0258_v9 plh0258_v10 plh0258_v11 pid syear
rename plh0258_v9 religious_affil

reshape wide religious_affil plh0258_v10 plh0258_v11, i(pid) j(syear)

foreach yr in 2013 2014 2015 2016 2017 {
    replace religious_affil2018 = religious_affil2019    ///
        if !missing(religious_affil`yr')                  ///
           & !missing(religious_affil2019)               ///
           & religious_affil`yr' == religious_affil2019

    replace religious_affil2018 = 4                       ///
        if !missing(plh0258_v11`yr')                      ///
           & !missing(plh0258_v112019)                   ///
           & plh0258_v11`yr' == plh0258_v112019           ///
           & plh0258_v112019 > 0

    replace religious_affil2018 = 7                       ///
        if !missing(plh0258_v10`yr')                      ///
           & !missing(plh0258_v102019)                   ///
           & plh0258_v10`yr' == plh0258_v102019           ///
           & plh0258_v102019 > 0
}

drop plh0258_v1*
reshape long religious_affil, i(pid) j(syear)

save `tmp'
restore
merge 1:1 pid syear using `tmp'

* Collapse employment status into four categories
gen byte erwerb = ///
    cond(plb0022_h == 9,                        1,  ///
    cond(inlist(plb0022_h, 3,4,5,6,7,8,10,11),  2,  ///
    cond(plb0022_h == 2,                        3,  ///
    cond(plb0022_h == 1,                        4,  .))))
drop plb0022_h

drop if pli0046 < 0

bysort cid: gen pp_in_hh = _N

drop if pp_in_hh < 2

egen total_ccare = total(pp_in_hh), by(cid)

gen share_ccare = pli0046/total_ccare

gen dev_share_ccare = abs(share_ccare-1/pp_in_hh)*100

* Rename sex variable
rename sex female

* Label employment
label define erwerb_lab ///
    1 "[1] Nicht erwerbstaetig"                     ///
    2 "[2] Nichtreguläre Erwerbs- und Dienstphasen" ///
    3 "[3] Teilzeit"                                ///
    4 "[4] Vollzeit"
label values erwerb erwerb_lab

* replace invalid incomes with missing
replace hghinc = . if hghinc < 0

* replace invalid family status with missing
replace pld0131_v1 = . if pld0131_v1 < 0

* replace invalid school leaving degrees
replace pgpsbil = . if pgpsbil < 0 

* replace negaitve values for gender
replace female = . if female < 0

* replace negaitve values for migration
replace migback = . if migback < 0

* Keep only 2018 data
keep if syear == 2018

* Missingness check for candidate covariates
quietly count
local totalcount = r(N)

local numvars corigin fprofedu mprofedu locchildh nums numb religious_affil
local remvars

foreach var in `numvars' {
    quietly count if `var' >= 0 & `var' != .
    local varcount = r(N)
    if `varcount'/`totalcount' < 0.1 {
        drop `var'
        local remvars `remvars' `var'
    }
}

local keepvars
foreach var in `numvars' {
    if !inlist("`var'", "`remvars'") {
        local keepvars `keepvars' `var'
    }
}
local numvars `keepvars'

foreach var in `numvars' {
    replace `var' = . if `var' < 0
}


* ============================
* HYPOTHESIS TEST FOR H3 *
* ============================
*
* Estimate whether having at least one daughter predicts more egalitarian
* attitudes regarding the division of household labour, using data from private
* household respondents with children in 2018.
*
* Model specification:
*   - Outcome: dev_share_ccare (deviation from equal division of care for persons)
*   - Key predictor: having_daughter (1 if any daughter born before interview year)
*   - Prespecified covariates:
*       • sumkids            = total number of biological children
*       • hghinc             = monthly household net income (EUR)
*       • female             = respondent’s sex (2 =female, 1 =male)
*       • age_at_int         = age at time of interview (2018)
*       • erwerb             = employment status (1–4 categories)
*		• pld0131_v1		 = family status
*       • migback            = migration background indicator
*       • sampreg            = geographic region of sample (east/west)
*       • pgpsbil            = school-leaving degree
*
* We supplement these controls with a LASSO-based, data-driven selection
* implemented via dsregress. The selected variables (from `numvars`) are
* stored in e(controls_sel) — duplicates are automatically dropped by Stata.
*
* The forward-selection can pick from the following list of variables:
*       • corigin            = Country of origin (country of birth) of the respondent (ISO numeric country codes)
*       • fprofedu           = Father’s highest completed professional education
*       • mprofedu           = Mother’s highest completed professional education
*       • locchildh          = Place where respondent was raised to age 15; 
*							   1=Large city, 2=Medium city, 3=Small city, 4=Countryside
*       • nums               = Number of sisters reported by respondent
*       • numb               = Number of brothers reported by respondent
*       • religious_affil    = Imputed religious affiliation for 2018
*
* Inference approach:
*   • Step 1: LASSO to identify additional relevant covariates
*   • Step 2: OLS regression including both prespecified and LASSO selected
*             controls, with robust standard errors to mitigate collinearity
*             and heteroskedasticity concerns.


* Step 1: LASSO-based selection of controls
version 16.0: dsregress dev_share_ccare having_daughter, ///
    controls((sumkids hghinc i.female age_at_int i.erwerb i.pld0131_v1 i.migback ///
	sampreg i.pgpsbil) `numvars') rseed(10101)
	
local sel `e(controls_sel)'

local common ""
foreach v of local sel {
    // only keep v if it appears (as a whole word) in numvars
    if strpos(" `numvars' ", " `v' ") {
        local common "`common' `v'"
    }
}

* Step 2: OLS regression with all controls and robust SEs
reg dev_share_ccare having_daughter sumkids hghinc i.female ///
    age_at_int i.pld0131_v1 i.erwerb i.migback sampreg i.pgpsbil `common', ///
    vce(cluster cid)


* ============================
* EXTRACT AND EXPORT RESULTS *
* ============================

    * (i)  Coefficient
    local coefficient: display %12.8f _b[having_daughter]

    * (ii) Standard error
    local se: display %12.8f _se[having_daughter]

    * (iii) Test statistic family
    local test_stat_family = "t"

    * (iv) Residual & model degrees of freedom
    local df_residual = e(df_r)
    local df_model    = e(df_m)

    * (v) p-value
    local p_value: display %12.8f _b[having_daughter] / _se[having_daughter]
    local p_value: display %12.8f 2 * ttail(e(df_r), abs(`p_value'))

    * (vi) Number of observations
    local num_obs = e(N)

    * (vii) Number of participants
    quietly levelsof pid if e(sample), local(participants)
    local num_participants : word count `participants'

    * (viii) Directionality interpretation
    local interpretation "negative coefficient is in line with hypothesis"

    // Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di "Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile

exit
