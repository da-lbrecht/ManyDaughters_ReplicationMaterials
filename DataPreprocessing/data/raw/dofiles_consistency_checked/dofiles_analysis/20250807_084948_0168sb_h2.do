********************************************************************************
********************** Do-File H3: #ManyDaughters Project (RT 0168sb) **********
********************************************************************************

* Authors: [REDACTED]
* (based on the example do file by Tina Wang and David Albrecht)

* ============================
* Setup *
* ============================

* clear all
version 16.1 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000
set type double

* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the 
* distributed file-package on your system *
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

* LOAD AND MERGE DATA * General Logic: Stata just sees rows and variables, if we 
* are matching pid and syear 1:1, then we are matching a panel to another panel. 
* If we are matching pid without syear m:1, we are matching variables for a person 
* in a dataset that is cross-sectional to ALL years of that person in the panel

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	// This dataset contains matching information, e.g., between partners in 
	// relationships (respondent’s pid and the unique partner’s pid) or between 
	// samples of SOEP in which the respondent was interviewed (26 different samples)
	keep cid pid hid syear parid partner sex gebjahr sampreg pop todjahr immiyear ///
	germborn corigin migback miginfo sexor birthregion birthregion_ew arefback phrf /// 
	phrf0 phrf1 piyear

	// Merge the ‘pgen’ dataset: consolidated person-level variables from 
	// various sources, providing user-friendly individual metrics
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)

	// Drop unneeded ‘pgen’ variables to streamline the dataset
	drop pgstib pgjobch pgisco88 pgisco08 pgisei08 pgisei88 pgkldb92 pgkldb2010 ///
		 pgmps08 pgmps92 pgsiops88 pgsiops08 pgegp88 pgegp08 pgerljob pgausb ///
		 pgerwzeit pgbetr pgjobend pgisced97 pgisced11 pgcasmin pgbilzeit ///
		 pgpsbilo pgpbbilo pgpsbila pgpbbila pgfield pgdegree pgtrainb pgtraina ///
		 pgtraind pgtrainc pgfdt_f pgbilztch pgbilztev pgpicoriga pgpicorigb ///
		 pgpicorigc pgsndtyp1 pgsndtyp2 pghonor1 pghonor2 pghonor3 ///
		 pgimpsnd3 pgimpsnd2 pgsndtyp3 pgsndjob2 pgsndjob3 pgimpsnd1 ///
		 pgsndjob1 pgmode inputdataset

	// Merge the ‘hgen’ dataset: household-level generated variables; keep only 
	// household net income (hghinc) for analyses
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) ///
	keepusing(hid syear hghinc)

	// Merge data on respondents’ biological parents from ‘bioparen’
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches)

	// Merge additional household evaluation variables from ‘hl’
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches)

	// Merge migration and country-of-origin details from ‘biol’
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches)

	// Merge fertility history records from ‘biobirth’
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)

	// Merge complete individual questionnaire data from ‘pl’
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)

	// Recode ‘not applicable’ responses (-2) in the child variables so they are 
	//treated as valid non-missing categories
	
	forvalues i = 1/19 {
		local yearname : display %02.0f `i'
		recode kidsex`yearname' (-2 = 3)
		recode kidgeb`yearname' (-2 = 1)
	}
	
	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset
	
* ============================
* DATA MANIPULATION *
* ============================

* Calculate age at interview year
gen double age_at_int = cond(gebjahr > 0, syear - gebjahr, .)

* Keep only respondents in private households (pop codes 1 or 2)
keep if inlist(pop, 1, 2)
drop pop

* Keep only respondents with at least one biological child
keep if sumkids > 0

* Flag respondents with at least one daughter born before the interview year
gen double having_daughter = 0
forvalues i = 1/19 {
    local yy: display %02.0f `i'
    replace having_daughter = 1 if kidsex`yy' == 2 & kidgeb`yy' < syear
}
drop kidsex* kidgeb*


* ============================
* IMPUTE RELIGIOUS AFFILIATION *
* ============================
tempfile tmp
preserve

keep plh0258_v9 plh0258_v10 plh0258_v11 pid syear
rename plh0258_v9 religious_affil
reshape wide religious_affil plh0258_v10 plh0258_v11, i(pid) j(syear)

* For each target panel year (2009, 2014, 2019), carry forward/match from later waves
* if the corresponding religion information is the same in one preceding and one subsequent 
* year assuming that religiosity was the same in between these two years
foreach tyr in 2009 2014 2019 {
    foreach yr in 2004 2006 2007 2008 2010 2011 2012 2013 2015 2016 2017 2018 2020 {
        local lyr = 2020 - `tyr'
        forvalues j = 1/`lyr' {
            local ty2 = `tyr' + `j'
            
            * exact-match carry forward if original missing or invalid
            replace religious_affil`tyr' = religious_affil`ty2'                             ///
                if (missing(religious_affil`tyr') | religious_affil`tyr' < 0)             ///
                   & !missing(religious_affil`ty2')                                      ///
                   & !missing(religious_affil`yr')                                       ///
                   & religious_affil`yr' == religious_affil`ty2'                          ///
                   & `yr' < `ty2'
            
            * use plh0258_v11 indicator  
            replace religious_affil`tyr' = 4                                             ///
                if (missing(religious_affil`tyr') | religious_affil`tyr' < 0)             ///
                   & !missing(plh0258_v11`ty2')                                         ///
                   & !missing(plh0258_v11`yr')                                          ///
                   & plh0258_v11`yr' == plh0258_v11`ty2'                                 ///
                   & plh0258_v11`ty2' > 0                                               ///
                   & `yr' < `ty2'
            
            * use plh0258_v10 indicator  
            replace religious_affil`tyr' = 7                                             ///
                if (missing(religious_affil`tyr') | religious_affil`tyr' < 0)             ///
                   & !missing(plh0258_v10`ty2')                                         ///
                   & !missing(plh0258_v10`yr')                                          ///
                   & plh0258_v10`yr' == plh0258_v10`ty2'                                 ///
                   & plh0258_v10`ty2' > 0                                               ///
                   & `yr' < `ty2'
        }
    }
}

drop plh0258_v1*
reshape long religious_affil, i(pid) j(syear)
save `tmp'
restore
merge 1:1 pid syear using `tmp'


* ============================
* CLEAN & RECODE OTHER CONTROLS *
* ============================

* Marital status: use pld0131_v1 if v2 missing/invalid, then drop invalid codes
replace pld0131_v2 = pld0131_v1 if missing(pld0131_v2) | pld0131_v2 < 0
replace pld0131_v2 = . 			if pld0131_v2 < 0

* Drop invalid religion codes
replace religious_affil = . 	if religious_affil < 0

* Collapse employment status into four categories
gen double erwerb = cond(plb0022_h == 9, 1,  								///
                  cond(inlist(plb0022_h, 3, 4, 5, 6, 7, 8, 10, 11), 2,  ///
                  cond(plb0022_h == 2, 3,  								///
                  cond(plb0022_h == 1, 4, .))))
drop plb0022_h

* Rename sex for clarity
rename sex female

* Label employment categories
label define erwerb_lab 								///
    1 "[1] Nicht erwerbstaetig"                        	///
    2 "[2] Nichtreguläre Erwerbs- und Dienstphasen"    	///
    3 "[3] Teilzeit"                                   	///
    4 "[4] Vollzeit"
label values erwerb erwerb_lab

* Household net income: drop invalid
replace hghinc = . if hghinc < 0

* Restrict to target survey years
keep if inlist(syear, 2009, 2014, 2019)

* Political attitudes index: drop missing/invalid, invert so higher = more progressive
drop if plh0004 < 0
replace plh0004 = 10 - plh0004
label define prog_lab 		///
    0  "[0] ganz rechts"   	///
    1  "[1]"               	///
    2  "[2]"               	///
    3  "[3]"               	///
    4  "[4]"               	///
    5  "[5]"               	///
    6  "[6]"               	///
    7  "[7]"               	///
    8  "[8]"               	///
    9  "[9]"               	///
    10 "[10] ganz links"
label values plh0004 prog_lab

* Clean freli and mreli
replace freli = . if freli < 0
replace mreli = . if mreli < 0

* replace invalid school leaving degrees
replace pgpsbil = . if pgpsbil < 0 

* replace negaitve values for gender
replace female = . if female < 0

* replace negaitve values for migration
replace migback = . if migback < 0

* ============================
* MISSINGNESS CHECK FOR CANDIDATES *
* ============================

quietly count
local totalcount = r(N)

local numvars corigin fprofedu mprofedu locchildh hlf0135 hlf0151 nums numb religious_affil
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
* HYPOTHESIS TEST FOR H2 *
* ============================
*
* Estimate whether having at least one daughter predicts more progressive
* political attitudes, using panel data from private-household respondents
* in survey years 2009, 2014, and 2019.
*
* Model specification:
*   - Outcome: plh0004 (political attitude index, higher = more progressive)
*   - Key predictor: having_daughter (1 if any daughter born before interview year)
*   - Prespecified covariates:
*       • sumkids           = total number of biological children
*       • hghinc            = monthly household net income (EUR)
*       • female            = respondent’s sex (2 =female, 1 =male)
*       • age_at_int        = age at interview (years)
*       • erwerb            = employment status (1–4 categories)
*       • pld0131_v2        = marital status (coded; invalid → missing)
*       • migback           = migration background indicator
*       • sampreg           = geographic region of sample (east/west)
*       • pgpsbil           = school-leaving degree
*
* We supplement these controls with a LASSO-based, data-driven selection
* implemented via dsregress. The selected variables (from `numvars`) are
* stored in e(controls_sel) — duplicates are automatically dropped by Stata.
*
* The LASSO can pick from the following list of variables:
*       • corigin           = Country of origin (ISO numeric country code)
*       • fprofedu          = Father’s highest completed professional education
*       • mprofedu          = Mother’s highest completed professional education
*       • locchildh         = Place respondent was raised to age 15; 1=Large city, 2=Medium city, 3=Small city, 4=Countryside
*       • hlf0135           = Residential area—distance to city-centre; 1=Centre flat, 2=<10 km, 3=10–<25 km, 4=25–<40 km, 5=40–<60 km, 6=≥60 km
*       • hlf0151           = Worry about crime in neighbourhood; 1=Not worried … 4=Very worried
*       • nums              = Number of sisters reported by respondent
*       • numb              = Number of brothers reported by respondent
*       • freli   			= father's religious affiliation
*       • mreli   			= mother's religious affiliation
*       • religious_affil   = religious affiliation
*
* Inference approach:
*   • Step 1: LASSO to identify additional relevant covariates
*   • Step 2: Random-effects panel regression (xtreg …, re) with SEs clustered
*             at the household level to mitigate collinearity and heteroskedasticity.
*

* Step 1: LASSO-based selection of controls
version 16.0: dsregress plh0004 having_daughter, ///
    controls((i.female sumkids hghinc age_at_int i.erwerb i.pld0131_v2 ///
              i.migback sampreg i.pgpsbil) `numvars') rseed(10101)

local sel `e(controls_sel)'

local common ""
foreach v of local sel {
    // only keep v if it appears (as a whole word) in numvars
    if strpos(" `numvars' ", " `v' ") {
        local common "`common' `v'"
    }
}
			  
* Step 2: Random-Effects panel regression with all controls and robust SEs (clustered on household level)
xtset pid syear

xtreg plh0004 having_daughter i.female sumkids hghinc ///
    age_at_int i.erwerb i.pld0131_v2 i.migback sampreg i.pgpsbil ///
    syear `common', vce(cluster cid) re



* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[having_daughter]

		* (ii) standard error
		local se = _se[having_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value     = 2 * normal(-abs(_b[having_daughter] / _se[having_daughter]))
*		local p_value: display %12.8f _b[having_daughter] / _se[having_daughter]
*		local p_value: display %12.8f 2 * normal(-abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local coef_d_daughter: display %12.8f _b[having_daughter]
		scalar coef_d_daughter = `coef_d_daughter'
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
