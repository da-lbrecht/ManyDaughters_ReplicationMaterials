********************************************************************************
********************** Do-File H4: #ManyDaughters Project (RT 0168sb) **********
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

	save "$DATA/h4_dataset.dta", replace // save merged dataset
	
* ============================
* Data manipulation (generating variables, cleaning variable values, dropping observations)
* ============================

* Calculate age at general interview in 2019
gen byte age_at_int = cond(gebjahr > 0, 2019 - gebjahr, .)

keep if age_at_int >= 18

* Keep only respondents in private households (pop codes 1 or 2)
keep if inlist(pop, 1, 2)
drop pop

* Keep only respondents with at least one biological child
keep if sumkids > 0

* Flag respondents with a daughter born before the interview year
gen byte having_daughter = 0

forvalues i = 1/19 {
local yy: display %02.0f `i'
replace having_daughter = 1 if kidsex`yy' == 2 /* female child */ & kidgeb`yy' < syear /* born (strictly) before interview year */
}

drop kidsex* kidgeb*

* Recode negative gender attitude items to missing
foreach var of varlist plh0395i* {
    replace `var' = . if `var' < 0
}

* Create summed gender attitudes scale for 2019
gen gender_attitudes = ///
    plh0395i01 + plh0395i02 + plh0395i03 + ///
    plh0395i04 + plh0395i05 + plh0395i06 ///
    if syear == 2019
drop plh0395i*

* Recode marital status to missing if invalid (<0) in 2019
replace pld0131_v2 = . if syear == 2019 & pld0131_v2 < 0

* Recode religious affiliation to missing if invalid (<0) in 2019
replace plh0258_v9 = . if syear == 2019 & plh0258_v9 < 0

* Collapse employment status (plb0022_h) into four categories
gen byte erwerb = ///
    cond(plb0022_h == 9,                                1,  ///
    cond(inlist(plb0022_h, 3,4,5,6,7,8,10,11),           2,  ///
    cond(plb0022_h == 2,                                3,  ///
    cond(plb0022_h == 1,                                4,  .))))
drop plb0022_h

* Rename sex variable for clarity
rename sex female

* Label employment categories
label define erwerb_lab ///
    1 "[1] Nicht erwerbstaetig"                        ///
    2 "[2] Nichtreguläre Erwerbs- und Dienstphasen"    ///
    3 "[3] Teilzeit"                                   ///
    4 "[4] Vollzeit"
label values erwerb erwerb_lab

* Replace invalid household incomes with missing
replace hghinc = . if hghinc < 0

* replace invalid school leaving degrees
replace pgpsbil = . if pgpsbil < 0 

* replace negaitve values for gender
replace female = . if female < 0

* replace negaitve values for migration
replace migback = . if migback < 0

* Keep only data from survey year 2019
keep if syear == 2019

* Count total observations for missingness checks
quietly: count
local totalcount = r(N)

* Initialize list of numeric vars to evaluate and empty removal list
local numvars corigin fprofedu mprofedu locchildh hlf0135 hlf0151 nums numb
local remvars

* Loop over each numeric var to assess missingness and drop if <10% nonmissing
foreach var in `numvars' {
    quietly: count if `var' >= 0 & `var' != .
    local varcount = r(N)

    if `varcount'/`totalcount' < .1 {
        local share = `varcount'/`totalcount'*100
        drop `var'
        local remvars `remvars' `var'
    }
}

* Build list of vars to keep (those not dropped)
local keepvars
foreach var in `numvars' {
    if !inlist("`var'","`remvars'") {
        local keepvars `keepvars' `var'
    }
}

* Update numvars to the final kept vars
local numvars `keepvars'

* Recode any remaining negative values to missing
foreach var in `numvars' {
    replace `var' = . if `var' < 0
}

* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)
*
* Test whether the effect of having a daughter on gender attitudes (H1)
* is more pronounced for men than for women.
*
* Model specification:
*   - Outcome: gender_attitudes (summed index of six items, cleaned of negatives)
*   - Key predictor: having_daughter (1 if any daughter born before interview year)
*   - Prespecified covariates:
*       • sumkids            = total number of biological children
*       • hghinc             = monthly household net income (EUR)
*       • female             = respondent’s sex (2 =female, 1 =male)
*       • age_at_int     	 = age at time of interview (2019)
*       • erwerb             = employment status (1–4 categories)
*       • pld0131_v2         = marital status (coded; invalid → missing)
*       • plh0258_v9         = religious affiliation (coded; invalid → missing)
*       • migback            = migration background indicator
*       • sampreg            = geographic region of sample (east/west)
*       • pgpsbil            = school-leaving degree
*
* We supplement these controls with a LASSO-based, data-driven selection
* implemented via dsregress. The selected variables (from `numvars`) are
* stored in e(controls_sel) — duplicates are automatically dropped by Stata.
* 
* The LASSO can pick from the following list of variables:
*		• corigin 			= Country of origin (country of birth) of the respondent (ISO numeric country codes)
*		• fprofedu 			= Father’s highest completed professional education
*		• mprofedu 			= Mother’s highest completed professional education
*		• locchildh 		= Place where respondent was raised to age 15; 1=Large city, 2=Medium city, 3=Small city, 4=Countryside
*		• hlf0135 			= Residential area—distance of dwelling to city centre; 1=City‑centre flat, 2=<10 km, 3=10–<25 km, 4=25–<40 km, 5=40–<60 km, 6=≥60 km
*		• hlf0151 			= Respondent’s level of worry about crime in their neighbourhood; 1=Not worried … 4=Very worried
*		• nums 				= Number of sisters reported by respondent
*		• numb 				= Number of brothers reported by respondent
*
* Inference approach:
*   • Step 1: LASSO to identify additional relevant covariates
*   • Step 2: OLS regression including both prespecified and LASSO selected
*             controls, with robust standard errors to mitigate collinearity
*             and heteroskedasticity concerns. We use an interaction term
*			  of having_daughter and the coefficient for being male (1.female)
*			  to estimate if the effect of having daughters differs between men
*			  and women.
*
* Step 1: LASSO-based selection of controls
version 16.0: dsregress gender_attitudes 1.having_daughter#1.female, ///
    controls((1.having_daughter 1.female sumkids hghinc i.female age_at_int ///
		i.erwerb i.pld0131_v2 ib(6).plh0258_v9 i.migback sampreg i.pgpsbil) `numvars') rseed(10101)

local sel `e(controls_sel)'

local common ""
foreach v of local sel {
    // only keep v if it appears (as a whole word) in numvars
    if strpos(" `numvars' ", " `v' ") {
        local common "`common' `v'"
    }
}
			 
* Step 2: OLS regression with all controls and robust SEs	
reg gender_attitudes 1.having_daughter 1.female 1.having_daughter#1.female sumkids hghinc i.female ///
    age_at_int i.erwerb i.pld0131_v2 ib(6).plh0258_v9 ///
    i.migback sampreg i.pgpsbil `common', ///
    robust



* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.having_daughter#1.female]

		* (ii) standard error
		local se = _se[1.having_daughter#1.female]


		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local tstat   = _b[1.having_daughter#1.female] / _se[1.having_daughter#1.female]
		local p_value = 2 * ttail(e(df_r), abs(`tstat'))


		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local coef_d_daughter_men: display %12.8f _b[1.having_daughter#1.female]
		scalar coef_d_daughter_men = `coef_d_daughter_men'
		local interpretation "negative coefficient is in line with hypothesis"

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
