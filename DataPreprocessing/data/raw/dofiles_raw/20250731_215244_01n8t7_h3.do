********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

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

    * biobirth contains information on fertility history
    merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
    * pgen contains user-friendly data on the individual level that are consolidated from different sources
    merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
    * pl contains all variables from the individual questionnaire
    merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
    * hgen contains generated variables on household level from the household interview; use for household net income
    merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

    label language EN // change language of the labels to English

    save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* FILTER & PREPARE DATA
* ============================

* Keep only participants from private households (adjust as needed)
keep if inlist(pop, 1, 2)


* ============================
* DEPENDENT VARIABLE: UNPAID WORK (female share)
* ============================

* Handle invalid values (set negatives to missing)
foreach var in pli0016_h pli0043_h pli0012_h pli0019_h pli0022_h pli0044_h plb0183_h plb0209 pli0049_h pli0031_h {
    replace `var' = 0 if `var' == -2
    replace `var' = . if `var' < 0
}

* Calculate unpaid weekly hours

* weekdays + Saturday + Sunday
gen housework_week = pli0043_h * 5 + pli0012_h + pli0016_h 

* weekdays + Saturday + Sunday
gen childcare_week = pli0044_h * 5 + pli0019_h + pli0022_h 

* weekdays + Saturday (no harmonized variable for sundays available)
gen repairs_week =  5 * pli0049_h + pli0031_h

* total unpaid hours per week
gen unpaid_hours_week = housework_week + childcare_week + repairs_week

* total paid hours per week
gen paid_hours_week = plb0183_h * plb0209

* total working hours per week
gen total_hours_week = unpaid_hours_week + paid_hours_week

* label for clarity
label variable total_hours_week    "Total working hours per week (paid + unpaid)"
label variable unpaid_hours_week   "Unpaid working hours per week"
label variable paid_hours_week     "Paid working hours per week"


* ============================
* RESTRICT TO HETEROSEXUAL COUPLES (same hid & year)
* ============================

tempfile partinfo
preserve
    keep pid syear hid sex unpaid_hours_week
    rename pid                 parid
    rename sex                 partner_sex
    rename unpaid_hours_week   partner_unpaid_hours
    save `partinfo'
restore

* now merge on parid×syear×hid
merge m:1 parid syear hid using "`partinfo'", keep(match) nogen

* Drop same‐sex pairs
keep if sex != partner_sex


* Build HH-Level unpaid totals

* sum respondent + partner
gen total_hours_hh_unpaid  = unpaid_hours_week + partner_unpaid_hours
gen female_hours_hh_unpaid = (sex==2)*unpaid_hours_week + ///
                             (partner_sex==2)*partner_unpaid_hours

* compute the female share
gen fem_unpaid_labor_share = female_hours_hh_unpaid / total_hours_hh_unpaid
label variable fem_unpaid_labor_share "Female share of household unpaid labor"


* ============================
* INDEPENDENT VARIABLE: HAS DAUGHTER
* ============================

* Generate indicator for having any children 
gen has_child = 0
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
    replace has_child = 1 if inlist(kidsex`i', 1, 2) & kidgeb`i' <= syear
}

* Filter out cases where has_child does not align with sumkids (e.g. stillbirths or children dying later in life)
drop if has_child == 0 & sumkids > 0
drop if has_child == 1 & sumkids == 0

* Count total number of children at time of survey to replace sumkids which is time-invariant
gen num_kids = 0
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
    replace num_kids = num_kids + 1 if inlist(kidsex`i', 1, 2) & kidgeb`i' <= syear
}


* Initialize has_daughter as missing
gen d_daughter = .

* Set to 0 if person has children 
replace d_daughter = 0 if has_child == 1

* Set to 1 if any daughter is found
foreach i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
    replace d_daughter = 1 if kidsex`i' == 2 & has_child == 1 & kidgeb`i' <= syear
}


* ============================
* CLEAN CONTROL VARIABLES
* ============================

* AGE DIFFERENCE

* Clean up
replace parid = . if parid == -2
gen age = syear - gebjahr
label variable age "Age at interview"
replace age = . if age < 0

* Build a tempfile of partner info (age & sex)
preserve
  keep pid syear age sex
  rename pid    parid
  rename age    partner_age
  rename sex    partner_sex
  tempfile part
  save `part'
restore

* Merge partner’s age & sex back onto each respondent
merge m:1 parid syear using `part', keep(master match) nogen
replace partner_age = . if partner_age < 0

label variable partner_age "Partner’s age"
label variable partner_sex "Partner’s sex (1=male; 2=female)"

* Compute male–female age gap (male age minus female age)
gen male_age   = cond(sex==1, age, cond(partner_sex==1, partner_age, .))
gen female_age = cond(sex==2, age, cond(partner_sex==2, partner_age, .))
gen age_diff_mf = male_age - female_age
label variable age_diff_mf "Male age minus female age (years)"


* EDUCATION DIFFERENCE 

* Clean up
replace pgbilzeit = . if pgbilzeit < 0
replace parid     = . if parid == -2

* Extract partner education & sex into a tempfile
preserve
    keep pid syear pgbilzeit sex
    rename pid        parid         
    rename pgbilzeit  partner_edu   
    rename sex        partner_sex   
    tempfile partedu
    save `partedu'
restore

* Merge partner’s education & sex back onto respondents
merge m:1 parid syear using `partedu', keep(master match) nogen
label variable partner_edu  "Partner’s years of education"
label variable partner_sex  "Partner’s sex (1=male; 2=female)"

* Compute the male‐minus‐female education gap
gen edu_diff_mf = . 
replace edu_diff_mf = pgbilzeit    - partner_edu if sex==1 & partner_sex==2  
replace edu_diff_mf = partner_edu   - pgbilzeit    if sex==2 & partner_sex==1  
label variable edu_diff_mf "Male years of education minus female years of education"


***** household size
egen hhsize = count(pid), by(hid syear)

***** region (east-west divide)
replace sampreg = . if sampreg < 0
gen eastgermany = 1 if sampreg == 2
replace eastgermany = 0 if sampreg == 1

***** Number of children
replace num_kids = . if num_kids < 0

***** Gender
replace sex = . if sex < 0
gen male = 1 if sex == 1
replace male = 0 if sex == 2

***** Education (in years)
replace pgbilzeit = . if pgbilzeit < 0

***** Income
replace hghinc = . if hghinc < 0
replace pglabgro = 0 if pglabgro < 0 & inlist(pgstib, 10, 11, 12, 13, 15) // Set unemployed people to 0 labor income
replace pglabgro = . if pglabgro < 0


***** Family status
replace pgfamstd = . if pgfamstd < 0


***** exclusion criteria
drop if pgfamstd > 6
drop if d_daughter == .
drop if sex == 3

drop if parid == .
drop if parid < 0


* ============================
* COLLAPSE DATA TO HOUSEHOLD-YEAR LEVEL
* ============================

* Tag couple‐members: respondent rows with a partner, plus that partner’s own row
gen is_couple = parid > 0 & !missing(parid)

* Collapse on those two rows per HH-year
collapse (mean) total_hours_hh_unpaid female_hours_hh_unpaid ///
         (mean) fem_unpaid_labor_share hghinc = hghinc avg_age = age ///
         avg_edu = pgbilzeit age_diff_mf = age_diff_mf edu_diff_mf = edu_diff_mf ///
         (max) d_daughter = d_daughter num_kids migback = migback ///
    if is_couple == 1, by(hid syear eastgermany)


* ============================
* HYPOTHESIS TEST FOR H3
* ============================

* To empirically test this hypothesis, we use a fixed-effects regression model. The dependent variable (fem_unpaid_labor_share) represents the share of unpaid household labor performed by female household members, calculated as the proportion of total household unpaid labor hours contributed by women.

* The main independent variable of interest is a binary indicator (d_daughter) indicating whether the household includes at least one daughter (1) or none (0). The regression controls for household income, the age difference between male and female household members, average household age, average household education, the educational difference between male and female household members, migration background, and interactions between region and survey year to account for regional and temporal variation. Standard errors are clustered at the household level to account for within-household correlations.

xtset hid syear
xtreg fem_unpaid_labor_share d_daughter hghinc age_diff_mf avg_age avg_edu edu_diff_mf i.migback i.syear##eastgermany, fe cluster(hid)
	
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = _se[d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter] / _se[d_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof hid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
