
********************************************************************************
****************** Do-File H3: #ManyDaughters Project ********************
********************************************************************************

* Changes from PAP
* The variable "hgnrpers" denoting number of persons living in a household from the dataset "pgen" that was present in the documentation was unfortunately not featured in the datasets made available to us, therefore we could not calculate the Nr of Adults control variable and consequently we dropped it from the analyses. 
* Recoded negative values for all control variables as missing values
* We have dropped the following individual-difference control variables from the final model because they were static: Education, Immigration Status, Sex
* HouseEquality measure: We have not excluded whole respondents with no values on any of the items used to calculate the HouseEquality variable, just their entries in years (Wave) where they had missing values; excluded also all households that had only one person living in them and those households that had sum of housework = 0 (the formula was also capped at 0 on the lower end)
* EastWest: will we code East as 0 and West as 1



* Author: [REDACTED]
* ============================
* Setup
* ============================

* clear all
version 19.5
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2
* ssc install reghdfe, replace
* ssc install ftools, replace

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
* Load and Merge Data
* ============================

use "$IN/pl.dta", clear	//
keep pid syear pli0040 pli0043_h pli0044_h pli0046 pli0049_h plb0186_h
merge 1:1 pid syear using "$IN/ppathl.dta", keepusing(pid cid hid syear gebjahr sex sampreg) nogen
merge 1:1 pid syear using "$IN/pgen.dta", keepusing(pid syear pgfamstd pgpartz pgbilzeit pgnation) nogen
merge m:1 pid using "$IN/biobirth.dta", keepusing(pid kidsex01-kidsex19 sumkids kidgeb01-kidgeb19) nogen


* ============================
* Data Preparation * 
* ============================

* Exclude respondents <18
gen AgeP = syear - gebjahr
keep if AgeP > 18

* Replace all negative values with missing in kidgeb01 to kidgeb19
foreach v of varlist kidgeb01-kidgeb19 {
    replace `v' = . if `v' < 0
}

foreach v of varlist kidsex01-kidsex19 {
    replace `v' = . if `v' < 0
}

* Exclude respondents with missing housework data
foreach var in pli0040 pli0043_h pli0044_h pli0046 pli0049_h {
    replace `var' = . if `var' < 0
}
drop if missing(pli0040, pli0043_h, pli0044_h, pli0046, pli0049_h)

* Employment
gen Empl = plb0186_h
replace Empl = 0 if plb0186_h == -2

* MaritalStatus
gen MaritalStatus = pgfamstd
replace MaritalStatus = . if pgfamstd < 0

* PartnerStatus
gen PartnerStatus = pgpartz
replace PartnerStatus = . if pgpartz < 0

* Education
gen Edu = pgbilzeit
replace Edu = . if pgbilzeit < 0

* East/West
gen EastWest = sampreg
replace EastWest = . if sampreg < 0

* Clean kid birth years / sex
forvalues i = 1/19 {
    replace kidgeb`=string(`i',"%02.0f")' = . if kidgeb`=string(`i',"%02.0f")' < 0
    replace kidsex`=string(`i',"%02.0f")' = . if kidsex`=string(`i',"%02.0f")' < 0
}

* BioKids (count per year)
gen BioKids = .

forvalues y = 2000/2020 {
    gen bk_`y' = 0
    forvalues i = 1/19 {
        local kidyear = "kidgeb" + string(`i', "%02.0f")
        quietly replace bk_`y' = bk_`y' + ///
            ( `y' > `kidyear' & !missing(`kidyear') ) if syear == `y'
    }
}

forvalues y = 2000/2020 {
    replace BioKids = bk_`y' if syear == `y'
    drop bk_`y'
}

* AgeD (age of oldest daughter)
gen AgeD = 0
forvalues y = 2000/2020 {
    gen aged_`y' = .
    forvalues i = 1/19 {
        gen temp_age = `y' - kidgeb`=string(`i',"%02.0f")'
        replace temp_age = . if kidsex`=string(`i',"%02.0f")' != 2
        replace aged_`y' = cond(syear==`y', min(aged_`y', temp_age), aged_`y')
        drop temp_age
    }
    replace aged_`y' = 0 if aged_`y' < 0
    replace AgeD = aged_`y' if syear == `y'
    drop aged_`y'
}

* HavingDaughter per year
gen HavingDaughter = 0
forvalues y = 2000/2020 {
    gen hd_`y' = 0
    forvalues i = 1/19 {
        gen temp_hd = (kidsex`=string(`i',"%02.0f")' == 2 & kidgeb`=string(`i',"%02.0f")' < `y')
        replace hd_`y' = hd_`y' + cond(missing(temp_hd), 0, temp_hd)
        drop temp_hd
    }
    replace HavingDaughter = 1 if hd_`y' > 0 & syear == `y'
    drop hd_`y'
}

* Wave
gen Wave = syear
drop syear

* Save derived dataset
save "${DATA}/h3_dataset.dta", replace

* Generate DV: HouseEquality

* Calculate total housework per person
gen total_hw = pli0040 + pli0043_h + pli0044_h + pli0046 + pli0049_h
replace total_hw = . if missing(total_hw)

* Generate subgroup variables
gen men_hw = total_hw if sex == 1
gen women_hw = total_hw if sex == 2
gen men = (sex == 1)
gen women = (sex == 2)

* Collapse to household-wave level
collapse (sum) total_hw men_hw women_hw men women, by(hid Wave)

* Rename and compute household-level stats
rename men n_men
rename women n_women
rename men_hw TM
rename women_hw TW

gen total_adults = n_men + n_women
keep if total_adults >= 2 & n_men > 0 & n_women > 0 & total_hw > 0
gen PM = n_men / total_adults
gen PW = n_women / total_adults
gen EM = total_hw * PM
gen EW = total_hw * PW
gen HouseEquality = 1 - (abs(TM - EM) + abs(TW - EW)) / total_hw
replace HouseEquality = 0 if HouseEquality < 0

tempfile equality
save `equality'

use "$DATA/h3_dataset.dta", clear
merge m:1 hid Wave using `equality', nogen

* Save the derived dataset
save "${DATA}/h2_dataset.dta", replace

* ============================
* Hypothesis Test H3
* ============================

xtset pid Wave
reghdfe HouseEquality HavingDaughter BioKids AgeD Empl MaritalStatus PartnerStatus Edu EastWest, absorb(pid Wave) vce(cluster pid)


* ============================
* Export Results
* ============================

local coefficient = _b[HavingDaughter]
local se = _se[HavingDaughter]
local test_stat_family = "t"
local df_residual = e(df_r)
local df_model = e(df_m)
local p_value = _b[HavingDaughter] / _se[HavingDaughter]
local p_value = 2 * ttail(e(df_r), abs(`p_value'))
local num_obs = e(N)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'

scalar coef_d = _b[HavingDaughter]
local interpretation "positive coefficient is in line with hypothesis"

file open myfile using "$OUT/h3_results.csv", write replace
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile

exit
