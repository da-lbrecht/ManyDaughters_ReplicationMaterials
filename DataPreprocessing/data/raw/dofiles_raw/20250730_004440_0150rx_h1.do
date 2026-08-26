***************************************************************************************************************************************
****************** Do-File H1 #ManyDaughters Project: (Having daughters increases gender-equality attitudes) **************************
***************************************************************************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 17
set maxvar 10000
set linesize 255
* ssc install ftools, replace
* ssc install reghdfe, replace
/*
if strpos("`c(pwd)'", "code") {
	global ROOT "`c(pwd)'/.."
}
else{
	global ROOT "`c(pwd)'"
} 

global IN   "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for datasets
global OUT  "$ROOT/out" // folder for output (results, tables)
*/
* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * 

	* use ppathl as a starting dataset
	use "$IN/ppathl.dta", clear 
	keep pid cid syear hid gebjahr sex erstbefr netto pop
	// pid: Individual unique Identifier (person id). The subsequent fixed-effect model needs to cluster by pid and must be able to identify the records of the same person in different years.
	// syear: Survey year. Together with pid, it forms the primary key of the panel (pid × syear).
	// hid: Family/Resident id. It is used to merge family-level data (such as income, housework duration).
	// sex: Gender. Used to construct parent dummy variables (MPD, MPS, FPD, FPS).
	// gebjahr: Year of birth. Calculate the age (Age = syear-gebjahr), and then apply the 18-50 age sample limit of PAP.
	// pop: Population types (for example, 1= private households, 2= institutional households). Robustness screening can be conducted (such as retaining only private residents).
	gen age = syear - gebjahr
	* Keep valid observations: interview year ≥ first interview; interview outcome acceptable
	keep if erstbefr <= syear & inlist(netto, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 29) 	 // Netto-codes 10-19 (and 29) define the respondents population of PGEN

	* Merge child info
	merge m:1 pid using "$IN/biobirth.dta", keepusing(kidgeb01 kidsex01)
	keep if _merge == 3
	drop _merge
	// kidgeb01: birth year of first child
	// kidsex01: gender of firsr child
	
	* Merge education
	merge m:1 pid syear using "$IN/biol.dta", keepusing(lb0191)
	keep if _merge == 3
	drop _merge
	// School-Leaving Degree: lb0191. This variable is not included into the data due to many missing values.
	
	* Merge country
	merge 1:1 pid syear using "$IN/pgen.dta", keepusing(pgnation)
	keep if _merge == 3
	drop _merge
	// pgnation: the nationality of the responder.
	
	* Merge survey controls and questionaires
	merge m:1 pid syear using "$IN/pl.dta", keepusing(plj0621 plj0622 plj0623 plj0624 plj0625 plc0017_h)
	keep if _merge == 3
	drop _merge
	// gender attitudes: plj0621 plj0622 plj0623 plj0624 plj0625	
	// income (Net Amount Of Wages, Salary Prev. Yr): plc0017_h. This variable is not included into the data due to many missing values.

		* gender attitudes *
		gen plj0624_r = 8 - plj0624 if !missing(plj0624)
		gen plj0625_r = 8 - plj0625 if !missing(plj0625)
		egen gender_att_raw = rowmean(plj0621 plj0622 plj0623 plj0624_r plj0625_r)
		egen gender_att = std(gender_att)
		label var gender_att "Standardized Gender Attitude Index (High = More Egalitarian)"
		
		
	* save data *
	label language EN // change language of the labels to English
	save "$DATA/h1_dataset.dta", replace // save merged dataset
	
* ============================
* Analysis *
* ============================

* FILTERS *
	
	* Restrict the sample to person-year observations in which respondents were aged between 18 and 50 years (inclusive), to focus on prime childbearing and child-rearing years *
	keep if inrange(age,18,50)
	
	* Exclude respondents who were observed only once in the panel, as fixed-effects models require at least two observations per individual *
	egen n_obs = count(syear), by(pid)
	keep if n_obs>=2 
	
	* Not impute missing outcome values *
	drop if missing(gender_att)

* GENERATE VARIABLES * 
	
	* Key explanatory variable (MPD: male parent of daughters) *
	gen byte MPD = (sex==1 & !mi(kidgeb01) & kidgeb01<=syear & kidsex01==2) 
	gen byte MPS = (sex==1 & !mi(kidgeb01) & kidgeb01<=syear & kidsex01==1)
	gen byte FPD = (sex==2 & !mi(kidgeb01) & kidgeb01<=syear & kidsex01==2)
	gen byte FPS = (sex==2 & !mi(kidgeb01) & kidgeb01<=syear & kidsex01==1)
	fvset base 0 MPD MPS FPD FPS	

	* Control variables *
	gen age_sq = age^2
	egen country_id = group(pgnation), label
	
* HYPOTHESIS TEST FOR H1 (Does having a daughter increase support for gender equality (attitudes)?) *

	* We test whether becoming a parent of a daughter (vs. a son) is associated with more egalitarian gender attitudes.
	* We estimate a fixed-effects panel regression model using individual-level panel data.
	* The dependent variable is a standardized index of gender equality attitudes (gender_att).
	* The key independent variables are four dummies: MPD (male parent of daughter), MPS (male parent of son), FPD (female parent of daughter), and FPS (female parent of son)
	* with people who do not have child during the survey duration as the reference group.

	global controls  c.age c.age_sq i.country_id i.syear
	reghdfe gender_att MPD MPS FPD FPS $controls, absorb(pid) vce(cluster pid)
	est store reg_main

* ============================
* Extract and Export Results – H1
* ============================

* Joint test of MPD = MPS and FPD = FPS
test (MPD = MPS) (FPD = FPS)
scalar F_joint = r(F)
scalar p_joint = Ftail(e(df_m), e(df_r), F_joint)

* Differences in coefficients
local diff_m = _b[MPD] - _b[MPS]
local diff_f = _b[FPD] - _b[FPS]

* Standard errors of differences
local se_m = sqrt(_se[MPD]^2 + _se[MPS]^2 - 2*e(V)["MPD","MPS"])
local se_f = sqrt(_se[FPD]^2 + _se[FPS]^2 - 2*e(V)["FPD","FPS"])

* Combined standard error (pooled diff, not necessary for F-test but for display)
local se_joint = sqrt(`se_m'^2 + `se_f'^2)

* Degrees of freedom
local df_r = e(df_r)
local df_m = e(df_m)

* N and cluster count
local N = e(N)
local N_pid = e(N_clust)

* Interpretation
local interp_joint = "positive coefficient is in line with hypothesis"


* ============================
* Display Results – H1
* ============================

di _n "{hline 60}"
di ">>> H1 Joint Test: MPD > MPS and FPD > FPS"
di "{hline 60}"
di "Diff MPD-MPS = " %6.3f `diff_m' "  (SE = " %6.3f `se_m' ")"
di "Diff FPD-FPS = " %6.3f `diff_f' "  (SE = " %6.3f `se_f' ")"
di "F(2, `df_r') = " %6.2f scalar(F_joint)
di "Joint p-value = " %6.4f scalar(p_joint)
di "N = `N', Participants = `N_pid'"
di "Interpretation: `interp_joint'"
di "{hline 60}"

* ============================
* Export results to CSV
* ============================
tempname hdl
file open `hdl' using "$OUT/h1_results.csv", write replace
file write `hdl' "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n

file write `hdl' "`diff_m'," "`se_joint'," "Wald," "`df_r'," "`df_m'," "`=scalar(p_joint)'," "`N'," "`N_pid'," "`interp_joint'" _n

file close `hdl'

