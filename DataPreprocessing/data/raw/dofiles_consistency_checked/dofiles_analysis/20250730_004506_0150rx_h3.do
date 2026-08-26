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

global IN   "$ROOT/data/raw" 
global DATA "$ROOT/data/derived" 
global OUT  "$ROOT/out" 
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
	merge m:1 pid syear using "$IN/pl.dta", keepusing(plf0028 plf0029 plf0030 plf0031 plf0032 plf0033 plf0034_h)
	keep if _merge == 3
	drop _merge

		* gender attitudes *
		gen housework_total = plf0028 + plf0029 + plf0030 + plf0031 + plf0032 + plf0033 + plf0034_h
		egen year_hid = group(syear hid)
		bysort year_hid: egen housework_family_total = total(housework_total)
		gen ratio_housework = housework_total / housework_family_total
replace ratio_housework = 0 if housework_family_total == 0
		
	* save data *
	label language EN // change language of the labels to English
	save "$DATA/h3_dataset.dta", replace // save merged dataset
	
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
	drop if missing(ratio_housework)

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
	
* HYPOTHESIS TEST FOR H3
* Does having daughters lead to greater gender equality in household behavior?

	* We test whether becoming a parent of a daughter (vs. a son) is associated with a more egalitarian division of household labor.
	* We estimate a fixed-effects panel regression model using individual-level panel data.
	* The dependent variable is the share of household labor performed by the male partner within the household (ratio_housework).
	* The key independent variables are four dummies: MPD (male parent of daughter), MPS (male parent of son), FPD (female parent of daughter), and FPS (female parent of son),
	* with people who do not have children during the survey duration as the reference group.
	* Key parameter of interest: βMPD > βMPS

	global controls  c.age c.age_sq i.country_id i.syear
	reghdfe ratio_housework MPD MPS FPD FPS $controls, absorb(pid) vce(cluster pid)
	est store reg_main

* ============================
* Extract and Export Results – H3
* ============================
test MPD = MPS
scalar t_m   = sign(_b[MPD]-_b[MPS]) * sqrt(r(F))
scalar p_m   = ttail(e(df_r), -t_m)

* Compute difference and SE
local diff_m = _b[MPD] - _b[MPS]
local se_m   = sqrt(_se[MPD]^2 + _se[MPS]^2 - 2*e(V)["MPD","MPS"])

* Sample size and df
local df_r   = e(df_r)
local df_m   = e(df_m)
local N      = e(N)
local N_pid  = e(N_clust)

* Save p-value as scalar to allow macro use
local p_m    = scalar(p_m)

* Interpretation: positive and p < 0.05
local interpretation_m = "positive coefficient is in line with hypothesis"


* ============================
* Display Results – H3
* ============================
di _n "{hline 60}"
di ">>> H3 – Daughter vs Son Effect (One-sided)"
di "{hline 60}"
di "Coefficients:"
di "   MPD (Male, Daughter)  = " %7.3f _b[MPD] "  (se=" %7.3f _se[MPD] ")"
di "   MPS (Male, Son)       = " %7.3f _b[MPS] "  (se=" %7.3f _se[MPS] ")"
di _n "Hypothesis tests:"
di "   H0: βMPD ≤ βMPS   (one-sided) p = " %6.4f `p_m'
di "Obs = " `N' ", Participants = " `N_pid'
di "{hline 60}"

* ============================
* Export results to CSV
* ============================
tempname h3
file open `h3' using "$OUT/h3_results.csv", write replace
file write `h3' "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
// file write `h3' "`diff_m',`se_m',`Wald',`df_r',`df_m',`p_m',`N',`N_pid',`interpretation_m'" _n
file write `h3' "`diff_m',`se_m'," "F," "`df_r',`df_m',`p_m',`N',`N_pid',`interpretation_m'" _n // Adapted by David Albrecht during consistency check

file close `h3'