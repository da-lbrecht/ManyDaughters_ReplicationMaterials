********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

//clear all
version 18.5
set maxvar 10000 // extend the maximum number of variables to 10,000

/*
* CONFIGURE FILE PATHS * (Please do not change)
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

* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidgeb* kidsex* sumkids)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources, for family status, education, and employment status
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgfamstd pgisced97 pgemplst)
	* pl contains all variables from the individual questionnaire, for building dependent variables (house spent in domestic work)
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pli0044_h pli0043_h pli0049_h pli0019_h pli0012_h pli0031_h pli0022_h pli0016_h pli0034_v4)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgtyp2hh)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * 
 
	* Only keep participants from private households
	keep if inlist(pop,1,2)
	
	* Only keep 18 – 67 years old 
	gen age = syear - gebjahr
	tab age, m
	keep if inrange(age, 18, 67)
	
	* Only keep heterosexual couples in the same household
	//use sexor (sexual orientation), hgtyp2hh (HH typology)
	tab1 sexor hgtyp2hh
	keep if sexor == 0 // probably heterosexual 
	keep if inlist(hgtyp2hh, 21, 41, 42, 43, 51, 52, 53, 61, 62) //HH more than 2 generations excluded
	drop if parid <0 //drop persons in HH that have no partner (no parid)
	
	* Note: we will run analysis at the household level later on and only keep for each household the male partner to account for their socio-demographics (see 'HYPOTHESIS TEST FOR H3'). This is not done now as information from both partners is needed for generating our dependent variable.

	
* GENERATE VARIABLES * 
    
	/** OLD **
	** Male partner's share in domestic work as measure of gender equality in HH
	//share on weedays, saturdays and sundays separately due to data availability
	
	sort hid syear pid
	
	// Weekdays
	tab1 pli0044_h pli0043_h pli0049_h //childcare, housework, repair work
	foreach v of varlist pli0044_h pli0043_h pli0049_h {
		replace `v' = . if `v' <0 & `v' != -2 //recoding missings
		replace `v' = 0 if `v' == -2 //0 hours when does not apply
	}
	gen domstcwd_sf = pli0044_h + pli0043_h + pli0049_h
	tab domstcwd_sf
	
	* partners work
	gen domstcwd_pt = domstcwd_sf[_n+1] if parid == pid[_n+1] & syear == syear[_n+1]
	replace domstcwd_pt = domstcwd_sf[_n-1] if parid == pid[_n-1] & syear == syear[_n-1] 
	tab domstcwd_pt
	
	* male partner's share
	gen domstcwd_rt = domstcwd_sf/(domstcwd_sf + domstcwd_pt)*100 if sex == 1 //for men
	replace domstcwd_rt = domstcwd_pt/(domstcwd_sf + domstcwd_pt)*100 if sex == 2 //for women
	tab domstcwd_rt
	
	// Saturdays
	tab1 pli0019_h pli0012_h pli0031_h //childcare, housework, repair work
	foreach v of varlist pli0019_h pli0012_h pli0031_h {
		replace `v' = . if `v' <0 & `v' != -2 //recoding missings
		replace `v' = 0 if `v' == -2 //0 hours when does not apply
	}
	gen domstcsat_sf = pli0019_h + pli0012_h + pli0031_h
	
	* partners work
	gen domstcsat_pt = domstcsat_sf[_n+1] if parid == pid[_n+1] & syear == syear[_n+1]
	replace domstcsat_pt = domstcsat_sf[_n-1] if parid == pid[_n-1] & syear == syear[_n-1]
	
	* male partner's share
	gen domstcsat_rt = domstcsat_sf/(domstcsat_sf + domstcsat_pt)*100 if sex == 1 //for men
	replace domstcsat_rt = domstcsat_pt/(domstcsat_sf + domstcsat_pt)*100 if sex == 2 //for women
	
	// Sundays
	tab1 pli0022_h pli0016_h pli0034_v4 //childcare, housework, repair work
	foreach v of varlist pli0022_h pli0016_h pli0034_v4 {
		replace `v' = . if `v' <0 & `v' != -2 //recoding missings
		replace `v' = 0 if `v' == -2 //0 hours when does not apply
	}
	gen domstcsun_sf = pli0022_h + pli0016_h + pli0034_v4
	
	* partners work
	gen domstcsun_pt = domstcsun_sf[_n+1] if parid == pid[_n+1] & syear == syear[_n+1]
	replace domstcsun_pt = domstcsun_sf[_n-1] if parid == pid[_n-1] & syear == syear[_n-1]
	tab domstcsun_pt, m
	
	* male partner's share
	gen domstcsun_rt = domstcsun_sf/(domstcsun_sf + domstcsun_pt)*100 if sex == 1 //for men
	replace domstcsun_rt = domstcsun_pt/(domstcsun_sf + domstcsun_pt)*100 if sex == 2 //for women
	*/
***********************************

** UPDATE: We use male partner's share in domestic work during WEEKDAYS as measure of gender equality in HH (dependent variable), as the items were surveyed in each year from 2000 to 2020. Items on Saturdays and Sundays cannot be combined with items on weekays because the former ones were only surveyed in selective years (and partly different years for Sat and Sun) **
	
	* own work (hours spent in HH)
	sort hid syear pid
	
	tab1 pli0044_h pli0043_h pli0049_h //childcare, housework, repair work
	foreach v of varlist pli0044_h pli0043_h pli0049_h {
		replace `v' = . if `v' <0 & `v' != -2 //recoding missings
		replace `v' = 0 if `v' == -2 //0 hours when does not apply
	}
	gen domstcwd_sf = pli0044_h + pli0043_h + pli0049_h
	tab domstcwd_sf
	
	* partners work
	gen domstcwd_pt = domstcwd_sf[_n+1] if parid == pid[_n+1] & syear == syear[_n+1]
	replace domstcwd_pt = domstcwd_sf[_n-1] if parid == pid[_n-1] & syear == syear[_n-1] 
	tab domstcwd_pt
	
	* male partner's share
	gen domstcwd_rt = domstcwd_sf/(domstcwd_sf + domstcwd_pt)*100 if sex == 1 //for men
	replace domstcwd_rt = domstcwd_pt/(domstcwd_sf + domstcwd_pt)*100 if sex == 2 //for women
	tab domstcwd_rt
	
******************************
	
	** Generate variable for whether participant has biological daughters 
	//adopted children & stepchildren not considered
	
	* generate variable on whether participant have Xth children in the survey year
	tab1 kidgeb*
	foreach v of varlist kidgeb01-kidgeb19 {
		gen `v'_s = .
	    replace `v'_s = 1 if `v' <= syear & `v' > 0 //having Xth child in suvey year
	    replace `v'_s = 0 if `v' > syear | `v' == -2 //not having Xth child
	}
	
	tabstat domstc*_rt, stat(p50 mean) c(s)
	
	* generate variable on number of birth kids in the survey year
	egen sumkids_s = anycount(kidgeb*_s), v(1) 
	foreach v of varlist kidgeb*_s {
		replace sumkids_s = . if `v' ==. //recode missings
	}
	lab var sumkids_s "Number of kids in the survey year"

	* generate variable on sex of Xth in the survey year
	tab1 kidsex*, m //1 male, 2 female; -2 (no applied): no Xth child overall
	forvalues c=1/9 {
		clonevar kidsex0`c'_s = kidsex0`c'
		replace kidsex0`c'_s = -2 if kidgeb0`c'_s == 0 //no xth child in survey year
	}
	
	forvalues c=10/19 {
		clonevar kidsex`c'_s = kidsex`c'
		replace kidsex`c'_s = -2 if kidgeb`c'_s == 0 //no Xth child in survey year
	}
	
	* generate variable on presence of birth daughters in the survey year
	egen kidsex_all = anymatch(kidsex*_s), v(2) // 0:0 daught.; 1:at least 1 daught.
	tab kidsex_all, m
	foreach v of varlist kidsex*_s{
		replace kidsex_all =. if `v' == -1 //-1: no answer in sex of kid, 5 obs.
		replace kidsex_all = 2 if kidsex_all == 1 & `v' == 1 //mixed seperated from only daughters 
	}
	
	*summarise categories
	lab define kidsex_all 0 "No daughter" 1 "Only daughter(s)" 2 "Mixed", modify
	lab val kidsex_all kidsex_all 
	tab kidsex_all, m
	
	** Generate dummy on whether youngest child <= 3 (as control variable)
	foreach v of varlist kidgeb01-kidgeb19 {
		gen `v'_age =  syear - `v'
		replace `v'_age = . if `v'_age < 0 // Xth child born after survey year
		replace `v'_age = . if `v'== -2 //no Xth child 
		replace `v'_age = .a if `v'== -1 //no answer 

		tab `v'_age, m
	}
	
	tab1 kid*_age
	egen youngest0to3 = rowmin(kid*_age)
	foreach v of varlist kidgeb01-kidgeb19 {
		replace youngest0to3 = .a if `v' == -1 //recode if no answer 
	}
	tab youngest0to3,m
	
	recode youngest0to3 (0/3 = 1 "Yes") (4/max = 0 "No"), gen(youngest0to3_d)
    replace youngest0to3_d = 0 if youngest0to3_d == . //recode if no children 
	tab youngest0to3_d, m
	
	** Further control variables
	tab1 sex hghinc pgisced97 pgemplst age sampreg migback sumkids_s
	* recode missings
	foreach v of varlist sex hghinc pgisced97 pgemplst age sampreg migbac {
		replace `v' = . if `v' <0
	}
	tab1 sex hghinc pgfamstd pgisced97 age sampreg migback sumkids_s,m

	* summarise categories, build age groups and dummy on whether 3 kids or more
	sum hghinc, d
	recode hghinc (min/1499 = 1 "<1500 EUR") (1500/2499 = 2 "1500-2499 EUR") (2500/3499 = 3 "2500-3499 EUR") (3500/4499 = 4 "3500-4499 EUR") (4500/max = 5 ">=4500 EUR" ), gen(hinc_5cat)
	tab hinc_5cat
	
	recode pgisced97 (0/2 = 1 "Low educ.") (3/5 = 2 "Medium educ.") (6 = 3 "High educ."), gen(educ_3cat)
	tab educ_3cat
	
	recode pgemplst (1 = 1 "Full-time employment") (2 = 2 "Regular part-time employment") (3 4 6 = 3 "Marginal part-time/(vocat.) training") (5 = 4 "Not employed"), gen(emplst_4cat)
	tab emplst_4cat
	
	recode age (min/24 = 1 "18-24") (25/39 = 2 "25-39") (40/54 = 3 "40-54") (55/67 = 4 "55-67"), gen(age_gr)
	tab age_gr
	
	recode sumkids_s (0 = 0 "No kid") (1 = 1 "1 kid") (2 = 2 "2 kids") (3/max = 3 "3 kids or more"), gen(sumkids_4cat)
	tab sumkids_4cat, m

*********************************	
/** OLD **
* HYPOTHESIS TEST FOR H3 * 
	
	/* We consider the male partner's share in domestic work as a good measure for gender equality in the household. We use three sets of variables denoting hours spent in childcare, housework, and repair work, respectively, during weekdays (pli0044_h, pli0043_h, pli0049_h), Saturdays (pli0019_h, pli0012_h, pli0031_h), and Sundays (pli0022_h, pli0016_h, pli0034_v4). We add the hours spent in the three domains together and calculate for each household the male partner's share, ranging from 0 to 100: A higher share indicates more gender equality in HH. We calculate the share for weekdays, Saturdays, and Sundays separately, as information for Saturdays and Sundays is not available in some years, while information for weekdays is available each year from 2000 to 2020. Therefore, we obtain three dependent variables.
	
	We run both random effects and fixed effects linear regresion analysis with plh0004 as dependent variable, applying robust standard errors. Our independent variable kidsex_all has three categories denoting whether having biological daughters in the survey year: `No daughter', `Only daughter(s), and `Mixed'. 
	
	We also include the following control variables: gender, age group, education, employment status, net household income, marital status, number of biological children, whether youngest biological child <= 3 years, region (West or East Germany), and migration background. For fixed effects models, gender and migraiton background as time-invariant variables are excluded.
	*/
	
	* Run analysis at household level
	//Only keep male partner in HH to account for his socio-demographics
	keep if sex == 1
	
	xtset pid syear
	
	//1. random effects model
	local c = 1
	foreach v of varlist domstcwd_rt domstcsat_rt domstcsun_rt {
		xtreg `v' i.kidsex_all i.age_gr i.educ_3cat i.emplst_4cat i.hinc_5cat i.sumkids_4cat i.youngest0to3_d i.sampreg i.migback, re vce(r)
		
		* Extract the required statistics
		
        * (i) coefficient estimates
		local coefficient_onlyd_re`c' = _b[1.kidsex_all]
		local coefficient_mixed_re`c' = _b[2.kidsex_all]

		* (ii) robust standard errors
		local se_onlyd_re`c' = _se[1.kidsex_all]
		local se_mixed_re`c' = _se[2.kidsex_all]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_re = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_re`c' = e(df_r)
		local df_model_re`c' = e(df_m)

		* (v) p-value
		local p_value_onlyd_re`c' = _b[1.kidsex_all] / _se[1.kidsex_all]
		local p_value_onlyd_re`c' = 2 * (1 - normal(abs(`p_value_onlyd_re`c'')))
		
		local p_value_mixed_re`c' = _b[2.kidsex_all] / _se[2.kidsex_all]
		local p_value_mixed_re`c' = 2 * (1 - normal(abs(`p_value_mixed_re`c'')))

		* (vi) number of observations
		local num_obs_re`c' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants_re`c' : word count `participants'

		* (viii) interpretation of results
		scalar coef_1kidsex_all_re`c' = _b[1.kidsex_all]
		scalar coef_2kidsex_all_re`c' = _b[2.kidsex_all]
		scalar p_value_1kidsex_all_re`c' = 2 * (1 - normal(abs(_b[1.kidsex_all] / _se[1.kidsex_all])))
		scalar p_value_2kidsex_all_re`c' = 2 * (1 - normal(abs(_b[2.kidsex_all] / _se[2.kidsex_all])))

		if coef_1kidsex_all_re`c' > .1 & p_value_1kidsex_all_re`c' < .05 {
			local interpretation_onlyd_re`c' "in line with hypothesis"
		} 
		else {
			local interpretation_onlyd_re`c' "not in line with hypothesis"
		}
		
		if coef_2kidsex_all_re`c' > .1 & p_value_2kidsex_all_re`c' < .05 {
			local interpretation_mixed_re`c' "in line with hypothesis"
		} 
		else {
			local interpretation_mixed_re`c' "not in line with hypothesis"
		
		}
	
	// Display the saved statistics
	di "Coefficient: `coefficient_onlyd_re`c''"
	di "Standard Error: `se_onlyd_re`c''"
	di "Coefficient: `coefficient_mixed_re`c''"
	di "Standard Error: `se_mixed_re`c''"
	di "Test Statistic Family: `test_stat_family_re'"
	di "Residual Degrees of Freedom: `df_residual_re`c''"
	di "Model Degrees of Freedom: `df_model_re`c''"
	di "p-value: " `p_value_onlyd_re`c''
	di "p-value: " `p_value_mixed_re`c''
	di "Number of observations: " `num_obs_re`c''
	di	"Number of participants: " `num_participants_re`c''
	di "Interpretation of results: " "`interpretation_onlyd_re`c''"
	di "Interpretation of results: " "`interpretation_mixed_re`c''"
	
	local c = `c'+1		
	}

	//2. fixed effects model
	local c = 1
	foreach v of varlist domstcwd_rt domstcsat_rt domstcsun_rt {
		xtreg `v' i.kidsex_all i.age_gr i.educ_3cat i.emplst_4cat i.hinc_5cat i.sumkids_4cat i.youngest0to3_d i.sampreg, fe vce(r)
		
		* Extract the required statistics
		
        * (i) coefficient estimates
		local coefficient_onlyd_fe`c' = _b[1.kidsex_all]
		local coefficient_mixed_fe`c' = _b[2.kidsex_all]

		* (ii) robust standard errors
		local se_onlyd_fe`c' = _se[1.kidsex_all]
		local se_mixed_fe`c' = _se[2.kidsex_all]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_fe = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_fe`c' = e(df_r)
		local df_model_fe`c' = e(df_m)

		* (v) p-value
		local p_value_onlyd_fe`c' = _b[1.kidsex_all] / _se[1.kidsex_all]
		local p_value_onlyd_fe`c' = 2 * ttail(e(df_r), abs(`p_value_onlyd_fe`c''))
		
		local p_value_mixed_fe`c' = _b[2.kidsex_all] / _se[2.kidsex_all]
		local p_value_mixed_fe`c' = 2 * ttail(e(df_r), abs(`p_value_mixed_fe`c''))

		* (vi) number of observations
		local num_obs_fe`c' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants_fe`c' : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_1kidsex_all_fe`c' = _b[1.kidsex_all]
		scalar coef_2kidsex_all_fe`c' = _b[2.kidsex_all]
		scalar p_value_1kidsex_all_fe`c' = 2 * ttail(e(df_r), abs(_b[1.kidsex_all] / _se[1.kidsex_all]))
		scalar p_value_2kidsex_all_fe`c' = 2 * ttail(e(df_r), abs(_b[2.kidsex_all] / _se[2.kidsex_all]))

		if coef_1kidsex_all_fe`c' > 0 & p_value_1kidsex_all_fe`c' <.05 {
			local interpretation_onlyd_fe`c' "in line with hypothesis"
		} 
		else {
			local interpretation_onlyd_fe`c' "not in line with hypothesis"
		}
		
		if coef_2kidsex_all_fe`c' > 0 & p_value_2kidsex_all_fe`c' <.05 {
			local interpretation_mixed_fe`c' "in line with hypothesis"
		} 
		else {
			local interpretation_mixed_fe`c' "not in line with hypothesis"
		
		}
	
	// Display the saved statistics
	di "Coefficient: `coefficient_onlyd_fe`c''"
	di "Standard Error: `se_onlyd_fe`c''"
	di "Coefficient: `coefficient_mixed_fe`c''"
	di "Standard Error: `se_mixed_fe`c''"
	di "Test Statistic Family: `test_stat_family_fe'"
	di "Residual Degrees of Freedom: `df_residual_fe`c''"
	di "Model Degrees of Freedom: `df_model_fe`c''"
	di "p-value: " `p_value_onlyd_fe`c''
	di "p-value: " `p_value_mixed_fe`c''
	di "Number of observations: " `num_obs_fe`c''
	di	"Number of participants: " `num_participants_fe`c''
	di "Interpretation of results: " "`interpretation_onlyd_fe`c''"
	di "Interpretation of results: " "`interpretation_mixed_fe`c''"
	
	local c = `c'+1		
	}


* EXPORT RESULTS * 

	* Export results to csv 
	local x1 "Male partner's share in domest. work: Workdays"
	local x2 "Male partner's share in domest. work: Saturdays"
	local x3 "Male partner's share in domest. work: Sundays" 
	
	file open myfile using "$OUT/h3_results.csv", write replace
    file write myfile " , coefficient,r_se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	forvalues c=1/3 {
		file write myfile "`x`c''" _n
		file write myfile "RE model" _n
	    file write myfile "only daugthers, `coefficient_onlyd_re`c'', `se_onlyd_re`c'', `test_stat_family_re', `df_residual_re`c'', `df_model_re`c'',`p_value_onlyd_re`c'',`num_obs_re`c'',`num_participants_re`c'',`interpretation_onlyd_re`c''" _n
	    file write myfile "mixed, `coefficient_mixed_re`c'', `se_mixed_re`c'', `test_stat_family_re', `df_residual_re`c'', `df_model_re`c'',`p_value_mixed_re`c'',`num_obs_re`c'',`num_participants_re`c'',`interpretation_mixed_re`c''" _n
		
		file write myfile "FE model" _n
	    file write myfile "only daugthers, `coefficient_onlyd_fe`c'', `se_onlyd_fe`c'', `test_stat_family_fe', `df_residual_fe`c'', `df_model_fe`c'',`p_value_onlyd_fe`c'',`num_obs_fe`c'',`num_participants_fe`c'',`interpretation_onlyd_fe`c''" _n
	    file write myfile "mixed, `coefficient_mixed_fe`c'', `se_mixed_fe`c'', `test_stat_family_fe', `df_residual_fe`c'', `df_model_fe`c'',`p_value_mixed_fe`c'',`num_obs_fe`c'',`num_participants_fe`c'',`interpretation_mixed_fe`c''" _n

	}

	file close myfile
*/	

********************************************


** UPDATE: Hypothesis test using male partner's share in various domestic work during WEEKDAYS as the only dependent variable. We only employ fixed effects model instead of employing both fixed and random effects models **	

* HYPOTHESIS TEST FOR H3 * 
	
	/* We consider the male partner's share in domestic work during weekdays as a good measure for gender equality in the household. We use three variables denoting hours spent during weekdays in childcare, housework, and repair work, respectively (pli0044_h, pli0043_h, pli0049_h). We add the hours spent in the three domains together and calculate for each household the male partner's share as our dependent variable, ranging from 0 to 100: A higher share indicates more gender equality in HH.
	
	We run fixed effects linear regresion analysis with domstcwd_rt as dependent variable, applying robust standard errors. Our independent variable kidsex_all has three categories denoting whether having biological daughters in the survey year: `No daughter', `Only daughter(s), and `Mixed'. 
	
	We also include the following control variables: age group, education, employment status, net household income, marital status, number of biological children, whether youngest biological child <= 3 years, region (West or East Germany). Gender and migraiton background as time-invariant variables are not included.
	*/
	
	* Run analysis at household level
	//Only keep male partner in HH to account for his socio-demographics
	keep if sex == 1
	
	xtset pid syear
	
	//fixed effects model
		xtreg domstcwd_rt i.kidsex_all i.age_gr i.educ_3cat i.emplst_4cat i.hinc_5cat i.sumkids_4cat i.youngest0to3_d i.sampreg, fe vce(r)
		
		* Extract the required statistics
		
        * (i) coefficient estimates
		local coefficient_onlyd_fe = _b[1.kidsex_all]
		local coefficient_mixed_fe = _b[2.kidsex_all]

		* (ii) robust standard errors
		local se_onlyd_fe = _se[1.kidsex_all]
		local se_mixed_fe = _se[2.kidsex_all]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family_fe = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual_fe = e(df_r) 
		local df_model_fe = e(df_m)

		* (v) p-value
		local p_value_onlyd_fe = _b[1.kidsex_all] / _se[1.kidsex_all]
		local p_value_onlyd_fe = 2 * ttail(e(df_r), abs(`p_value_onlyd_fe'))
		
		local p_value_mixed_fe = _b[2.kidsex_all] / _se[2.kidsex_all]
		local p_value_mixed_fe = 2 * ttail(e(df_r), abs(`p_value_mixed_fe'))

		* (vi) number of observations
		local num_obs_fe = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants_fe : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_1kidsex_all_fe = _b[1.kidsex_all]
		scalar coef_2kidsex_all_fe = _b[2.kidsex_all]
		scalar p_value_1kidsex_all_fe = 2 * ttail(e(df_r), abs(_b[1.kidsex_all] / _se[1.kidsex_all]))
		scalar p_value_2kidsex_all_fe = 2 * ttail(e(df_r), abs(_b[2.kidsex_all] / _se[2.kidsex_all]))

		if coef_1kidsex_all_fe > 0 & p_value_1kidsex_all_fe <.05 {
			local interpretation_onlyd_fe "in line with hypothesis"
		} 
		else {
			local interpretation_onlyd_fe "not in line with hypothesis"
		}

		local interpretation_mixed_fe "positive coefficient is in line with hypothesis"
	
	// Display the saved statistics
	di "Coefficient: `coefficient_onlyd_fe'"
	di "Standard Error: `se_onlyd_fe'"
	di "Coefficient: `coefficient_mixed_fe'"
	di "Standard Error: `se_mixed_fe'"
	di "Test Statistic Family: `test_stat_family_fe'"
	di "Residual Degrees of Freedom: `df_residual_fe'"
	di "Model Degrees of Freedom: `df_model_fe'"
	di "p-value: " `p_value_onlyd_fe'
	di "p-value: " `p_value_mixed_fe'
	di "Number of observations: " `num_obs_fe'
	di	"Number of participants: " `num_participants_fe'
	di "Interpretation of results: " "`interpretation_onlyd_fe'"
	di "Interpretation of results: " "`interpretation_mixed_fe'"

    //Residual Degrees of Freedom NOT applicable for RE-Models, not to export

* EXPORT RESULTS * 

	* Export results to csv 
	
	file open myfile using "$OUT/h3_results.csv", write replace
    file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	
*	file write myfile "only daugthers, `coefficient_onlyd_fe', `se_onlyd_fe', `test_stat_family_fe', `df_residual_fe', `df_model_fe',`p_value_onlyd_fe',`num_obs_fe',`num_participants_fe',`interpretation_onlyd_fe'" _n //commented out by Tina Wang
	file write myfile "`coefficient_mixed_fe', `se_mixed_fe', `test_stat_family_fe', `df_residual_fe', `df_model_fe',`p_value_mixed_fe',`num_obs_fe',`num_participants_fe',`interpretation_mixed_fe'" _n

	file close myfile


********************************************************************************

exit
