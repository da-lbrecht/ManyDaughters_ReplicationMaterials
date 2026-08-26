********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

//clear all
version 18.5
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
ssc install vreverse

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
global OUT "$ROOT/ou
*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidgeb* kidsex* sumkids)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources, for family status, education, and employment status
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgfamstd pgisced97 pgemplst)
	* pl contains all variables from the individual questionnaire, use for dependent variable (left-leaning preferences)
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0004)
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
	
	* Only keep heterosexual individuals
	keep if sexor == 0 //probabally heterosexual

	* Only keep 18 – 67 years old 
	gen age = syear - gebjahr
	tab age, m
	keep if inrange(age, 18, 67)
	
	* Only keep years 2005, 2009, 2014, 2019 (selected DV available in these years)
	keep if inlist(syear, 2005, 2009, 2014, 2019)

* GENERATE VARIABLES * 
    
	** plh0004 (left/right self-placement) as measure for left-leaning political preferences
	tab plh0004 	
	replace plh0004 = . if plh0004 < 0 //recode missings
	
	* reverse the variable so that the higher the value the more left-leaning
	vreverse plh0004, gen(rightleft)
	tab rightleft
    lab define rightleft 0 "0 -Far right" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10 -Far left", modify
	tab rightleft

	** Generate variable for whether participant has biological daughters 
	//adopted children & stepchildren not considered
	
	* generate variable on whether participant have Xth children in the survey year
	tab1 kidgeb*
	foreach v of varlist kidgeb01-kidgeb19 {
		gen `v'_s = .
	    replace `v'_s = 1 if `v' <= syear & `v' > 0 //having Xth child in suvey year
	    replace `v'_s = 0 if `v' > syear | `v' == -2 //not having Xth child
	}
	
	* generate variable on number of kids in the survey year
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
	
	* generate variable on presence of daughters in the survey year
	egen kidsex_all = anymatch(kidsex*_s), v(2) // 0:0 daught.; 1:at least 1 daught.
	tab kidsex_all, m
	foreach v of varlist kidsex*_s{
		replace kidsex_all =. if `v' == -1 //-1: no answer in sex of kid, 2 obs.
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
	tab1 sex hghinc pgfamstd pgisced97 pgemplst age sampreg migback sumkids_s
	* recode missings
	foreach v of varlist sex hghinc pgfamstd pgisced97 pgemplst age sampreg migbac sumkids_s {
		replace `v' = . if `v' <0
	}
	tab1 sex hghinc pgfamstd pgisced97 pgemplst age sampreg migback sumkids,m

	* summarise categories, build age groups and dummy on whether 3 kids or more
	sum hghinc, d
	recode hghinc (min/1499 = 1 "<1500 EUR") (1500/2499 = 2 "1500-2499 EUR") (2500/3499 = 3 "2500-3499 EUR") (3500/4499 = 4 "3500-4499 EUR") (4500/max = 5 ">=4500 EUR" ), gen(hinc_5cat)
	tab hinc_5cat
	
	recode pgfamstd (1 6 7 8 = 1 "Married") (3 = 2 "Single") (2 4 5 = 3 "Separated/divorced/widowed"), gen(famstd_3cat)
	tab famstd_3cat
	
	recode pgisced97 (0/2 = 1 "Low educ.") (3/5 = 2 "Medium educ.") (6 = 3 "High educ."), gen(educ_3cat)
	tab educ_3cat
	
	recode pgemplst (1 = 1 "Full-time employment") (2 = 2 "Regular part-time employment") (3 4 6 = 3 "Marginal part-time/(vocat.) training") (5 = 4 "Not employed"), gen(emplst_4cat)
	tab emplst_4cat
	
	recode age (min/24 = 1 "18-24") (25/39 = 2 "25-39") (40/54 = 3 "40-54") (55/67 = 4 "55-67"), gen(age_gr)
	tab age_gr
	
	recode sumkids_s (0 = 0 "No kid") (1 = 1 "1 kid") (2 = 2 "2 kids") (3/max = 3 "3 kids or more"), gen(sumkids_4cat)
	tab sumkids_4cat, m
	
	
* HYPOTHESIS TEST FOR H2 * 
** UPDATE: We only employ random effects model instead of employing both random and fixed effects models **

	/* We consider political left-right self-placement (plh0004) as a good measure of left-leaning preferences. We reverse the direction of the variable so that higher value denotes more left-leaning preferences (on a scale from 0 to 10).
	
	The dependent variables are availabe in four years: 2005, 2009, 2014, 2019. We run random effects linear regresion analysis with plh0004 as dependent variable, applying robust standard errors. Our independent variable kidsex_all has three categories denoting whether having biological daughters in the survey year: `No daughter', `Only daughter(s), and `Mixed'. 
	
	We also include the following control variables: gender, age group, education, employment status, net household income, marital status, number of biological children, whether youngest biological child <= 3 years, region (West or East Germany), and migration background.
	*/
	
	xtset pid syear
	
	//random effects model
	xtreg rightleft i.kidsex_all i.sex i.age_gr i.educ_3cat i.emplst_4cat i.hinc_5cat i.famstd_3cat i.sumkids_4cat i.youngest0to3_d i.sampreg i.migback, re vce(r)
	
		* Extract the required statistics
		
        * (i) coefficient estimates
		local coefficient_onlyd = _b[1.kidsex_all]
		local coefficient_mixed = _b[2.kidsex_all]

		* (ii) robust standard errors
		local se_onlyd = _se[1.kidsex_all]
		local se_mixed = _se[2.kidsex_all]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value_onlyd = _b[1.kidsex_all] / _se[1.kidsex_all]
		local p_value_onlyd = 2 * (1 - normal(abs(`p_value_onlyd')))
		
		local p_value_mixed = _b[2.kidsex_all] / _se[2.kidsex_all]
		local p_value_mixed = 2 * (1 - normal(abs(`p_value_mixed')))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_1kidsex_all = _b[1.kidsex_all]
		scalar coef_2kidsex_all = _b[2.kidsex_all]
		scalar p_value_1kidsex_all = 2 * (1 - normal(abs(_b[1.kidsex_all] / _se[1.kidsex_all])))
		scalar p_value_2kidsex_all = 2 * (1 - normal(abs(_b[2.kidsex_all] / _se[2.kidsex_all])))

		if coef_1kidsex_all > .01 & p_value_1kidsex_all < .05 {
			local interpretation_onlyd"in line with hypothesis"
		} 
		else {
			local interpretation_onlyd "not in line with hypothesis"
		}
		
		local interpretation_mixed "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient_onlyd'"
	di "Standard Error: `se_onlyd'"
	di "Coefficient: `coefficient_mixed'"
	di "Standard Error: `se_mixed'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value_onlyd'
	di "p-value: " `p_value_mixed'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation_onlyd'"
	di "Interpretation of directionality: " "`interpretation_mixed'"
	
	
	/*//2. fixed effects model
	xtreg rightleft i.kidsex_all i.age_gr i.educ_3cat i.emplst_4cat i.hinc_5cat i.famstd_3cat i.sumkids_4cat i.youngest0to3_d i.sampreg, fe vce(r)
	
		* Extract the required statistics
		
        * (i) coefficient estimates
		local coefficient_onlyd2 = _b[1.kidsex_all]
		local coefficient_mixed2 = _b[2.kidsex_all]

		* (ii) robust standard errors
		local se_onlyd2 = _se[1.kidsex_all]
		local se_mixed2 = _se[2.kidsex_all]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family2 = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual2 = e(df_r)
		local df_model2 = e(df_m)

		* (v) p-value
		local p_value_onlyd2 = _b[1.kidsex_all] / _se[1.kidsex_all]
		local p_value_onlyd2 = 2 * ttail(e(df_r), abs(`p_value_onlyd2'))
		
		local p_value_mixed2 = _b[2.kidsex_all] / _se[2.kidsex_all]
		local p_value_mixed2 = 2 * ttail(e(df_r), abs(`p_value_mixed2'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_1kidsex_all2 = _b[1.kidsex_all]
		scalar coef_2kidsex_all2 = _b[2.kidsex_all]
		scalar p_value_1kidsex_all2 = 2 * ttail(e(df_r), abs(_b[1.kidsex_all] / _se[1.kidsex_all]))
		scalar p_value_2kidsex_all2 = 2 * ttail(e(df_r), abs(_b[2.kidsex_all] / _se[2.kidsex_all]))

		if coef_1kidsex_all2 > 0 & p_value_1kidsex_all2 <.05 {
			local interpretation_onlyd2"in line with hypothesis"
		} 
		else {
			local interpretation_onlyd2 "not in line with hypothesis"
		}
		
		if coef_2kidsex_all2 > 0 & p_value_2kidsex_all2 <.05 {
			local interpretation_mixed2 "in line with hypothesis"
		} 
		else {
			local interpretation_mixed2 "not in line with hypothesis"
		
		}
	

	// Display the saved statistics
	di "Coefficient: `coefficient_onlyd2'"
	di "Standard Error: `se_onlyd2'"
	di "Coefficient: `coefficient_mixed2'"
	di "Standard Error: `se_mixed2'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value_onlyd2'
	di "p-value: " `p_value_mixed2'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of results: " "`interpretation_onlyd2'"
	di "Interpretation of results: " "`interpretation_mixed2'"
	*/
	
* EXPORT RESULTS * 

	* Export results to csv 
	
	file open myfile using "$OUT/h2_results.csv", write replace
    file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
*	    file write myfile "only daugthers, `coefficient_onlyd', `se_onlyd', `test_stat_family',`df_model',`p_value_onlyd',`num_obs',`num_participants',`interpretation_onlyd'" _n // commented out by Tina Wang
	    file write myfile "`coefficient_mixed', `se_mixed', `test_stat_family',`df_residual', `df_model',`p_value_mixed',`num_obs',`num_participants',`interpretation_mixed'" _n

	file close myfile

********************************************************************************

exit
