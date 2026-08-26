********************************************************************************
****************** Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * 
* ============================

//clear all
version 18.5
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
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
global OUT "$ROOT/out" // folder for output (results, tables)
*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidgeb* kidsex* sumkids)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources, for family status, education, and employment status
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgfamstd pgisced11 pgemplst)
	* pl contains all variables from the individual questionnaire; for dependent variables (support for gender equality)
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0298_v2 plh0302_v2 plh0308_v2)
	* hgen contains generated variables on household level from the household interview; for household net income and household composition
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgtyp2hh)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


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
	
	* Only keep year 2018 as selected DVs only in this year
	keep if syear == 2018
	

* GENERATE VARIABLES * 
    
	/** OLD **
	** plh0298_v2, plh0302_v2, plh0308_v2 as measures for support for gender equaity

	tab1 plh0298_v2 plh0302_v2 plh0308_v2 
	
	* clean missings
	foreach v of varlist plh0298_v2 plh0302_v2 plh0308_v2 {
		replace `v' = . if `v' < 0
	}
		
	* check correlation
	pwcorr plh0298_v2 plh0302_v2 plh0308_v2, sig obs 
	//no cor. btw last item and former two; Treat them as three different DVs
	
	* reverse the first two items to the same direction as the third
	//higher value means strong support for gender equality 
	vreverse plh0298_v2, gen(plh0298_v2_r)
	vreverse plh0302_v2, gen(plh0302_v2_r)
	codebook plh0298_v2_r plh0302_v2_r
	lab define plh0298_v2_r 1 "1 -Completely agree" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7 -Completely disagree", modify
	lab define plh0302_v2_r 1 "1 -Completely agree" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7 -Completely disagree", modify
	tab1 plh0298_v2_r plh0302_v2_r
	*/
	
********************
	
	
** UPDATE: Use plh0298_v2 & plh0302_v2 as measures for support for gender equaity, combine them in to one dependent variable **

	tab1 plh0298_v2 plh0302_v2 
	
	* clean missings
	foreach v of varlist plh0298_v2 plh0302_v2 plh0308_v2 {
		replace `v' = . if `v' < 0
	}
		
	* check correlation
	pwcorr plh0298_v2 plh0302_v2, sig obs 
	//high corr.: 0.77, and highly sig.
	
	* combine items into one DV 
	gen gndrequal_tmp = plh0298_v2 + plh0302_v2 //scale 2-14
	
	* reverse item: higher value, stronger support for gender equality 
	vreverse gndrequal_tmp, gen(gndrequal)
	lab drop gndrequal 
	drop gndrequal_tmp
	tab gndrequal
	
*******************
	
	** Generate variable for whether participant has daughters 
	//adopted children and stepchildren not considered
	
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
	egen kidsex_all = anymatch(kidsex*_s), v(2) // 0: 0 daughter; 1: at least 1 daughter 
	tab kidsex_all, m
	foreach v of varlist kidsex*_s{
		replace kidsex_all =. if `v' == -1 //-1: no answer in sex of kid, 0 obs.
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
	tab1 sex hghinc pgfamstd pgisced11 pgemplst age sampreg migback sumkids
	* recode missings
	foreach v of varlist sex hghinc pgfamstd pgisced11 pgemplst age sampreg migback sumkids sexor {
		replace `v' = . if `v' <0
	}
	tab1 sex hghinc pgfamstd pgisced11 pgemplst age sampreg migback sumkids,m

	* summarise categories, build age groups and dummy on whether 3 kids or more
	sum hghinc, d
	recode hghinc (min/1499 = 1 "<1500 EUR") (1500/2499 = 2 "1500-2499 EUR") (2500/3499 = 3 "2500-3499 EUR") (3500/4499 = 4 "3500-4499 EUR") (4500/max = 5 ">=4500 EUR" ), gen(hinc_5cat)
	tab hinc_5cat
	
	recode pgfamstd (1 6 = 1 "Married") (3 = 2 "Single") (2 4 5 = 3 "Separated/divorced/widowed"), gen(famstd_3cat)
	tab famstd_3cat
	
	recode pgisced11 (0/2 = 1 "Low educ.") (3/5 = 2 "Medium educ.") (6/8 = 3 "High educ."), gen(educ_3cat)
	tab educ_3cat
		
	recode pgemplst (1 = 1 "Full-time employment") (2 = 2 "Regular part-time employment") (3 4 6 = 3 "Marginal part-time/(vocat.) training") (5 = 4 "Not employed"), gen(emplst_4cat)
	tab emplst_4cat
	
	recode age (min/24 = 1 "18-24") (25/39 = 2 "25-39") (40/54 = 3 "40-54") (55/67 = 4 "55-67"), gen(age_gr)
	tab age_gr
	
	recode sumkids_s (0 = 0 "No kid") (1 = 1 "1 kid") (2 = 2 "2 kids") (3/max = 3 "3 kids or more"), gen(sumkids_4cat)
	tab sumkids_4cat, m
	
	
/** OLD **	
* HYPOTHESIS TEST FOR H4 * 

	/* We consider attitudes towards the following three statements as good measures of support for gender equality: 1) Children below the age of 6 suffer if their mother works (plh0298_v2); 2) Children below the age of 3 suffer if their mother works (plh0302_v2); 3) Best if man and woman work the same amount so they can share the responsibility (plh0308_v2). We reverse the direction of the first two variables (plh0298_v2_r and plh0302_v2_r) so that for all three variables (ranging from 1 to 7): the higher the value, the stronger the support for gender equality. 
	
	The three dependent variables are only availabe in 2018 (The first two variables and a similar version of the third variable were also surveyed in 2012 but on a much smaller, completely different group of participants.). Therefore, we run linear OLS regresion analysis for each dependent variable, applying sampling weights (phrf) and standard errors at the household level. Our independent variable kidsex_all has three categories denoting whether having biological daughters in the survey year: `No daughter', `Only daughter(s), and `Mixed'. We interact kidsex_all with sex (female or male) to test whether having daughters leads to a larger increase in the support of gender equality for men than for women.
	
	We also include the following control variables: gender, age group, education, employment status, net household income, marital status, number of biological children, whether youngest biological child <= 3 years, region (West or East Germany), and migration background.
	*/

local c = 1
	foreach v of varlist plh0298_v2_r plh0302_v2_r plh0308_v2 {
		reg `v' i.kidsex_all##ib2.sex i.age_gr i.educ_3cat i.emplst_4cat i.hinc_5cat i.famstd_3cat i.sumkids_4cat i.youngest0to3_d i.sampreg i.migback [pw=phrf], vce(cluster hid)
		
		* Extract the required statistics
		
        * (i) coefficient estimates
		local coefficient_onlyd`c' = _b[1.kidsex_all]
		local coefficient_mixed`c' = _b[2.kidsex_all]
		
		local coefficient_onlyd`c'_male = _b[1.kidsex_all#1.sex]
		local coefficient_mixed`c'_male = _b[2.kidsex_all#1.sex]
		
		local coefficient_male`c' = _b[1.sex]

		* (ii) standard errors
		local se_onlyd`c' = _se[1.kidsex_all]
		local se_mixed`c' = _se[2.kidsex_all]
		
		local se_onlyd`c'_male = _se[1.kidsex_all#1.sex]
	    local se_mixed`c'_male = _se[2.kidsex_all#1.sex]
		
		local se_male`c' = _se[1.sex]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual`c' = e(df_r)
		local df_model`c' = e(df_m)

		* (v) p-value
		local p_value_onlyd`c' = _b[1.kidsex_all] / _se[1.kidsex_all]
		local p_value_onlyd`c' = 2 * ttail(e(df_r), abs(`p_value_onlyd`c''))
		
		local p_value_mixed`c' = _b[2.kidsex_all] / _se[2.kidsex_all]
		local p_value_mixed`c' = 2 * ttail(e(df_r), abs(`p_value_mixed`c''))		
		
		local p_value_onlyd`c'_male = _b[1.kidsex_all#1.sex] / _se[1.kidsex_all#1.sex]
		local p_value_onlyd`c'_male = 2 * ttail(e(df_r), abs(`p_value_onlyd`c'_male'))
		
		local p_value_mixed`c'_male = _b[2.kidsex_all#1.sex] / _se[2.kidsex_all#1.sex]
		local p_value_mixed`c'_male = 2 * ttail(e(df_r), abs(`p_value_mixed`c'_male'))
		
		local p_value_male`c' = _b[1.sex] / _se[1.sex]
		local p_value_male`c' = 2 * ttail(e(df_r), abs(`p_value_male`c''))

		* (vi) number of observations
		local num_obs`c' = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants`c' : word count `participants'

		* (viii) interpretation of results 
		scalar coef_1kidsex_all`c'_male = _b[1.kidsex_all#1.sex]
		scalar coef_2kidsex_all`c'_male = _b[2.kidsex_all#1.sex]
		scalar p_value_1kidsex_all`c'_male = 2 * ttail(e(df_r), abs(_b[1.kidsex_all#1.sex] / _se[1.kidsex_all#1.sex]))
		scalar p_value_2kidsex_all`c'_male = 2 * ttail(e(df_r), abs(_b[2.kidsex_all#2.sex] / _se[2.kidsex_all#2.sex]))
		
		if coef_1kidsex_all`c'_male > 0 & p_value_1kidsex_all`c'_male < .05 {
			local interpretation_onlyd`c'_male "in line with hypothesis"
		} 
		else {
			local interpretation_onlyd`c'_male "not in line with hypothesis"
		}
		
		if coef_2kidsex_all`c'_male > 0 & p_value_2kidsex_all`c'_male < .05 {
			local interpretation_mixed`c'_male "in line with hypothesis"
		} 
		else {
			local interpretation_mixed`c'_male "not in line with hypothesis"
		}

	// Display the saved statistics
	di "Coefficient: `coefficient_onlyd`c''"
	di "Standard Error: `se_onlyd`c''"
	di "Coefficient: `coefficient_mixed`c''"
	di "Standard Error: `se_mixed`c''"
	di "Coefficient: `coefficient_onlyd`c'_male'"
	di "Standard Error: `se_onlyd`c'_male'"
	di "Coefficient: `coefficient_mixed`c'_male'"
	di "Standard Error: `se_mixed`c'_male'"
	di "Coefficient: `coefficient_male`c''"
	di "Standard Error: `se_male`c''"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual`c''"
	di "Model Degrees of Freedom: `df_model`c''"
	di "p-value: " `p_value_onlyd`c''
	di "p-value: " `p_value_mixed`c''
	di "p-value: " `p_value_onlyd`c'_male'
	di "p-value: " `p_value_mixed`c'_male'
	di "p-value: " `p_value_male`c''
	di "p-value: " `p_value_male`c''
	di "Number of observations: " `num_obs`c''
	di	"Number of participants: " `num_participants`c''
	di "Interpretation of results: " "`interpretation_onlyd`c'_male'"
	di "Interpretation of results: " "`interpretation_mixed`c'_male'"
	
	local c = `c'+1	
	}

	
* EXPORT RESULTS * 

	* Export results to csv 
	local x1 "Child under 6 NOT suffer when mother works"
	local x2 "Child under 3 NOT suffer when mother works"
	local x3 "Best if man and woman work same amount to share responsibility"	
	
	file open myfile using "$OUT/h4_results.csv", write replace
    file write myfile " , coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	forvalues c=1/3 {
		file write myfile "`x`c''" _n
	    file write myfile "only daugthers, `coefficient_onlyd`c'', `se_onlyd`c'', `test_stat_family', `df_residual`c'', `df_model`c'',`p_value_onlyd`c'',`num_obs`c'',`num_participants`c'',`interpretation_onlyd`c''" _n
	    file write myfile "mixed, `coefficient_mixed`c'', `se_mixed`c'', `test_stat_family', `df_residual`c'', `df_model`c'',`p_value_mixed`c'',`num_obs`c'',`num_participants`c'',`interpretation_mixed`c''" _n
	    file write myfile "male, `coefficient_male`c'', `se_male`c'', `test_stat_family', `df_residual`c'', `df_model`c'',`p_value_male`c'',`num_obs`c'',`num_participants`c'',`interpretation_male`c''" _n
		file write myfile "only daugthers # male, `coefficient_onlyd`c'_male', `se_onlyd`c'_male', `test_stat_family', `df_residual`c'', `df_model`c'',`p_value_onlyd`c'_male',`num_obs`c'',`num_participants`c'',`interpretation_onlyd`c'_male'" _n
	    file write myfile "mixed # male, `coefficient_mixed`c'_male', `se_mixed`c'_male', `test_stat_family', `df_residual`c'', `df_model`c'',`p_value_mixed`c'_male',`num_obs`c'',`num_participants`c'',`interpretation_mixed`c'_male'" _n
		
	}

	file close myfile
*/

********************************************


** UPDATE: Hypothesis test with new generated dependent variable instead of three separated ones **
	
* HYPOTHESIS TEST FOR H4 * 

	/* We consider attitudes towards the following two statements as good measures of support for gender equality: 1) Children below the age of 6 suffer if their mother works (plh0298_v2); 2) Children below the age of 3 suffer if their mother works (plh0302_v2). We combine the two items into one variable by adding them together and reverse the direction the variable (ranging from 2 to 14): the higher the value, the stronger the support for gender equality. We thereby obtain our dependent variable (gndrequal)
	
	The original items (plh0298_v2 & plh0302_v2) are only availabe in 2018 (They were also surveyed in 2012 but on a much smaller, completely different group of participants.). Therefore, we run linear OLS regresion analysis for the dependent variable (gndrequal), applying sampling weights (phrf) and standard errors at the household level. Our independent variable kidsex_all has three categories denoting whether having biological daughters in the survey year: `No daughter', `Only daughter(s), and `Mixed'. 
	
	We also include the following control variables: gender, age group, education, employment status, net household income, marital status, number of biological children, whether youngest biological child <= 3 years, region (West or East Germany), and migration background.
	*/

	reg gndrequal i.kidsex_all##ib2.sex i.age_gr i.educ_3cat i.emplst_4cat i.hinc_5cat i.famstd_3cat i.sumkids_4cat i.youngest0to3_d i.sampreg i.migback [pw=phrf], vce(cluster hid)
		
		* Extract the required statistics
		
        * (i) coefficient estimates
		local coefficient_onlyd = _b[1.kidsex_all]
		local coefficient_mixed = _b[2.kidsex_all]
		
		local coefficient_onlyd_male = _b[1.kidsex_all#1.sex]
		local coefficient_mixed_male = _b[2.kidsex_all#1.sex]
		
		local coefficient_male = _b[1.sex]

		* (ii) standard errors
		local se_onlyd = _se[1.kidsex_all]
		local se_mixed = _se[2.kidsex_all]
		
		local se_onlyd_male = _se[1.kidsex_all#1.sex]
	    local se_mixed_male = _se[2.kidsex_all#1.sex]
		
		local se_male = _se[1.sex]
		
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value_onlyd = _b[1.kidsex_all] / _se[1.kidsex_all]
		local p_value_onlyd = 2 * ttail(e(df_r), abs(`p_value_onlyd'))
		
		local p_value_mixed = _b[2.kidsex_all] / _se[2.kidsex_all]
		local p_value_mixed = 2 * ttail(e(df_r), abs(`p_value_mixed'))		
		
		local p_value_onlyd_male = _b[1.kidsex_all#1.sex] / _se[1.kidsex_all#1.sex]
		local p_value_onlyd_male = 2 * ttail(e(df_r), abs(`p_value_onlyd_male'))
		
		local p_value_mixed_male = _b[2.kidsex_all#1.sex] / _se[2.kidsex_all#1.sex]
		local p_value_mixed_male = 2 * ttail(e(df_r), abs(`p_value_mixed_male'))
		
		local p_value_male = _b[1.sex] / _se[1.sex]
		local p_value_male = 2 * ttail(e(df_r), abs(`p_value_male'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of results 
		scalar coef_1kidsex_all_male = _b[1.kidsex_all#1.sex]
		scalar coef_2kidsex_all_male = _b[2.kidsex_all#1.sex]
		scalar p_value_1kidsex_all_male = 2 * ttail(e(df_r), abs(_b[1.kidsex_all#1.sex] / _se[1.kidsex_all#1.sex]))
		scalar p_value_2kidsex_all_male = 2 * ttail(e(df_r), abs(_b[2.kidsex_all#2.sex] / _se[2.kidsex_all#2.sex]))
		
		if coef_1kidsex_all_male > 0 & p_value_1kidsex_all_male < .05 {
			local interpretation_onlyd_male "in line with hypothesis"
		} 
		else {
			local interpretation_onlyd_male "not in line with hypothesis"
		}
		
		local interpretation_mixed_male "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient_onlyd'"
	di "Standard Error: `se_onlyd'"
	di "Coefficient: `coefficient_mixed'"
	di "Standard Error: `se_mixed'"
	di "Coefficient: `coefficient_onlyd_male'"
	di "Standard Error: `se_onlyd_male'"
	di "Coefficient: `coefficient_mixed_male'"
	di "Standard Error: `se_mixed_male'"
	di "Coefficient: `coefficient_male'"
	di "Standard Error: `se_male'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value_onlyd'
	di "p-value: " `p_value_mixed'
	di "p-value: " `p_value_onlyd_male'
	di "p-value: " `p_value_mixed_male'
	di "p-value: " `p_value_male'
	di "p-value: " `p_value_male'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of results: " "`interpretation_onlyd_male'"
	di "Interpretation of results: " "`interpretation_mixed_male'"
	

* EXPORT RESULTS * 

	* Export results to csv 
	
	file open myfile using "$OUT/h4_results.csv", write replace
    file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n

	* file write myfile "only daugthers, `coefficient_onlyd', `se_onlyd', `test_stat_family', `df_residual', `df_model',`p_value_onlyd',`num_obs',`num_participants',`interpretation_onlyd'" _n //commented out by Tina Wang
	*    file write myfile "mixed, `coefficient_mixed', `se_mixed', `test_stat_family', `df_residual', `df_model',`p_value_mixed',`num_obs',`num_participants',`interpretation_mixed'" _n //commented out by Tina Wang
	*    file write myfile "male, `coefficient_male', `se_male', `test_stat_family', `df_residual', `df_model',`p_value_male',`num_obs',`num_participants',`interpretation_male'" _n //commented out by Tina Wang
	*	file write myfile "only daugthers # male, `coefficient_onlyd_male', `se_onlyd_male', `test_stat_family', `df_residual', `df_model',`p_value_onlyd_male',`num_obs',`num_participants',`interpretation_onlyd_male'" _n //commented out by Tina Wang
	    file write myfile "`coefficient_mixed_male', `se_mixed_male', `test_stat_family', `df_residual', `df_model',`p_value_mixed_male',`num_obs',`num_participants',`interpretation_mixed_male'" _n

	file close myfile
********************************************************************************

exit
