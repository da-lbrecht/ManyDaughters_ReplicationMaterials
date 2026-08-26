********************************************************************************
****************** Do-File H3: #ManyDaughters Project - RT 0139pp **************
********************************************************************************

* Authors: [REDACTED]

/* Summary

We followed the pre-analysis plan (PAP) as initially registered. 

We list here the deviations from the PAP:
1. we restricted the sample to respondents living in private households (keep if inlist(pop, 1, 2)), based on the pop variable in ppathl. At the time of writing the PAP, this classification was not identified due to lack of data access. 
2. We restricted the sample to couples with realistic conjoint domestic work and remove those for which the sum of hours spent on housework and childcare over the two parents is larger than 48 hours a day.

In addition, following Footnote 1 of the PAP, we included a dummy for East Germany using the sampreg variable from ppathl. Although we were initially unsure whether sampreg would be available for all individuals—since this was not clearly documented—we confirmed its availability and proceeded accordingly.	
	

*/
* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install reghdfe, replace

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

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)  keepusing(kidsex01 kidgeb01-kidgeb19 kidmon01-kidmon19 kidpnr01-kidpnr19)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgcasmin pgfamstd pgemplst pgpartnr pgmonth)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing( plh0308_v2 pli0043_h pli0044_h ple0040)
	* hl contains all variables from the household questionnaire
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) keepusing(hlc0043)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English
	

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h3_dataset.dta", clear
* FILTERS * 
 
	* only keep participants from private households
	keep if inlist(pop,1,2)
	
	* remove disabled 
	drop if ple0040==1
	
	***** We include respondent who are biological parents at the time of the interview (biolparenti,2018 = 1, as described in Table 1) Biological parent. Dummy variable equal to 1 if the respondent has at 	least one child at time t: kidgeb01 ≤ t.
	gen biolparent = 0
	replace biolparent = 1 if kidgeb01 < syear & kidgeb01 > 0
	replace biolparent = 1 if kidgeb01 == syear & kidmon01 <= pgmonth & kidmon01 > 0
	keep if biolparent == 1
	
	* keep cohabiting couples
	keep if pgpartnr>0 & !missing(pgpartnr)
	
	* keep couples who are the only couple in the household: as we focus on couples with at least one child and how the distribution of domestic time, we restrict our sample to household with strict couples
	bysort syear hid (pid): gen n = _N
	keep if n==2
	drop n
	
	* exclude same sex couples (sex1 + sex2 = 3)
	bys syear hid: egen temp = sum(sex)
	keep if temp==3
	drop temp
	
	* generate id within the couple (1=male/father; 2=female/mother)
	bysort syear hid (sex): gen couple_id = _n

	
	* keep couples who share the same first kid
	forval i = 1/19 {
		local var = string(`i', "%02.0f") 
		bysort syear hid (pid): gen same_kid`var' = (kidpnr`var' == kidpnr`var'[_n-1]) & kidpnr`var'>0 & kidpnr`var'[_n-1]>0
		bysort syear hid: egen temp = max(same_kid`var')
		replace same_kid`var' = temp
		drop temp
	}
	 
keep if same_kid01==1
	
	* Remove couples who had twins or more as a first child_10to14
	g twins = 0
	replace twins = 1 if kidmon01 == kidmon02 & kidgeb01 == kidgeb02 & same_kid01==1 & same_kid02==1
	
	drop if twins==1
	
	* count number of (shared) children by age group
gen child_1to4 = 0
gen child_5to9 = 0
gen child_10to14 = 0

forval i = 1/19 {
    local var = string(`i', "%02.0f") 
    
	g age_`var' = syear - kidgeb`var'
    replace child_1to4 = child_1to4 + 1 if age_`var'  <= 4 & same_kid`var' == 1
    replace child_5to9 = child_5to9 + 1 if inrange(age_`var' , 5, 9) & same_kid`var' == 1
    replace child_10to14 = child_10to14 + 1 if inrange(age_`var' , 10, 14) & same_kid`var' == 1
	drop age_`var'
}
	*bysort syear (hid): gen same_kid01 = (kidpnr01 == kidpnr01[_n-1])
	*bysort hid: egen temp = max(same_kid01)
	*replace same_kid01 = temp
	*drop temp
	
	keep if same_kid01==1
	* drop if missing controls
	gen sample=1
	local x   pgcasmin pgfamstd pgemplst germborn pli0043_h pli0044_h hghinc germborn
	foreach var in `x'{
	replace sample=0 if `var'<0
	}
	keep if sample==1
	
	* drop if marital status is 7 or 8
	drop if inlist(pgfamstd, 7, 8)
	
* GENERATE VARIABLES (individual level) * 

	* age at the time of the interview
	g age = syear - gebjahr
	g age_sqrt = age*age
	
	* dummies for education
	g edu_high = inlist(pgcasmin, 6, 7)
	g edu_tertiary = inlist(pgcasmin, 8, 9)
	
	* dummies for employment
	g employed = .
	replace employed = 1 if inlist(pgemplst, 1, 2)
	replace employed = 0 if inlist(pgemplst, 3, 4, 5, 6, 7)
	
	* Generate dummy: 1 if born in Germany or immigrated before 1950
	gen germanborn = (germborn == 1)

	
	* Genenrate dummy east/west (see footnote 1 in the PAP)
	* Drop missing or invalid sampreg values
	drop if missing(sampreg) | sampreg < 0

	* Generate East Germany dummy
	gen east = (sampreg == 2)
	label var east "Respondent in East Germany (sampreg == 2)"
	
	* domestic work
	g dw = pli0043_h + pli0044_h
	* drop if sum of domestic work is > 48 hours
	bys syear hid: egen dw_couple = sum(dw)
	drop if dw_couple > 30  /* ADD WHY WE REMOVE TOP 1% */
	
	* Generate dummy variable for whether participant has a daugther as a first child (1) or a son (0)
	gen d_daughter = (kidsex01 == 2 & kidgeb01 <= syear & kidgeb01 > 0)


* RESHAPE DATA so that each observation is a partner dyad
	keep syear hid pid d_daughter dw dw_couple employed  pgfamstd hghinc couple_id age age_sqrt pgcasmin edu_high edu_tertiary hlc0043 germborn east child_1to4 child_5to9 child_10to14
	reshape wide pid dw d_daughter employed pgcasmin pgfamstd  age age_sqrt edu_high edu_tertiary hlc0043 germborn east, i(hid syear hghinc child_1to4 child_5to9 child_10to14) j(couple_id)

	drop if missing(pid1) | missing(pid2)

	
* GENERATE VARIABLES (household level) * 

	* Generate dummy variable for whether a couple has a daugther as a first child (1) or a son (0)
	g d_daughter = d_daughter1 ==1 
	replace  d_daughter = . if d_daughter1 != d_daughter2 /* This is just to check for consistency and make sure both mother and father share the same treatment variable */
	
	* Generate dummy variable for the marital status of the couple	
	g married = pgfamstd1==1 
	replace married =0 if pgfamstd1!=pgfamstd2  /* We assign the value 0 (unmarried) when mother and father are not stating the same marital status. */
	
	* Generate dummy variable for whether the couple is with the same level of education
	g sameeduc = pgcasmin1 == pgcasmin2
	replace sameeduc = . if missing(pgcasmin1) | missing(pgcasmin2)
	
	* Generate dummy for highest education
	g edu_high_couple = max(edu_high1, edu_high2)
	g edu_tertiary_couple = max(edu_tertiary1, edu_tertiary2)
	
	* Generate dummy for both born in Germany
	g both_germborn = germborn1==1 & germborn2 ==1
	g none_germborn = germborn1==0 & germborn2 ==0
	
	* Generate dummy for both living in East
	g both_east = east1==1 & east2 ==1
	g none_east = east1==0 & east2 ==0
	
	* Generate dummy for both employed, male breadwinner, female breadwinner
	g both_employed = employed1==1 & employed2==1 
	g male_bw = employed1==1 & employed2==0 
	g female_bw = employed1==0 & employed2==1 
	
	* Log household income
	g log_hinc = log(hghinc+1)
	
	* Generate number of children in the households
	g num_children = hlc00431 if hlc00431 == hlc00432
	replace num_children = . if hlc00431<0
	
	* Generate the gender equality index DW_equal = min(DW_wife, DW_husband)/max(DW_wide, DW_husband)
	g min_DW = min(dw1, dw2)
	g max_DW = max(dw1, dw2)
	g DW_equal = min_DW/max_DW

rename age1 age_m
rename age2 age_f
rename age_sqrt1 age_sqrt_m
rename age_sqrt2 age_sqrt_f

drop if age_m>65 | age_f>65
drop if age_m<18 | age_f<18


*************************************************************************************
* HYPOTHESIS TEST FOR H3 * 

/*
We estimate a pooled Ordinary Least Squares (OLS) regression model to examine whether having a daughter as the first child is associated with greater gender equality within the household. The dependent variable is a ratio measuring the distribution of unpaid domestic work (housework and childcare) within cohabiting couples. This index is defined as the ratio of the smaller to the larger number of hours reported by each partner on these tasks and ranges from 0 (complete inequality) to 1 (perfect equality). Higher values reflect a more balanced division of responsibilities.

The analysis is restricted to cohabiting couples observed between 2000 and 2020 who are jointly listed as the biological parents of the same first child, live in private households, are between 18 and 65 years old, and do not report same-sex partnerships or registered disabilities. We exclude couples whose first birth resulted in twins or higher-order multiples. These restrictions ensure that the transition to parenthood was experienced jointly and that observed differences in household labor division are not confounded by disability status or unrelated prior family structures. We further exclude cases with missing or invalid values for hours of domestic work.

The key independent variable is d_daughter, a binary indicator equal to 1 if the couple’s first biological child is a daughter born on or before the interview date, and 0 if the first child is a son. The model includes a rich set of controls: the mother’s and father’s age and age squared; indicators for whether both partners have the same level of education and the highest level of education within the couple; a dummy for being married; a dummy for both partners being employed; a dummy for whether the mother is the primary earner; the logarithm of net household income; dummies for whether both partners were born in Germany or not; and indicators for the number of children in the household within the age brackets 0–4, 5–9, and 10–14.

The model controls for year fixed effects, and standard errors are clustered at the household level. A positive and statistically significant coefficient on d_daughter (β₁ > 0, p < 0.05) will be interpreted as evidence supporting the hypothesis that having a daughter is associated with greater gender equality in the division of unpaid domestic work. We report the coefficient estimate, its t-statistic, the corresponding p-value, and the 95% confidence interval.
*/
	
	
	
global controls "age_m age_sqrt_m age_f age_sqrt_f i.married i.sameeduc edu_high_couple edu_tertiary_couple log_hinc both_germborn none_germborn both_east none_east both_employed male_bw female_bw child_1to4 child_5to9 child_10to14"

reghdfe DW_equal d_daughter $controls, a(i.syear) vce(cluster hid)


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
		local interpretation "positive coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di	"Number of couples: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit



