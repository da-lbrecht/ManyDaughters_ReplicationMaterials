********************************************************************************
************************ Do-File H1: #ManyDaughters Project ********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 18 // version of stata
// version 14 // version of stata

set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
ssc install rwolf2

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

* LOAD AND MERGE DATA *

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all other datasets we use

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(bioinfo sumkids kidsex*)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing( ///
		plh0395i* /// Gender-support attitudes
		plh0004 /// Left-Right scale
		pli0019_h pli0022_h pli0044_h /// Time on Childcare
		pli0012_h pli0016_h pli0043_h /// Time on Housework
		pli0011 pli0054 pli0040 /// Time on Errands 
		pli0057 pli0055 pli0046 /// Time on Support persons
		)
			
		/* Details about the pli0* var: 
		pli0011 = 	Hours Sundays Running Errands
		pli0012_h = Housework hours, Saturday [harmonized]
		pli0016_h = Housework hours, Sunday [harmonized]
		pli0019_h = Childcare hours, Saturday [harmonized]
		pli0022_h = Childcare hours, Sunday [harmonized]
		pli0040 = 	Hours Weekday Running Errands
		pli0043_h = Housework hours, weekdays [harmonized]
		pli0044_h = Childcare hours, weekdays [harmonized] 
		pli0046 = 	Hours Weekdays Care For Persons
		pli0054 = 	Errands hrs. (week)
		pli0055 = 	Hours Saturday Support Persons In Care
		pli0057 = 	Hours Sunday Support Persons In Care

		*/ 		

	label language EN // change language of the labels to English
	
	recode sex (1 = 2) (2 = 1) // Recode sex variable to match H4 hypothesis
	label define sex_EN 1 "[1] Female" 2 "[2] Male", modify

	save "$DATA/dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * 

		
	* Exclusion Rule 1
	* Only 2019 survey
	keep if syear == 2019

	
	* Exclusion Rule 2
	* Only respondents for which the source of the biographic information is sufficiently good
	keep if inlist(bioinfo, 1, 2, 3, 4)
 
 
	* Exclusion Rule 3 
	* Only keep respondents with at least 1 kid
	keep if sumkids > 0	

	
	* Exclusion rules to be implemented at the analysis stage for H1
	foreach var of varlist plh0395i*{
		gen tag_`var' = cond(inlist(`var', 1, 2, 3, 4, 5, 6, 7), 0, 1) 
	}
	egen h1_tag = rowtotal(tag_*)
	replace h1_tag = 1 if h1_tag > 0 // Tag non-respondents to at least one gender-support attitudes questions
	drop tag*	

	
	* Exclusion rules to be implemented at the analysis stage for H2
	gen h2_tag = cond(inlist(plh0004, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 0, 1) // Tag non-respondents to the political preferences question

	
	* Exclusion rules to be implemented at the analysis stage for H3
	tempvar dup
	duplicates tag hid, generate(`dup') // Tag duplicated hids 
	gen h3_tag = cond(`dup' == 1 & inlist(partner, 1, 2, 3) & inlist(sex, 1, 2), 0, 1) // Tag HIDs with number of people != 2 and/or HIDs with uncertainty wrt the partnership status + non-respondents to the sex question
	tempvar sex_partner1 sex_partner2
	bysort hid: egen `sex_partner1' = max(sex)
	bysort hid: egen `sex_partner2' = min(sex)		
	forvalues i = 1/2{
		replace `sex_partner`i'' = . if h3_tag == 1 // Remove already tagged obs from sex_partner vars
	}
	replace h3_tag = 1 if `sex_partner1' == `sex_partner2' // Tag same-sex couples
	foreach var of varlist pli0*{
		gen tag_`var' = cond(inlist(`var', -2, -3, -5, .), 1, 0) 
	}
	tempvar rowtag
	egen `rowtag' = rowtotal(tag_*)
	replace h3_tag = 1 if `rowtag' > 0 // Tag respondents with at least one -2, -3 or -5 ('Does not apply', 'Implausible', 'Not included in this version of questionnaire') or missing in one of the pli0*
	drop tag*
	tempvar tag_partner1 tag_partner2	
	bysort hid: egen `tag_partner1' = max(h3_tag)
	bysort hid: egen `tag_partner2' = min(h3_tag)		
	forvalues i = 1/2{
		replace `tag_partner`i'' = . if h3_tag == 1 // Remove already tagged obs from tag_partner vars
	}
	replace h3_tag = 1 if `tag_partner1' == 1 | `tag_partner2' == 1 // Tag respondents whose partner is tagged

	
	* Exclusion rules to be implemented at the analysis stage for H4 
	foreach var of varlist plh0395i*{
		gen tag_`var' = cond(inlist(`var', 1, 2, 3, 4, 5, 6, 7), 0, 1) 
	}
	egen h4_tag = rowtotal(tag_*)
	replace h4_tag = 1 if h4_tag > 0 // Tag non-respondents to at least one gender-support attitudes questions
	drop tag*	
	replace h4_tag = 1 if inlist(sex, 1, 2) == 0 // Tag non-respondents to the sex question	
	
	
* GENERATE VARIABLES *

	* Dependent variable for H1: composite index for gender support based on attitudes
	tempvar mean_var
	egen `mean_var' = rowmean(plh0395i*)
	gen dep_var_h1 = (`mean_var' - 1)/(7 - 1) // Normalized score between 0 and 1

	
	* Dependent variable for H2: Left_Right political preferences
	tempvar recode_plh0004
	gen `recode_plh0004' = 10 - plh0004 // Original variable is recoded so as to match the hypothesis (higher value = more leftist)
	gen dep_var_h2 = (`recode_plh0004' - 0)/(10 - 0) // Normalized score between 0 and 1	
	
	
	* Dependent variable for H3: Share of women-specific stereotypical activities done by men within the household
	tempvar h_gender_typical_activities h_gender_typical_activities_hh share_gender_typical_activities
	egen `h_gender_typical_activities' = rowtotal(pli0*) // Total hours declared on women-specific stereotypical activities at the individual level		
	bysort hid: egen `h_gender_typical_activities_hh' = sum(`h_gender_typical_activities') // Total hours declared on women-specific stereotypical activities at the household level	
	gen `share_gender_typical_activities' = (`h_gender_typical_activities' / `h_gender_typical_activities_hh') // Share of women-specific stereotypical activities in the household
	replace `share_gender_typical_activities' = 0.5 if `share_gender_typical_activities' == . // If no women-specific stereotypical activities declared by the household, consider equal
	gen dep_var_h3 = `share_gender_typical_activities' if sex == 2 // Share of women-specific stereotypical activities done by men in the household


	* Dependent variable for H4: Same as for H1
	tempvar mean_var
	egen `mean_var' = rowmean(plh0395i*)
	gen dep_var_h4 = (`mean_var' - 1)/(7 - 1) // Normalized score between 0 and 1	
	
		
	* Independent variable of interest: having daugthers
	foreach var of varlist kidsex*{
		recode `var' (-1 -2 = .) (2 = 1) (1 = 0)
	}
	tempvar sum_girls
	egen `sum_girls' = rowtotal(kidsex*)
	gen d_daughter = cond(`sum_girls' == 0, 0, 1)
	local missing_ks = 0
	ds kidsex*
	foreach var in `r(varlist)'{
		local ++missing_ks
		replace d_daughter = . if `var' == . & sumkids >= `missing_ks' // Remove those for which information about the kids' sex is (partly) missing
	}
	gen int_sex_daughter = cond(sex == 2 & d_daughter == 1, 1, 0) // Variable of interest for H4 (interaction between sex and having daughters)


* HYPOTHESIS TEST FOR H1

	/*  Description of the model for H1
	
	Our dependent variable for testing H1 (effect of having daughters on support for gender equality as measured by attitudes) is a composite index from survey items on attitudes towards gender (plh0395i01 to plh0395i06). The index is normalized between 0 and 1.	
	
	We hypothesize that, conditional on having kids, having a daughter is a random event. Thus, we do not add control variables in the regression. 
	
	We consider a tobit model with both upper (at 1) and lower (at 0) censhorships with our index as dependent variable and a binary variable for having at least one daughter as independent variable. 
	
	Further, we run the model only on observations that are not tagged for H1.	
	
	In order to correct for multiple hypothesis testing (FWER), we also run the 3 other regressions (see the other .do files for a description of the models).
	
	*/
	
	rwolf2 /// Multiple-Hypothesis Testing correction
	(tobit dep_var_h1 d_daughter if h1_tag == 0, ll(0) ul(1)) /// Model for H1
	(tobit dep_var_h2 d_daughter if h2_tag == 0, ll(0) ul(1)) /// Model for H2
	(tobit dep_var_h3 d_daughter if h3_tag == 0, ll(0) ul(1)) /// Model for H3
	(tobit dep_var_h4 i.sex i.d_daughter int_sex_daughter if h4_tag == 0, ll(0) ul(1)) /// Model for H4
	, indepvars(d_daughter, d_daughter, d_daughter, int_sex_daughter) onesided(positive) reps(1000)
	
	local rw_pval = e(rw_dep_var_h1_d_daughter) // Save MHT-corrected p-value

	
	tobit dep_var_h1 d_daughter if h1_tag == 0, ll(0) ul(1)
	


* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]
		
		* (ii) standard error
		local se = _se[d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = `rw_pval'

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
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
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
