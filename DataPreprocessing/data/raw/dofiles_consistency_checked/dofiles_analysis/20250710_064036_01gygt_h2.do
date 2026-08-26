********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)


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
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(sumkids kidgeb* kidsex* kidmon*)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgmonth pgcasmin pglfs pgemplst pgfamstd pgisco88 pgisco08 pgegp88 pgegp08)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plk0001_v1 plk0001_v2 plk0001_v3 /// partner ID
 plh0308_v2 ///  gender role attiudes
		pli0012_h pli0016_h pli0019_h pli0022_h pli0043_h pli0044_h /// household work
		plh0004	/// left-right self-placement
		)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgnuts1_ew hgtyp2hh)
	* hl contains all variables from the household questionnaire
	merge m:1 hid syear using "$IN/hl.dta", nogen keep(master matches) keepusing(hlk0044_v1 hlk0044_v2)	
	
	label language EN // change language of the labels to English


	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* only keep participants with any child at least in one year; keep respondents age 18 or above
	* remove respondents in same-sex partnerships, respondents where partner lives abroad, and respondents who are still in school
	gen age = syear - gebjahr

	keep if sumkids>0
	keep if age>=18
	keep if pgfamstd<6
	keep if pgcasmin>0


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Generate dummy variables for whether participant has at least one daughter (1) or not (0)
	
	// ever has a daughter
	egen daughter_ever = rowmax(kidsex01-kidsex19)
	mvdecode daughter_ever, mv(-1)
	replace daughter_ever = daughter_ever-1
	
	// has a daughter one year prior to survey year
	mvdecode kidgeb* pgmonth, mv(-3 -2 -1)
	gen has_daughter = 0
	
	forvalues i = 1/19 {
    local s = string(`i', "%02.0f")   // to match 01, 02, ..., 19
    replace has_daughter = 1 if kidsex`s' == 2 & kidgeb`s' < syear & !missing(kidsex`s') & !missing(kidgeb`s') & !missing(syear)
	}
	
	// indicator: has daughter vs. never has daughter (only son(s))
	
	gen daughter = .
	replace daughter = 0 if has_daughter==0 & daughter_ever==0
	replace daughter = 1 if has_daughter==1 & daughter_ever==1
		
	// number of children one year prior to survey year
	gen n_children = 0
	
	forvalues i = 1/19 {
    local s = string(`i', "%02.0f")
    replace n_children = n_children + 1 if kidgeb`s' < syear & !missing(kidgeb`s') & !missing(syear)
	}
	recode n_children (5/30=5), gen(n_children6)	// recode to adjust for potential outliers
	
	* Control variables: for matching
	
	mvdecode pgcasmin sex pglfs pgemplst pgfamstd pgegp88 pgegp08, mv(-10/-1)
	
	recode sex (1=0 "male")(2=1 "female")(else=.), gen(female)
	
	recode pgcasmin (1=1)(2=2)(3=3)(4 5=4)(6=5)(7=6)(8=7)(9=8)(else=.), gen(edu)
	
	gen emplstat = .
	replace emplstat = 1 if pgemplst==1 // full-time
	replace emplstat = 2 if pgemplst==2 | pgemplst==3 // part-time
	replace emplstat = 3 if pgemplst==5 & pglfs == 2 // nw: retired
	replace emplstat = 4 if pgemplst==5 & pglfs == 6 // nw: unemployed
	replace emplstat = 5 if pgemplst==5 & (pglfs==1 | pglfs == 3 | pglfs==5 | pglfs==8 | pglfs==9 | pglfs==10 | pglfs==13) // nw: other
	replace emplstat = 5 if pgemplst==3  // nw: other (vocational training)
	
	lab def emplstat 1 "full-time" 2 "part-time" 3 "NW: retired" 4 "NW: unemployed" 5 "NW: other" 
	lab val emplstat emplstat
	
	recode hgtyp2hh (11 12 13=1)(21 31 33=2)(32 34 35 41 51=3)(36 42 52 61=4)(43 53 62=5)(else=.), gen(hhsize) // approximation, as hhbrutto dataset is not available
	gen hhinc = hghinc/(sqrt(hhsize))
	
	recode hgnuts1_ew (21=0 "west")(22=1 "east")(else=.), gen(east)
	
	* Dependent variables
	
	mvdecode plh0004 plh0308_v2 pli0012_h-pli0044_h, mv(-8/-1)
	
	
	// Left-right self-placement
	gen leftright = plh0004
	


	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Please add a verbal description of your model here
	
	/*
	We use plh0004 as a measure for left-leaning political preferences (H2).
	Here: we want to test if political preferences are more left-wing due to having a daughter. 
	Analysis: We estimate the ATET of having a daughter based on a psmatch model (matched on several covariates). 
	A negative, significant effect (ATET daughter 1 vs 0) on leftright is in line with the hypothesis 
	
	*/
	
	teffects psmatch (leftright) (daughter sex age i.edu i.emplstat i.pgfamstd n_children6 hhinc east i.syear, probit), atet 


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		mat coef=r(table)
		local coefficient = el(coef,1,1)

		* (ii) standard error
		local se = el(coef,2,1)

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable: NOT APPLICABLE
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = el(coef,4,1)

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'
		
		* (viii) interpretation of directionality
		scalar coef_d_daughter = el(coef,1,1)
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
