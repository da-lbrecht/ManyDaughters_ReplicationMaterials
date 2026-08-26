********************************************************************************
****************** Do-File H4: #ManyDaughters Project - RT 0139pp **************
********************************************************************************

* Authors: [REDACTED]

/* Summary

We followed the pre-analysis plan (PAP) as initially registered. 

The only deviation concerns the sample selection: we restricted the sample to respondents living in private households (keep if inlist(pop, 1, 2)), based on the pop variable in ppathl. At the time of writing the PAP, this classification was not identified due to lack of data access. 

In addition, following Footnote 1 of the PAP, we included a dummy for East Germany using the sampreg variable from ppathl. Although we were initially unsure whether sampreg would be available for all individuals—since this was not clearly documented—we confirmed its availability and proceeded accordingly.	
	
*/

* ============================
* Setup * 
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:
*ssc install outreg2





* CONFIGURE FILE PATHS * 
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

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex01-kidsex19 kidgeb01-kidgeb19 kidmon01-kidmon19)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgcasmin pgfamstd pgemplst pgmonth)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing( plh0308_v2)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h4_dataset.dta", replace // save merged dataset


	
	
* ============================
* Analysis
* ============================

use "$DATA/h4_dataset.dta", clear

* FILTERS * (
 
	*only keep participants from private households
	keep if inlist(pop,1,2)
	
	****** For Hypothesis 4, we restrict the sample to respondents observed in the 2018 wave, which includes a survey question about respondents’ attitudes toward gender 	equality.
	* Keep only 2018 wave
	keep if syear == 2018
* Keep only valid responses to plh0308_v2 (values 1 to 7)
keep if inrange(plh0308_v2, 1, 7)

	***** We include respondent who are biological parents at the time of the interview (biolparenti,2018 = 1, as described in Table 1) Biological parent. Dummy variable equal to 1 if the respondent has at least one child at time t: kidgeb01 ≤ t.
	gen biolparent = 0
	replace biolparent = 1 if kidgeb01 < syear & kidgeb01 > 0
	replace biolparent = 1 if kidgeb01 == syear & kidmon01 <= pgmonth & kidmon01 > 0
	keep if biolparent == 1



* GENERATE VARIABLES *

	* generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* hasdaughterit Dummy variable equal to 1 if the respondent’s first biological child is a daughter at time t, based on kidsex01 = 2 and kidgeb01 ≤ t
	* we keep the variable name as d_daughter
	gen d_daughter = (kidsex01 == 2 & kidgeb01 <= syear & kidgeb01 > 0)

	
	
	*set of three dummy variables indicating whether the respondenthas two, three, or four or more children.
	gen numchildren = 0
		forvalues i = 1/19 {
			local var = string(`i', "%02.0f")
			quietly replace numchildren = numchildren + ///
			(!missing(kidgeb`var') & kidgeb`var' > 0 & kidgeb`var' <= syear)
}

	gen child2      = (numchildren == 2)
	gen child3      = (numchildren == 3)
	gen child4plus  = (numchildren >= 4)

	
	*CONTROLS

	* three dummies for the age of the first children
	
	gen firstchild_age = syear - kidgeb01 if kidgeb01 > 0 & kidgeb01 <= syear
	gen first0_4    = inrange(firstchild_age, 0, 4)
	gen first5_9     = inrange(firstchild_age, 5, 9)
	gen first10_14   = inrange(firstchild_age, 10, 14)

	
	* Generate respondent age
	gen age = syear - gebjahr if gebjahr > 0 & gebjahr <= syear

	* Generate squared age
	gen age_sqrt = age^2 if !missing(age)

	* Generate male dummy (1 = male, 0 = female), exclude invalid/missing values
	gen male = .
	replace male = 1 if sex == 1
	replace male = 0 if sex == 2

	* Create two education dummies based on pgcasmin
	* Drop respondents with no answer to education
	*** SHOULD WE DROP??????
	drop if pgcasmin < 0 | missing(pgcasmin)

	* Generate education dummies
	gen edu_high     = inrange(pgcasmin, 6, 7)

	gen edu_tertiary = inrange(pgcasmin, 8, 9)
	* Reference category: CASMIN 0–5 (lower secondary or less)

	
	***five marital status dummies
	* Drop invalid or missing values only
	drop if pgfamstd < 0 | missing(pgfamstd)

	* Generate marital status dummies
	gen married         = inlist(pgfamstd, 1, 7)
	gen married_sep     = inlist(pgfamstd, 2, 8)
	gen divorced        = (pgfamstd == 4)
	gen widowed         = (pgfamstd == 5)
	gen partner_abroad  = (pgfamstd == 6)
	* Reference category: Single (pgfamstd == 3)

	
	
	*employment status
	
	* Drop invalid or missing responses
	drop if pgemplst < 0 | missing(pgemplst)

	* Generate employment status dummy
	gen employed = inlist(pgemplst, 1, 2)


	* the logarithm of household income
	* Drop invalid or missing household income
	drop if hghinc < 0 | missing(hghinc)

	* Generate log of household net income (add 1 to avoid log(0))
	gen ln_hhinc = ln(hghinc + 1)

	
	*dummy for being born in Germany.
	* Drop invalid or missing values
	drop if germborn < 0 | missing(germborn)

	* Generate dummy: 1 if born in Germany or immigrated before 1950
	gen germanborn = (germborn == 1)

	
	* Genenrate dummy east/west (see footnote 1 in the PAP)
	* Drop missing or invalid sampreg values
	drop if missing(sampreg) | sampreg < 0

	* Generate East Germany dummy
	gen east = (sampreg == 2)


	
****GENERATE THE INTERACTION TERM
gen male_daugther= male * d_daughter

	
	

*************************************************************************************
* HYPOTHESIS TEST FOR H4 * 

/*
We estimate a linear Ordinary Least Squares (OLS) regression model to examine whether having a daughter as the first child influences support for gender equality. The dependent variable is based on the 2018 SOEP item plh0308_v2, which asks respondents whether they agree with the statement: “It’s best if the man and the woman work the same amount so they can share the responsibility for taking care of the family and household equally.” Responses range from 1 (“Do not agree at all”) to 7 (“Completely agree”), with higher values indicating more egalitarian views.

The analysis is restricted to respondents interviewed in 2018 who were living in private households, had at least one biological child already born at the time of the interview, and provided a valid response to the gender attitude question. The key independent variable is d_daughter, a binary indicator equal to 1 if the respondent’s first biological child is a daughter born on or before the interview date, and 0 if the first child is a son.

The key independent variable is d_daughter, a binary indicator equal to 1 if the respondent’s first biological child is a daughter born on or before the interview date, and 0 if the first child is a son. The model includes a set of control variables accounting for family composition, demographics, and socioeconomic characteristics. These include indicators for the number of children (two, three, or four or more, with one child as the reference category), the age group of the first child (0–4, 5–9, 10–14), respondent’s age and age squared, sex (male), education level (upper secondary and tertiary, with lower education as the reference), marital status (married, separated, divorced, widowed, partner abroad; single as the reference), employment status (employed), the logarithm of net monthly household income, a dummy for being born in Germany or having immigrated before 1950, and a regional indicator for East Germany.

The model is estimated using heteroskedasticity-robust standard errors. The coefficient of interest is the one associated with the interaction term between d_daughter and male. A positive and statistically significant coefficient (_b[d_daughter#male] > 0, p < 0.05) will be interpreted as evidence supporting Hypothesis 4, suggesting that the effect of having a daughter on support for gender equality is stronger among fathers than among mothers.
*/
	
* Estimate OLS with robust standard errors
reg plh0308_v2 ///
    d_daughter male_daugther ///
    child2 child3 child4plus ///
    first0_4 first5_9 first10_14 ///
    age age_sqrt male ///
    edu_high edu_tertiary ///
    married married_sep divorced widowed partner_abroad ///
    employed ln_hhinc germanborn east ///
    if inrange(plh0308_v2, 1, 7), robust

	

* EXTRACT AND EXPORT RESULTS * 
	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[male_daugther]

		* (ii) standard error
		local se = _se[male_daugther]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[male_daugther] / _se[male_daugther]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_male_daugther = _b[male_daugther]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit

