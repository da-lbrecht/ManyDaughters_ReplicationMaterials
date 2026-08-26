********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

*clear all
version 14.2 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
ssc install unique
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

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(biovalid sumkids biokids kidsex* kidgeb*)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgmonth pgbilzeit pgemplst pgfamstd)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(ple0040 plj0621-plj0625)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgnuts1)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS *
	* Keep only relevant years when dependent variables were measured
	keep if inlist(syear, 2016, 2017, 2018)

	* I noticed that participants in my sample seem to have been asked questions
	* about the position of women in society in one year only (2016, 2017 or 
	* 2018. Since I didn't have access to the full dataset and I could not 
	* find information to confirm this, I checked how many participants with
	* valid answers in one year (1-7) had not been asked the question in other
	* years when the question was being asked ([-5] Not included in this 
	* version of the questionnaire)
	* Generate indicator for valid response (1-7)
	gen valid_response = inrange(plj0621, 1, 7)
	* Generate indicator for "not included" (-5)
	gen not_included = (plj0621 == -5)
	* Summarise by pid
	bysort pid: egen n_valid = total(valid_response)
	bysort pid: egen n_not_included = total(not_included)
	bysort pid: egen n_obs = count(plj0621)
	* Identify pids with exactly 1 valid response and other obs as -5
	gen one_valid_others_not_included = (n_valid == 1 & n_valid + n_not_included == n_obs)
	tab plj0621 syear if plj0621>0
	unique pid if one_valid_others_not_included==1
	* The total number of observations equals that of unique pids who were not
	* asked more than once. Thus I assume the entire dataset for this question
	* is a repeated cross section.
 
	* Only keep participants from private households
	keep if inlist(pop,1,2)
	* Only keep participants with individual level data
	keep if netto>9 & netto<20
	* Only participants who have at least one child for whom there is information
	keep if biovalid == 31
	* Only participants aged 18+
	keep if (syear*12+pgmonth-gebjahr*12-gebmonat)>215 & (syear*12+pgmonth-gebjahr*12-gebmonat)<.
	* Only participants with valid household income
	keep if hghinc>=0 & hghinc<.
	* Only participants with valid number of years of education
	keep if pgbilzeit>=0 & pgbilzeit<.
	* Only participants with valid employment information
	keep if pgemplst>=0 & pgemplst<.
	* I had to remove the pre-registered variable below, because it wasn't asked in the version of the
	* survey where attitudes about women in society (dependent variable) were asked.
// 	* Only participants with valid disability information
// 	keep if ple0040>=0 & ple0040<.
// 	rename ple0040 disabled
// 	recode disabled 2=0
	* Only participants with valid marital status information
	keep if pgfamstd>=0 & pgfamstd<.
	rename pgfamstd marital_stat
	label define pgfamstd_EN 5 "Widowed" 2 "Separated" 3 "Single" 4 "Divorced", modify
	* Only participants with valid sample region
	keep if sampreg>0 & sampreg<.
	rename sampreg east_germany
	recode east_germany 1=0 2=1
	* Only participants with valid migration background information
	keep if migback~=.
	recode migback 1=0 2=1 3=2
	label define migback_EN 0 "None" 1 "Direct" 2 "Indirect", modify
	* Only participants with valid regional dummies
	keep if hgnuts1~=.
	* Only participants with valid answers on questions regarding the position
	* of women
	keep if plj0621>0 & plj0621<8
	keep if plj0622>0 & plj0622<8
	keep if plj0623>0 & plj0623<8
	keep if plj0624>0 & plj0624<8
	keep if plj0625>0 & plj0625<8
	

* GENERATE VARIABLES *

	* Generate dummy for kidNN being present in the HH in the survey year
	forvalues i = 1/19 {
		local j : display %02.0f `i'
		gen d_kid`j' = .
	}
	forvalues i = 1/19 {
		local j : display %02.0f `i'
		replace d_kid`j' = cond(syear-kidgeb`j'>=0 & kidgeb`j'>0,1, ///
		cond(kidgeb`j'>0,0,.))
	}
	* Generate the number of kids respondent has in survey year
	egen total_kids = rowtotal(d_kid01-d_kid19), missing
	
	* Generate dummy for daughterNN being present in the HH in the survey year
	forvalues i = 1/19 {
		local j : display %02.0f `i'
		gen d_daughter`j' = .
	}
	forvalues i = 1/19 {
		local j : display %02.0f `i'
		replace d_daughter`j' = cond(syear-kidgeb`j'>=0 & kidgeb`j'>0 & ///
		kidsex`j'==2,1,cond(kidgeb`j'>0,0,.))
	}
	* Generate a dummy for having at least one daugther in survey year
	egen d_daughter = anymatch(d_daughter01-d_daughter19), values(1)
	
	* Generate the number of daughters respondent has in survey year
	egen total_daughters = rowtotal(d_daughter01-d_daughter19), missing
	
	* Generate the age variables as they appear in regressions: squared and 
	* divided by 100
	gen age = (syear*12+pgmonth-gebjahr*12-gebmonat)/12
	gen age2_100 = (((syear*12+pgmonth-gebjahr*12-gebmonat)/12)^2)/100
	
	* Generate the log of household income
	gen loghinc=log(hghinc)
	
	* Generate dummy for being in full-time employment
	gen ft_employed = cond(pgemplst>1 & pgemplst~=.,0,cond(pgemplst==1,1,.))
		
	* Generate the dependent variable for H1
	* First, recode variables for which lower numbers indicate more support
	* for gender equality
	recode plj0623 plj0624 plj0625 (7=1) (6=2) (5=3) (3=5) (2=6) (1=7)
	pca plj0621 plj0622 plj0623 plj0624 plj0625, comp(1)
	predict pc_women


* HYPOTHESIS TEST FOR H1 *

	* I run a linear OLS regression analysis with the PCA of the 5 variables for
	* support for the more equal position of women in society (plj0621-plj0625) as
	* dependent variable and the total number of daughters as independent variable.
	* The controls are the ones listed in the PAP, with adjustments as 
	* documented below.
	* _b[total_daughters] is the test for H1.
	reg pc_women total_daughters i.total_kids age2_100 loghinc ///
	pgbilzeit ft_employed i.marital_stat east_germany i.migback ib3.hgnuts1 ///
	i.syear
	
	* Changes from PAP:
	* - in PAP I thought I would have a (rather) balanced panel for the 3 years, not a
	* repeated cross section. Hence I pre-registered an individual fixed effects
	* OLS regression. I had to drop individual fixed effects and also the clustering
	* of standard errors at the individual level.
	* - I calculated age differently, using syear instead of pgpiyear, since 
	* pgpiyear was only recorded in some years.
	* - For years of education, I used pgbilzeit instead of pgbilzt, as the 
	* second variable wasn't in SOEP Core, but in SOEP IS (misunderstanding).
	* - For living in East/West Germany, I used sampreg instead of bgsampreg, 
	* as this was the correct variable in SOEP Core (misunderstanding).
	* - I had to drop the control variable for being disabled, ple0040, as none
	* of the individuals with valid answers on the variables used to construct
	* the dependent variable had been asked this question (they all had code -5).
	* - I restricted the estimation sample to those in private households, with
	* pop 1 or 2.
	* - Unspecified in PAP: the baseline NUTS region is Berlin.

	
* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[total_daughters]

		* (ii) standard error
		local se = _se[total_daughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[total_daughters] / _se[total_daughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local interpretation "positive coefficient is in line with hypothesis"


	* Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di "Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
