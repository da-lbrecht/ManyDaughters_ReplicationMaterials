********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED] and [REDACTED], using a template by Tina Wang and David Albrecht

* ============================
* Setup
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES
* install all packages you need here, for example:
* ssc install outreg2

* CONFIGURE FILE PATHS
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
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset

	* Set up the panel structure
	xtset pid syear
	
* ============================
* Analysis
* ============================

* FILTERS
	* Inclusion criteria as described in the PEP:
	* Exclude respondents without children: the respondent has, in at least one year of the sample period, at least one biological child (sumkids> 0)
	keep if sumkids>0
	
	* Exclude nonbinary respondents: the respondent's gender is clearly identified as either male or female
	keep if inlist(sex,1,2)
	
	* Exclude respondents whose gender changed: [gender] is same during the entire sample period (to facilitate the test of gender-specific hypotheses, using \texttt{biobirth/sex}), and
	bysort pid (syear): gen change = sex != sex[1]
	bysort pid: egen dropme = max(change)
	drop if dropme == 1
	
	
    * Exclude respondents with non-binary children
	forval i = 1/19 {
		local var = "kidsex" + string(`i', "%02.0f")
		keep if inlist(`var', 1, 2, -2)	// male, female, does not apply
	}
	

* GENERATE VARIABLES
	* Create "has_child": is 0 before the birth of the first child, starts to be 1 in the year of the birth of the first child
	gen has_child=0
	replace has_child=1 if syear>=kidgeb01

	* Create "has_daughter": is 0 before the birth of the first daughter, starts to be 1 in the year of the birth of the first daughter
	* Up to 19 children included
	gen has_daughter = 0
	forvalues i = 1/19 {
		local j = string(`i', "%02.0f")
		replace has_daughter = 1 if syear >= kidgeb`j' & kidsex`j' == 2
	}
		
	* Create age of the respondent (in the year of the survey)
	gen age=syear-gebjahr
	
	* Create age of the respondent squared
	gen age2=age^2
	
	* Individual's number of sons and daughters
	gen numbersons=0
	gen numberdaughters=0
	
	forval i = 1/19 {
		local j = string(`i', "%02.0f")
		replace numbersons = numbersons + 1 if kidsex`j' == 1 & syear>=kidgeb`j'
		replace numberdaughters = numberdaughters + 1 if kidsex`j' == 2 & syear>=kidgeb`j'
	}

	* OUTCOME VARIABLE
	* We use plh0011_h (Do you support a political party?) and plh0012_h (Which party do you support?) to construct a measure of political preferences along the left–right spectrum.
	* For respondents who support a specific party, we assign the corresponding Left–Right Ideology score (0–10) from the Chapel Hill Expert Survey (CHES) for the relevant period.
	* We use the CHES values for all supported parties (when there is only one supported party), with the exception of CDU/CSU support (then we use the simple average of the CDU and CSU CHE values).
	* Respondents who do not report support for a specific party are assigned a value of 5.
	* We then reverse the score to create our outcome variable, which reflects how left-leaning the respondent's political preferences are.
	
	* Left-right preferences (CHES panel)
	*1999	left-right
	*PDS	1.266667
	*Grüne	3
	*SPD	4
	*CDU	5.866667
	*FDP	6.066667
	*CSU	7.2
	*Republikaner	9.214286
	*DVU	9.769231

	gen chesvalue=.
	replace chesvalue=4 if plh0012_h==1&syear>=1999&syear<2002
	replace chesvalue=5.866667 if plh0012_h==2&syear>=1999&syear<2002
	replace chesvalue=7.2 if plh0012_h==3&syear>=1999&syear<2002
	replace chesvalue=6.066667 if plh0012_h==4&syear>=1999&syear<2002
	replace chesvalue=3 if plh0012_h==5&syear>=1999&syear<2002
	replace chesvalue=1.266667 if plh0012_h==6&syear>=1999&syear<2002
	replace chesvalue=9.214286 if plh0012_h==7&syear>=1999&syear<2002 
	replace chesvalue=6.5333335 if plh0012_h==13&syear>=1999&syear<2002 

	*2002	left-right
	*PDS	1.64
	*Grüne	3.36
	*SPD	4
	*CDU	5.93
	*FDP	6.07
	*CSU	7.36

	replace chesvalue=4 if plh0012_h==1&syear>=2002&syear<2006
	replace chesvalue=5.93 if plh0012_h==2&syear>=2002&syear<2006
	replace chesvalue=7.36 if plh0012_h==3&syear>=2002&syear<2006
	replace chesvalue=6.07 if plh0012_h==4&syear>=2002&syear<2006
	replace chesvalue=3.36 if plh0012_h==5&syear>=2002&syear<2006
	replace chesvalue=1.64 if plh0012_h==6&syear>=2002&syear<2006
	replace chesvalue=6.645 if plh0012_h==13&syear>=2002&syear<2006

	*2006	left-right
	*Linkspartei/PDS	1.27
	*Grüne	3.09
	*SPD	3.55
	*CDU	6.36
	*FDP	6.64
	*CSU	7.64

	replace chesvalue=3.55 if plh0012_h==1&syear>=2006&syear<2010
	replace chesvalue=6.36 if plh0012_h==2&syear>=2006&syear<2010
	replace chesvalue=7.64 if plh0012_h==3&syear>=2006&syear<2010
	replace chesvalue=6.64 if plh0012_h==4&syear>=2006&syear<2010
	replace chesvalue=3.09 if plh0012_h==5&syear>=2006&syear<2010
	replace chesvalue=1.27 if plh0012_h==6&syear>=2006&syear<2010
	replace chesvalue=7 if plh0012_h==13&syear>=2006&syear<2010

	*2010	left-right
	*Linke	1.3125
	*Grüne	3.625
	*SPD	3.625
	*CDU	6.125
	*FDP	6.6
	*CSU	7.066667

	replace chesvalue=3.625 if plh0012_h==1&syear>=2010&syear<2014
	replace chesvalue=6.125 if plh0012_h==2&syear>=2010&syear<2014
	replace chesvalue=7.066667 if plh0012_h==3&syear>=2010&syear<2014
	replace chesvalue=6.6 if plh0012_h==4&syear>=2010&syear<2014
	replace chesvalue=3.625 if plh0012_h==5&syear>=2010&syear<2014
	replace chesvalue=1.3125 if plh0012_h==6&syear>=2010&syear<2014
	replace chesvalue=6.5958335 if plh0012_h==13&syear>=2010&syear<2014

	*2014	left-right
	*Linke	1.230769
	*Piraten	3.25
	*Grüne	3.615385
	*SPD	3.769231
	*Tierschutz	4.666667 (not in the SOEP)
	*CDU	5.923077
	*FDP	6.538462
	*CSU	7.230769
	*AfD	8.923077
	*NPD	10

	replace chesvalue=3.769231 if plh0012_h==1&syear>=2014&syear<2019
	replace chesvalue=5.923077 if plh0012_h==2&syear>=2014&syear<2019
	replace chesvalue=7.230769 if plh0012_h==3&syear>=2014&syear<2019
	replace chesvalue=6.538462 if plh0012_h==4&syear>=2014&syear<2019
	replace chesvalue=3.615385 if plh0012_h==5&syear>=2014&syear<2019
	replace chesvalue=1.230769 if plh0012_h==6&syear>=2014&syear<2019
	replace chesvalue=10 if plh0012_h==7&syear>=2014&syear<2019
	replace chesvalue=6.576923 if plh0012_h==13&syear>=2014&syear<2019
	replace chesvalue=3.25 if plh0012_h==26&syear>=2014&syear<2019
	replace chesvalue=8.923077 if plh0012_h==27&syear>=2014&syear<2019

	*2019	left-right
	*Linke	1.428571
	*Piraten	2.142857
	*Tierschutz	2.333333
	*Grüne	3.238095
	*SPD	3.619048
	*CDU	5.857143
	*FDP	6.428571
	*CSU	7.190476
	*AfD	9.238095

	replace chesvalue=3.619048 if plh0012_h==1&syear>=2019
	replace chesvalue=5.857143 if plh0012_h==2&syear>=2019
	replace chesvalue=7.230769 if plh0012_h==3&syear>=2019
	replace chesvalue=6.428571 if plh0012_h==4&syear>=2019
	replace chesvalue=3.238095 if plh0012_h==5&syear>=2019
	replace chesvalue=1.428571 if plh0012_h==6&syear>=2019
	replace chesvalue=6.5238095 if plh0012_h==13&syear>=2019
	replace chesvalue=2.142857 if plh0012_h==26&syear>=2019
	replace chesvalue=9.238095 if plh0012_h==27&syear>=2019

	* Respondents who indicate no party preference (plh0011_h == 2) or respond "don't know" (plh0011_h == 3) are assigned a default score of 5.
	replace chesvalue=5 if plh0011_h==2|plh0011_h==3

	* Inverting the variable
	gen leftism=10-chesvalue
	
	
* HYPOTHESIS TEST FOR H2
	* Our model is a fixed effects panel regression estimating how having a daughter relates to left-leaning political preferences over time.
	* Standard errors are clustered at the household level.
	* Dependent variable: leftism - continuous index measuring left-leaning political preferences (see above)
	* Key predictor: has_daughter - whether the household has at least one daughter in a given year
	* Controls: whether the household has any children (has_child), age (age), age squared (age2), and number of sons and daughters (numbersons, numberdaughters).

	xtreg leftism has_daughter has_child age age2 numbersons numberdaughters, fe cluster(cid)


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[has_daughter]

		* (ii) standard error
		local se = _se[has_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value (one-sided test)
		local p_value = _b[has_daughter] / _se[has_daughter]
		if _b[has_daughter] > 0 {
			local p_value = ttail(e(df_r), abs(`p_value'))
		}
		else {
			local p_value = 1-ttail(e(df_r), abs(`p_value'))
		}
		

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[has_daughter]
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
