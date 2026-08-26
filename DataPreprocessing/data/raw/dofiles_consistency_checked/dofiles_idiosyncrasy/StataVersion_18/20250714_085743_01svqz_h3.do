********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
// ssc install outreg2

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
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset

	**merge info for partners
	preserve
	keep cid pid syear parid bioage
	rename bioage agepartner
	tempfile partner
	save `partner'
	restore
	
	merge 1:1 cid pid syear parid using `partner'
	
	
* ============================
* Analysis
* ============================

* FILTERS * 
	*Adults under the age of 65
keep if bioage<65 & bioage>=18

	*Adults with at least 1 child
drop if sumkids==0 

	* Keep participants with spouse or partner 
keep if partner==1 | partner==2 
drop if pgpartnr==.

	* Keep nuclear household where only 1 couple cohabit
by cid (pid), sort: gen nbhh = sum(pid != pid[_n-1])
by cid (pid): replace nbhh = nbhh[_N]
g couple=(nbhh==2)

keep if couple==1

	* In survey years for which Time Use on childcare and chores are surveyed
keep if syear==2001 | syear==2003 | syear==2005 | syear==2007 | syear==2009 | syear==2011 | ///
		syear==2012 | syear==2013 | syear==2015 | syear==2017 | syear==2019 
		
* GENERATE VARIABLES * (Please adapt according to your needs)

	* Time Use Chores (Weekly)
		*weekdays
	gen chores_week=pli0043_h
	replace chores_week=. if pli0043_h<0

	gen childcare_week=pli0044_h
	replace childcare_week=. if pli0044_h<0
		*saturday
	gen chores_sat=pli0012_h
	replace chores_sat=. if pli0012_h<0

	gen childcare_sat=pli0019_h
	replace childcare_sat=. if pli0019_h<0
		*sunday
	gen chores_sun=pli0016_h
	replace chores_sun=. if pli0016_h<0

	gen childcare_sun=pli0022_h
	replace childcare_sun=. if pli0022_h<0

	* Outcome variable: Share domestic work
	egen domwork=rowtotal(chores* childcare*)

	bys cid syear : egen totaldomwork=total(domwork)
	g share_males=domwork/totaldomwork if sex==1

	* Generate code for children / daughters / first born
	recode kidsex* (-2=.) (-1=.) (1=0) (2=1)

	egen atleast1daughter=rowtotal(kidsex*)
	replace atleast1daughter=(atleast1daughter>0) if !missing(atleast1daughter)
	g d_daughter=atleast1daughter

	gen daughterFB=(kidsex01==1) if kidsex01!=-2 & kidsex01!=-1	

	* Generate age
	g age=bioage 
	g age2=(age)^2
	
	* Generate age at first birth
	recode kidgeb01 (-3=.) (-2=.) (-1=.)
	g agefb=kidgeb01-gebjahr
	g agefb2=(agefb)^2
	
	* Generate spouse' age squared
	g agepartner2=(agepartner)^2

	
**Filter one observation per household // at household level
keep if sex==1

	* For each control variable, we replace missing values with -99 and include in our regressions dummy variables to indicate observations with originally missing values. This approach allows us to retain all observations even when some control variables have missing data.
	foreach var in sumkids age age2 agefb agefb2 agepartner agepartner2 {
		gen `var'_mv = 1 if `var' == . & share_males != .
		replace `var'_mv = 0 if `var'_mv == . & `var' != . & share_males != .
		replace `var' = -99 if `var' == . & share_males != .
	}

* HYPOTHESIS TEST FOR H3 * 

	* We consider a model using two-stage least squares

	* As a good proxy of gender equality in the household (hypothesis 3), we use the share of housework performed by men (childcare + household chores).
	* The more gender-equal a household is, the more the man contributes to housework.
	* Accordingly, we run a regression analysis where we instrument "having at least one daughter" with a dummy equal to one if the first born was a daugher,
	* and we use "share_males" as dependent variable.


ivregress 2sls share_males (d_daughter = daughterFB) sumkids age age2 agefb agefb2 agepartner agepartner2 sumkids_mv age_mv age2_mv agefb_mv agefb2_mv agepartner_mv agepartner2_mv i.syear, vce(cluster cid)



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

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
		matrix M = r(table)
		local col = colnumb(M, "d_daughter")
		local p_value = M[4, `col']
		
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
