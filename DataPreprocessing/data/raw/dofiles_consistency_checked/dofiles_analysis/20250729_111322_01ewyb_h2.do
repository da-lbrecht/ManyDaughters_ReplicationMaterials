********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2

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
global startyear 2000
global endyear 2020
* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
keep(syear cid pid hid partner gebjahr sex migback sampreg)
	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidgeb01-kidgeb19 kidsex01-kidsex19)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgemplst pglabgro pgvebzeit pgpartnr pgcasmin pgfamstd)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0004 plh0012_h)
	* bioparen contains all variables from the individual questionnaire on the individual's parent
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches) keepusing(fsedu msedu)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h2_dataset_cs.dta", replace // save merged dataset


* ============================
* Analysis
* ============================


* GENERATE VARIABLES * (Please adapt according to your needs)
* Clean up missing values according to plan
mvdecode _all , mv(-1=. \ -2=.a \ -3=.c \ -4=.d \ -5=.b \ -6=.e \ -7=.f \ -8=.g \ -9=.h \ -10=.i)

* Age
gen age = syear - gebjahr if !missing(syear, gebjahr)

* Male
gen male = (sex == 1) if !missing(sex)
label var male "Male (yes/no)"

* Education 
gen university=.
replace university=1 if inlist(pgcasmin,8,9)
replace university=0 if !inlist(pgcasmin,8,9)

label def unilab 1 "Universität" 0 "Sonstiges"
label values university unilab 

label var university "Höchster Bildungsabschluss (CASMIN)"

* Marital status
tab syear pgfamstd
gen married=.
replace married=1 if inlist(pgfamstd,1,2,6,7)
replace married=0 if inlist(pgfamstd,3,4,5)
label var married "Married"
tab married

* Migration background
rename 		migback migback_c
gen			migback=migback_c
recode		migback (1=0) (2=1) (3=0)
label 		define migl 0 "No migration background" 1 "Direct migration background"
label 		value migback migl

* East indicator (1 = East Germany)
gen east=.
replace east=1 if sampreg==2
replace east=0 if sampreg==1
label var east "Ostdeutschland (ja/nein)"

// Labour Force Status
gen lfs = . if pgemplst==. | pgemplst < 0
replace		lfs = 0 if pgemplst==5  // not working (nicht erwerbstätig)
replace 	lfs = 1 if pgemplst==2 | pgemplst==4 | pgemplst==6 | pgemplst==1 | pgemplst==3 // full-time employment (Voll erwerbstätig, Ausbildung/Lehre)

* Value Labels
label def 	llfs 0 "Not employed" 1 "Employed"
label val 	lfs llfs

* Gross hourly wage 
gen workhours = pgvebzeit
replace workhours = 0 if lfs==0

gen brstdlov=pglabgro/((52/12)*pgvebzeit ) // basierend auf vertraglichen Stunden
label var brstdlov "Bruttostundenlohn (vertragliche Arbeitszeit)"
replace brstdlov = 0 if lfs==0

* Trimming am 1. und 99. Perzentil (pro Jahr)


*foreach var of varlist brstdlov {
*	qui gen t`var'=.
*	forval i = $startyear/$endyear {
*		qui centile `var'          if syear==`i', c(1 99)
*		qui replace t`var' = `var' if syear==`i' & inrange(`var', r(c_1), r(c_2))
*	}
*}

foreach var of varlist brstdlov {
	qui gen t`var' = .
	forval i = $startyear/$endyear {
		// Exclude zeros (i.e., non-employed) when calculating percentiles
		centile `var' if syear==`i' & `var' > 0  & `var' !=., c(1 99)
		
		// Assign trimmed values for employed people only
	    qui	replace t`var' = `var' if syear==`i' & inrange(`var', r(c_1), r(c_2))
		
		// Add zeros back in
		qui replace t`var' = 0 if syear==`i' & `var' == 0
	}
}


* Count number of children (born before syear)

forvalues i = 1/19 {
    gen _childage`i' = syear - kidgeb`=string(`i', "%02.0f")' if !missing(kidgeb`=string(`i', "%02.0f")', syear)
    gen childpresent`i' = (kidgeb`=string(`i',"%02.0f")' <= syear) if !missing(kidgeb`=string(`i',"%02.0f")', syear)
	
    gen childu16_`i' = (_childage`i' < 16 & _childage`i' >= 0) if !missing(_childage`i')

    gen daughter`i' = (kidsex`=string(`i',"%02.0f")' == 2 & _childage`i' >= 0) ///
        if !missing(kidsex`=string(`i',"%02.0f")', _childage`i')
}

egen numchild = rowtotal(childpresent*)
egen childu16 = rowtotal(childu16_*)
egen numdaughters = rowtotal(daughter*)

egen numchildu16 = rowtotal(childu16*)

	* generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* For illustrative purposes, we will generate the variable randomly:

gen d_daughter=.
replace d_daughter=1 if numdaughters>0 & numchild>0
replace d_daughter=0 if numdaughters<=0 & numchild>0

drop _childage* childpresent* childu16_* daughter*

* Parent's education
gen father_abi =.
replace father_abi = 1 if fsedu == 4 | fsedu == 7
replace father_abi = 0 if inrange(fsedu,1,3)| inrange(fsedu,5,6) | inrange(fsedu,8,9)

gen mother_abi =.
replace mother_abi = 1 if msedu == 4 | msedu == 7
replace mother_abi = 0 if inrange(msedu,1,3)| inrange(msedu,5,6) | inrange(msedu,8,9)


* ---------------  political attitudes measure
*

	
	tab plh0004
	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)


	* Please add a verbal description of your model here

* We investigate whether having a daughter is associated with more left-leaning political preferences. The dependent variable is a given index in the SOEP. Respondents rate their political views on a scale from 0 (far left) to 10 (far right). The variable measuring this is called plh0004 and available in the years 2005, 2009, 2014, and 2019. Based on the variables listed in our PAP, we choose the existing left-right index as our main outcome variable. The results with the alternative outcome using the party affiliation are available upon request. 

* Interpretation:
* - Coefficient on d_daughter captures whether having at least one daughter is associated with more left-leaning political preferences.
* - Controls include sex, age, education, migration background, region (East), marital status, number of children, number of children under 16, labor force status, monthly income, working hours, and survey year fixed effects.
* - Standard errors are clustered at the individual level.

* Disclaimer:
* In our PAP, we proposed three alternative model specifications (1)w/o controls, 2)with core controls and 3)with additional controls) to test the hypothesis. However, as per the current request to report only a single regression and associated p-value, we have selected the specification we consider most appropriate and transparent for capturing the effect. In our PAP this specification is listed as sepcification 2, i.e. the model with core controls. As explained on page 2, sentence one in our PAP, it includes key demographic characteristics of the respondent, including age, gender, education, migration background, East/West Germany indicator, employment variables (where this is not the outcome), and marital status. Note we forgot to list the marital status and the education variable in our table but included it here now, as we did mention it in our text. This model is reported and submitted in the present do-file. The results from the alternative specifications mentioned in the PAP are available upon request. 

	reg plh0004 d_daughter male age university migback east numchild childu16 lfs tbrstdlov workhours married i.syear,  cluster(pid)

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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
