********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED], [REDACTED] (working with a template by Tina Wang, David Albrecht)

* ============================
* Setup * 
* ============================

* clear all
* version 19 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000
set more off
set seed 1980
capture log close

* INSTALL PACKAGES * 
ssc install outreg2

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

	use "$IN/ppathl.dta", clear 

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(sumkids kidpnr?? kidgeb?? kidsex?? kidmon??) 
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pid syear pgcasmin pgbilzeit pgmonth pgpiyear pgfamstd pgemplst pglfs pgpartz pgpartnr) 
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0308_v2) // gender attitudes
	* Biography * 
	merge 1:1 pid syear using "$IN/biol.dta", nogen keep(master matches) keepusing(lb1110 lb1113 lb1114 lb1115 lb1119 lb1120 lb1121) 

	label language EN 
	
	* Missing *	
	mvdecode _all, mv(-8/-1 = .)
	save "$DATA/h4_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* FILTERS * 
 
	* The analysis sample consists of individuals from private households who conducted an individual questionnaire
	keep if inlist(pop,1,2,5,6) & inrange(netto,10,19) 
	sort pid syear

* GENERATE VARIABLES *

	* DV *
	clonevar equal_wc_2018_1=plh0308_v2

*Controls*

	* age
	gen age = syear - gebjahr if gebjahr<.
	
	* education (CASMIN)
	recode pgcasmin (1/3 = 1 "Low education") (4/7 = 2 "Intermediate education") (8/9 = 3 "High Education") (0 . = 4 "Other"), gen(edu)
	tab pgcasmin edu, mis

	* marital status
	recode pgfamstd (1 2 6 = 1 "Married" ) (3 4 5 7 8 = 0 "Other" ), gen(fam) 
	ta pgfamstd fam, mis
		
	* east/west
	recode sampreg (2 = 0), gen(west)
	tab sampreg west, mis
	
	* labor force status
	tab pgemplst
	recode pgemplst (1 2 3 4 6 = 1 "work") (5 = 0 "not employed"), gen(work)
	ta pgemplst work, mis
	
	* sex
	recode sex (1 = 0 "male")(2 = 1 "female"), gen(gender)
	ta sex gender, mis
	
* Treatment var *

	* Generate dummy variable for whether first kid was daughter (1) or son (0) 
	gen daughter = cond(kidsex01 == 1,0,cond(kidsex01 == 2,1,.z)) 			
	tab daughter kidsex01, mis 	
	
	* Generate indicator for child born 1-24 months before interview
	gen int_year = syear
	replace int_year = pgpiyear if syear!=pgpiyear & pgpiyear !=. 		// correct, if interview year is differnt than survey year
	
	gen int_mth = ((int_year-1984)*12+pgmonth)	 						// Monthly timepoint of interview
	gen geb_mth = (((kidgeb01-1984)*12+kidmon01))	 					// Monthly timepoint of birth first child
	gen msb = int_mth-geb_mth 
	
	recode msb (1/24 = 1) (else = 0), into(valid)						// indicator: child born 1-24 months before interview
	replace msb=. if msb<1 | msb>24										// time since birth
	
	* Check for mutliple births *
	gen multiple = (kidgeb01 == kidgeb02 & kidmon01 == kidmon02) & (kidgeb01 < . & kidmon01 < .)
	ta multiple valid
	
	* Housekeeping *
	keep if valid==1 & syear==2018 & multiple!=1 
	keep pid equal_wc_2018_1 daughter age west edu fam syear msb hid cid gender work

* HYPOTHESIS TEST FOR H4 * 

	* Model description: We calculate two OLS regressions separately for male respondents and female respondents regressing
	* gender equality attitudes as measured in 2018 on having a daughter rather than a son as first child with some
	* sociodemographic controls and an indicator on months passed since birth of first child. Based on these two regressions,
	* we estimate the difference in the daughter effect for male and female respondents simultaneously (using the suest 
	* command) and calculate a Wald X2 test for equality of daughter coefficients of male and female respondents to test for 
	* statistical significance of the difference.

	reg equal_wc_2018_1 ib0.daughter if gender==0	// only fathers 
	estimates store m_father         
	reg equal_wc_2018_1 ib0.daughter if gender==1	// only mothers
	estimates store m_mother
	

	
	suest m_father m_mother, vce(cluster hid) 
	test [m_father_mean]1.daughter = [m_mother_mean]1.daughter

	
* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) difference in coefficient estimate of daughters for fathers and mothers
		local coefficient = _b[m_father_mean:1.daughter]-_b[m_mother_mean:1.daughter]

		* (ii) Chi2 value
		local chi2=  r(chi2)

		* ADDED
		* (ii) Standard error (from lincom)
		lincom [m_father_mean]1.daughter - [m_mother_mean]1.daughter
		local se = r(se)
		* ADDED END

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "Chi² - Waldtest"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = r(df)

		* (v) p-value
		local p_value = r(p)

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_father_minus_mother = _b[m_father_mean:1.daughter]-_b[m_mother_mean:1.daughter]
		local interpretation "positive coefficient is in line with hypothesis"
		* (iX) Control variables
		local controls "`sigvars'"
		
		
	// Display the saved statistics
	di "Difference in coefficients [Father-Mother]: `coefficient'"
	di "Standard Error: N/A"
	di "chi2 - value: `chi2'"
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
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation', `controls'" _n
	file close myfile


********************************************************************************

exit
