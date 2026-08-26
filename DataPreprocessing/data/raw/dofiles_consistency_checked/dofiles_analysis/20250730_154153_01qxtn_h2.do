********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED], [REDACTED] (working with a template by Tina Wang, David Albrecht)

* ============================
* Setup * 
* ============================

* clear all
version 19.5 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

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
	keep cid pid hid syear parid partner sex gebjahr psample sampreg pop netto sexor phrf0 phrf1 piyear

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) ///
	keepusing(sumkids kidpnr?? kidgeb?? kidsex?? kidmon??)
	
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) ///
	keepusing(pid syear pgpiyear pgcasmin pgbilzeit pgmonth pgpiyear pgfamstd pgemplst pglfs pgpartz pgpartnr)
	
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
	keepusing(plh0004 pld0152 plh0301 plh0303 plh0307 plh0301 plh0308_v1 plh0308_v2 plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 pli0010 pli0036 pli0051)


	label language EN 

* Missings *	
	mvdecode _all, mv(-8/-1 = .)
	save "$DATA/h2_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================
* FILTERS * 

	* The analysis sample consists of individuals from private households who conducted an individual questionnaire 
	keep if inlist(pop,1,2,5,6) & inrange(netto,10,19) 
	sort pid syear
	
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

	* Generate indicator for child born 1-12 months before interview
	gen int_year = syear
	replace int_year = pgpiyear if syear!=pgpiyear & pgpiyear !=. 		// correct, if interview year is differnt than survey year

	gen int_mth = ((int_year-1984)*12+pgmonth)	 						// Monthly timepoint of interview
	gen geb_mth = (((kidgeb01-1984)*12+kidmon01))	 					// Monthly timepoint of birth first child
	
	gen msb = int_mth-geb_mth 
	recode msb (1/12 = 1) (else = 0), into(valid)						// indicator: child born 1-24 months before interview	
	
	* Check for mutliple births *
	gen multiple = (kidgeb01 == kidgeb02 & kidmon01 == kidmon02) & (kidgeb01 < . & kidmon01 < .)
	ta multiple valid, mis
	
* Dependent var *
	recode plh0004 (10=0) (9=1) (8=2) (7=3) (6=4) (5=5) (4=6) (3=7) (2=8) (1=9) (0=10), gen(left) 
	ta plh0004 left, m
	
* Housekeeping *
	
	keep if valid==1 & inlist(syear,2005, 2009,2014,2019) & multiple!=1
	keep pid plh0004 daughter age west edu fam left syear msb hid cid gender work
	
	bys syear: ta daughter, m 											//check number of daughters and sons per survey year
	

	* HYPOTHESIS TEST FOR H2 * 

	* Model description: Linear regression regressing political partisanship on having a daughter rather 
	* than a son as first child with some sociodemographic controls. The idea is that having a daughter 
	* as a first child is randomly distributed among parents and uncorrelated with unobserved characteristics
	* of parents. Excluding multiple births and estimating robust clustered standard errors at the household level.


	reg left i.daughter c.age i.edu i.fam i.gender i.west i.work i.syear, vce(cluster hid)		

* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.daughter]

		* (ii) standard error
		local se = _se[1.daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.daughter] / _se[1.daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[1.daughter]
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
