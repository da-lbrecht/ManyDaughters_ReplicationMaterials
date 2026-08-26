********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED], [REDACTED] (working with a template by Tina Wang, David Albrecht)

* ============================
* Setup * 
* ============================

* clear all
* version 19.5 // indicate the version of stata you are using here
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
* FOR WOMEN *
 
	use "$IN/ppathl.dta", clear
	keep cid pid hid syear parid partner sex gebjahr psample sampreg pop netto sexor phrf0 phrf1 piyear

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) ///
	keepusing(sumkids kidpnr?? kidgeb?? kidsex?? kidmon??)
	
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) ///
	keepusing(pid syear pgcasmin pgbilzeit pgmonth pgpiyear pgfamstd pgemplst pglfs pgpartz pgpartnr)
	
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
	keepusing(plh0004 pld0152 plh0301 plh0303 plh0307 plh0301 plh0308_v1 plh0308_v2 plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 pli0010 pli0036 pli0051 pli0044_h pli0019_h pli0022_h)
	sort pgpartnr syear
	keep if sex == 2							//keep only women
	drop if pgpartnr == -2 | pgpartnr == .		// keep only women with  valid partner pid
	save "$DATA/h3_dataset_wom.dta", replace 	// save merged dataset for women
	
	
* Prepare data from male partner *
	use "$IN/ppathl.dta", clear
	keep pid syear sex gebjahr 
	
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) ///
	keepusing(syear pid pli0010 pli0036 pli0051 pli0019_h pli0044_h pli0022_h)
	
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) ///
	keepusing(pid syear pgcasmin pgemplst)
	
	keep if sex == 1							//keep only men
	
	foreach u in pgcasmin gebjahr pgemplst pli0010 pli0036 pli0051 pli0019_h pli0044_h pli0022_h{
		rename `u' part_`u'	
	}
	
	rename pid pgpartnr
	
	drop if pgpartnr == -2 | pgpartnr == . //nothing should be dropped here (safety measure for merging with the large sample)
	
	keep pgpartnr syear part*
	sort pgpartnr syear
	merge 1:1 pgpartnr syear using "$DATA/h3_dataset_wom.dta"
	
	keep if _merge==3 //keep only matched couples (outcome: differnce in care work between couples)
	drop _merge
	
* English labels *	
	label language EN // change language of the labels to English

* Missings *	
	mvdecode _all, mv(-8/-1 = .)
	save "$DATA/h3_dataset.dta", replace // save merged dataset


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
	
	gen part_age = syear - part_gebjahr if part_gebjahr<.
	
	* education (CASMIN)
	recode pgcasmin (1/3 = 1 "Low education") (4/7 = 2 "Intermediate education") (8/9 = 3 "High Education") (0 . = 4 "Other"), gen(edu)
	tab pgcasmin edu, mis
	
	recode part_pgcasmin (1/3 = 1 "Low education (partner)") (4/7 = 2 "Intermediate education (partner)") (8/9 = 3 "High Education (partner)") (0 . = 4 "Other (partner)"), gen(part_edu)
	tab part_pgcasmin part_edu, mis
	

	* marital status
	recode pgfamstd (1 2 6 = 1 "Married" ) (3 4 5 7 8 = 0 "Other" ), gen(fam) 
	ta pgfamstd fam, mis
		
	* east/west
	recode sampreg (2=0), gen(west)
	tab sampreg west, m
	
	* labor force status
	tab pgemplst, mis
	recode pgemplst (1 = 1 "full-time") (2 = 2 "part-time") (5 = 3 "not employed") (3 4 6 = 4 "other"), gen(work)
	ta pgemplst work, mis
	
	tab part_pgemplst, mis
	recode part_pgemplst (1 = 1 "full-time (partner)") (2 = 2 "part-time (partner)") (5 = 3 "not employed (partner)") (3 4 6 = 4 "other (partner)"), gen(part_work)
	ta part_pgemplst part_work, mis
	
	* sex
	recode sex (1 = 0 "male")(2 = 1 "female"), gen(gender)
	
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
	ta multiple valid
	
* Dependent var * 
	gen care_gap_fem = part_pli0044_h - pli0044_h 	//pos. value means male partner takes on more carework
	
	bys daughter: ta care_gap_fem 					//check care gaps by daughters/sons
	bys valid: ta care_gap_fem 						//check care gaps by sample
	ta sexor valid									//check sexual orientation by sample
	
* Housekeeping *
	
	keep if valid==1 & sexor==0
	
	sort pid syear
	quietly bys pid:  gen dup = cond(_N==1,0,_n)	//check for duplicates
	ta dup, m
	list syear pid hid daughter msb dup if dup>0  	//check
	drop if dup > 1 								//keep only first interviews 
	
	keep pid *gap* daughter age west edu* fam hid syear msb part_pli0044_h ///
	part_age part_edu part_work pli0044_h gender *work
	
	ta daughter, m 
	


* HYPOTHESIS TEST FOR H3 *

	* Model descritption: 
	reg care_gap_fem i.daughter c.age c.part_age i.edu i.part_edu i.fam i.west i.work i.part_work i.syear

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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
