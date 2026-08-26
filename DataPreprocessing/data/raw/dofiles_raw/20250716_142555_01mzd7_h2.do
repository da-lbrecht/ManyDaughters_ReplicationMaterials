********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* RT: C. [REDACTED], I. [REDACTED]

* ============================
* Setup *
* ============================

*clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:

* ssc install outreg2
/*
* CONFIGURE FILE PATHS * (Please do not change)
* the following code automatically sets paths based on where you saved the distributed file-package on your system *


* Unfortunately the original template for the paths with c(pwd) doesn not work for us. We had to adjust.
* The original c(pwd) is preserved and should work externally.

if  ("`c(username)'" == "saueri009") {
	global ROOT "Z:\isauer\#ManyDaughters\SOEP\#ManyDaughters_RT_AnalysisPackage"
}

	
if  ("`c(username)'" != "saueri009") {
		
		if strpos("`c(pwd)'", "code") {
			global ROOT "`c(pwd)'/.."
		}
		else {
			global ROOT "`c(pwd)'"
		}
}

global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for datasets
global OUT "$ROOT/out" // folder for output (results, tables)

*show all current globals
macro list
*/
* ============================
* Data Preparation * 
* ============================


* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kid* sumkids)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing( pgpbbil* pgfamstd pgmonth)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0004 )
	* hgen contains generated variables on household level from the household interview; use for household net income

	label language EN // change language of the labels to English

	

	* Example: only keep participants from private households
	keep if inlist(pop,1,2)

	*recode missing vrom negative values to stata missing notation
	mvdecode _all, mv(-1 = .a \ -2 = .b \ -3 = .c \ -4 = .d \ -5 = .e \ -6 = .f \ -7 = .g \ -8 = .h )
	
	
	
	* GENERATE VARIABLES * 
	
	****************************************************************************

	****************************************************************************
	*generate age variable
	gen age  = syear - gebjahr + 1  
	la var age "Age" //(2 Decim.)

	
	****************************************************************************
	gen mig = 0  // migback
	replace mig = 1 if inlist(migback,2,3) //  direct or indirect migration background
	lab var mig "=1 if direct or indirect migration background" //(2 Decim.)

	****************************************************************************
	* *generate categorical education variable
	gen educ = .	
	replace educ = 1 if  pgpbbil03 == 1 
	*Vocational training
	replace educ = 2 if inrange(pgpbbil01,1,6) 
	replace educ = 2 if inrange(pgpbbilo, 1,4)	//Berufsbildung im Osten
	replace educ = 2 if inlist(pgpbbila, 1,2,3,5,6,8,11,12,13,15,16,18)  // Berufliche Bildung im Ausland
	* University degree
	replace educ = 3 if inrange(pgpbbil02,1, 9)
	replace educ = 3 if inlist(pgpbbila,4,7,9,14,17,19)
	
	replace educ = 4 if missing(educ)
	
	#delimit;
		lab define voc_lab
		1  "No vocational training"		//no voc_i
		2  "Vocational training"		//voc_i
		3  "University degree" 
		4  "Missing", replace	//uni_i
	;
	#delimit cr
	lab val educ voc_lab
	la var educ "Education"
	tab educ,m
	
	
	
	****************************************************************************
	* generate East-West indicator 
	gen east = .
	replace east = 1 if sampreg == 2
	replace east = 0 if sampreg == 1
	replace east = 2 if missing(east)
	#delimit;
		lab define east_lab
		0  "West"		//no voc_i
		1  "East"		//voc_i
		2  "Missing", replace	//uni_i
	;
	#delimit cr
	lab val east east_lab
	la var east "East"
	tab east,m
	
	****************************************************************************
	*Dummy variable indicating females
	gen female = 1 if sex == 2
	replace female = 0 if sex == 1
	
	#delimit;
		lab define female_lab
		0  "Male"		
		1  "Female"
		2   "Missing", replace	
	;
	#delimit cr
	lab val female female_lab
	
	la var female "Female" 
	
	
	****************************************************************************
	* Political preferences
	
	****************************************************************************
	* variable for leftist political attitudes based on reversed scaled plh0004
	
	rename plh0004 polit 
	tab polit, m
	*reverse scale
	recode polit (0=10) (1=9) (2=8) (3=7) (4=6) (5=5) (6=4) (7=3)  (8=2)  (9=1)  (10=0)
	
	#delimit;
		lab define polit_lab
		0  "0"
		1  "1"					
		2  "2"			
		3  "3" 			
		4  "4"
		5  "5" 
		6  "6"
		7  "7"
		8  "8"
		9  "9"
		10 "10", replace			
	;
	#delimit cr
	lab val polit polit_lab
	la var polit "Political preferences (0 right - 10 left)"
	
	*Normalization to a [0,1] scale and reverse scaled
	gen polit_index = (polit - 0)/(10-0)
	la var polit_index "Political preferences (0 right - 1 left)"
	
	sum polit_index
	hist polit_index

	
	
	****************************************************************************
	* Generate dummy variable for whether participant has at least one daughter (1) or not (0) for each year
	* having a daughter (In the PAP as d_i)
	* Generate a variable that indicates for each survey year whether one has at least a daughter 
	* kid dataset is not in [REDACTED], therfore format it into panel structure manually.
	
		
	qui sum syear
	* min max span of years
	scalar min_years = r(min)
	scalar max_years = r(max)
	scalar list 
 
	*create empty variable for each year separately
	cap drop d_daughter_* d_son_*
	foreach i of numlist `=min_years'(1)`=max_years' {
			display in red "Daughter: `i'" 
			gen 	d_daughter_`i' = .
			lab var d_daughter_`i' "Daughter in `i'"
			
			gen 	d_son_`i' = .
			lab var d_son_`i' "Son in `i'"	
	}
	
	* delete 0 from the variable name for child birth date, sex, etc. 
	foreach i of numlist 1(1)9 {
			display in red "Child: `i'" 
			
			rename kidpnr0`i' kidpnr`i'
			rename kidgeb0`i' kidgeb`i'   
			rename kidmon0`i' kidmon`i'
			rename kidsex0`i' kidsex`i' 
		}
	
		sort pid syear 

	* interview month in stata data format
	cap drop interview_y_m
	gen interview_y_m=ym(syear,pgmonth)   if !missing(syear) & !missing(pgmonth) 
	
	foreach i of numlist 1(1)19 {
			display in red "Child: `i'" 
			gen birth_y_m_kid`i'=ym(kidgeb`i',kidmon`i')   if !missing(kidgeb`i') & !missing(kidmon`i') 		
	}
	
	* indicate for every daughter/son year combination if the child is female 
	* (Childbirth before Interview in respective year)
	foreach y of numlist `=min_years'(1)`=max_years' {
		foreach i of numlist 1(1)19 {
			display in red "Child: `i'" 
			 
			* daughters
			replace d_daughter_`y' = 1 if kidsex`i' == 2  & syear == `y' &    birth_y_m_kid`i' <= interview_y_m  & !missing(birth_y_m_kid`i') & !missing(interview_y_m)
			*sons
			replace      d_son_`y' = 1 if kidsex`i' == 1  & syear == `y' &    birth_y_m_kid`i' <= interview_y_m  & !missing(birth_y_m_kid`i') & !missing(interview_y_m)
		}
	}
	
	* for the case where birth month is noch available but the birth was definitely in the year before the interview based on year of birth.
	foreach y of numlist `=min_years'(1)`=max_years' {
		foreach i of numlist 1(1)19 {
			display in red "Child: `i'" 
			* indicate for every daughter/son year combination if the child is female/male (Childbirth before Interview in respective year 
			* fill in the missings based only on years not interview months) 
			
			* daughters
			replace d_daughter_`y' = 1 if kidsex`i' == 2  & syear == `y' &    kidgeb`i' <= syear  & !missing(kidgeb`i') & missing(d_daughter_`y')
			* sons
			replace      d_son_`y' = 1 if kidsex`i' == 1  & syear == `y' &    kidgeb`i' <= syear  & !missing(kidgeb`i') & missing(d_son_`y')
		}
	}
	

	* The main minimum one daughter and minimum one son indicators
	* minimum one daughter
	egen d_daughter=rowtotal(d_daughter_*)
	*minimum one son
	egen d_son=rowtotal(d_son_*)
		
	lab var d_daughter "at least one daughter in year t"
	lab var d_daughter "at least one son in year t"
	
	
	* e.g. 
	* br  pid syear kidsex1 kidsex2 kidgeb1 kidgeb2 kidmon1  kidmon2  birth_y_m_kid1 birth_y_m_kid2 interview_y_m d_daughter  d_son sumkids 
	

		
	* FILTERS * 
	
	****************************************************************************
	* clean the final sample
	
	*keep only observations with GE_index not missing
	keep if !missing(polit_index)
	
		
	* Political preferences only in 2018 in the survey / keep only 2018
	keep  if inlist(syear, 2005, 2009, 2014, 2019)
	
	
		
	*keep only thouse who at some point became parents 
	keep if !missing(sumkids) & sumkids>0

	* keep only relevant variables 
	keep polit_index pid syear d_daughter  age mig educ east female  
	

	save "$DATA/h2_dataset.dta", replace // save the merged dataset
	
	
	
* ============================
* Analysis
* ============================


	
	xtset pid syear


* HYPOTHESIS TEST FOR H2 * 

	* We estimate a linear fixed effects model. Thus we analyze individual within 
	* variation in political preferences after becoming a parent to a daughter.

	* polit_index is measure of left-leaning political preferences normalized to [0,1]
	* 0 stands for the rightest, and 1 for the leftist preference on the scale.
	* We regress the preferences onto the age, migration background, vocational
	* education, East German affiliation, and sex of an individual for the sake 
	* of completeness. However, the estimates of constant variables will be 
	* omitted in the FE estimation. We use individual level cluster standard errors. 
	
	xtreg polit_index i.d_daughter c.age i.mig i.educ i.east i.female i.syear, fe vce(cluster pid)
	
* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.d_daughter]

		* (ii) standard error
		local se = _se[1.d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.d_daughter] / _se[1.d_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
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
