********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* RT: C. [REDACTED], I. [REDACTED]

* ============================
* Setup *
* ============================

* //clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:

* ssc install outreg2
/*
* CONFIGURE FILE PATHS * (Please do not change)
/*
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
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0298_v1 plh0298_v2  plh0302_* plh0308_* plh0004 plh0343_v1 plh0343_v2)
	* hgen contains generated variables on household level from the household interview; use for household net income

	label language EN // change language of the labels to English

	

	* Only keep participants from private households
	keep if inlist(pop,1,2)

	*recode missing vrom negative values to stata missing notation
	mvdecode _all, mv(-1 = .a \ -2 = .b \ -3 = .c \ -4 = .d \ -5 = .e \ -6 = .f \ -7 = .g \ -8 = .h )
	
	

	
	* GENERATE VARIABLES * 
	
	****************************************************************************
	*generate a GE_index based on  plh0298_v1 plh0302_v1 plh0308_v1

	
	*reverse code plh0308_v2
	recode plh0308_v2 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
	count

	* generate GE Index 
	cap drop additive_index
	gen additive_index = plh0298_v2 +	plh0302_v2 +	plh0308_v2
	tab additive_index
	
	cap drop GE_index
	gen GE_index = (additive_index - 3)/(21-3)
	
	* 0 ==> no   support for gender equality 
	* 1 ==> Full Support for gender equality 

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
	* variable for leftist political attitudes based on reversed scaled plh0004
	cap drop polit_*
	gen polit_2005_h =  plh0004 if syear == 2005
	gen polit_2009_h =  plh0004 if syear == 2009
	gen polit_2014_h =  plh0004 if syear == 2014
	gen polit_2019_h =  plh0004 if syear == 2019
	
	bysort pid: egen polit_2005 = max(polit_2005_h)
	bysort pid: egen polit_2009 = max(polit_2009_h)
	bysort pid: egen polit_2014 = max(polit_2014_h)
	bysort pid: egen polit_2019 = max(polit_2019_h)

	cap drop polit_2018
	gen polit_2018 = .
	replace polit_2018 = polit_2019 if !missing(polit_2019) & syear == 2018 // impute with the value from 2019
	
	replace polit_2018 = polit_2014 if !missing(polit_2014) & syear == 2018 & missing(polit_2018) // if missing in 2019
	replace polit_2018 = polit_2009 if !missing(polit_2009) & syear == 2018 & missing(polit_2018) // if still missing
	replace polit_2018 = polit_2005 if !missing(polit_2005) & syear == 2018 & missing(polit_2018) // if still missing
	
	tab polit_2018  if syear == 2018 ,m

	rename polit_2018 polit

	*reverse scale
	recode polit (0=10) (1=9) (2=8) (3=7) (4=6) (5=5) (6=4) (7=3)  (8=2)  (9=1)  (10=0) 
	
	replace polit = 11 if missing(polit)
	
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
		10 "10"
		11 "Missing", replace			//missing
	;
	#delimit cr
	lab val polit polit_lab
	
	la var polit "Political preferences (0 right - 10 left)"
	
	
	
	tab polit,m // 
	
	
	
	
	****************************************************************************
	* Categorical variable capturing marrital status 
	tab pgfamstd,m //
	
	cap drop marrital_stat
	gen marrital_stat = .

	replace marrital_stat = 1 if inlist(pgfamstd, 1, 6, 7, 8 )
	replace marrital_stat = 2 if inlist(pgfamstd, 2, 4 )
	replace marrital_stat = 3 if inlist(pgfamstd, 3, 5 )
	
	replace marrital_stat = 4 if missing(marrital_stat)

	#delimit;
		lab define marr_lab
		1  "Married"					//Married_i
		2  "Separated/divorced"			//separated_div_i
		3  "Widowed/single" 			//sidowed_single_i
		4  "Missing", replace			//missing
	;
	#delimit cr
	lab val marrital_stat marr_lab
	la var marrital_stat "Marrital status"
	
	tab marrital_stat,m
	
	
	 

	****************************************************************************
	* Variable for religiosity 
	
	tab syear plh0343_v1  // measured only for refugee samples unfortunately. therefore 

	*Impute religiosity from 2016 all samples (plh0343_v2) and from m1-m3 for  2013, 2017, 2018, 2019 (plh0343_v1) 

	*2016 all samples (plh0343_v2 )
	cap drop religiosity*
	gen religiosity_2016_h =  plh0343_v2 if syear == 2016
	
	*m1-m3 for  2013, 2017, 2018, 2019 (plh0343_v1) 
	gen religiosity_2013_h =  plh0343_v1 if syear == 2013 & inlist(psample, 15, 17, 18, 19) 
	gen religiosity_2017_h =  plh0343_v1 if syear == 2017 & inlist(psample, 15, 17, 18, 19) 
	gen religiosity_2018_h =  plh0343_v1 if syear == 2018 & inlist(psample, 15, 17, 18, 19) 
	gen religiosity_2019_h =  plh0343_v1 if syear == 2019 & inlist(psample, 15, 17, 18, 19) 
	
	cap drop religiosity_h
	bysort pid: egen religiosity_h = max(religiosity_2016_h) if !missing(religiosity_2016_h)
	
	replace religiosity_h = religiosity_2018_h if !missing(religiosity_2018_h) & missing(religiosity_h)
	replace religiosity_h = religiosity_2019_h if !missing(religiosity_2019_h) & missing(religiosity_h)
	replace religiosity_h = religiosity_2017_h if !missing(religiosity_2017_h) & missing(religiosity_h)
	replace religiosity_h = religiosity_2013_h if !missing(religiosity_2013_h) & missing(religiosity_h)
	
	bysort pid: egen religiosity = max(religiosity_h)

	* Missing category 5 
	replace religiosity = 5 if missing(religiosity)
	
	* Value labels
	#delimit;
		lab define rel_lab
		1  "1"					
		2  "2"			
		3  "3" 
		4  "4" 
		5  "Missing", replace //missing
	;
	#delimit cr
	lab val religiosity rel_lab
	lab var 	religiosity "Religiosity"
	
	
	
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
	*br  pid syear kidsex1 kidsex2 kidgeb1 kidgeb2 kidmon1  kidmon2  birth_y_m_kid1 birth_y_m_kid2 interview_y_m d_daughter  d_son sumkids 
	

	* FILTERS * 
	
	****************************************************************************
	* clean the final sample
	
	*keep only observations with GE_index not missing
	keep if !missing(GE_index)
	
	*keep only parents 
	keep if !missing(sumkids) & sumkids>0
	
	* Attitudes only in 2018 in the survey / keep only 2018
	keep  if inlist(syear, 2018)
	
	* keep only relevant variables 
	keep pid syear d_daughter GE_index age mig educ east female religiosity  marrital_stat polit 
	

	save "$DATA/h1_dataset.dta", replace // save the merged dataset

	
* ============================
* Analysis
* ============================
	
* HYPOTHESIS TEST FOR H1 * 

	* We regress  the gender eqiality index (GE_index) [0,1] onto the indicator 
	* of having at least one daughter in 2018 and a group of control variables 
	* that, according to previous findings, have an effect on the gender 
	* equality attitudes. 

	* Accordingly, we run a linear OLS regression analysis with GE_index as 
	* dependent variable and the dummy variable d_daughter and additional 
	* controls as independent variables. We also control for missing values of 
	* control variables. 
	
	* The coefficient estimate of variable d_daughter is of main interest.
	* We use robust standard errors. 
	
	reg GE_index i.d_daughter c.age i.mig i.educ i.east i.female  i.marrital_stat  i.polit i.religiosity, r


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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
