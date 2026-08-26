********************************************************************************
******************* Do-File H1: #ManyDaughters Project ********************
********************************************************************************
*Authors: [REDACTED]

* ============================
* Setup *
* ============================
*clear all
version 19.5 // indicate the version of stata used for this analysis
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
* install all packages you need here, for example:
ssc install outreg2
ssc install reghdfe, replace
ssc install ftools, replace
ssc install csdid, replace

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
	merge m:1 pid using "$IN/biobirth.dta", nogen keepusing(sex bioage biokids sumkids kidsex02 kidsex01 kidgeb02 kidgeb01)
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keepusing(pgmode pgnation pgpartz pgpartnr pglabgro pgsndjob pgfamstd)
	merge 1:1 pid syear using "$IN/pl.dta", nogen keepusing(plh0004 plb0176_h plb0585 plb0584 plb0517 plc0177 plc0178 plh0308_v2 pld0133 plc0177 plc0178)
	drop eintritt erstbefr austritt psample letztbef sampreg netto nett1 casemat pop todjahr todinfo immiyear germborn corigin gebmonat loc1989 migback miginfo germborninfo corigininfo immiyearinfo sexor sexorinfo locinfo birthregion birthregion_ew arefback arefinf rv_id prgroup pbleib phrf phrf0 phrf1 piyear
	label language EN // change language of the labels to English
	save "$DATA/h1_dataset.dta", replace // save merged dataset
	
* ============================
* Analysis
* ============================

* FILTERS *
 
*We include only families with 1-2 children. If participants had children during our included panel years, this was reflected in sumkids but not biokids, so we use sumkids as our basis for exclusion.
drop if sumkids>2 | sumkids==0

*We include data limited to the years 2000 to 2020.
drop if syear<2000 | syear>2020

*We include only responses from individuals over the age of 18.
drop if bioage <18

*We include only Computer-Assisted Personal Interviews (CAPI) to avoid experimenter effects
drop if pgmode!=140
	
	
* GENERATE VARIABLES *
*Tidy Control Variables
rename bioage Age
rename sex Male
gen Income = pglabgro if pglabgro>=0
replace Income = Income + pgsndjob if pgsndjob>0
replace pgnation = . if pgnation==-1
rename pgnation Nationality
gen Partner_In_House = 1 if pld0133==1
replace Partner_In_House = 0 if pld0133==2
replace Partner_In_House = 0 if pld0133==-2  


*Dependant variable
*Our dependent variable will be agreement that men and women should share household responsibilities equally -- "Completely Disagree" (1) to "Fully agree" (7).
rename plh0308_v2 Equal_responsibility
drop if Equal_responsibility <0

*Independant variables
*We create a binary variable which takes the value 1 if a respondent answers to one or more of the kidsex variables that the sex of their child is 'Female' and '0' otherwise.
gen At_least_One_Daughter = 0
foreach var of varlist kidsex01-kidsex02 {
    replace At_least_One_Daughter = 1 if `var' == 2
}

*We also consider a pre (0) and post (1) treatment dummy coded as 0 before respondents have a daughter and 1 after respondents have a daughter
// gen After_Having_Daughter = 0
// replace After_Having_Daughter = 1 if kidsex01==2 & (kidgeb01<=syear|syear<2000)
// replace After_Having_Daughter = 1 if kidsex02==2 & kidgeb02<=syear

*To use the Callaway & Sant'Anna (2021) method of staggered DiD as a robustness check we also generate a variable for the first year respondents had a daughter
// gen First_Year_Daughter = .
// replace First_Year_Daughter = kidgeb01 if kidsex01==2 & kidgeb01>2000
// replace First_Year_Daughter = kidgeb02 if kidsex02==2 & kidsex01!=2 & kidgeb02>2000


* HYPOTHESIS TEST FOR H1 *
*To test our first hypothesis that having daughters leads to stronger support for gender equality, we conduct a staggered difference-in-difference estimation with time (syear) and individual-level (pid) fixed effects and clustered standard errors, of agreement that men and women should share household responsibilities equally (Equal_responsibility) on the dummy for having a daughter or not (At_least_One_Daughter), a dummy for pre and post having the first daughter (After_Having_Daughter) and an interaction term of the two dummies (At_least_One_Daughter x After_Having_Daughter).
//reghdfe Equal_responsibility i.At_least_One_Daughter##i.After_Having_Daughter, absorb(pid syear) vce(cluster pid)
//reghdfe Equal_responsibility i.At_least_One_Daughter##i.After_Having_Daughter Age Male Income i.Nationality Partner_In_House sumkids, absorb(pid syear) vce(cluster pid)

// For robustness we also conduct a staggered diff in diff using the Callaway & Sant'Anna (2021) method to account for treatment effect heterogeneity
//csdid Equal_responsibility At_least_One_Daughter, ivar(pid) time(syear) gvar(First_Year_Daughter) method(dripw) vce(cluster pid)
//csdid Equal_responsibility At_least_One_Daughter Age Male Income i.Nationality Partner_In_House sumkids, ivar(pid) time(syear) gvar(First_Year_Daughter) method(dripw) vce(cluster pid)

// Panel Analysis not possible because responses to Equal_responsibility were only given in 2018, so we provide the cross-sectional alternative to the analysis in our preanalysis plan. 

* HYPOTHESIS TEST FOR H1 - CROSS-SECTIONAL*
reg Equal_responsibility At_least_One_Daughter
reg Equal_responsibility At_least_One_Daughter Age Male Income i.Nationality Partner_In_House sumkids

* EXTRACT AND EXPORT RESULTS *
	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[At_least_One_Daughter]

		* (ii) standard error
		local se = _se[At_least_One_Daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[At_least_One_Daughter] / _se[At_least_One_Daughter]
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
******************** End of Do-File #ManyDaughters Project *********************
********************************************************************************

exit