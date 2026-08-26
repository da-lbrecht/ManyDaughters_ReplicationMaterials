********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Pau Balart and [REDACTED]. Adapted from the dofile authored by Tina Wang, David Albrecht.

* ============================
* Setup *  
* ============================

* clear all
version 16 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2

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

	save "$DATA/h1_dataset.dta", replace // save merged dataset

	
	clear 
	use "$DATA/h1_dataset.dta" 

* ============================
* Analysis
* ============================

* FILTERS *  
 
	* We only keep participants from private households
	keep if inlist(pop,1,2)


	*Since our main dependent variable for H1 is asked in the survey year 2018, we only keep participants having children before that year (2018 is not included because we don't know if birth took place before or after taking the survey).
	keep if kidgeb01<2018 & kidgeb01>0 
	

	

* GENERATE VARIABLES * (Please adapt according to your needs)



*Dependent variable:  Level of agreement with the statement: “It’s best if the man and the woman work the same amount so they can share the responsibility for taking care of the family and household equally.”. We recode as missing the non-valid values of the dependent variable: 

replace plh0308_v2=. if plh0308_v2<0   

*We create a dummy taking value 1 if the participants have a daughter born before 2019. 
**First, we code the year when the first daughter was born (FDY). 

gen FDY=. 

forval i= 1 (1) 9{
replace FDY=kidgeb0`i'	if kidsex0`i'==2 &FDY==.
}
replace FDY=kidgeb10	if kidsex10==2 &FDY==.
forval i= 1 (1) 9{
replace FDY=kidgeb1`i'	if kidsex1`i'==2 & FDY==.
}

**Second, we generate a dummy variable that takes value 1 if the daughter was already born at the survey year. 
gen having_daughter=0 
replace having_daughter=1 if FDY< syear 

** Third, we relabel our variable for having a daughter according to the example provided (convinient for storing the results)
rename having_daughter d_daughter

	
*We want to retrive the number of kids born before 2018 (the provided variable sumkids aggregates all births up to 2020)
gen sumkids_dyanamic = .

replace  sumkids_dyanamic=0 if syear < kidgeb01  |  kidgeb01<0
replace  sumkids_dyanamic=1 if syear >= kidgeb01 & kidgeb01>0 & (syear<kidgeb02 | kidgeb02<0  )
replace  sumkids_dyanamic=2 if syear >= kidgeb02 & kidgeb02>0 & ( syear<kidgeb03 | kidgeb03<0 ) 
replace  sumkids_dyanamic=3 if syear >= kidgeb03 & kidgeb03>0 &  ( syear<kidgeb04  | kidgeb04<0 )  
replace  sumkids_dyanamic=4 if syear >= kidgeb04 & kidgeb04>0 & ( syear<kidgeb05  | kidgeb05<0 )  
replace  sumkids_dyanamic=5 if syear >= kidgeb05 & kidgeb05>0 & ( syear<kidgeb06  | kidgeb06<0  ) 
replace  sumkids_dyanamic=6 if syear >= kidgeb06 & kidgeb06>0 & ( syear<kidgeb07  | kidgeb07<0 )   
replace  sumkids_dyanamic=7 if syear >= kidgeb07 & kidgeb07>0 & ( syear<kidgeb08  | kidgeb08<0 )  
replace  sumkids_dyanamic=8 if syear >= kidgeb08 & kidgeb08>0 & ( syear<kidgeb09    | kidgeb09<0)   
replace  sumkids_dyanamic=9 if syear >= kidgeb09 & kidgeb09>0 & ( syear<kidgeb10    | kidgeb10<0 )  
replace  sumkids_dyanamic=10 if syear >= kidgeb10 & kidgeb10>0 & ( syear<kidgeb11    | kidgeb11<0 )  
replace  sumkids_dyanamic=11 if syear >= kidgeb11 & kidgeb11>0 & ( syear<kidgeb12    | kidgeb12<0 )  
replace  sumkids_dyanamic=12 if syear >= kidgeb12 & kidgeb12>0 & ( syear<kidgeb13   | kidgeb13<0 )  
replace  sumkids_dyanamic=13 if syear >= kidgeb13 & kidgeb13>0 & ( syear<kidgeb14   | kidgeb14<0 )  
replace  sumkids_dyanamic=14 if syear >= kidgeb14 & kidgeb14>0 & ( syear<kidgeb15    | kidgeb15<0 )  
replace  sumkids_dyanamic=15 if syear >= kidgeb15 & kidgeb15>0 & ( syear<kidgeb16    | kidgeb16<0 )  
replace  sumkids_dyanamic=16 if syear >= kidgeb16 & kidgeb16>0 & ( syear<kidgeb17    | kidgeb17<0 )  
replace  sumkids_dyanamic=17 if syear >= kidgeb17 & kidgeb17>0 &  (syear<kidgeb18    | kidgeb17<0 )  
replace  sumkids_dyanamic=18 if syear >= kidgeb18 & kidgeb18>0 & ( syear<kidgeb19    | kidgeb18<0 )  
replace  sumkids_dyanamic=19 if syear >= kidgeb19  & kidgeb19!=. & kidgeb19>0
	
	
*Clean missing values for income variable: 
replace hghinc=. if hghinc<0   

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

/*	* Please add a verbal description of your model here: 

Our measure of support for gender equality is based on respondents' level of agreement with the statement: “It’s best if the man and the woman work the same amount so they can share the responsibility for taking care of the family and household equally.” Accordingly, we estimate a linear OLS regression where the dependent variable is plh0308_v2, and the key independent variable is a dummy, d_daughter, which takes the value 1 if the respondent had at least one daughter at the time of responding (survey year: 2018).

Our objective is to estimate the causal effect of having a daughter on gender equality attitudes. While the gender of a child is generally considered to be randomly assigned, fertility decisions are not. Therefore, we restrict our sample to respondents who were already parents at the time of the survey.

However, it is important to acknowledge that subsequent fertility decisions may be influenced by the gender of previously born children, i.e. "gender stopping rules" (Angrist and Evans, 1998). If such decisions are driven by gender preferences, this could threaten the causal interpretation of our estimates. We tested the possibility of gender stopping rules by regressing the number of children on the gender of the first born. By doing so, we found no evidence of correlation (coef 0.008833, p-value=0.848), indicating that gender stopping rules are not extended among the participants. Additionally, to mitigate this concern, we control for the number of children the respondent had at the time of completing the questionnaire. 

We include the following controls: participant gender, net monthly income of the household, a dummy for german born participants, and birth year of the participant fixed effects. 

*/
	
 



reg plh0308_v2 d_daughter  i.sex  i.germborn hghinc  i.gebjahr sumkids_dyanamic         , vce(robust)



  

 

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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit


/*Test for gender stopping rules: 

	clear 
	use "$DATA/h1_dataset.dta" 

* ============================
* Analysis
* ============================

* FILTERS *  
 
	* We only keep participants from private households
	keep if inlist(pop,1,2)


	*Since our main dependent variable for H1 is asked in the survey year 2018, we only keep participants having children before that year (2018 is not included because we don't know if birth took place before or after taking the survey).
	keep if kidgeb01<2018 & kidgeb01>0 
	


gen kidsex01bis= kidsex01  if kidsex01>0 & kidsex01!=.

 collapse (firstnm) sumkids kidsex01bis, by(pid)
 reg sumkids i.kidsex01bis
 
reg sumkids i.kidsex01bis

      Source |       SS           df       MS      Number of obs   =     2,921
-------------+----------------------------------   F(1, 2919)      =      0.04
       Model |  .056665328         1  .056665328   Prob > F        =    0.8476
    Residual |  4476.03508     2,919  1.53341387   R-squared       =    0.0000
-------------+----------------------------------   Adj R-squared   =   -0.0003
       Total |  4476.09175     2,920  1.53290813   Root MSE        =    1.2383

-------------------------------------------------------------------------------
      sumkids |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
2.kidsex01bis |   .0088329   .0459487     0.19   0.848    -.0812623    .0989281
        _cons |   2.227679   .0312721    71.24   0.000     2.166361    2.288996
-------------------------------------------------------------------------------

 
 