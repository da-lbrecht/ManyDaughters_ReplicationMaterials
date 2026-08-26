********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

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


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Example: generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* For illustrative purposes, we will generate the variable randomly:
	
	gen d_daughter =0 
forvalues i=1(1)9{
	replace d_daughter = 1 if kidsex0`i'==2
}
forvalues i=10(1)19{
	replace d_daughter = 1 if kidsex`i'==2
}	


tab1 plh0301 plh0303 plh0307 plh0308_v1 plh0308_v2

ta plh0004 // political view
	
* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	* As an example, and for illustrative purposes we consider a: linear OLS regression

	* For illustrative purposes, let's further assume that having a job that requires hard manual labor (plb0085) is a good measure of political attitudes (hypothesis 2),
	* if having a job with less manual labor, the respondent is more likely to have left-leaning political attitudes.
	* Accordingly, we run a linear OLS regression analysis with plb0085 as dependent variable and the dummy variable d_daughter as independent variable

	
	
qui g politicalview=plh0004	
qui replace politicalview=. if plh0004<0
	
.........................................
//Gender: females
//................................................................................................................................
qui g female=1 if sex==2
qui replace female=0 if sex==1


.........................................
//Age: 
//................................................................................................................................
qui g age=bioage 
qui replace age=. if bioage<0

qui g age2=age*age





//Employment Status:
//................................................................................................................................
qui g emp_stat= pgemplst
qui replace emp_stat=. if pgemplst<0


//Marital Status
//................................................................................................................................
qui gen mar_stat=pgfamstd
qui replace mar_stat=. if pgfamstd<0


.........................................
//Education using ISCED-2011-Classification
//................................................................................................................................

 qui g education= pgisced11 
 qui replace education=. if pgisced11<0
 
 
.........................................
// Labour income net
......................................... 
qui g wageinc= pglabnet
qui replace wageinc=. if wageinc<0
qui replace wageinc=1 if wageinc==0
qui g lwage_i=log(wageinc)


.........................................	

.........................................	

//................................................................................................................................
//Big 5 personality traits inventory
//................................................................................................................................

qui replace  plh0212=. if  plh0212<0 //1
qui replace  plh0213=. if  plh0213<0 //2
qui replace  plh0214=. if  plh0214<0 //3
qui replace  plh0215=. if  plh0215<0 //4
qui replace  plh0216=. if  plh0216<0 //5
qui replace  plh0217=. if  plh0217<0 //6
qui replace  plh0218=. if  plh0218<0 //7
qui replace  plh0219=. if  plh0219<0 //8
qui replace  plh0220=. if  plh0220<0 //9
qui replace  plh0221=. if  plh0221<0 //10
qui replace  plh0222=. if  plh0222<0 //11
qui replace  plh0223=. if  plh0223<0 //12
qui replace  plh0224=. if  plh0224<0 //13
qui replace  plh0225=. if  plh0225<0 //14
qui replace  plh0226=. if  plh0226<0 //15
qui recode  plh0214 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
qui recode  plh0223 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
qui recode  plh0218 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
qui recode  plh0226 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)


qui gen conscientious5=plh0212+ plh0218+ plh0222 if syear==2005
qui gen extraversion5= plh0213+ plh0219+ plh0223 if syear==2005
qui gen agreeableness5=plh0214+ plh0217+ plh0224 if syear==2005
qui gen openness5=     plh0215+ plh0220+ plh0225 if syear==2005
qui gen neuriticism5=  plh0216+ plh0221+ plh0226 if syear==2005

qui gen conscientious7=plh0212+ plh0218+ plh0222 if syear==2009
qui gen extraversion7= plh0213+ plh0219+ plh0223 if syear==2009
qui gen agreeableness7=plh0214+ plh0217+ plh0224 if syear==2009
qui gen openness7=     plh0215+ plh0220+ plh0225 if syear==2009
qui gen neuriticism7=  plh0216+ plh0221+ plh0226 if syear==2009

qui gen conscientious13=plh0212+ plh0218+ plh0222 if syear==2013
qui gen extraversion13= plh0213+ plh0219+ plh0223 if syear==2013
qui gen agreeableness13=plh0214+ plh0217+ plh0224 if syear==2013
qui gen openness13=     plh0215+ plh0220+ plh0225 if syear==2013
qui gen neuriticism13=  plh0216+ plh0221+ plh0226 if syear==2013

qui gen conscientious17=plh0212+ plh0218+ plh0222 if syear==2017
qui gen extraversion17= plh0213+ plh0219+ plh0223 if syear==2017
qui gen agreeableness17=plh0214+ plh0217+ plh0224 if syear==2017
qui gen openness17=     plh0215+ plh0220+ plh0225 if syear==2017
qui gen neuriticism17=  plh0216+ plh0221+ plh0226 if syear==2017

qui gen conscientious19=plh0212+ plh0218+ plh0222 if syear==2019
qui gen extraversion19= plh0213+ plh0219+ plh0223 if syear==2019
qui gen agreeableness19=plh0214+ plh0217+ plh0224 if syear==2019
qui gen openness19=     plh0215+ plh0220+ plh0225 if syear==2019
qui gen neuriticism19=  plh0216+ plh0221+ plh0226 if syear==2019
	
	
qui egen conscientious= rowmean(conscientious5 conscientious7 conscientious13 conscientious17 conscientious19)
qui egen extraversion= rowmean(extraversion5 extraversion7 extraversion13 extraversion17 extraversion19)
qui egen agreeableness= rowmean(agreeableness5 agreeableness7 agreeableness13 agreeableness17 agreeableness19)
qui egen openness= rowmean(openness5 openness7 openness13 openness17 openness19)
qui egen neuriticism= rowmean(neuriticism5 neuriticism7 neuriticism13 neuriticism17 neuriticism19)
		
......................................................................

	
/*	
xi: reg politicalview d_daughter female age age2 lwage_i i.mar_stat i.education i.emp_stat i.migback i.syear conscientious extraversion agreeableness openness neuriticism  , r
*/


xi: reg politicalview d_daughter female age age2 lwage_i i.mar_stat i.education i.emp_stat , r



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
		local interpretation "negative coefficient is in line with hypothesis"


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
	file write myfile "coefficient, se, family, residual_df, model_df, p_value, num_obs, num_participants, interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value', `num_obs', `num_participants',`interpretation'" _n
	file close myfile



********************************************************************************

exit
