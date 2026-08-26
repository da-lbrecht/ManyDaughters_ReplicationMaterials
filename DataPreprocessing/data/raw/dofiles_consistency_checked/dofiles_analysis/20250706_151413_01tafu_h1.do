********************************************************************************
****************** Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

/*H1. Having daughters leads to stronger support for gender equality (as measured by attitudes).
That is, denoting X as a measure of support for gender equality,
(X for respondents with daughters) > (X for respondents without daughters).
*/

* ============================
* Setup * 
* ============================

* clear all
version 18.5 // version of stata we are using 
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
ssc install outreg2
ssc install revrs
ssc install fre

* CONFIGURE FILE PATHS * 
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
	keep cid pid hid syear parid partner sex gebjahr 
	
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) 
	
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keepusing(plh030* pmonin) keep(master matches)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

	*First we needed to determine which attitude variables would be part of the attitude-scale (based on cronbachs alpha); this we checked on the full sample before following any excludion criteria. See the procedure below that is commented out (line 67-110). After checking which variables should be included, we followed the same procedure starting on line 172 - but this time using the final analytical sample.
	
	 use "$DATA/h1_dataset.dta", clear //use full sample to calculate cronbachs alpha

	/*
	keep if syear ==2012 //items asked in 2012 survey

	//replace missings with "." (negative values refer to missings)
	//reverse code some variables (plh0303 plh0304 plh0308_v1 plh0309) to make higher score more gender equal
		 
		*Women Should Rather Care About Family Than Career (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu) 
		replace plh0301 = . if plh0301<0 

		*Kind unter 3 J. leidet wenn Mutter arbeitet (FID) [2012] (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu)
		replace plh0302_v1 = . if plh0302_v1 <0

		*Men Involved in Housework (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu)
		replace plh0303 = . if plh0303 <0
			revrs plh0303 
			drop plh0303
			 
		*Children Suffer When Father Focused On Career //(1 Stimme voll zu /4 Stimme ueberhaupt nicht zu)
		replace plh0304 = . if plh0304 <0 
			revrs plh0304 
			drop plh0304 

		*Men Care more About Financial Security (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu)
		replace plh0307 = . if plh0307 <0 

		*Mann/Frau in gleich in Arbeit u. Haushalt/Familie (FID) [2012] (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu) 
		replace plh0308_v1 = . if plh0308_v1 <0
			revrs plh0308_v1 
			drop plh0308_v1

		*Working Mothers Equal Emotional (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu)
		replace plh0309 = . if plh0309 <0
			revrs plh0309 
			drop plh0309
				
	* Check Cronbachs alpha >= 0.7
	factor plh0301 plh0302_v1 revplh0303 revplh0304 plh0307 revplh0308_v1 revplh0309  //Factor1 eigenvalue 1.9, suggested factor 1.
	alpha  plh0301 plh0302_v1 revplh0303 revplh0304 plh0307 revplh0308_v1 revplh0309, item 

	*Item 304 does not fit (also loads onto factor3 on its own, and is very unique)
	*Alpha currently 0.67
	*If remove item plh0304 (Children Suffer When Father Focused On Career), new alpha 0.72
	alpha plh0301 plh0302_v1 revplh0303 plh0307 revplh0308_v1 revplh0309, item //new alpha: 0.72
	*/


* FILTERS * 

	*Only keep participants who:
	*(1) have children
	*(2) have one child only (drop observations once second child arrives). Twins counted as 'having daughter' if at least both are girls.
	*(3) use last year of panel where we have full information 

	use "$DATA/h1_dataset.dta", clear 
	
		*(1a) Keep only participants who have children:
		fre kidgeb01
		keep if kidgeb01 != -2  //these persons never have children. 
		drop if kidgeb01 < 0 //Also drop [-1] No Answer [-2] Does not apply [-3] Answer improbable
		

		*Drop years before child1 was born
		drop if syear < kidgeb01 


		*(1b) Keep only if sex of child is known 
		drop if kidsex01 < 0 // missings 

	
		*(2) have one child only (dropped once second child arrives). Except if twins, then counted as 'having daughter' if both of them are girls 

		gen twins = 1 if kidgeb01==kidgeb02 & kidmon01==kidmon02 & kidgeb02 != -2 //month and year of birth same, and second child present. 
		drop if twins==1 & kidsex02 < 0 // if 2nd twins gender unknown 
		
		drop if syear > kidgeb02 & twins !=1  //drop observations (surveyyears) once 2nd child is born (except first child is twin)
		drop if syear > kidgeb03 & twins ==1 //drop observations (surveyyears) once 3rd child is born (if first child is twin)
		

		*(3) items only asked in the 2012 survey
		keep if syear ==2012 
		
		
* GENERATE VARIABLES * 

	*generate dummy variable for participants sex
	fre sex
	gen d_sex = sex-1
	fre d_sex
	label variable d_sex "Sex of participants (0=Male, 1=Female)"

	*Generate dummy variable for whether participant`s child is a girl (1) or boy (0)
	gen d_daughter = 1 if kidsex01== 2 //girl
	replace d_daughter=0 if kidsex01==1 //boy	
	drop if twins==1 & d_daughter==0 & kidsex02==2 //drop if first child is boy, but second twin is girl 
	drop if twins==1 & d_daughter==1 & kidsex02==1 //drop if first child is girl, but second twin is boy 
	label variable d_daughter "Sex of first child (0=son, 1=daughter)"
	fre d_daughter
	
	*Select gender attitude items for scale construction 
	
	*Based on previous test of cronbachs alpha on the whole sample, keep only relevant 'gender attitude items'
	*replace missings with "." (negative values refer to missings)
	*reverse code some variables (plh0303 plh0304 plh0308_v1 plh0309) to make higher score refer to more gender equal
	 
		*Women Should Rather Care About Family Than Career (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu) 
		replace plh0301 = . if plh0301<0 
		
		*Kind unter 3 J. leidet wenn Mutter arbeitet (FID) [2012] (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu)
		replace plh0302_v1 = . if plh0302_v1 <0

		*Men Involved in Housework (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu)
		replace plh0303 = . if plh0303 <0
		revrs plh0303 
		drop plh0303
					 
		*Men Care more About Financial Security (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu)
		replace plh0307 = . if plh0307 <0 

		*Mann/Frau in gleich in Arbeit u. Haushalt/Familie (FID) [2012] (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu) 
		replace plh0308_v1 = . if plh0308_v1 <0
		revrs plh0308_v1 
		drop plh0308_v1

		*Working Mothers Equal Emotional (1 Stimme voll zu /4 Stimme ueberhaupt nicht zu)
		replace plh0309 = . if plh0309 <0
		revrs plh0309 
		drop plh0309

		
	*Construct the gender attitude scale that will be our dependent variable:

		*Take the mean of all items. At least 4 items need to be answered
		egen numnomissing = rownonmiss(plh0301 plh0302_v1 revplh0303 plh0307 revplh0308_v1 revplh0309) //how many items present (not missing)
		egen attitude_scale = rowmean(plh0301 plh0302_v1 revplh0303 plh0307 revplh0308_v1 revplh0309) if numnomissing >=4
		sum attitude_scale 

		*Convert to scale: 0-1
		gen attitude_scale_01 = (attitude_scale - r(min)) / (r(max) - r(min))



* HYPOTHESIS TEST FOR H1 * 

	* We assume that a scale composing of various items in support for gender equality (attitude_scale_01) is a good measure of support for gender equality (hypothesis 1), 
	* if having a daughter, the respondent more strongly supports gender equality (has a higher score on the 0-1 scale).
	* Accordingly, we run a linear OLS regression analysis with `attitude_scale_01' as the dependent variable and the dummy variable `d_daughter' as the independent variable


reg attitude_scale_01 d_daughter, cluster(hid)


* EXTRACT AND EXPORT RESULTS * 

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

		* (viii) interpretation based of directionality and p-value
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
