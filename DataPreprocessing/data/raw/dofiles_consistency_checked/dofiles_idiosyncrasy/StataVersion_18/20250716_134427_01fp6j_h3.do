********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

*********
* Setup * 
*********

* clear all
version 18
set maxvar 10000
set seed 8942

* INSTALL PACKAGES *
* net install st0718_1.pkg, from(http://www.stata-journal.com/software/sj24-3/)

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

********************
* Data Preparation * 
********************

* LOAD AND MERGE DATA *

use pid cid syear sex phrf gebjahr using "$IN/ppathl.dta"  , clear // use ppathl as a starting dataset
	
/*gebjahr retrieved from ppathl instead of ppath as planned in the PAP (identical information)*/

* Merge ppathl with datasets

* biobirth contains information on fertility history
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex01 kidgeb01 kidgeb02 kidmon01 kidmon02)
* pgen contains user-friendly data on the individual level that are consolidated from different sources
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgtatzeit pgemplst pgpartz pgisced97 pgpiyear pgmonth)
* pl contains all variables from the individual questionnaire
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plh0301 plh0303 plh0004 pli0043_h)

label language EN // change language of the labels to English
compress
save "$DATA/h1_dataset.dta", replace // save merged dataset


**********************
* GENERATE VARIABLES *
**********************

******************
* Help-Variables *
******************

* Birthdate first child
recode kidgeb01 (-100/-1=.)
recode kidmon01 (-100/-1=.)
gen birthdate_1=mdy(kidmon01, 15, kidgeb01) //day not recorded, set to the 15th of the month
lab var birthdate_1 "Birthdate of first child"

* Birthdate second child
recode kidgeb02 (-100/-1=.)
recode kidmon02 (-100/-1=.)
gen birthdate_2=mdy(kidmon02, 15, kidgeb02) //day not recorded, set to the 15th of the month
lab var birthdate_2 "Birthdate of second child"

* Date of interview
recode pgpiyear (-100/-1=.)
replace pgpiyear=syear if pgpiyear>=. /*not mentioned in PAP, we did not know about the high number of missing*/
recode pgmonth (-100/-1=.)
replace pgmonth=2 if pgmonth>=. /*not mentioned in PAP, we did not know about the high number of missing, February=mode*/

gen intdate =mdy(pgmonth, 15, pgpiyear)
format intdate %td
lab var intdate "Date of interview"


**********************
* Treatment-Variable *
**********************

* Daughter-Variable, Hypotheses 1, 2, 3, 4
gen daughters=1 if intdate-birthdate_1>0 & kidsex01==2 //respondent has a female child
replace daughters=0 if intdate-birthdate_1>0 & kidsex01==1 //respondent has a male child
replace daughters=. if intdate-birthdate_1<=0 // respondent does not (yet) have a child
replace daughters=. if intdate>=. // missing date of interview
replace daughters=. if birthdate_1>=. /*typo in PAP: ">0." --> corrected to ">=." */
lab var daughters "Daughter"
lab define daughterslbl 1 "daughter" 0 "son"
lab value daughters daughterslbl
*tab daughters kidsex01, m



*************
* Moderator *
*************

* Sex of respondent (female-dummy), Hypotheses 3, 4
gen female=1 if sex==2
replace female=0 if sex==1
replace female=. if sex<=0
tab female sex, m
lab var female "female respondent"
lab define femalelbl 1 "female" 0 "male"
lab value female femalelbl
*tab female sex, m

************
* Controls *
************

* Year of birth, Hypotheses 3, 4
recode gebjahr (-100/-1=.), gen(birthyear)
lab var birthyear "Year of birth"

* Education, Hypotheses 3, 4
recode pgisced97 (-100/-1=.) (0 1 2=0 "low") (3 4=1 "middle") (5 6=2 "high"), gen(educ)
lab var educ "Education"
*tab pgisced97 educ , m


************
* Outcomes *
************

/*
* Family versus Career Attitudes, Hypotheses 1,4 
recode plh0301 (-100/-1=.), gen (famvscareer) // no need to recode any further, as high values already indicate gender equality. Incorrect in the preliminary stata code submitted within the PAP.
lab var famvscareer "Gender equality attitudes: family versus career"
lab define attlbl 1 "gender inequality" 4 "gender equality"
lab value famvscareer attlbl
tab famvscareer plh0301, m
	
* Men should invole in Housework Attitudes, , Hypotheses 1,4 
recode plh0303 (-100/-1=.) (4=1) (3=2) (2=3) (1=4), gen(menhousework)
lab var menhousework "Gender equality attitudes: men should involve in housework"
lab value menhousework attlbl
tab menhousework plh0303, m


* Left versus right political ideology, Hypothesis 2	
recode plh0004 (-100/-1=.) (10=0) (9=1) (8=2) (7=3) (6=4) (5=5) (4=6) (3=7) (2=8) (1=9) (0=10), gen(left)	
lab var left "left political ideology"
lab define lftlbl 0 "right" 10 "left"
lab value left lftlbl
tab left plh0004, m
*/

* Working hours, Hypothesis 3
recode pgtatzeit (-100/-1=.), gen(workhours)
replace workhours=0 if pgemplst==5 // not working equals 0 hours
lab var workhours "Working hours"
*tab workhours, m

* Housework, Hypothesis 3
recode pli0043_h (-100/-1=.) (16/24=16), gen(housework) //top coded to 16 hours per day
lab var housework "Hours of housework"
*tab housework, m


************************
*** Filter Variables ***
************************

* Parity 2 Indicator (Defining Analytic Sample), Hypotheses 1, 2, 3, 4
gen parity2=1 if intdate-birthdate_2>0 & intdate-birthdate_2<.
lab var parity2 "Second child is born"
* if parity2=1: a second child is already born

* Cohabitation, Hypothesis 3
gen cohab=1 if pgpartz!=0 & pgpartz!=5
lab var cohab "cohabiting with a partner"


*********************************
*** Apply sample restrictions ***
*********************************

* We estimate the following regressions using a sample restricted to respondents with exactly one child.
* Note: There was a typo in the preliminary Stata code provided in the PAP. Although the verbal description 
* of the sample restriction was correct, the code mistakenly included `parity2==1`, which would select 
* respondents with at least two children—contradicting the intended analytic sample.


keep if parity2!=1 // Hypotheses 1, 2, 3, 4

* Restrict the analyses to cohabiting couples 
keep if cohab==1 // Hypothesis 3

keep if !missing(daughters, female, syear, educ, birthyear, phrf) & phrf!=0
keep if !missing(workhours) | !missing(housework)

count

*********************************
*********************************

	 *HYPOTHESIS TEST FOR H3 

 	*****************************************************************************************************************************************************************
	*  We assume that gender-specific causal effects of having a daughter are identified wihin strata defined by gender [female], cohort [birthyear] and education [educ] 
	* in a sample of parents of a single child (see PAP)
	* !! NOTE THIS IS THE AVERAGE CAUSAL EFFECT IN THE POPULATION OF PARENTS OF A SINGLE CHILD !!
	* We further assume that hours spent on labor [pgtatzeit] and housework [pli0043_h] are suitable behavioral measures related to gender equality,
	* at least among people living with a partner (hypothesis 3): 
	* More time spent on labor and less time spent on housework is more likely to reflect gender equal behavior for women;
	* less time spent on labor and more time spent on housework is more likely to reflect gender equal behavior for men.
	* Accordingly, we run a seemingly unrelated linear regression analysis with workhours [pgtatzeit] and housework [pli0043_h] as dependent variables
	* and the dummy variable daughter as independent variable in a sample of respondents with a single child (parity2!=1) living with a partner (cohab==1)
	* We apply weights cross-sectionally calibrated weights (phrf)
	* To account for non-independence of observatons, we cluster standard errors at the household level
	* The null we test against is that the effect of having daughters is >=0 on workours for men and on housework for women and
	* <=0 on workhours for men and on housework for women.
	* I.e., we have to consider a one-sided test adjusted for multiple testing
	* In practice, we multiply the first to estimates by -1 and test all against >=0
	* Since the used method provides a joint confidence band, this is equal to assessing whether there is at least 1 significant effect
	* We report all coefficients and statistics but only export the result with the smallest p-value to comply to the provided template
	* !! NOTE THAT AT AN A PRIORI CONFIDENCE LEVEL OF 0.05 WITHOUT PREDERMINED SIGN OF THE EFFECT, THE CONFIDENCE LEVEL BECOMES 0.025
	*****************************************************************************************************************************************************************
	* The below code follows the conception outlined in the PAP
	* There is one deviation, which reflects perceived risk (2) already anticipated in the PAP:
	* as full stratification was not possible, we had to impose some constraints
	* We decided to only include interactions of daughters x female and all pairwise and triple interactions of these with the additional variables in the model
	* We refrained from more refined methods to determine the functional form on a 5% subsample.
	* We considered switching to a matching or IPW-weighting based approach but abandonded the idea in compliance with out PAP - but note that our approach would
	* call for sensitivity analysis w.r.t. to functional form assumptions when applied to the full estimation sample.
	* Note that the sample restriction is already implemented in the data preparation and therefore omitted for redundancy
	*****************************************************************************************************************************************************************

	*obtain point esttimates for daughter effects on both outcomes
	regress workhours (daughters##female)##(syear educ c.birthyear) [iw=phrf]
	est sto e1
	regress housework (daughters##female)##(syear educ c.birthyear) [iw=phrf]
	est sto e2
	
		*for later reporting [N, df]
		regress workhours (daughters##female)##(syear educ c.birthyear) [pw=phrf], vce(cluster cid)
		est sto e1w
		regress housework (daughters##female)##(syear educ c.birthyear) [pw=phrf], vce(cluster cid)
		est sto e2w

	*obtain standard errors accounting for correlated errors of outcomes and dependence of observations
	suest e1 e2, vce(cluster cid)
	
	*obtain conditional marginal effect 
	margins, dydx(daughters) predict(equation(e1_mean)) predict(equation(e2_mean)) at(female=(0 1) (mean) syear educ birthyear) post
	// equivalent to running above regression with sample-demeaned covariates
	
	*flip signs to align direction of tes
	nlcom (-_b[1.daughters:1._predict#1._at]) (_b[1.daughters:1._predict#2._at]) (_b[1.daughters:2._predict#1._at]) (-_b[1.daughters:2._predict#2._at]), post
	
	*obtain inference statistics accounting for one-sided multiple testing
	sotable, alternative(upper)
	
	*post-processing output
	mat r = r(results)									// full results table
	mata: st_matrix("R", sort(st_matrix("r")',4))		// this sorts results accoring to pvalues (lowest first) 
	mat colnames R = b se z pvalues low up 
	mata: st_matrix("O",order(st_matrix("r")',4)')
	tokenize "workhours:male workhours:female housework:male housework:female"
	local rn  "``=O[1,1]''  ``=O[1,2]'' ``=O[1,3]'' ``=O[1,4]''" 
	mat rownames R =  `rn'
	mat colnames R = b se z P>|z| low up

	local eq : roweq(R)
	local eq = word("`eq'",1)
	if "`eq'"=="workhours" est resto e1w
	if "`eq'"=="housework" est resto e2w

		qui {
* EXTRACT AND EXPORT RESULTS 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = R[1,1]

		* (ii) standard error
		local se = R[1,2]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = R[1,4]

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar sign_daughter = sign(R[1,1])
		local interpretation "positive coefficient is in line with hypothesis"


		* (ix) evaluation of H1
		* ADDED; NOT EXPORTED
		scalar sign_cb = sign(R[1,5])
		if sign_cb ==1 {
			local evaluation "reject H0"
		} 
		else {
			local evaluation "cannot reject H0"
		}

	// Display the saved statistics
	
	noi di "{hline}"
	noi di "RESULTS"
	noi di "Seemingly unrelated linear regression of {it: workhours & housework} on {it:daughters}"
	noi di "P-value and lower CI band adjusted for multiple one-sided testing"
	noi di "{hline}"
	noi matlist R[1...,1..5], format(%6.3f)
	noi di "{hline}"
	noi di ""
	noi di "{it:Exported statistics for outcome with highest z / lowest p}
	noi di ""
	noi di "Coefficient: `coefficient'"
	noi di "Standard Error: `se'"
	noi di "Test Statistic Family: `test_stat_family'"
	noi di "Residual Degrees of Freedom: `df_residual'"
	noi di "Model Degrees of Freedom: `df_model'"
	noi di "p-value: " `p_value'
	noi di "Number of observations: " `num_obs'
	noi di	"Number of participants: " `num_participants'
	noi di "Interpretation of directionality: " "`interpretation'"
	noi di "*Statistical assessment on H1: " "`evaluation'"
	noi di "{hline}"
	}
	
	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit

********************************************************************************
	


