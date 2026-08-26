********************************************************************************
****************** Do-File H1: ManyDaughters Project – Larissa *****************
********************************************************************************

* Author: [REDACTED]

* ============================
* Setup
* ============================

* clear all
version 14
set maxvar 10000

* INSTALL PACKAGES

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
* Data Preparation
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

	save "$DATA/h1_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* FILTERS * 
 
	keep if syear == 2019
	gen age = syear - gebjahr
	keep if age >= 18 & age <= 60
	keep if sumkids >= 1
	keep if sex > 0

* GENERATE VARIABLES * 

	* Has at least one daughter
	gen hasdaughter = 0
	replace hasdaughter = 1 if kidsex01 == 2 | kidsex02 == 2 | kidsex03 == 2 | ///
    kidsex04 == 2 | kidsex05 == 2 | kidsex06 == 2 | kidsex07 == 2 | ///
    kidsex08 == 2 | kidsex09 == 2 | kidsex10 == 2 | kidsex11 == 2 | ///
    kidsex12 == 2 | kidsex13 == 2 | kidsex14 == 2 | kidsex16 == 2 | ///
    kidsex17 == 2 | kidsex18 == 2 | kidsex19 == 2
	
		
	* The first kid is a daughter
	gen first_daughter = .
	replace first_daughter = 1 if kidsex01 == 2
	replace first_daughter = 0 if kidsex01 == 1


	* Education
	rename pgisced97 educ_aux
	gen educ = .
	replace educ = 1 if inlist(educ_aux, 0, 1, 2)
	replace educ = 2 if educ_aux == 3
	replace educ = 3 if educ_aux == 4
	replace educ = 4 if educ_aux == 5
	replace educ = 5 if educ_aux == 6
	label define educlabel 1 "low" 2 "middle voc" 3 "voc + ab" 4 "higher voc" 5 "higher educ"
	label values educ educlabel

	* Income
	gen hhnetincome = hghinc if hghinc >= 0
	gen log_hhnetincome = ln(hhnetincome)

	* Gender attitudes index
	foreach v in plh0395i01 plh0395i02 plh0395i03 plh0395i04 plh0395i05 plh0395i06 {
		replace `v' = . if `v' <= 0
	}
	*Checking the combrach's alpha to see if one dimention is enough 
	alpha plh0395i01 plh0395i02 plh0395i03 plh0395i04 plh0395i05 plh0395i06
	factor plh0395i01 plh0395i02 plh0395i03 plh0395i04 plh0395i05 plh0395i06
	*I'm keeping only the first factor, since is the only one with einigenvalues greater than 1.
	gen genderatt = plh0395i01 + plh0395i02 + plh0395i03 + plh0395i04 + plh0395i05 + plh0395i06


* HYPOTHESIS TEST FOR H1 *


	* IV two-stage least squares (2SLS)

	* The hypothesis is that having daughters (hasdaughter) leads to stronger support for gender equality measured by attides (genderatt).
	* I was asked to do a causal inference, so not a regular correlation analysis.
	* Accordingly, I use as a instrument a variable that affects whether someone has a daughter but does not directly affect gender attitudes.
	* This variable here is  the sex of the first child since whether the first child is a girl is random.
	* The IV estimator is a  two-stage least square regression analysis with genderatt as dependent variable and the dummy variable hasdaughter as independent variable.
	


	* Linear regression model
	*reg genderatt hasdaughter [aweight=phrf]
	*reg genderatt hasdaughter i.educ age i.sex [aweight=phrf]

	* Intrument variable regression usinf the gender of the first kid as a instrument 

	ivregress 2sls genderatt (hasdaughter = first_daughter) i.educ age i.sex log_hhnetincome [aweight=phrf]



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics
		* (i) coefficient estimate
		local coefficient = _b[hasdaughter]

		* (ii) standard error
		local se = _se[hasdaughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)
					
		* (v) p-value
		matrix M = r(table)
		local col = colnumb(M, "hasdaughter")
		local p_value = M[4, `col']

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[hasdaughter]
		local interpretation "positive coefficient is in line with hypothesis"
		
	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: `p_value'"
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"



	* Export results
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile

********************************************************************************

exit
