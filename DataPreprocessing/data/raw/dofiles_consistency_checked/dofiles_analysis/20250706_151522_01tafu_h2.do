********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

/*H2. Having daughters leads to more left-leaning political preferences.
That is, denoting Y as a measure of left-leaning political preferences,
(Y for respondents with daughters) > (Y for respondents without daughters).
*/
* ============================
* Setup * 
* ============================

* clear all
version 18.5 // version of stata we are using 
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * [indicate all packages we make use of???]
* install all packages you need here, for example:
ssc install outreg2
ssc install revrs
ssc install fre

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
	keep cid pid hid syear parid partner sex gebjahr  
	
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) 

	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keepusing(plh0004 pmonin) keep(master matches)
	
	label language EN // change language of the labels to English

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h2_dataset.dta", clear 

* FILTERS * 

	*Only keep participants who:
	*(1) have children
	*(2) have one child only (drop observations once second child arrives). Twins counted as 'having daughter' if at least both are girls.
	*(3) use last year of panel where we have full information 

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
	

	
	*H2-specific variable(s) and sample selection: Left-leaning political preferences 

		*Reverse code political preferences variable: 
		 gen leftist = plh0004  
		 replace leftist = . if leftist<0
		 revrs leftist
		 replace revleftist = revleftist-1
		 fre revleftist plh0004
		 label variable revleftist "Political attitude 0=very right / 10=very left"
		 drop leftist
		 keep if !missing(revleftist)
	
		*Take latest survey year available
		bysort pid (syear): gen n = _n 
		bysort pid: gen N = _N 
		keep if n==N



* HYPOTHESIS TEST FOR H2 *

	* We assume that a scale ranging from very right to very left (attitude_scale_01) is a good measure of right vs. left-leaning political preferences (hypothesis 2), 
	* if having a daughter, the respondent is more likely to have left-leaning political preferences (score higher on the leftist scale).
	* Accordingly, we run a linear OLS regression analysis with `revleftist' as the dependent variable and the dummy variable `d_daughter' as the independent variable
	
reg revleftist d_daughter  
reg revleftist d_daughter, cluster(hid) 



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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
