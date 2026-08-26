********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 19.5 // indicate the version of stata you are using here
*set maxvar 10000 // extend the maximum number of variables to 10,000

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

* LOAD AND MERGE DATA * (Please adapt according to your needs)
use pid syear plh0303 pli0043_h   plh0012_h  plh0013_h   using "$IN/pl.dta", clear
label language EN
save "$IN/pl short.dta", replace

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex*)
	merge 1:1 pid syear using "$IN/pl short.dta", nogen keep(master matches) 
	merge 1:1 pid syear using "$IN/pl short.dta", nogen keep(master matches) 
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hgsize)
	
	label language EN // change language of the labels to English
	keep pid hid syear kidsex* plh0012_h plh0013_h plh0303 pli0043_h hgsize
	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================
* GENERATE VARIABLES * (Please adapt according to your needs)

	*Create variables
	egen haskids=rowmax( kidsex*)
	gen d_daughter=1 if haskids==2
	replace d_daughter=0 if d_daughter==.
	replace haskids=1 if haskids>0
	*Behavioral support
	gen behaviorsupport=pli0043_h  if pli0043_h >-.5
	*Size of household 
	replace hgsize=. if hgsize==-1
	sum hgsize, meanonly 
	replace hgsize=r(max)-hgsize
* FILTERS * (Please adapt according to your needs)
 
	keep if haskids==1

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)

	* Please add a verbal description of your model here
/*Same as Hypothesis 1, but using a Normal distribution regression model. */

	reg behaviorsupport d_daughter##c.hgsize

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

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
		scalar coef_d_daughter = _b[1.d_daughter]
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
