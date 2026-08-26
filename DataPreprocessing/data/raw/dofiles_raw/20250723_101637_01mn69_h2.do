********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 15.1 
*set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install reghdfe, replace
ssc install ftools


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

	*Given limits on number of variables, create subset of pl dataset
	use  pid syear plh0004 using "$IN/pl.dta", clear
	save "$IN/pl_subset.dta", replace
	
	*use ppathl as a starting dataset
	use "$IN/ppathl.dta", clear 
	keep pid syear hid sex gebjahr parid partner
	
	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hgnuts1 hghinc)
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pid syear pgemplst pgpsbil)
	merge 1:1 pid syear using "$IN/pl_subset.dta", nogen keep(master matches) keepusing(pid syear plh0004)
	
	* Merge all biobirth ( contains information on fertility history)
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)

	label language EN 


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)

*Drop if not registered partner:
keep if partner == 1 | partner == 2

*Drop if outcome variables missing:
drop if missing(plh0004)
keep if plh0004 >= 0
 
* GENERATE VARIABLES * (Please adapt according to your needs)

*Dummy variable for whether participant has at least one daughter (1) or not (0)
	gen daughter = 0

	forvalues i = 1/9 {
		replace daughter = 1 if kidgeb0`i' <= syear & kidsex0`i' == 2  & kidgeb0`i' > 0
	}

	forvalues i = 10/19 {
		replace daughter = 1 if kidgeb`i' <= syear & kidsex`i' == 2  & kidgeb`i' > 0
	}
	
	gen n_children = 0

forvalues i = 1/9 {
    replace n_children = n_children + 1 if kidgeb0`i' <= syear & kidgeb0`i' > 0
}

forvalues i = 10/19 {
    replace n_children = n_children + 1 if kidgeb`i' <= syear & kidgeb`i' > 0
}

	
	
*Respondents age
	gen age = syear - gebjahr

*Drop if less than 18. 
	drop if age <18

*Generate main DV:
	gen left = 10 - plh0004


*Select variables of interest:
keep pid parid daughter n_children sex age left pgemplst pgpsbil hgnuts1 hghinc syear

*Save dataset
save "$DATA/h2_dataset.dta", replace

*Drop pid
keep parid daughter n_children sex age left pgemplst pgpsbil hgnuts1 hghinc syear

*Rename parid to pid (for matching)
rename parid pid

*Keep only men:
keep if sex == 1

*Rename and drop other variables:
keep pid daughter n_children age left pgemplst pgpsbil hgnuts1 hghinc syear
rename daughter daughter_M
rename n_children n_children_M
rename age age_M
rename left left_M
rename pgemplst pgemplst_M
rename pgpsbil pgpsbil_M
rename hgnuts1 hgnuts1_M
rename hghinc hghinc_M

*Merge with previous dataset: 
merge 1:1 pid syear using "$DATA/h2_dataset.dta"
keep if _merge == 3
drop _merge

*Rename variables again
keep if sex == 2
keep age_M daughter_M n_children_M pgemplst_M hghinc_M pgpsbil_M hgnuts1_M left_M age daughter left pgemplst n_children hghinc pgpsbil hgnuts1 pid syear
rename daughter daughter_F
rename n_children n_children_F
rename age age_F
rename left left_F
rename pgemplst pgemplst_F
rename pgpsbil pgpsbil_F
rename hgnuts1 hgnuts1_F
rename hghinc hghinc_F

*Keep if same info on shared variables: 
keep if hgnuts1_M == hgnuts1_F
keep if daughter_M == daughter_F
keep if n_children_M == n_children_F

*Define household variables and drop variables:

*Income
keep if hghinc_M > 0 & hghinc_F > 0
gen Income = (hghinc_M + hghinc_F) / 2
gen log_Income = log(Income*12)
drop hghinc_M hghinc_F Income

*Daughter
rename daughter_M daughter
drop daughter_F

*n_children_M:
rename n_children_M n_children
drop n_children_F

*Attitudes:
gen left = (left_F + left_M) / 2
drop left_F left_M

*Age: 
gen age = (age_F + age_M) / 2
drop age_F age_M

*Drop if employment is missing:
drop if pgemplst_M < 0
drop if pgemplst_F < 0

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	* As an example, and for illustrative purposes we consider a: linear OLS regression

	* For illustrative purposes, let's further assume that having a job that requires hard manual labor (plb0085) is a good measure of support for gender equality (hypothesis 1), 
	* if having a job with less manual labor, the respondent is more likely to support gender equality.
	* Accordingly, we run a linear OLS regression analysis with plb0085 as dependent variable and the dummy variable d_daughter as independent variable

	reghdfe left daughter c.age##c.age i.pgemplst_M i.pgemplst_F log_Income n_children, absorb(pid syear) vce(cluster pid)




* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[daughter]

		* (ii) standard error
		local se = _se[daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[daughter] / _se[daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[daughter]
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
