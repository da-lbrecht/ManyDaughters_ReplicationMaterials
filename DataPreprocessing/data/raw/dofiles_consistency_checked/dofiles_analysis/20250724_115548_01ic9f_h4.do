********************************************************************************
****************** Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 17 // indicate the version of stata you are using here
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

***
* Prepare the biobirth data set so that it contains only individuals with only boys or only girls


	use "$IN/biobirth.dta", clear // use biobirth as a starting dataset
	
*drop individuals with missing information on at least one of their children's sex
	forvalues x = 1/19 {
		local varname = cond(`x' < 10, "kidsex0`x'", "kidsex`x'")
		drop if `varname' == -1
	}

* recode missing values for kidsex variables
	forvalues x = 1/19 {
		local varname = cond(`x' < 10, "kidsex0`x'", "kidsex`x'")
		replace `varname' = . if `varname' == -2
	}

* preparation for treatment indicator: only girls (vs. only boys) 	
	egen maxsex = rowmax(kidsex??)	
	egen minsex = rowmin(kidsex??)	

* drop individuals without children
	drop if maxsex == .
* drop individuals with children of different sex
	drop if maxsex != minsex
* drop individuals with unknown birth year of the first child
	keep if kidgeb01 > 0 & kidgeb01 < .

* generate the treatment indicator: only girls (vs. only boys)
	gen 	d_daughter = 1 if maxsex == 2
	replace d_daughter = 0 if maxsex == 1

* keep only relevant variables
	keep pid sex gebjahr kidpnr01 kidsex01 kidgeb01 d_daughter	
	save "$DATA/temp_biobirth.dta", replace // save dataset


*********
* LOAD AND MERGE DATA * 
	use pid syear plh0308_v2 using "$IN/pl.dta", clear // pl contains all variables from the individual questionnaire
	
	merge 1:1 pid syear using "$IN/ppathl.dta", nogen keep(master matches) keepusing(migback phrf) // ppathl contains background characteristics

	merge m:1 pid using "$DATA/temp_biobirth.dta", nogen keep(matches) // this is the above prepared dataset
	
	label language EN // change language of the labels to English

	
****
* APPLY SAMPLE RESTRICTIONS
	drop if (plh0308_v2 < 0) | (plh0308_v2 == .) // drop individuals with missing values on the outcome variable
	keep if syear == 2018 /*outcome was only measured in this year; this restriction should not be binding as we already dropped all individuals with missing values on the outcome */
	drop if (gebjahr < 0) | (gebjahr == .) // drop individuals with missing birth year info
	keep if inlist(sex, 1, 2) // drop individuals with missing gender info

	keep if syear > kidgeb01 // keep only individuals who answered the question for the outcome variable when the first child was already born
	keep if syear < kidgeb01 + 18 // keep only individuals who answered the question for the outcome variable when the first child was 18 years or younger
	
	drop if migback < 0 | migback == .
	drop if kidpnr01 < 0 | kidpnr01 == .  
	drop if phrf < 0 | phrf == .
	
	
	
	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* HYPOTHESIS TEST FOR H4 * 
/*To test H4, we will estimate the following linear regression model with plh0308_v2 as outcome:

y_1i=α+β_1 d_i+β_2 m_i+δd_i×m_i   +X_1i γ_1+X_2i γ_2+x_3i γ_3+ε_i              (4)

y_1i : Support for gender equality (outcome variable)
d_i: Dummy indicating whether parent i has only biological daughters
m_i: Dummy indicating whether parent i is male
d_i×m_i: Interaction term equal to 1 if parent i is male and has only biological daughters; 0 otherwise
X_1i: Vector of fixed effects for the birth year of the first child
X_2i: Vector of fixed effects for the parent's birth year
x_3i: Indicator for whether the parent has a migration background
ε_i: Error term
*/

	* GENERATE FURTHER VARIABLES *
gen mh = inrange(migback,2,3)
gen male = (sex == 1)
gen d_daughter_male = d_daughter * male
	
* HYPOTHESIS TEST FOR H4 * 
regress plh0308_v2 d_daughter d_daughter_male male i.kidgeb01 i.gebjahr mh [pweight=phrf], vce(cluster kidpnr01)





* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter_male]

		* (ii) standard error
		local se = _se[d_daughter_male]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter_male] / _se[d_daughter_male]
		local p_value = ttail(e(df_r), abs(`p_value')) /*p-value for one-sided test as stated in PAP*/

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter_male = _b[d_daughter_male]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
