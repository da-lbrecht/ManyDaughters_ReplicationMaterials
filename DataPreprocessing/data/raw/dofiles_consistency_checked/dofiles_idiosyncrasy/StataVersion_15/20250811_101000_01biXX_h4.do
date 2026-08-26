********************************************************************************
****************** H4 Analysis: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* clear all
version 15 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)

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

	save "$DATA/h4_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Generate dependent variable: Gender Equality Attitudes composite index (same as H1)
	display "Creating gender equality attitudes composite index..."
	
	* Variables measuring attitudes toward women career aspiration (plh0301)
	gen wom_care_fam = plh0301
	recode wom_care_fam (-8 -5 -1 = .)
	
	* Variables measuring attitudes toward men care on financial security (plh0307)
	gen men_fin_sec = plh0307
	recode men_fin_sec (-8 -5 -1 = .)
	
	* Reverse coding to ensure higher values indicate stronger support for gender equality
	recode wom_care_fam (1=4) (2=3) (3=2) (4=1)
	recode men_fin_sec (1=4) (2=3) (3=2) (4=1)
	
	* Keep only cases with non-missing data on both attitude variables
	keep if !missing(men_fin_sec) & !missing(wom_care_fam)
	
	* Standardize individual attitude variables (z-scores)
	egen wom_care_fam_std = std(wom_care_fam)
	egen men_fin_sec_std = std(men_fin_sec)
	
	* Create composite index using averaging
	egen gender_2item = rowmean(wom_care_fam_std men_fin_sec_std)
	egen gender_equality_attitudes = std(gender_2item)
	
	* Generate independent variables for H4
	display "Creating daughters and gender interaction variables..."
	
	* Count total number of children (non-missing kidsex values)
	egen total_children = rownonmiss(kidsex01-kidsex19)
	
	* Count daughters (kidsex == 2)
	egen total_daughters = anycount(kidsex01-kidsex19), values(2)
	
	* Create binary indicator for having at least one daughter
	gen d_daughter = (total_daughters >= 1) if total_children > 0
	replace d_daughter = . if total_children == 0
	
	* Gender (female dummy)
	gen female = (sex == 2) if !missing(sex)
	
	* Create interaction term between d_daughter and female
	gen d_daughter_x_female = d_daughter * female
	
	* Generate control variables
	display "Creating control variables..."
	
	* Age
	gen age = bioage
	replace age = . if bioage == -2
	
	* Education (years)
	gen education = pgbilzeit
	recode education (-2 = .) (-1 = .)
	
	* Partner status
	recode partner (2=1) (3=1)
	
	* Employment status
	gen employed = (pgemplst == 1) if !missing(pgemplst)
	
	* Log household income
	gen log_hhincome = ln(pglabgro)

* HYPOTHESIS TEST FOR H4 * 

	* H4: Having daughters leads to a larger increase in support of gender equality for men than for women
	* We test whether the effect of having daughters on gender equality attitudes is larger for men than women
	* This requires testing the interaction effect: β3 < 0 (negative interaction coefficient)
	
	* Linear OLS regression with bootstrapped standard errors clustered at household level
	* Model: gender_equality_attitudes = β₀ + β1×d_daughter + β2×female + β3×d_daughter_x_female + controls + ε
	* Key interpretation:
	* β1 = Effect of having daughters for men (reference group)
	* β1 + β3 = Effect of having daughters for women  
	* β3 = Difference in daughter effect between women and men
	* For H4 to be supported: β3 < 0 (effect is larger for men)

	bootstrap, reps(2000) seed(12345): ///
		regress gender_equality_attitudes d_daughter female d_daughter_x_female age education partner employed log_hhincome, ///
		vce(cluster cid)

* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate (we focus on the interaction term β3)
		local coefficient = _b[d_daughter_x_female]

		* (ii) standard error
		local se = _se[d_daughter_x_female]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value (one-tailed test as specified in PAP)
		* We test H0: β3 ≥ 0 vs H1: β3 < 0 (one-tailed test)
		local t_stat = _b[d_daughter_x_female] / _se[d_daughter_x_female]
		local p_value = normal(`t_stat')  // one-tailed p-value for negative effect

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local interpretation "negative coefficient is in line with hypothesis"

	// Display the saved statistics
	di "Coefficient (interaction term β3): `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value (one-tailed): " `p_value'
	di "Number of observations: " `num_obs'
	di	"Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"
	
	// Display additional information for interpretation
	di ""
	di "Additional interpretation information:"
	di "Effect of daughters for men (β1): " _b[d_daughter]
	di "Effect of daughters for women (β1 + β3): " _b[d_daughter] + _b[d_daughter_x_female]
	di "Difference (Men's effect - Women's effect = -β3): " -_b[d_daughter_x_female]

	* Export results to csv 
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile
	
********************************************************************************

exit


