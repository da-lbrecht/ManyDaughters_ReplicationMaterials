********************************************************************************
****************** Example Do-File H4: #ManyDaughters Project ******************
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
ssc install outreg2

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

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)


* GENERATE VARIABLES * (Please adapt according to your needs)



		// Create panel
		// pid: "Person ID"
		sort pid syear
		xtset pid syear



		// sex: "Gender"
		// 1 == "Male", 2 == "Female"
		keep if sex == 1 | sex == 2

		// Gender dummy
		gen d_male = 1 if sex == 1
		replace d_male = 0 if sex == 2



		// bioage: "Age When Surveyed Biography"
		gen ln_age = ln(bioage)



		// pgfamstd: "Marital Status In Survey Year"
		// 1 == "Married"
		gen d_married = 0 if pgfamstd > 0 & pgfamstd != .
		replace d_married = 1 if pgfamstd == 1

		// 2 == "Married, But Separated"
		gen d_separated = 0 if pgfamstd > 0 & pgfamstd != .
		replace d_separated = 1 if pgfamstd == 2

		// 4 == "Divorced"
		gen d_divorced = 0 if pgfamstd > 0 & pgfamstd != .
		replace d_divorced = 1 if pgfamstd == 4



		// pglabnet: "Current Labor Net Income in Euro"
		gen ln_income_net = ln(pglabnet)



		// pgcasmin: "CASMIN Classification" (Education)
		// 3 == "Basic Vocational Qualification", ..., 7 == "Vocational Maturity Certif"
		// 8 == "Lower Tertiary Education", 9 == "Higher Tertiary Education"
		gen d_vocational = 0 if pgcasmin > 0 & pgcasmin != .
		replace d_vocational = 1 if pgcasmin >= 3 & pgcasmin <= 7

		gen d_higher_edu = 0 if pgcasmin > 0 & pgcasmin != .
		replace d_higher_edu = 1 if pgcasmin >= 8 & pgcasmin <= 9



		// migback: "Migration background"
		gen d_migback = 0 if migback == 1
		replace d_migback = 1 if migback == 2 | migback == 3



		// Kids in general: Needed for FE
		gen tot_kids = 0
		forvalues i = 1/19 {
			local v = "kidsex" + string(`i', "%02.0f")
			replace tot_kids = tot_kids + 1 if `v' == 1 | `v' == 2
		}


		// Daughters
		gen daughters_nr = 0

		forvalues i = 1/19 {
			local varname = "kidsex" + string(`i', "%02.0f")
			replace daughters_nr = daughters_nr + 1 if `varname' == 2
		}

		
		// plh0174: Satisfaction With Housework
		// 0 == "Completely dissatisfied" ... 10 == "Completely satisfied"
		// satis_housew == 1 if respondents are stronger satisfied with housework
		gen satis_housew = 1 if plh0174 >= 8 & plh0174 <= 10
		replace satis_housew = 0 if plh0174 >= 0 & plh0174 <= 7
		
		// Global definition of always same control variables
		local controls ln_age ln_income_net d_married d_divorced d_separated d_vocational d_higher_edu d_migback


* HYPOTHESIS TEST FOR H4 * 
* This section tests whether having daughters leads to a larger increase in support for 
* gender equality among men compared to women (H4). 
* 
		* Dependent Variable:  
		* As in Hypothesis 1, we use the item plh0174, where respondents report their satisfaction 
		* with the current division of housework.  
		* We construct the binary variable satis_housew, coded as 1 if the respondent reports 
		* high satisfaction (values 8–10), and 0 otherwise. A value of 1 is interpreted as 
		* an attitudinal indicator of perceived fairness in the domestic arrangement, 
		* and thus as a proxy for a supportive stance toward gender-balanced domestic roles.
		* This assumes that satisfaction reflects personal approval of the arrangement, 
		* not necessarily objective equality.
		* 
		* Model Specification:  
		* We estimate a logistic regression to test whether having daughters influences support 
		* for gender equality differently for men and women. To test this gender-specific 
		* heterogeneity, we include a continuous measure of the number of daughters 
		* (daughters_nr) and an interaction term with the respondent's gender (d_male). 
		* Total number of children fixed-effects (i.tot_kids) are included to control for 
		* family size, and standard errors are clustered at the respondent level to account 
		* for repeated observations in the panel. The model is specified as follows:  

logit satis_housew c.daughters_nr##d_male `controls' i.tot_kids, cluster(pid)

		* where satis_housew is the dependent variable, daughters_nr is the number of daughters, 
		* and d_male is the respondent's gender (1=male, 0=female). The double interaction ## 
		* both basic and the interacted effect to the model. It further includes `controls' as 
		* the vector of additional covariates, and i.tot_kids includes total children fixed effects. 
		* 
		* Interpretation of Results:  
		* The primary coefficient of interest is the interaction term (c.daughters_nr#1.d_male), 
		* which captures whether the effect of daughters on gender attitudes differs between 
		* men and women. A positive and statistically significant interaction coefficient would 
		* indicate that having daughters has a stronger effect on support for gender equality 
		* among men. Statistical significance will be assessed at the 10%, 5%, and 1% levels, and 
		* z-statistics will be reported. Residual degrees of freedom are not applicable due to the 
		* maximum likelihood nature of the logit model.



* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b["c.daughters_nr#1.d_male"]

		* (ii) standard error
		local se = _se["c.daughters_nr#1.d_male"]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local z_value = _b["c.daughters_nr#1.d_male"] / _se["c.daughters_nr#1.d_male"]
		local p_value = 2 * (1 - normal(abs(`z_value')))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b["c.daughters_nr#1.d_male"]
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
