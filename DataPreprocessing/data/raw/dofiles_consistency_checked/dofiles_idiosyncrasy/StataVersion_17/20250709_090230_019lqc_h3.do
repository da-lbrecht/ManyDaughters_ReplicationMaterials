********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
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

	save "$DATA/h3_dataset.dta", replace // save merged dataset


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


		
		// Remove singles
		drop if parid == -2


		
		// Merge with partner
		preserve

		keep pid syear pglabnet
		rename pid parid
		rename pglabnet partner_income

		tempfile partners
		save partners.dta, replace

		restore

		merge 1:1 parid syear using partners.dta, keep(match) nogen


		gen ihs_income_gap = asinh(pglabnet - partner_income) if !missing(pglabnet, partner_income)


		

		// Global definition of always same control variables
		local controls d_male ln_age ln_income_net d_married d_divorced d_separated d_vocational d_higher_edu d_migback



* HYPOTHESIS TEST FOR H3 * 
* This section tests whether having daughters leads to greater gender equality within the household (H3). 
		* 
		* Dependent Variable:  
		* Gender equality in the household is measured using a continuous outcome variable that captures 
		* intra-household economic inequality. Specifically, we calculate the income gap as the difference 
		* between the respondent's net individual labor income (pglabnet) and their partner's net labor 
		* income (partner_income, retrieved via merging on partner ID). Singles (parid == -2) are excluded 
		* from the analysis as no within-household gap can be computed. Since this variable may include 
		* zero and negative values and is skewed, we apply the inverse hyperbolic sine (IHS) transformation 
		* to normalize its distribution:  
		* 
		*     gen ihs_income_gap = asinh(pglabnet - partner_income) if !missing(pglabnet, partner_income)
		* 
		* The IHS transformation behaves similarly to a logarithmic transformation for large values 
		* but remains defined at zero and for negative inputs (Rehm et al., 2022). This approach allows 
		* us to retain the full range of observations without imposing arbitrary truncation.  
		* 
		* Model Specification:  
		* We estimate a linear panel model using ordinary least squares (OLS) to test whether having 
		* daughters influences the transformed income gap. To control for general parenthood effects, 
		* we include fixed effects for the total number of children (absorb(tot_kids)). Standard errors 
		* are clustered at the household level (hid) to account for repeated observations within households.  
		* The model is specified as follows:   

areg ihs_income_gap daughters_nr `controls', absorb(tot_kids) cluster(hid)

		* where ihs_income_gap is the dependent variable, daughters_nr is the number of daughters, 
		* `controls' is the vector of control variables, absorb(tot_kids) includes total children fixed 
		* effects, and cluster(hid) adjusts standard errors for within-household correlation over time.  
		* 
		* Interpretation of Results:  
		* The primary coefficient of interest is that on daughters_nr. A negative and statistically 
		* significant coefficient would indicate that having more daughters is associated with a smaller 
		* income gap, suggesting more equal economic contributions within households. Statistical 
		* significance will be evaluated at the 10%, 5%, and 1% levels, with t-statistics reported. 
		* Residual degrees of freedom are reported as appropriate for OLS estimation. 
		

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[daughters_nr]

		* (ii) standard error
		local se = _se[daughters_nr]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local t_value = _b[daughters_nr] / _se[daughters_nr]
		local p_value = 2 * ttail(e(df_r), abs(`t_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[daughters_nr]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile
	
********************************************************************************

exit
