********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht
* Team: 010cqb

* ============================
* Setup * 
* ============================

*clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000
/*
* CONFIGURE FILE PATHS * 
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

	* use  pl dataset as starting point
	use "$IN/pl.dta", clear // 
	
	* keep only those of the survey year 2018 (i.e., where dependent variable was elicited)
	keep if syear == 2018
	
	* merge with biobirth
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	
	* merge with pgen to obtain highest obtained level of education
	merge m:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	
	* define the dependent variable; for H1 plh0308_v2
		* drop if not elicited or not answered
		drop if plh0308_v2 == -1 | plh0308_v2 == -5
	
	
	* Now we create covariates, if necessary 
	
		* Number of daughters
		
			* Create new variables, number of daughter and children up until survey year
			gen num_daughters = 0
			gen num_children = 0
			
			* Loop over child slots 1 to 19
			forval i = 1/19 {
				local sexvar = "kidsex" + string(`i', "%02.0f")
				local yearvar = "kidgeb" + string(`i', "%02.0f")
			
				* Check that both variables exist
				capture confirm variable `sexvar'
				if !_rc {
					capture confirm variable `yearvar'
					if !_rc {
						* Only include child if birth year ≤ ssyear
						replace num_children = num_children + ( `sexvar' > 0 & `yearvar' <= syear)
						replace num_daughters = num_daughters + (`sexvar' == 2 & `yearvar' <= syear)
					}
				}
			}
			
		
		* Control variables: gender, with reference being female [2]; pla0009_v2
		* Deviation from PAP: we state to use pla0009_h. However, this is not present in the pl dataset.
		* Therefore we use pla0009_v2.
		rename pla0009_v2 gender
	
		* Control variables: age and age squared, 
		gen age = 2018 - ple0010_h
		gen age_sqrd = age^2
		
		* Control variables: income, already there; plc0013_h

		rename plc0013_h income
		
		* Control variables: type of emplyoment, reference being full-time employed [1]; plb0022_h
		rename plb0022_h employment
		
		* Control variables: marital status , reference being married; pld0131_v1; 
		* according to the pap, we merge the first two (married as reference)
		* Deviation from the PAP: Same issue as with gender. pld0131_h not present.
		* Therefore we use pld0131_v1
		gen marital_status = .
		
		* Merge categories 1 and 2 into 1 ("Married")
		replace marital_status = 1 if inlist(pld0131_v1, 1, 2)

		* Keep others as-is
		replace marital_status = pld0131_v1 if !inlist(pld0131_v1, 1, 2)
		
		* Label the new variable
		label define marital_lbl 1 "Verheiratet" 3 "Ledig" 4 "Geschieden" 5 "Verwitwed"
		label values marital_status marital_lbl
		label variable marital_status "Marital status (1 = Married)"
		
		* Control variables: political party supported, reference being SPD [1]; plh0012_h
		* Deviation from PAP: before this question, it was asked if there was a political leaning (yes and no)
		* if yes, they were asked which political party.
		* We will use both questions now. If they indicated to have a political leaning, we take the party they favor. 
		* Otherwise we make a new category being no political leaning
		gen pol_affiliation = .
		
		replace pol_affiliation = 99 if plh0011_v2 == 2 // those who declared to have no political leaning
		label define plh0012_h 99 "[99] keine Präferenz", add // add label to variable

		replace pol_affiliation = plh0012_h if plh0011_v2 == 1 // if respondent indicated to have a preference
		label values pol_affiliation plh0012_h // copy labels from harmonized to new one
		
		
		* Control variable: having obtained a university degree; based on pgpbbil02
		gen higher_education = .
		replace higher_education = 1 if pgpbbil02 > 0
		replace higher_education = 0 if pgpbbil02 <= 0 | missing(pgpbbil02)
		
		label variable higher_education "Binary if individual obtained higher education"
		
		* collect the variables necessary for the analysis
		keep pid plh0308_v2 num_daughters num_children sumkids gender age age_sqrd income employment pld0131_v1 marital_status pol_affiliation pgpbbil02 higher_education
		

		save "$DATA/h1_dataset.dta", replace // save preprocessed dataset
	

* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* only those with children; sumkids > 0 
	drop if num_children <= 0
	
	* drop if missing values; use gender as main indicator
	drop if missing(gender)
	
	* drop negative values for wage
	drop if income < 0
	
	* drop if negative values for emplyoment
	drop if employment < 0 
	
	* drop if negative values for marital_status
	drop if marital_status < 0 
	
	* drop those with no answer for political affiliation
	drop if pol_affiliation == -1
	
* GENERATE VARIABLES *


* HYPOTHESIS TEST FOR H1 *
	
	* Description:
	* We assume that nature randomly assigns the gender of children and based on the subset
	* of people who became parents (unobserved variables might drive the decision to become a parent
	* in the first place), we estimate the effect of one additional daughter on a question related
	* to gender equity attitudes elicited in 2018 (relate to shared responsibility within households;
	* likert scale item from 1 - 7 with higher values indicating higher attitudes towards gender equity)
	* Additionally, we include control variables used in similar papers in the past. Finally,
	* we use heteroskedastic robust standard errors. 

	reg plh0308_v2 num_daughters i.num_children ib2.gender age age_sqrd income i.employment i.marital_status i.pol_affiliation i.higher_education, robust



* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[num_daughters]

		* (ii) standard error
		local se = _se[num_daughters]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		* Note that accroding to our PAP we would define p < 0.005 to indicate significance
		local p_value = _b[num_daughters] / _se[num_daughters]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		local interpretation "positive coeffficient is in line with hypothesis"
		

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
