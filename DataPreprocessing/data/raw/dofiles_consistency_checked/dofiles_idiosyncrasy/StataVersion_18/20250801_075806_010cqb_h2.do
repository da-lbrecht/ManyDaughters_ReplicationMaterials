********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht
* Team: 010cqb

* ============================
* Setup * 
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000


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

	* use  pl dataset as starting point
	use "$IN/pl.dta", clear // 
	
	* keep only those of the survey years 2005, 2009, 2014 and 2019 (i.e., where dependent variable was elicited)
	keep if syear == 2005 | syear == 2009 | syear == 2014 | syear == 2019
	
	* merge with biobirth
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	
	* merge with pgen to obtain highest obtained level of education
	merge m:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	
	* define the dependent variable; for H2 plh0004 (political preference on a  0 [left] to 10 [right] scale)
		* drop if not elicited or not answered
		drop if plh0004 == -1 | plh0004 == -5
		
		* reverse scale it to make coefficients more intuitive; higher values more left-leaning 
		gen plh0004_rev = 10 - plh0004
	
	
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
		gen age = syear - ple0010_h
		gen age_sqrd = age^2
		
		* Control variables: income, already there; plc0013_h
		rename plc0013_h income
		
		* Control variables: type of emplyoment, reference being full-time employed [1]; plb0022_h
		rename plb0022_h employment
		
		* Control variables: marital status , reference being married; pgfamstd; 
		* according to the pap, we merge the first two (married as reference)
		* Deviation from the PAP: Same issue as with gender. pld0131_h not present.
		* Therefore we use pld0131_v1
		gen marital_status = .
		
		* Merge categories 1 and 2 into 1 ("Married")
		replace marital_status = 1 if inlist(pgfamstd, 1, 2)

		* Keep others as-is
		replace marital_status = pgfamstd if !inlist(pgfamstd, 1, 2)
		
		* Label the new variable
		label define marital_lbl 1 "Verheiratet" 3 "Ledig" 4 "Geschieden" 5 "Verwitwed" 6 "Ehepartner Ausland" 7 "Eingetragene gleichgeschlechtliche Partnerschaft"
		label values marital_status marital_lbl
		label variable marital_status "Marital status (1 = Married)"
		
		* Control variable: having obtained a university degree; based on pgpbbil02
		gen higher_education = .
		replace higher_education = 1 if pgpbbil02 > 0
		replace higher_education = 0 if pgpbbil02 <= 0 | missing(pgpbbil02)
		
		label variable higher_education "Binary if individual obtained higher education"
		
		* collect the variables necessary for the analysis
		keep pid syear plh0004 plh0004_rev num_daughters num_children sumkids gender age age_sqrd income employment pld0131_v1 marital_status pgpbbil02 higher_education
		

		save "$DATA/h2_dataset.dta", replace // save preprocessed dataset
	

* ============================
* Analysis
* ============================

* FILTERS *

	* We only filter for plausible values (non negative)
 
	* only those with children; num_children > 0 
	drop if num_children <= 0
	
	* drop if missing values; use gender as main indicator
	drop if missing(gender)
	drop if gender < 0 
	
	* drop negative values for wage
	drop if income < 0
	
	* drop if negative values for emplyoment
	drop if employment < 0 
	
	* drop if marital status negative
	drop if marital_status < 0 
	
	* drop where only one observations, otherwise can't use fixed effects
	bysort pid: gen n_obs = _N
	gen singleton = n_obs == 1
	tab singleton
	drop if singleton == 1 
	


* GENERATE VARIABLES *



* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	
	* Description:
	* Again, we assume that nature randomly assigns the gender of children and based on the subset
	* of people who became parents (unobserved variables might drive the decision to become a parent
	* in the first place), we estimate the effect of one additional daughter on a policial preferences 
	* elicited using a 11-point likert scale, which we reverse coded to make it more intuitive 
	* (higher values indicating higher more left-leaning political preferences)
	* Additionally, we include control variables and subject fixed effects. Finally,
	* we use cluster robust standard errors on the subject level.
	
	xtset pid syear


	xtreg plh0004_rev num_daughters i.num_children ib2.gender age age_sqrd income i.employment i.marital_status i.higher_education i.syear, fe cluster(pid)




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
		scalar coef_d_daughter = _b[num_daughters]
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
