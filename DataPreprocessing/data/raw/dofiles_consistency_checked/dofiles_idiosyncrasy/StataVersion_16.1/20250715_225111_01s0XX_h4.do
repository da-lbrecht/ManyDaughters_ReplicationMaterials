********************************************************************************
****************** Do-File H4: #ManyDaughters Project ******************
********************************************************************************

* Initial Authors: Tina Wang, David Albrecht
* Edited by: Research Team - [REDACTED] 

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 16.1 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2

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



keep plh0357 plh0298_v2 plh0302_v2 plh0308_v2 plh0359 plh0360 hid pid syear parid sex sumkids kidgeb01-kidgeb19 kidsex01-kidsex19 hghinc pgbilzeit partner migback	
	
	save "$DATA/h4_dataset.dta", replace // save merged dataset
	

* ============================
* Analysis
* ============================
* Decode missings into Stata missings
mvdecode _all, mv(-1= .a \ -2=.b \ -3=.c \ -5=.e \ -8=.h) 


* FILTERS * (Please adapt according to your needs)
 
	* exclude respondents without partner
 	drop if partner == 0 
	

* GENERATE VARIABLES * (Please adapt according to your needs)
	* Generate Outcome Variable: SupportGE
	
	fre plh0357 plh0298_v2 plh0302_v2 plh0308_v2 plh0359 plh0360 
	 
	 recode plh0298_v2 (7=1) (6=2) (5=3) (3=5) (2=6) (1=7), gen(plh0298_v2_rev)
	 tab plh0298_v2 plh0298_v2_rev
	 drop plh0298_v2
	recode plh0302_v2 (7=1) (6=2) (5=3) (3=5) (2=6) (1=7), gen(plh0302_v2_rev)
	tab plh0302_v2 plh0302_v2_rev
	 drop plh0302_v2

	egen SupportGEmean = rowmean(plh0357 plh0298_v2_rev plh0302_v2_rev plh0308_v2 plh0359 plh0360)

	egen SupportGErowmiss = rowmiss(plh0357 plh0298_v2_rev plh0302_v2_rev plh0308_v2 plh0359 plh0360)
	fre SupportGErowmiss 
	replace SupportGEmean = . if SupportGErowmiss > 2 // leads to exclusion of 6 cases 
	drop SupportGErowmiss
	sum SupportGEmean 
	gen SupportGE = (SupportGEmean - r(mean)) / r(sd)
	sum SupportGE
	drop SupportGEmean plh0357 plh0298_v2_rev plh0302_v2_rev plh0308_v2 plh0359 plh0360
	
	
	
	* generate dummy variable for whether participant has at least one daughter born before 2018 (1) or not (0), 0 includes individuals without children born before 2018
	fre kidsex* // no more than 12 kids
	gen Daughters = ((kidsex01==2& kidgeb01<2018)  | (kidsex02==2& kidgeb02<2018) | (kidsex03==2& kidgeb03<2018) | (kidsex04==2& kidgeb04<2018) | (kidsex05==2& kidgeb05<2018) | (kidsex06==2& kidgeb06<2018) | (kidsex07==2& kidgeb07<2018) | (kidsex08==2& kidgeb08<2018) | (kidsex09==2& kidgeb09<2018) | (kidsex10==2& kidgeb10<2018) | (kidsex11==2& kidgeb11<2018) | (kidsex12==2& kidgeb12<2018)) 
	fre Daughters if syear==2018
	bysort pid (syear): replace Daughters = Daughters[_n-1] if missing(Daughters) // write forward info from previous years if missing 
	fre Daughters if syear==2018	
	
	
	* generate dummy variable whether respondent is male (1) or not (0)
	fre sex 
	recode sex (2=0) , gen(male)
	
	* Count value of the total number of kids
	fre sumkids 
	
	* Household income
	fre hghinc if syear==2018 
	bysort pid (syear): replace hghinc = hghinc[_n-1] if missing(hghinc) // write forward info from previous years if missing 
	fre hghinc if syear==2018	
	
	* education in years
	fre pgbilzeit if syear==2018
		bysort pid (syear): replace pgbilzeit = pgbilzeit[_n-1] if missing(pgbilzeit) // write forward info from previous years if missing 
	fre pgbilzeit if syear==2018


	* Migration Background 
	fre migback if syear == 2018
	
	* partnershiop status
	fre partner


* HYPOTHESIS TEST FOR H4 * (Please adapt according to your needs)

	* Please add a verbal description of your model here
	* Please add a verbal description of your model here

	* I refrain from using the alternative specification using a 2SLS IV-regression, as I have serious concerns about the exclusion restriction being violated. Thus, I stick with the main specification: linear OLS regression

	* Having Daughters (Daughters) leads to a stronger increase in support for Gender Equality (SuopportGE) for men than for women.
	* Accordingly, we run a linear OLS regression analysis with SuopportGE as dependent variable and the dummy variable Daughters fully interacted with male as independent variable, additionally controlling for total number of children (sumkids), income (hghinc), education (pgbilzeit), marital status (partner), migration background (migback) using heteroskedasticity robus standard errors

	regress SupportGE ib0.Daughters##ib0.male sumkids hghinc pgbilzeit ib1.partner ib1.migback , robust


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.Daughters#1.male]

		* (ii) standard error
		local se = _se[1.Daughters#1.male]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.Daughters#1.male] / _se[1.Daughters#1.male]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_Daughtersmale = _b[1.Daughters#1.male]
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
