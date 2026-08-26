********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2

/*
* CONFIGURE FILE PATHS * (Please do not change)
*Sorry, this didn't work smoothly for us
/* the following code automatically sets paths based on where you saved the distributed file-package on your system */
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
/*

	global ROOT "C:\Users\matmo\Google Drive\ManyDaughters\#ManyDaughters_RT_AnalysisPackage"
	*global ROOT "G:\.shortcut-targets-by-id\1qHcF2GKvEB6wl4uhwJ2hQmX6eBX27Nmf\#ManyDaughters_RT_AnalysisPackage"
*/ // Commented out by David Albrecht for reproducibility check
	global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
	global DATA "$ROOT/data/derived" // folder for datasets
	global OUT "$ROOT/out" // folder for output (results, tables)

*/
* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	* Merge ppathl with all datasets, that you deem relevant for your analysis. 
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	gsort pid cid syear
	
 
* ============================
* Analysis
* ============================


* GENERATE VARIABLES * (Please adapt according to your needs)


	***Independent variables

	*Age
		gen age=syear-gebjahr
		
	*Education	
		gen years_edu=pgbilzeit   // Amount Of Education Or Training In Years
		replace years_edu=. if years<0

	*Homosexual	 
		preserve
			keep pid sex syear
			collapse (last) sex, by(pid)
			rename pid parid         
			rename sex partner_sex   
			 
			tempfile partners
			save `partners'
		restore
		merge m:1 parid using `partners'
		 
		gen heterosexual = .
		replace heterosexual = 1 if sex != partner_sex & !missing(sex) & !missing(partner_sex)
		replace heterosexual = 0 if sex == partner_sex & !missing(sex) & !missing(partner_sex)
		 
		drop _merge
	
	*Gender of child
		gen daughter=.
		replace daughter=1 if kidsex01==2
		replace daughter=0 if kidsex01==1
	
	*Age child
		gen age_child=  syear - kidgeb01 if kidgeb01>0&syear>0

	*Gender respondent
		gen 	male=.
		replace male=1 if sex==1
		replace male=0 if sex==2
	
	*Married
		gen married=.
		replace married = 0 if pgfamstd>0&pgfamstd!=1
		replace married = 1 if pgfamstd==1
		
	*Nationality
		gen nationality=pgnation if pgnation>0
		
	*Log of monthly net income (EUR)	
		gen ln_income=log(hghinc) if hghinc>0
			
	
	***Dependent variables
		
	*Gender equality in attitudes (for H1 and H4)	
		egen gender_equality=std(plh0308_v2) if plh0308_v2>0&plh0308_v2!=. // in plh0308_v2 a higher number represents stronger agreement. For a robustness test we could also use plh0308_v1 or combine them. But we decided to use plh0308_v2 to make it more parsimonious.
		
	*Leftist politics (for H2)
		gen  leftist=plh0004 if plh0004>=0 // values <0 report no answers or question not included
		egen leftist_std=std(10-leftist)
 	
	*Hours chores weekday (for H3): https://paneldata.org/soep-core/datasets/pl/pli0043_h
		gen hrs_chores=pli0043_h if pli0043_h>=0
		
			*For males only, we merge the hours worked by their partners
			preserve
			keep if male==0
			drop if parid==.|syear==.|cid==.
			drop if parid<0
			keep pid hrs_chores syear cid age years_edu ln_income nationality
			gsort pid syear
			collapse (last) hrs_chores age years_edu ln_income nationality, by(pid syear cid)
			ren pid parid
			foreach var of varlist hrs_chores age years_edu ln_income nationality{
				rename `var' female_`var'   	
			}
				
			tempfile partners
			save `partners'
			restore
			
		drop if parid==.|syear==.|cid==.
		drop if parid<0	
		merge 1:1 parid syear cid using `partners'	
	
	*Gender Equality Index
	gen gei=-abs((hrs_chores-female_hrs_chores)/(hrs_chores+female_hrs_chores))	
	egen gei_std=std(gei)
	
		***Independent, household level covariates
		*Mean age
			gen age_mean_hh=(age+female_age)/2 if age>0&female_age>0
		*Maximum years of education
			egen years_edu_max=rowmax(years_edu female_years_edu)
		*Father's nationality
			*original nationality
		*Marital status
			*original married
		*HH Income
			*original ln_income

* FILTERS * (Please adapt according to your needs)
 
	/*	Inclusion/exclusion criteria according to TEAM's PAP: 
	1. Include individuals >18 years with one child only  [age>18&sumkids==1]
	2. Exclude homosexuals (who could potentially choose their child's gender)
	*/	

*Deviations from PAP
	*We said "we would append observations from 2016 and 2017 and keep the first time an individual appeared", but we ended up using only 2018.
	*In PAP we proposed to use an Equality index using Bhp_504_q56, Bhp_505_q56 Bhp_506_q56, bhp_507_q56, and hp_508_q56 (named plj0621 plj0622 plj0623 plj0624 plj0625 in the actual dataset) but later realized it was only asked to immigrants
	*We do not include "urban/rural" nor "religious_freq", as stated in the PAP, because of the high number of missing observations found
	
		gen sample=(age>18 & sumkids==1 & age_child>0 & heterosexual!=0) // We set age_child>0 because apparently in the 5% data there are "yet to become parents" observations that at the moment of being surveyed have imputed an age of the child that occurs in the future, so when we build the child's age, it's a negative number. 
		
		*For h2
			cap drop  n	n_aux filter_year_sample
			gsort pid cid syear
			bys pid cid  : gen n=_n if leftist!=.
			bys pid cid:  egen n_aux=min(n)
		gen filter_year_sample_h2=(n==n_aux&n!=.) 		

		*For h3	
			cap drop  n	n_aux  
			gsort pid cid syear
			bys pid cid  : gen n=_n if gei_std!=.
			bys pid cid:  egen n_aux=min(n)
		gen filter_year_sample_h3=(n==n_aux&n!=.) 		

	save "$DATA/h1_dataset.dta", replace // save merged dataset

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Please add a verbal description of your model here

	/*	
	We construct the outcome variable, gender_equality, by standardizing responses (mean = 0, standard deviation = 1) to survey question plh0308_v2, which asks respondents the extent to which they agree with the statement: "Best if man and woman work the same amount so they can share the responsibility." Higher values of gender_equality thus indicate stronger support for gender equality in labor participation.

	We estimate a linear regression model where the primary independent variable is an indicator for whether the respondent has a daughter. The specification controls for a rich set of covariates: respondent's gender, age, income, years of education, nationality, marital status, survey year, and the age of the child. The coefficient on the daughter variable is interpreted as the marginal effect of having a daughter (versus a son) on support for gender equality, expressed in standard deviation units.

	The sample is restricted to individuals with exactly one child. Since the outcome variable is only available in the 2018 wave, this restriction ensures that there is a single observation per respondent. To minimize child's gender selection, we exclude homosexuals.
	*/

	global controls_h1 "years_edu male age age_child married ln_income i.syear i.nationality" 
	
	*h1 -> only 2018
	reg gender_equality daughter $controls_h1  if sample==1  , robust
	
	
 * EXTRACT AND EXPORT RESULTS *  

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
	// file open myfile using "$OUT/h1_results_MM.csv", write replace // Restored original naming convention for reproducibility
	file open myfile using "$OUT/h1_results.csv", write replace 
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
