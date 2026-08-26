********************************************************************************
****************** Example Do-File H2: #ManyDaughters Project ******************
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

	save "$DATA/h2_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS *
 
gen year=2019

*** 1) Keep the latest available year 
keep if syear==year

*** 2) Keep individuals with 1 to 5 children
gen number_kids=.
replace number_kids= 1 if kidsex01==1 | kidsex01==2
replace number_kids= 2 if kidsex02==1 | kidsex02==2
replace number_kids= 3 if kidsex03==1 | kidsex03==2
replace number_kids= 4 if kidsex04==1 | kidsex04==2
replace number_kids= 5 if kidsex05==1 | kidsex05==2
replace number_kids= 6 if kidsex06==1 | kidsex06==2
keep if number_kids>=1 & number_kids <=5

*** 3) Exclude families with first-born twins
*** Proxy for the first-born is the first and second child are born in the same year and in the sam months
drop if (kidgeb01 == kidgeb02) & (kidmon01==kidmon02) & kidgeb01>0

*** 4) Exclude not heterosexual individuals 
drop if sexor!=0

*** 5) Keep only individuals with an identity as a man or a woman
keep if sex ==1 | sex==2 

*** 6) Keep only individuals who are not younger as 25 and not older than 60
gen age = year-ple0010_h
keep if age >=25 & age <=60

*** 7) Keep only individuals who were born in Germany
keep if corigin==1

**** Private housholds only
keep if inlist(pop,1,2)


* GENERATE VARIABLES * 

** Dependent variable H2:
replace plh0004 =. if plh0004 <0
sum  plh0004 , d
gen z_plh0004  = (plh0004 -r(mean))/r(sd)


** Independent variable:
***** First born child is daughter
gen d_daughter=1 if kidsex01==2 // 1 if the first-born child is female
replace d_daughter=0 if kidsex01==1 // 0 if the first-born child is m


** Control variables: 
*** male dummy
gen male=1 if sex ==1
replace male =0 if sex ==2

*** age --> see above

*** number of other daughters
gen num_daughters_02_05=0
replace num_daughters_02_05= num_daughters_02_05+1 if kidsex02==2
replace num_daughters_02_05= num_daughters_02_05+1 if kidsex03==2
replace num_daughters_02_05= num_daughters_02_05+1 if kidsex04==2
replace num_daughters_02_05= num_daughters_02_05+1 if kidsex05==2

*** number of children --> see above 

*** race of the respondent (dummy equals one if White-Caucasian vs. zero if not)
* not included in year 2019

*** age of the first-born child at the point of the survey
gen age_kid_01= year - kidgeb01 if kidgeb01>0

*** highest educational level achieved by the respondent (linear value, 0 = without school degree, 1 = school degree, 2 = professional degree (Ausbildung), 3 = university degree)
* not available. Instead number of years in education
gen education_yrs= pgbilzeit if pgbilzeit>=0

*** marital status (dummy, equals one if married or living with a partner, zero otherwise)
* we control only for married, as living in together (not-marries) is not available.
gen married=1 if pgfamstd==1
replace married =0 if pgfamstd!=1

*** political party affiliation
* will reduce the sample by half

*** religion
* not included in year 2018

*** employment status (linear value, 0 not employed, 1 part-time, 2 full-time, 3 self-employed)
gen employment = 0 if pgemplst ==5
replace employment = 1 if pgemplst ==2 | pgemplst ==4
replace employment = 3 if pgemplst ==1

*** income
gen income = pglabgro if pglabgro>=0
* The missing values are set to 0 for those respondents who chose "doesn't apply" and are not employed
replace income =0 if pglabgro ==-2 & employment ==0

*** east/west Germany (dummy)
gen west=1 if sampreg  ==1
replace west=0 if sampreg==2

* HYPOTHESIS TEST FOR H2 * (Please adapt according to your needs)

	* Main dependent variable for H2: plh0004 
	* The latest available year is 2019.
	
	* Let's  assume that answer to the statement "In politics people often talk about "left" and "right" when it comes to characterize different political attitudes. If you think about your own political views: Where would you place yours?" is a good measure of political attitudes (hypothesis 2), 
	* if indicating the values closer to 0 signal respondent's left-leaning and indicating the values closer to 10 signal respondent's right-leaning.
	* Accordingly, we run a linear OLS regression analysis with normalized plh0004 as the dependent variable and the dummy variable d_daughter (the first child is a girl) as the independent variable
	* We use control variables as specified in the PAP and defined above. 

reg z_plh0004 d_daughter male age num_daughters_02_05 number_kids age_kid_01 education_yrs married income employment west, ro


* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = _se[d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter] / _se[d_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
