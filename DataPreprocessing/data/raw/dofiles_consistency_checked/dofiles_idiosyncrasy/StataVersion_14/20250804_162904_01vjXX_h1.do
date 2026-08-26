********************************************************************************
*********************  Do-File H1: #ManyDaughters Project **********************
********************************************************************************

* Authors: Tina Wang, David Albrecht, adapted by 01vjXX

* ============================
* Setup * 
* ============================

*clear all
version 14 // stata version
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
ssc install outreg2
ssc install psmatch2
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

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing (pid kidsex01 kidpnr01 kidgeb01 kidmon01 kidgeb02 kidgeb03 kidgeb04 kidgeb05 kidgeb06 kidgeb07 kidgeb08 kidgeb09 kidgeb10 kidgeb11 kidgeb12 kidgeb13 kidgeb14 kidgeb15 kidgeb16 kidgeb17 kidgeb19 kidsex02 kidsex03 kidsex04 kidsex05 kidsex06 kidsex07 kidsex08 kidsex09 kidsex10 kidsex11 kidsex12 kidsex13 kidsex14 kidsex15 kidsex16 kidsex17 kidsex19)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing (pid syear pgisced11)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(pid syear hid plj0621 plj0622 plj0623 plj0624 plj0625 plc0010 plc0011 plh0298_v1 plh0298_v2 plh0300_v1 plh0300_v2 plh0302_v1 plh0302_v2 plh0308_v1 plh0308_v2 plh0357 plh0358 plh0359)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

* Deviation from  PAP. The original analysis proved infeasible due to a lack of observations.
* Therefore I switched analysis strategies, instead of a fixed effects model I perform propensity score matching and do a linear regression
* I also change the dependent variable i.e. how gender attitudes are measured. Instead of plj0621 and plj0622 (the two items with the only acceptable alpha from plj0621:plj0625)
* I use plh0357 and plh0359 instead, which are attitudes towards homosexuality. The original items were about working women. These are arugably different aspects of gender equality.

* FILTERS * 
	keep if syear == 2018 // questions only asked in 2018 

* GENERATE VARIABLES * 

	** recode all missings
	mvdecode _all, mv(-8/-1 = .)
	
	** recode sex of child - this is a universal variable that does not yet show WHEN the birth was 
	tab kidsex01
	recode kidsex01 (1=0) (2=1), gen(kidsex01F) // change coding from 1,2 to 0,1 - 1 is female, 0 is male
	
	
	** now indicate whether that child was a boy or girl
	gen hadDaughter = .
	replace hadDaughter = 1 if kidsex01F == 1 // 
	replace hadDaughter = 0 if kidsex01F == 0 // by definition children whose sex is unknown are exluded through this approach
	
	drop if hadDaughter == . // take out observations of those who don't have a son or daughter
	
	** gender attitudes as measured by attitudes towards gay marriage and gay parenting
	
	rename plh0357 gay_marriage // rename variables
	rename plh0359 gay_parent // rename variables
	
	alpha gay_marriage gay_parent // look at alpha between the two

	drop if gay_marriage == . // drop missings
	drop if gay_parent == . // drop missings
	 
	gen genderAtt = (gay_marriage + gay_parent) / 2 // create index

	** look at matching and control variables
	
	* born in East Germany
	*tab birthregion_ew
	
	*gen east = . 
	*replace east = 1 if birthregion_ew == 22 
	*replace east = 0 if birthregion_ew == 21
	*tab east, missing // about one third missings, those not from germany?
	* collides with migraiton background - decision to only use migration background 
	
	* age at birth
	gen age_at_birth = kidgeb01 - gebjahr
	
	* migration background
	tab migback
	gen migback_binary = . 
	replace migback_binary = 1 if migback == 2
	replace migback_binary = 0 if migback == 1 
	replace migback_binary = 0 if migback == 3
	replace migback_binary = 0 if migback == 4
	tab migback_binary
	
	* female
	tab sex
	recode sex (1=0) (2=1), gen(female) // change coding from 1,2 to 0,1 - 1 is female, 0 is male
	tab female
	
	* total nr of children
	egen numChildren = rownonmiss(kidgeb01-kidgeb19) // this is based on knowing year of birth, there are some cases where year of birth is unknown 
	egen numChildren2 = rownonmiss(kidsex01-kidsex19) // this is based on knowing sex of child
	
	gen flag_children = .
	replace flag_children = 1 if numChildren != numChildren2
	
	* 2 observations have a different value here depending on whether sex of child or year of birth of child is used 
	* choice made to use date of birth to establish whether someone has a child or not
	
	* education
	tab pgisced11

	gen edu = . 
	replace edu = 1 if pgisced >=0 &  pgisced <= 2 // low educated
	replace edu = 2 if pgisced >=3 & pgisced <= 5 // middle educated
	replace edu = 3 if pgisced >=6 & pgisced <= 8 // highly educated
	tab edu
	
	*income 
	tab hghinc, missing
	
	histogram hghinc // look at distribution
	
	gen log_income = log(hghinc) // create logged income
	histogram log_income // check distribution of logged income
	
	
	** Propensity score matching -  migration background, income, nr of children, sex, education. 

	psmatch2 hadDaughter c.age_at_birth i.migback_binary log_income i.female c.numChildren i.edu, neighbor(1) caliper(0.05) noreplace
	
	gen matched = (_weight != .)
	
	* look at balance
	pstest c.age_at_birth i.migback_binary c.log_income i.female c.numChildren i.edu, both


* HYPOTHESIS TEST FOR H1 * 
	
	* My original analysis was planned as: 
	* I run a fixed effects model. I assume that gender equality attitudes can be measured through an index created by two items (plj0621 and plj0622) that ask about attitudes towards women working. 
	* The variable is coded as 1 = strongly disagree and 7 = strongly agree. A higher value indicates more liberal gender equality attitudes.
	* I run a fixed-effects model with standard errors clustered at the individual level.
	* The variable genderAtt is the dependent variable and the dummy variable hadDaughter is the independent variable. 
	* I also include year fixed effects in the initial model. If sample size proves too small they will be removed. 
	* However, this was infeasible (I can provide the code upon request). I had already noted this potential problem in the PAP. 
	* Therefore I switched strategies and did the following: 
	* I identified questions on gender equality attitudes that had a higher rate of responses, albeit only in 2018 (plh0357 and plh0359)
	* I then used PSM to match participants on age at birth of first child, migration background, logged income, gender, number of children, level of education
	* I then performed an OLS regression uses the propensity scores
	
	* with weights 
	reg genderAtt hadDaughter i.female c.age_at_birth i.migback_binary c.log_income c.numChildren i.edu [pw=_weight] if matched, vce(robust)
	
* EXTRACT AND EXPORT RESULTS * 
	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[hadDaughter]

		* (ii) standard error
		local se = _se[hadDaughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[hadDaughter] / _se[hadDaughter]
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
	file open myfile using "$OUT/h1_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
