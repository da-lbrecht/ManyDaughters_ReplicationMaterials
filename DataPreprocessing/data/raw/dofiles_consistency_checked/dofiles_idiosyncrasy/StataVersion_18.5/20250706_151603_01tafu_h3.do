********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

/*H3. Having daughters leads to more gender equality in the household (as measured by behaviors
or outcomes).
That is, denoting Z as a measure of gender equality in the household,
(Z for respondents with daughters) > (Z for respondents without daughters).
*/

* ============================
* Setup * 
* ============================

//clear all
version 18.5 // version of stata we are using 
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * [indicate all packages we make use of???]
* install all packages you need here, for example:
ssc install outreg2
ssc install revrs
ssc install fre

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

* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)
	keep cid pid hid syear parid partner sex gebjahr 
	
	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) 

	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keepusing(pgpartz pgpartnr) keep(master matches) //I need this for partner identification!

	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keepusing(pli0043* pli0040 pli0044* pmonin) keep(master matches)
	
	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace


* ============================
* Analysis
* ============================

* FILTERS * 

	*Only keep participants who:
	*(1) have children
	*(2) have one child only (dropped once second child arrives). Twins/Multiplets counted as 'having daughter' if at least one of them is a girl 
	*(3) use last year of panel where we have full information (but second child has not arrived yet!)

	use "$DATA/h3_dataset.dta", clear 
	
		*(1a) Keep only participants who have children:
		fre kidgeb01
		keep if kidgeb01 != -2  //these persons never have children. 
		drop if kidgeb01 < 0 //Also drop [-1] No Answer [-2] Does not apply [-3] Answer improbable
		

		*Drop years before child1 was born
		drop if syear < kidgeb01 


		*(1b) Keep only if sex of child is known 
		drop if kidsex01 < 0 // missings 

	
		*(2) have one child only (dropped once second child arrives). Except if twins, then counted as 'having daughter' if both of them are girls 

		gen twins = 1 if kidgeb01==kidgeb02 & kidmon01==kidmon02 & kidgeb02 != -2 //month and year of birth same, and second child present. 
		drop if twins==1 & kidsex02 < 0 // if 2nd twins gender unknown 
		
		drop if syear > kidgeb02 & twins !=1  //drop observations (surveyyears) once 2nd child is born (except first child is twin)
		drop if syear > kidgeb03 & twins ==1 //drop observations (surveyyears) once 3rd child is born (if first child is twin)


* GENERATE VARIABLES * 

	*(1)generate dummy variable for participants sex
	fre sex
	gen d_sex = sex-1
	fre d_sex
	label variable d_sex "Sex of participants (0=Male, 1=Female)"

	*(2)Generate dummy variable for whether participant`s child is a girl (1) or boy (0)
	gen d_daughter = 1 if kidsex01== 2 //girl
	replace d_daughter=0 if kidsex01==1 //boy	
	drop if twins==1 & d_daughter==0 & kidsex02==2 //drop if first child is boy, but second twin is girl 
	drop if twins==1 & d_daughter==1 & kidsex02==1 //drop if first child is girl, but second twin is boy 
	label variable d_daughter "Sex of first child (0=son, 1=daughter)"
	fre d_daughter
	
	*(3)Construct total hours respondent does childcare, errands, housework: 'CareAndHouse'			
	gen CaH1 = pli0044_v3 if pli0044_v3 >= 0 //Childcare during week
	gen CaH2 = pli0040 if pli0040 >= 0       //errands during week
	gen CaH3 = pli0043_h if pli0043_h >= 0   //housework during week

		*Keep only persons with all items answered:
		egen rownonmiss =  rownonmiss(CaH1 CaH2 CaH3)
		keep if rownonmiss==3
		
		*Construct variable indicating 
		egen CareAndHouse =  rowtotal(CaH1 CaH2 CaH3)  
		

	*(4)Construct variable for 'relative' hours (comparing partners hours to each other) 

		*First identify partners (pgpartnr). Partners need to be in the same household.
		keep if !missing(pgpartz) & pgpartz != 0 & pgpartz != 9 //if pgpartz is 0, or [9] no partner (identified) 
		*If PGPARTZ is coded 1, 2, 3 or 4, a partnership was defined and PGPARTNR is then assigned the value of the unchanging person ID number (=PERSNR) of the partner.
		drop if pgpartz==5 //drop also if PGPARTZ==5 (pgpartnr is then -2)

		*Generate 'partnermerge' that assigns couples the same id, which corresponds either to theirs or their partners pid.
		gen partnermerge = 	.
		replace partnermerge = pid  if pid < pgpartnr
		replace partnermerge = pgpartnr if pgpartnr < pid

		*partners should also be in the same household:
		gen notsamehouse= 1 if partnermerge==partnermerge[_n+1] & hid != hid[_n+1]
		drop if notsamehouse== 1 & (syear==syear[_n+1] | syear==syear[_n-1])


		*create variable 'partners_CareAndHouse' indicating partners hours
		bysort partnermerge syear: gen partners_CareAndHouse = CareAndHouse[_n+1] if syear==syear[_n+1]
		bysort partnermerge syear: replace partners_CareAndHouse = CareAndHouse[_n-1] if syear==syear[_n-1] & missing(partners_CareAndHouse)


		*How much hours each partner spends relative to the other (mainly to identify if both partners have a value)
		*Then take Latest survey year which has info on relative_h 
		gen relative_h = (CareAndHouse*100)/(CareAndHouse + partners_CareAndHouse) 
		sort partnermerge syear
		
		keep if !missing(relative_h)
		bysort partnermerge syear: gen n_partner = _N
		keep if n_partner == 2 //keep only years in which both couples present

		bysort partnermerge (syear): gen latest = syear == syear[_N]  //identify latest year==1 
		keep if latest==1


	*(5)Create the IV: `equality' (equality in the household)
	*We want 1 to be fully equal, so:  1-	(|(my hours/total)-0.5 | *2)
	* 1- (|(CareAndHouse/(CareAndHouse + partners_CareAndHouse))-0.5 | *2)
	gen share =	(CareAndHouse/(CareAndHouse + partners_CareAndHouse))-0.5
	replace share = share*(-1) if share <0
	gen equality = 1- (share*2)
	sum equality, d //0-1



* HYPOTHESIS TEST FOR H3 * 

	* We assume that the division of childcare and housework tasks among couples (equality) is a good measure of gender equality in the household (hypothesis 3), 
	* if having a daughter, the respondent is more likely to have higher gender equality in the household (a higher score on scale indicating equality within the household regarding division of childcare and household tasks).
	* Accordingly, we run a linear OLS regression analysis with `equality' as dependent variable and the dummy variable `d_daughter' as independent variable

	reg equality d_daughter, cluster(hid) 


* EXTRACT AND EXPORT RESULTS * 

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

		* (viii) interpretation based of directionality and p-value
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
