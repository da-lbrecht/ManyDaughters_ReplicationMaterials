********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht, [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 15.1 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2
ssc install ivreg2
ssc install ranktest

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
	* bioparen contains info on parental background
	merge m:1 pid using "$IN/bioparen.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


	
* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	*keep if inlist(pop,1,2)
	
	* Dropping those with missing values in kids' birthyear or sex
	foreach val in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 {
	drop if kidgeb`val'==-3 | kidgeb`val'==-1
	drop if kidsex`val'==-1
	}

* GENERATE VARIABLES * (Please adapt according to your needs)

	/*
	* Interview date (can't use this one for identifying whether kids born before interview because birth months often missing)
	recode pmonin(-1=.)
	recode ptagin (-1=.)
	gen interview_date = mdy(pmonin, ptagin, syear)
	format interview_date %td
	*/
		
	* NUMBER OF DAUGHTERS
			
	* Coding kids' birth years for those without kids to missing
	foreach var of varlist kidgeb* {
	recode `var' (-2=.)
	}
		
	gen numdaughter=0
	replace numdaughter=1 if kidsex01==2 & kidgeb01<=syear
	replace numdaughter=numdaughter+1 if kidsex02==2 & kidgeb02<=syear
	replace numdaughter=numdaughter+1 if kidsex03==2 & kidgeb03<=syear
	replace numdaughter=numdaughter+1 if kidsex04==2 & kidgeb04<=syear
	replace numdaughter=numdaughter+1 if kidsex05==2 & kidgeb05<=syear
	replace numdaughter=numdaughter+1 if kidsex06==2 & kidgeb06<=syear
	replace numdaughter=numdaughter+1 if kidsex07==2 & kidgeb07<=syear
	replace numdaughter=numdaughter+1 if kidsex08==2 & kidgeb08<=syear
	replace numdaughter=numdaughter+1 if kidsex09==2 & kidgeb09<=syear
	replace numdaughter=numdaughter+1 if kidsex10==2 & kidgeb10<=syear
	replace numdaughter=numdaughter+1 if kidsex11==2 & kidgeb11<=syear
	replace numdaughter=numdaughter+1 if kidsex12==2 & kidgeb12<=syear
	replace numdaughter=numdaughter+1 if kidsex13==2 & kidgeb13<=syear
	replace numdaughter=numdaughter+1 if kidsex14==2 & kidgeb14<=syear
	replace numdaughter=numdaughter+1 if kidsex15==2 & kidgeb15<=syear
	replace numdaughter=numdaughter+1 if kidsex16==2 & kidgeb16<=syear
	replace numdaughter=numdaughter+1 if kidsex17==2 & kidgeb17<=syear
	replace numdaughter=numdaughter+1 if kidsex18==2 & kidgeb18<=syear
	replace numdaughter=numdaughter+1 if kidsex19==2 & kidgeb19<=syear
	
	gen numson=0
	replace numson=1 if kidsex01==1 & kidgeb01<=syear
	replace numson=numson+1 if kidsex02==1 & kidgeb02<=syear
	replace numson=numson+1 if kidsex03==1 & kidgeb03<=syear
	replace numson=numson+1 if kidsex04==1 & kidgeb04<=syear
	replace numson=numson+1 if kidsex05==1 & kidgeb05<=syear
	replace numson=numson+1 if kidsex06==1 & kidgeb06<=syear
	replace numson=numson+1 if kidsex07==1 & kidgeb07<=syear
	replace numson=numson+1 if kidsex08==1 & kidgeb08<=syear
	replace numson=numson+1 if kidsex09==1 & kidgeb09<=syear
	replace numson=numson+1 if kidsex10==1 & kidgeb10<=syear
	replace numson=numson+1 if kidsex11==1 & kidgeb11<=syear
	replace numson=numson+1 if kidsex12==1 & kidgeb12<=syear
	replace numson=numson+1 if kidsex13==1 & kidgeb13<=syear
	replace numson=numson+1 if kidsex14==1 & kidgeb14<=syear
	replace numson=numson+1 if kidsex15==1 & kidgeb15<=syear
	replace numson=numson+1 if kidsex16==1 & kidgeb16<=syear
	replace numson=numson+1 if kidsex17==1 & kidgeb17<=syear
	replace numson=numson+1 if kidsex18==1 & kidgeb18<=syear
	replace numson=numson+1 if kidsex19==1 & kidgeb19<=syear
	
	gen numchild=numdaughter+numson
	
	gen numchild0to5=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb01<=5 & syear-kidgeb01>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb02<=5 & syear-kidgeb02>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb03<=5 & syear-kidgeb03>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb04<=5 & syear-kidgeb04>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb05<=5 & syear-kidgeb05>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb06<=5 & syear-kidgeb06>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb07<=5 & syear-kidgeb07>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb08<=5 & syear-kidgeb08>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb09<=5 & syear-kidgeb09>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb10<=5 & syear-kidgeb10>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb11<=5 & syear-kidgeb11>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb12<=5 & syear-kidgeb12>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb13<=5 & syear-kidgeb13>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb14<=5 & syear-kidgeb14>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb15<=5 & syear-kidgeb15>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb16<=5 & syear-kidgeb16>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb17<=5 & syear-kidgeb17>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb18<=5 & syear-kidgeb18>=0
	replace numchild0to5=numchild0to5+1 if syear-kidgeb19<=5 & syear-kidgeb19>=0
	replace numchild0to5=. if numchild==.

	
	* Daughter dummy
		
	gen d_daughter=numdaughter > 0 if !missing(numdaughter)
	gen d_son=numson > 0 if !missing(numson)
	sort pid syear
	
	* Cumulative exposure to having a daughter
	bys pid (syear): gen cumexpdaughter = sum(d_daughter)
	
	* Sex of first child (potential IV)
 	gen firstgirl = .
	replace firstgirl=1 if numchild>=1 & kidsex01 == 2
	replace firstgirl=0 if numchild>=1 & kidsex01 == 1
	
	gen secondgirl = .
	replace secondgirl=1 if numchild>=1 & kidsex02 == 2
	replace secondgirl=0 if numchild>=1 & kidsex02 == 1
	
	gen girl_first_only = .
	replace girl_first_only = 1 if numchild == 1 & kidsex01 == 2
	replace girl_first_only = 0 if numchild == 1 & kidsex01 == 1
		
	*Check: 
	*browse pid syear gebjahr sumkids kidgeb01 kidsex01 kidgeb02 kidsex02 numchild* numdau d_dau cumexpd firstgirl girl_first_only if gebjahr>1965
		
	
	* Leftist party preference (SPD, Linke, Greens, Pirates)
	
	tab plh0011_h
	tab plh0012_h
	
	gen leftwing=0
	replace leftwing=1 if plh0012_h==1 |  plh0012_h==5 |  plh0012_h==6 |  plh0012_h==9 |  plh0012_h==16 |  plh0012_h==17 |  plh0012_h==26 
	replace leftwing=. if plh0012_h==-1 | plh0012_h==-4 | plh0012_h==-5
	
	rename plh0011_h partypreference
	recode partypreference (2=0) (-5=.) (-1=.) (-2=.)
	
	
	
	* Gender attitudes (remember that only 5 percent of entire sample)

	foreach var of varlist plh0298_v1 plh0298_v2 plh0301 ///
	plh0302_v1 plh0302_v2 plh0303 plh0304 plh0307 plh0308_v1 plh0308_v2 plh0309 plj062* {
	recode `var' (-8=.) (-5=.) (-1=.)
	}
	
	sum plh0298_v1 plh0298_v2 plh0301 plh0302_v1 plh0302_v2 plh0303 plh0304 plh0307 plh0308_v1 plh0308_v2 plh0309
	
		* Coding reversed
	replace plh0298_v2=8-plh0298_v2
	replace plh0302_v2=8-plh0302_v2
	
	replace plj0623=8-plj0623
	replace plj0624=8-plj0624
	replace plj0625=8-plj0625
	egen genderatt_migrants=rowmean(plj0621-plj0625)
	egen momworkchildnotsuffer=rowmean(plh0298_v2 plh0302_v2)
	
	
	* Time use
	rename pgtatzeit workhours
	recode workhours (-2=0) (-1=.) (-3=.)
	
	rename pli0043_h houseworkhours
	rename pli0044_h childcarehours
	recode houseworkhours childcarehours (-5=.) (-3=.) (-2=.) (-1=.)
	
	gen unpaidhours=houseworkhours+childcarehours
	
	
	* Control variables
	rename sex male
	recode male (2=0)
	
	gen age=syear-gebjahr
	
	gen birthc90plus=0
	replace birthc90plus=1 if gebjahr>=1990 & gebjahr<=2010
	gen birthc80s=0
	replace birthc80s=1 if gebjahr>=1980 & gebjahr<=1989
	gen birthc70s=0
	replace birthc70s=1 if gebjahr>=1970 & gebjahr<=1979
	gen birthc60s=0
	replace birthc60s=1 if gebjahr>=1960 & gebjahr<=1969
	gen birthc50s=0
	replace birthc50s=1 if gebjahr>=1950 & gebjahr<=1959
	gen birthc40s=0
	replace birthc40s=1 if gebjahr>=1940 & gebjahr<=1949
	gen birthc39minus=0
	replace birthc39minus=1 if gebjahr<=1939 & gebjahr>=1880

	recode pgbilzeit (-2=.) (-1=.)
	
	recode germborn (2=0)

	recode corigin (-1=.)

	recode pgfamstd (-3=.) (-1=.)

	gen mar=0
	replace mar=1 if pgfamstd==1
	replace mar=. if pgfamstd==.
	
	recode hghinc(-3=.) (-2=.) (-1=.)
	gen lnhhinc=ln(hghinc+1)
	
	recode pgemplst (-2=.)
	tab pgemplst, gen(pgemplstd)

	gen employed=0
	replace employed=1 if pglfs==11
	replace employed=. if pglfs==. | pglfs==-2
	
	gen eastgerm=.
	replace eastgerm=1 if sampreg==2
	replace eastgerm=0 if sampreg==1

	preserve
	keep if syear==2015
	collapse plh0258_h, by(pid)
	rename plh0258_h relig2015
	recode relig2015 (-1=.)
	sort pid
	save relig2015.dta, replace
	restore
	
	sort pid
	merge pid using relig2015.dta
	tab _merge
	drop _merge
	
	qui tab relig2015, gen(relig2015d)
	
	qui tab syear, gen(syeard)

	

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)
	* IV regression using attitude measured in 2018
	* instrument: sex of first child
	
* H1 GENDER ATTITUDES ESTIMATE: IV regression (sex of first child as instrument)

	ivreg2 plh0298_v2 male birthc* pgbilzeit leftwing mar germborn numchild lnhhinc eastgerm (d_daughter = firstgirl), r first
 
	sum plh0298_v2 plh0302_v2 plh0308_v2 d_daughter firstgirl male birthc* pgbilzeit i.pgemplst mar germborn numchild lnhhinc eastgerm if e(sample)
	
	

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[d_daughter]

		* (ii) standard error
		local se = _se[d_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(N) - e(df_m) - 1
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[d_daughter] / _se[d_daughter]
		local p_value = 2 * ttail((e(N) - e(df_m) - 1), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_d_daughter = _b[d_daughter]
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
