********************************************************************************
********************** Do-File H4: #ManyDaughters Project **********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 16.1 
set maxvar 10000 

* INSTALL PACKAGES * 

cap which outreg2
* if _rc ssc install outreg2

* CONFIGURE FILE PATHS * 
/*

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
//syear piyear - Erhebungsjahr
	* biobirth contains information on fertility history
merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
//bioyear - Erhebungsjahr
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
//syear pgpiyear - Erhebungsjahr
	* pl contains all variables from the individual questionnaire
merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
//syear iyear - Erhebungsjahr
	* hgen contains generated variables on household level from the household interview; use for household net income
merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) //keepusing(hid syear hghinc)

label language EN // change language of the labels to English

save "$DATA/h4_dataset.dta", replace 


* ============================
* Analysis
* ============================

use "$DATA/h4_dataset.dta", clear 

sort pid syear

* CODE MISSINGS *

foreach var of varlist kidgeb01 kidgeb02 gebjahr syear piyear plh0298_v1 plh0302_v1 plh0308_v1 plh0298_v2 plh0302_v2 plh0308_v2 kidsex01 sex pgpsbil plg0014_v7 plg0014_v6 plg0014_v5 plg0014_v4 kidgeb01 gebjahr plh0258_h migback pld0131_v1 pld0131_v2 pld0131_v3 hghinc plb0022_h plb0186_h plc0446 birthregion_ew pla0011 pla0011 pla0017 pla0020 pla0023 pla0026 pla0029 pla0032 pla0035 pla0038 pla0041 pla0044 {
recode `var' -1=. -2=.a -3=.b -4=.c -5=.d -8=.e
*[-1] No answer -> .
*[-2] Does not apply -> .a
*[-3] Answer improbable/Not valid -> .b
*[-5] Not included in this version of the questionnaire -> .d
*[-8] Question this year not part of survey -> .e
}


* FILTERS * 

gen exclusion=1
//first child was born between the two waves 2012 and 2018
replace exclusion=0 if kidgeb01>2012 & kidgeb01<2018
//no further kids during this period
replace exclusion=1 if kidgeb02>=2012 & kidgeb02<=2017
//(all missings) pld0176 parents who live with the child in the same household at the time the 2018 survey was conducted  

//younger than 18 or older than 42 years of age at the birth of the first child
replace exclusion=1 if (kidgeb01-gebjahr)<18
replace exclusion=1 if (kidgeb01-gebjahr)>42

//alternative exclusion if e(sample)<400
gen 	exclusion2=0
replace exclusion2=1 if (kidgeb01-gebjahr)<18
replace exclusion2=1 if (kidgeb01-gebjahr)>42


* GENERATE VARIABLES * 
//naming convention for vars: *_t=wave at which the outcomes are measured; *_bl=baseline wave

* DV *
gen gender_equality_attitudes_t=(16 - plh0298_v2 - plh0302_v2 + 2*plh0308_v2)

egen gender_equality_attitudes_tstd=std(gender_equality_attitudes_t)

foreach var of varlist gender_equality_attitudes_t gender_equality_attitudes_tstd {
foreach i of numlist 1/6 {
replace `var'=`var'[_n+`i'] if pid==pid[_n+`i'] & `var'[_n+`i']!=. & syear==2012
}
}

* MAIN IV *
//plh0301 Women Should Rather Care About Family Than Career
//plh0303 Men Involved in Housework
//plh0304 Children Suffer When Father Focused On Career
//plh0307 Men Care more About Financial Security
//plh0309 Working Mothers Equal Emotional
gen gender_equality_attitudes_bl=(10 - 2*plh0308_v1 + plh0298_v1 + plh0302_v1 )
egen gender_equality_attitudes_blstd=std(gender_equality_attitudes_bl)

gen gender_equality_attitudes_std=gender_equality_attitudes_blstd if syear==2012 & kidgeb01<2012 & kidgeb02>2012
replace gender_equality_attitudes_std=gender_equality_attitudes_tstd if syear==2018 & kidgeb01<2018 & kidgeb02>2018

//(all missings) pld0175 Sex Of First Child
gen daughter=(kidsex01==2)

* CONTROLS1 *
//(not found) pl/pla0009_h Sex [harmonised]
gen father=(sex==1)

//(almost all missings) plg0037 School-leaving degree
gen 	edu5=(!mi(plg0014_v7))
replace edu5=1 if plg0014_v6>=1 & plg0014_v6<=5 & mi(plg0014_v7)
replace edu5=1 if plg0014_v5>=1 & plg0014_v5<=5 & mi(plg0014_v7) & mi(plg0014_v6)
replace edu5=1 if plg0014_v4>=1 & plg0014_v4<=5 & mi(plg0014_v7) & mi(plg0014_v6) & mi(plg0014_v5)
replace edu5=1 if pid==pid[_n-1] & edu5[_n-1]==1

foreach i of numlist 1/4 {
gen edu`i'=(pgpsbil==`i')
replace edu`i'=1 if pid==pid[_n-1] & edu`i'[_n-1]==1
replace edu`i'=0 if edu5==1
}
replace edu1=0 if edu2==1 | edu3==1 | edu4==1
replace edu2=0 if edu3==1 | edu4==1
replace edu3=0 if edu4==1

//(all missings) pld0174 Birth Year Of First Child –> age child at wave t=t-pld0174
gen child_age=(syear-kidgeb01) if syear>=kidgeb01

gen child_age_t=(2018-kidgeb01) if syear==2012

//(all missings) ple0001 Age Of Person Surveyed
gen age=(syear-gebjahr)

gen age_t=(2018-gebjahr) if syear==2012

//year of interview-> dummies for times t and t-1
gen year=syear 


* CONTROLS2 *
//plh0258_h Church, Religion [harmonised]
gen catholic=(plh0258_h==1)
gen protestant=(plh0258_h==2)
gen nonreligious=(plh0258_h==6)

foreach i of numlist 1/3 { //Religion alle 4 Jahre abgefragt
	local j=`i'-2
	local l=`i'-3
replace catholic=1 if pid==pid[_n-`i'] & plh0258_h[_n-`i']==1  & !mi(plh0258_h[_n-`i']) & mi(plh0258_h)
replace protestant=1 if pid==pid[_n-`i'] & plh0258_h[_n-`i']==2 & !mi(plh0258_h[_n-`i']) & mi(plh0258_h)
replace nonreligious=1 if pid==pid[_n-`i'] & plh0258_h[_n-`i']==6 & !mi(plh0258_h[_n-`i']) & mi(plh0258_h)
}
replace catholic=0 if catholic==1 & (protestant==1 | nonreligious==1) & pid==pid[_n-1] & catholic[_n-1]==0
replace protestant=0 if protestant==1 & (catholic==1 | nonreligious==1) & pid==pid[_n-1] & protestant[_n-1]==0
replace nonreligious=0 if nonreligious==1 & (catholic==1 | protestant==1) & pid==pid[_n-1] & nonreligious[_n-1]==0

//migback Migration background
gen migrant=(migback==2 | migback==3)

//(not found) urban/rural

//(not found) pld0131_h 
gen 	married=(pld0131_v1==1 | pld0131_v1==2 | pld0131_v1==6) if syear>=1984 & syear<=2018
replace married=(pld0131_v2==1 | pld0131_v2==2 | pld0131_v2==6) if syear==2019
replace married=(pld0131_v3==1 | pld0131_v3==2 | pld0131_v3==6) if syear==2020
replace married=1 if married[_n-1]==married[_n+1] & married[_n-1]==1

//(not found) hlc0005_v2 Monthly HH net income (Euro) [2002-2023] 
sum hghinc, d
gen 	hhincome_cap=hghinc
replace hhincome_cap=`r(p99)' if hhincome_cap>`r(p99)' & !mi(hhincome_cap)

//plb0022_h Erwerbsstatus [harmonised]
gen 	fulltime=(plb0022_h==1)
replace fulltime=1 if plb0022_h[_n-1]==1 & plb0022_h[_n-1]==plb0022_h[_n+1] & pid[_n-1]==pid[_n+1]
gen 	parttime=(plb0022_h==2 | plb0022_h==4)
replace parttime=1 if (plb0022_h[_n-1]==2 | plb0022_h[_n-1]==4) & plb0022_h[_n-1]==plb0022_h[_n+1] & pid[_n-1]==pid[_n+1]

//plb0186_h Actual Weekly Working Hours [harmonised]
sum plb0186_h, d
gen 	workhours_cap=plb0186_h
replace workhours_cap=((workhours_cap[_n-1]+workhours_cap[_n+1])/2) if mi(workhours_cap) & !mi(workhours_cap[_n-1]) & !mi(workhours_cap[_n+1]) & pid[_n-1]==pid[_n+1]
replace workhours_cap=0 if mi(workhours_cap)
replace workhours_cap=`r(p99)' if workhours_cap>=`r(p99)'

gen workhours_miss=(mi(plb0186_h))

//plc0446 disability
gen disability=(plc0446==1)


* CONTROLS3 *
//birthregion_ew Birth place: German Federal Land (East-West Version)
gen birth_east=(birthregion_ew==22)

//(not found) lb1190 Mother's employment status, when respondent was under 6 years old 

//(all missings) lb0067 No. Of Years Living With Single Mother

//pla0017-pla0044 -- X. Sibling, Sex
gen 	siblings_female=0 
replace siblings_female=1 if pla0017==2
replace siblings_female=2 if pla0020==2
replace siblings_female=3 if pla0023==2
replace siblings_female=4 if pla0026==2
replace siblings_female=5 if pla0029==2
replace siblings_female=6 if pla0032==2
replace siblings_female=7 if pla0035==2
replace siblings_female=8 if pla0038==2
replace siblings_female=9 if pla0041==2
replace siblings_female=10 if pla0044==2

gen siblings_male=0 
replace siblings_male=1 if pla0017==1
replace siblings_male=2 if pla0020==1
replace siblings_male=3 if pla0023==1
replace siblings_male=4 if pla0026==1
replace siblings_male=5 if pla0029==1
replace siblings_male=6 if pla0032==1
replace siblings_male=7 if pla0035==1
replace siblings_male=8 if pla0038==1
replace siblings_male=9 if pla0041==1
replace siblings_male=10 if pla0044==1


* HYPOTHESIS TEST FOR H4 * 

//having a daughter, the respondent is more likely to support gender equality (having more progressive attitudes on working mothers and division of family responsibility)
//the effect is greater on fathers than on mothers
//we analyze the change in attitude before and after having a child within the period of 2012 and 2018
//we generate a gender equality attitudes index from plh0308_v[1,2] plh0298_v[1,2] plh0302_v[1,2] and then standardize
//we run a OLS regression analysis with gender equality attitude index 2018 as dependent variable and interaction between dummy variables daughter and father as main independent variable as well as the dummy variable daughter, gender equality attitude index 2012, and the dummy variable father (Model 1)
//should Model 1 result in insufficient observations, we run Model 2 where we analyze gender equality attitudes in the wave immidiatly followed by having a child
//the second and third regressions are robustness checks for Model 1

gl CONTROLS1 "edu1 edu2 edu3 edu4 edu5 child_age_t age_t year"
gl CONTROLS2 "catholic protestant nonreligious migrant married hhincome_cap hgfhinc fulltime parttime workhours_cap workhours_miss disability"
gl CONTROLS3 "birth_east siblings_female siblings_male"

gl reg1 "reg gender_equality_attitudes_tstd daughter##father gender_equality_attitudes_blstd $CONTROLS1 if exclusion==0 & syear==2012, robust"
gl reg2 "reg gender_equality_attitudes_tstd daughter##father gender_equality_attitudes_blstd $CONTROLS1 $CONTROLS2 if exclusion==0 & syear==2012, robust"
gl reg3 "reg gender_equality_attitudes_tstd daughter##father gender_equality_attitudes_blstd $CONTROLS1 $CONTROLS2 $CONTROLS3 if exclusion==0 & syear==2012, robust"

//if e(sample)<400 
gl CONTROLS1b "edu1 edu2 edu3 edu4 edu5 child_age age year"

gl reg1b "reg gender_equality_attitudes_std daughter##father $CONTROLS1b if exclusion2==0, robust"
gl reg2b "reg gender_equality_attitudes_std daughter##father $CONTROLS1b $CONTROLS2 if exclusion2==0, robust"
gl reg3b "reg gender_equality_attitudes_std daughter##father $CONTROLS1b $CONTROLS2 $CONTROLS3 if exclusion2==0, robust"

sum gender_equality_attitudes_tstd gender_equality_attitudes_blstd $CONTROLS1 $CONTROLS2 $CONTROLS3 if exclusion==0 & syear==2012
sum gender_equality_attitudes_std $CONTROLS1b $CONTROLS2 $CONTROLS3 if exclusion2==0 & gender_equality_attitudes_std!=.

//using imputation for reg2 and reg3
mi import wide, imputed(hhincome_cap=hgi1hinc hgi2hinc hgi3hinc hgi4hinc hgi5hinc) clear

//accounting for error 
local i=1
foreach reg in reg1 reg2 reg3 {
capture $`reg'
if _rc != 2000 & _rc != 2001 {
    if `i'==1 {
	$`reg'
	}
	else {
	mi estimate: $`reg'    
	}
}
}

capture $reg1
if e(sample)<400 {
$reg1b 
mi estimate: $reg2b
mi estimate: $reg3b 
}

	
* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

local i=1
foreach reg in reg1 /*reg2 reg3 reg1b reg2b*/ reg3b {
	
capture $`reg'
if _rc != 2000 & _rc != 2001 {
	if `i'==1 {
		$`reg'
	}
	else {
		mi estimate, saving(results, replace): $`reg' 
		estimates use results
	}
		
	* (i) coefficient estimate
	local coefficient = _b[1.daughter#1.father]

	* (ii) standard error
	local se = _se[1.daughter#1.father]

	* (iii) family of the test statistic (e.g. z, t, F, Chi²)
	local test_stat_family = "t"

	* (iv) residual and model degrees of freedom (DFs) if applicable
	local df_residual = e(df_r)
	local df_model = e(df_m)

	* (v) p-value
	local p_value = _b[1.daughter#1.father] / _se[1.daughter#1.father]
	local p_value = 2 * ttail(e(df_r), abs(`p_value'))

	* (vi) number of observations
	local num_obs = e(N)

	* (vii) number of participants
	quietly levelsof pid if e(sample), local(participants)
	local num_participants : word count `participants'

	* (viii) interpretation of directionality
	scalar coef_daughter = _b[1.daughter#1.father]
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
	}
		
* Export results to csv 
capture $reg1
if `i'==1 & _rc != 2000 & _rc != 2001 {
	file open myfile using "$OUT/h4_results.csv", write replace
}
else {
	file open myfile using "$OUT/h4_results.csv", write replace
}
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile
	
local i=`i'+1
}

********************************************************************************

exit
