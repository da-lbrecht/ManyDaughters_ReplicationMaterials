********************************************************************************
********************** Do-File H3: #ManyDaughters Project **********************
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
merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches)

label language EN // change language of the labels to English

save "$DATA/h3_dataset.dta", replace 


keep cid hid pid parid syear kidgeb01 pli0044_h pli0019_h pli0022_h pli0043_h pli0012_h pli0016_h

rename pid parid2

rename parid pid

rename parid2 parid

rename pli*_h pli*_hpar

save "$DATA/h3_dataset_par.dta", replace



* ============================
* Analysis
* ============================

use "$DATA/h3_dataset.dta", clear 

merge 1:1 cid hid parid pid kidgeb01 syear using "$DATA/h3_dataset_par.dta", nogen keep(master matches)

sort pid syear

* CODE MISSINGS *

foreach var of varlist kidgeb01 kidgeb02 gebjahr syear piyear pld0133 pli0044_h pli0019_h pli0022_h pli0043_h pli0012_h pli0016_h pli0044_hpar pli0019_hpar pli0022_hpar pli0043_hpar pli0012_hpar pli0016_hpar kidsex01 sex pgpsbil plg0014_v7 plg0014_v6 plg0014_v5 plg0014_v4 kidgeb01 gebjahr plh0258_h migback pld0131_v1 pld0131_v2 pld0131_v3 hghinc plb0022_h plb0186_h plc0446 birthregion_ew pla0011 pla0011 pla0017 pla0020 pla0023 pla0026 pla0029 pla0032 pla0035 pla0038 pla0041 pla0044 {
recode `var' -1=. -2=.a -3=.b -4=.c -5=.d -8=.e
*[-1] No answer -> .
*[-2] Does not apply -> .a
*[-3] Answer improbable/Not valid -> .b
*[-5] Not included in this version of the questionnaire -> .d
*[-8] Question this year not part of survey -> .e
}


* FILTERS * 

gen exclusion=1
//first child was born between the two waves
replace exclusion=0 if kidgeb01>2000 & kidgeb01<2021
//no further kids during this period -> implemented in generating domestic work variables

//(all missings) pld0176 parents who live with the child in the same household

//younger than 18 or older than 42 years of age at the birth of the first child
replace exclusion=1 if (kidgeb01-gebjahr)<18
replace exclusion=1 if (kidgeb01-gebjahr)>42
//(not found) lb0646: Lives Together With Partner
replace exclusion=1 if pld0133!=1


* GENERATE VARIABLES * 
//naming convention for vars: *_t=wave at which the outcomes are measured; *_bl=baseline wave
foreach var of varlist pli0044_h pli0019_h pli0022_h pli0043_h pli0012_h pli0016_h pli0044_hpar pli0019_hpar pli0022_hpar pli0043_hpar pli0012_hpar pli0016_hpar {
gen 	`var'cap=`var'
replace `var'cap=12 if `var'cap>12 & !mi(`var')
}

* DV *
//total domestic work person surveyed at wave t
gen total_childcare=(5*pli0044_hcap+pli0019_hcap+pli0022_hcap) if (syear-kidgeb01)<=18 & !mi(pli0044_hcap) & !mi(pli0019_hcap) & !mi(pli0022_hcap)

gen total_housework=(5*pli0043_hcap+pli0012_hcap+pli0016_hcap) if (syear-kidgeb01)<=18 & !mi(pli0043_hcap) & !mi(pli0012_hcap) & !mi(pli0016_hcap)

gen total_childcare_t=total_childcare if (syear-kidgeb01)>=1 & !mi(kidgeb01) & syear<kidgeb02
foreach i of numlist 1/20 {
replace total_childcare_t=. if pid==pid[_n-`i'] & total_childcare_t[_n-`i']!=.
}

gen total_housework_t=total_housework if (syear-kidgeb01)>=1 & !mi(kidgeb01) & syear<kidgeb02
foreach i of numlist 1/20 {
replace total_housework_t=. if pid==pid[_n-`i'] & total_housework_t[_n-`i']!=.
}

gen total_domesticwork_t=(total_childcare_t + total_housework_t) if !mi(total_childcare_t) & !mi(total_housework_t)

//total domestic work partner at wave t
gen total_childcare_par=(5*pli0044_hparcap+pli0019_hparcap+pli0022_hparcap) if (syear-kidgeb01)<=18 & !mi(pli0044_hparcap) & !mi(pli0019_hparcap) & !mi(pli0022_hparcap)

gen total_housework_par=(5*pli0043_hparcap+pli0012_hparcap+pli0016_hparcap) if (syear-kidgeb01)<=18 & !mi(pli0043_hparcap) & !mi(pli0012_hparcap) & !mi(pli0016_hparcap)

gen total_childcare_tpar=total_childcare_par if (syear-kidgeb01)>=1 & !mi(kidgeb01) & syear<kidgeb02
foreach i of numlist 1/20 {
replace total_childcare_tpar=. if pid==pid[_n-`i'] & total_childcare_tpar[_n-`i']!=.
}

gen total_housework_tpar=total_housework_par if (syear-kidgeb01)>=1 & !mi(kidgeb01) & syear<kidgeb02
foreach i of numlist 1/20 {
replace total_housework_tpar=. if pid==pid[_n-`i'] & total_housework_tpar[_n-`i']!=.
}

gen total_domesticwork_tpar=(total_childcare_tpar + total_housework_tpar) if !mi(total_childcare_tpar) & !mi(total_housework_tpar)

//share domestic work person surveyed at wave t
foreach var of varlist total_domesticwork_t total_domesticwork_tpar total_housework_t total_housework_tpar {
bysort pid: egen `var'fill = total(`var')
}

gen share_domesticwork_t=total_domesticwork_tfill/(total_domesticwork_tfill+total_domesticwork_tparfill) if !mi(total_domesticwork_tfill) & !mi(total_domesticwork_tparfill)

gen share_housework_t=total_housework_tfill/(total_housework_tfill+total_housework_tparfill) if !mi(total_housework_tfill) & !mi(total_housework_tparfill)


* MAIN IV *
//total domestic work person surveyed at baseline wave
gen total_childcare_bl=total_childcare if (syear-kidgeb01)<=(-1) & !mi(kidgeb01) & !mi(pli0044_hcap) & !mi(pli0019_hcap) & !mi(pli0022_hcap)
foreach i of numlist 1/20 {
replace total_childcare_bl=. if pid==pid[_n+`i'] & total_childcare_bl[_n+`i']!=.
}

gen total_housework_bl=total_housework if (syear-kidgeb01)<=(-1) & !mi(kidgeb01) & !mi(pli0043_hparcap) & !mi(pli0012_hcap) & !mi(pli0016_hparcap)
foreach i of numlist 1/20 {
replace total_housework_bl=. if pid==pid[_n+`i'] & total_housework_bl[_n+`i']!=.
}

gen total_domesticwork_bl=(total_childcare_bl + total_housework_bl) if !mi(total_childcare_bl) & !mi(total_housework_bl)

//total domestic work partner at baseline wave
gen total_childcare_blpar=total_childcare_par if (syear-kidgeb01)<=(-1) & !mi(kidgeb01)
foreach i of numlist 1/20 {
replace total_childcare_blpar=. if pid==pid[_n+`i'] & total_childcare_bl[_n+`i']!=.
}

gen total_housework_blpar=total_housework_par if (syear-kidgeb01)<=(-1) & !mi(kidgeb01)
foreach i of numlist 1/20 {
replace total_housework_blpar=. if pid==pid[_n+`i'] & total_housework_bl[_n+`i']!=.
}

gen total_domesticwork_blpar=(total_childcare_blpar + total_housework_blpar) if !mi(total_childcare_blpar) & !mi(total_housework_blpar)

//share domestic work person surveyed at baseline wave
foreach var of varlist total_domesticwork_bl total_domesticwork_blpar total_housework_bl total_housework_blpar {
bysort pid: egen `var'fill = total(`var')
}

gen share_domesticwork_bl=total_domesticwork_blfill/(total_domesticwork_blfill+total_domesticwork_blparfill) if !mi(total_domesticwork_blfill) & !mi(total_domesticwork_blparfill)

gen share_housework_bl=total_housework_blfill/(total_housework_blfill+total_housework_blparfill) if !mi(total_housework_blfill) & !mi(total_housework_blparfill)

//(all missings) pld0175 Sex Of First Child
gen daughter=(kidsex01==2)


* CONTROLS1 *
//year of interview-> dummies for times t and t-1
gen year_t=syear if total_housework_t!=.

gen year_bl=syear if total_housework_bl!=.

foreach var of varlist year_t year_bl {
bysort pid: egen `var'fill = total(`var'), missing
replace `var'=`var'fill
}

//(not found) pl/pla0009_h Sex [harmonised] -> father dummy
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
gen child_age_t=(year_t-kidgeb01)

//(all missings) ple0001 Age Of Person Surveyed
gen age_t=(year_t-gebjahr)


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

gen hhincome_miss=(mi(hghinc))

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


* HYPOTHESIS TEST FOR H3 * 

//having a daughter, parents are more likely to share the domestic and housework tasks equally between mothers and fathers within the same household
//we analyze the change in the time share on domestic and housework before and after having a child within the period of 2000 and 2020
//we generate an index of time spend on domestic work for the respondent and their partner, and then calculate the share for each partner
//we run a OLS regression analysis with either share of domestic work as dependent variable as main independent variable as well as the dummy variable daughter, share of domestic work at baseline wave, and the dummy variable father
//the second and third regressions of Model 1 are robustness checks, regressions of Model 2 are also robustness checks

gl CONTROLS1 "father edu1 edu2 edu3 edu4 edu5 child_age_t age_t year_bl year_t"
gl CONTROLS2 "catholic protestant nonreligious migrant married hhincome_cap hgfhinc fulltime parttime workhours_cap workhours_miss disability"
gl CONTROLS3 "birth_east siblings_female siblings_male"

gl reg1 "reg share_domesticwork_t daughter share_domesticwork_bl $CONTROLS1 if exclusion==0 & syear==year_bl, cluster(hid)"
gl reg2 "reg share_domesticwork_t daughter share_domesticwork_bl $CONTROLS1 $CONTROLS2 if exclusion==0 & syear==year_bl, cluster(hid)"
gl reg3 "reg share_domesticwork_t daughter share_domesticwork_bl $CONTROLS1 $CONTROLS2 $CONTROLS3 if exclusion==0 & syear==year_bl, cluster(hid)"

gl reg4 "reg share_housework_t daughter share_housework_bl $CONTROLS1 if exclusion==0 & syear==year_bl, cluster(hid)"
gl reg5 "reg share_housework_t daughter share_housework_bl $CONTROLS1 $CONTROLS2 if exclusion==0 & syear==year_bl, cluster(hid)"
gl reg6 "reg share_housework_t daughter share_housework_bl $CONTROLS1 $CONTROLS2 $CONTROLS3 if exclusion==0 & syear==year_bl, cluster(hid)"

sum share_domesticwork_* share_housework_* $CONTROLS1 $CONTROLS2 $CONTROLS3 if exclusion==0 & syear==year_bl

//using imputation for reg2 and reg3
mi import wide, imputed(hhincome_cap=hgi1hinc hgi2hinc hgi3hinc hgi4hinc hgi5hinc) clear

//accounting for error 
local i=1
foreach reg in reg1 reg2 reg3 {
capture $`reg'
if _rc != 2000 & _rc != 2001 {
    if `i'==1 | `i'==4 {
	$`reg'
	}
	else {
	mi estimate: $`reg'    
	}
}
}


* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

foreach reg in reg1 /*reg2 reg3 reg4 reg5 reg6*/ {
	
	capture $`reg'
	if _rc != 2000 & _rc != 2001 {
	$`reg'
				
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
	scalar coef_daughter = _b[daughter]
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
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile
	
	local i=`i'+1
}

********************************************************************************

exit
