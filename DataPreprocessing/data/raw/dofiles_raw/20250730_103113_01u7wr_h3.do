********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install csdid, replace
ssc install drdid, replace
ssc install carryforward

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

global niterate=10

************************************************
* PGEN
************************************************
use "$IN/pgen.dta", clear
label language EN
keep hid pid syear pgcasmin cid pgexppt pglabgro pgnation pglfs pglabnet pgtatzeit pgemplst pgexpft pgisced11 pgfamstd 
save "$DATA/pgens.dta", replace

************************************************
* PPATHL
************************************************
use "$IN/ppathl.dta", clear
label language EN
keep hid pid cid syear gebjahr migback arefback germborn migback sex corigin gebmonat immiyear sexor  
save "$DATA/ppathls.dta", replace

************************************************
* PL
************************************************
use "$IN/pl.dta", clear
label language EN
*plj0023
keep hid pid syear cid ///
    plh0173 plh0180  pli0022_h ple0036 ple0014 ple0023 pli0051 pli0010 ple0015 ///
    plh0181 ple0027 ple0028 plh0042 plh0178 plh0035 plh0033 plh0175 ple0020 ple0029 ///
    plh0179 pli0019_h ple0011 pli0046 ple0004 ple0008 pli0044_h ple0005 plj0022 ple0026 ///
    ple0032 ple0035 plh0176 pli0043_h plj0175 pli0012_h ple0021 plh0171 plh0335 ple0034 ///
    ple0017 plh0032 ple0030 plb0022_h plh0038 pli0036 plh0174 ple0033 ple0031 ple0018 ///
    ple0016 pli0016_h plh0182 plh0177 ple0022 ple0019 plb0219 ple0013 plh0336 ple0012 ///
    plh0040 plb0018 plb0037_h plb0241_h plc0010 pld0062_v2 pld0068_v2 pld0083_v2 ///
    plh0033 plh0036 plh0037 plh0038 plh0039 plh0040 plh0162 plh0171 plh0172 plh0174 ///
    plh0176 plh0178 plh0184 plh0185 plh0186 plh0187 plh0189 plh0193 plh0195 plh0204_h ///
    plh0335 pli0043_h pli0046 pli0049_h pli0051 pli0162 plj0566 plj0567 plh0004
drop if hid==.
compress
save "$DATA/pls.dta", replace

************************************************
* HGEN
************************************************
use "$IN/hgen.dta", clear
label language EN
keep hid cid syear hghinc hghmonth
save "$DATA/hgens.dta", replace

************************************************
* BIOBIRTH
************************************************
use "$IN/biobirth.dta", clear
label language EN
keep pid gebjahr cid biovalid sumkids biokids kidsex* kidgeb* // hid
foreach var of var kidgeb*  kidsex* {
	qui replace `var'=. if `var'<0
}
g firstboy=0  // the year of birth of first boy
g firstgirl=0 // the year of birth of first girl
g kidboys=0
g kidgirls=0
foreach var of var kidsex* {
	local zz=(substr("`var'",-2,2))
	replace firstboy=kidgeb`zz' if kidsex`zz'==1 & firstboy==0
	replace firstgirl=kidgeb`zz' if kidsex`zz'==2 & firstgirl==0
	replace kidboys=kidboys+1 if kidsex`zz'==1
	replace kidgirls=kidgirls+1 if kidsex`zz'==2
}
g kidsum=kidboys+kidgirls
label var firstboy "The year of birth of first boy"
label var firstgirl "The year of birth of first girl"
label var kidboys "Sum of sons"
label var kidgirls "Sum of daughters"
label var kidsum "All children"

g hasonlybiokids=(biokids==sumkids)
replace hasonlybiokids=0 if sumkids==0
label var hasonlybiokids "person has all biological children"
g x=.
keep pid firstboy-x
drop x
qui compress
save "$DATA/biobirths.dta", replace

************************************************
** MERGE DATA
************************************************

* Load main file
use "$DATA/pls.dta", clear
* Merge with ppathls
merge 1:1 pid syear using "$DATA/ppathls.dta", keep(match master) nogenerate
* Merge with pgens
merge 1:1 pid syear using "$DATA/pgens.dta", keep(match master) nogenerate
* Merge with household income 
merge m:1 hid syear using "$DATA/hgens.dta", keep(match master) nogenerate
* Merge with biobirths (by pid and cid)
merge m:1 pid using "$DATA/biobirths.dta", keep(match master) nogenerate
sort pid syear
qui compress
label language EN
* Save the final dataset
save "$DATA/0sample_raw.dta", replace

************************************************

use "$DATA/0sample_raw.dta", clear

** Replace all negative numeric values with missing
ds , has(type numeric)
foreach var of varlist `r(varlist)' {
    qui replace `var' = . if `var' < 0
}

** BIOBIRTH - carryforward variables in all years
isid pid syear
sort pid syear
gl list firstboy-hasonlybiokids 
foreach var of var $list {
	by pid: carryforward `var',replace
}

g sexr=sex-1

** GENDER SCORE MODEL - limited set of variables (large sample)
gl regvar_out pgexppt plb0018 pli0043_h pli0046 pli0049_h pli0051
gl regvar_att plh0033  plh0037 plh0040 plh0174 plh0176 plh0178 plh0204_h
di "variables not used:"
d pgexpft plh0039 plh0335 plh0193 plh0195 plh0162 plh0036 plh0038
misstable sum pgexpft plh0039 plh0335 plh0193 plh0195 plh0162 plh0036 plh0038
su sexr $regvar_out 
su sexr $regvar_att

** FILL MISSING VALUES - CREATE SHADOW VARIABLES (80% of cases)
misstable pat $regvar_out
misstable pat $regvar_att
cap drop _x*
*foreach var of var plh0176 plh0174  plh0204_h {
foreach var of var $regvar_out {
	g _x`var'=(`var'==.)
	replace `var'=0 if `var'==.
}
foreach var of var $regvar_att {
	g _y`var'=(`var'==.)
	replace `var'=0 if `var'==.
}

corr $regvar_out
corr $regvar_att

** LOGIT STARTING VALUES 
logit sexr $regvar_out _x*,  iterate($niterate) tolerance(1e-1)
matrix b1=e(b)
** MELOGIT ESTIMATION
melogit sexr $regvar_out _x*  || cid:, from(b1) iterate($niterate) tolerance(1e-1)  startgrid(2) startvalues(iterate($niterate))
predict gender_score1, mu

** LOGIT STARTING VALUES 
logit sexr $regvar_att _y*, iterate($niterate) tolerance(1e-1)
matrix b2=e(b)
** MELOGIT ESTIMATION
melogit sexr $regvar_att _y* || cid:, or  from(b2) iterate($niterate) tolerance(1e-1)  startgrid(2) startvalues(iterate($niterate))
predict gender_score2, mu

/*cap drop gender_score*
melogit sexr $regvar_out _x* || cid:, or  iterate(10)
predict gender_score1, mu
melogit sexr $regvar_att _y* || cid:, or  iterate(10)
predict gender_score2, mu
*/

su gender_sc*
label var gender_score1 "support for gender equality measured by behaviors or outcome"
label var gender_score2 "support for gender equality measured by attitudes"

** POLITICAL ATTITUDES
sort hid pid syear
xtset pid syear
g leftright=plh0004
*bro hid pid syear plh0004 left
carryforward leftright, gen(leftright2)
label var leftright  "political orientation 0=left, 10=right"
label var leftright2 "leftright with carryforward"

isid pid syear
keep pid syear hid cid sex gebjahr germborn sexor hghinc hghmonth pgfamstd pglab* pgempl firstboy-hasonly gender_* leftright*
* Save the final dataset
qui compress
save "$DATA/0sample_clean.dta", replace







* ============================
* Analysis
* ============================

use "$DATA/0sample_clean.dta", clear
sort hid pid syear
	
* FILTERS * (Please adapt according to your needs)
// drop families with no information on birth-year of a child 
drop if firstboy == . | firstgirl == .
// drop family with children born before study period and leave at least one year pre-treatment
su syear
sca surveystart=r(min)
drop if firstgirl <= surveystart & firstgirl != 0  
drop if firstboy  <= surveystart & firstboy  != 0
// keep family in the sample until first son is born
drop if firstboy <= syear & firstboy != 0 
// drop if boy is born before girl
drop if firstboy <= firstgirl & firstboy != 0 & firstgirl != 0 
ta firstgirl firstboy

/// extra filter for H3
/*
The household identifier does not uniquely identify set of parents. So we keep only observations with maximum two parents in one year per household. Each parent may have daughter born in different year, we keep the year of the 'older' daughter.  
*/
gen p =1
bys hid syear: egen np = sum(p)
bys hid: egen mnp = max(np)

drop if mnp > 2

bys hid: egen _min = min(firstgirl)
bys hid: egen _max = max(firstgirl)
g firstgirl2=0
replace firstgirl2=_min if _min~=0
replace firstgirl2=_max if firstgirl2==0
drop _* p np mnp

collapse (mean) gender_score1 firstgirl2 , by(hid syear)


/*
gen n = 1 if firstgirl == 0
bys hid: egen sn = sum(n)
bys hid: egen firstgirl_test = max(firstgirl)
replace firstgirl2 = firstgirl_test if sn != 0 & firstgirl2 == 0
corr firstgirl2 _firstgirl
sort hid pid syear 
bro sex hid pid gebjahr syear first* p np mnp n sn if sn != 0 & firstgirl2 == 0 & firstgirl_test>0
bro sex hid pid gebjahr syear first* p np mnp n sn kids* if sn~=0 //firstgirl_test>0
drop n np mnp
*/

* GENERATE VARIABLES * (Please adapt according to your needs)
/*
The analysis is at household level, so we collapse the dependent variable to household level. and take the year of birth of oldest daughter in the household as treatment period. 
*/
* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)
* H3: Having daughters leads to more gender equality in the household (as measured by behaviors or outcomes).

	* Please add a verbal description of your model here
/*
We use the measure of gender equality 'gender_score1' --- index adopted from Wandschneider et al. (2022) as dependent variable ranging from 0 to 1, with 1 representing a belief in more differentiated gender roles (behavior/outcome) --- as dependent variable.
The model is basic two-way-fixed-effects model (TWFE). Treatment group are parents (pid) with daughter(s). The year starting from the year a girl is born is the treatment period (firstgirl). 
Method: Callaway and Sant'Anna (2020) staggared diff-diff estimator. Control group: never treated and not-yet-treated. We provide the ATT after treatment as main result --- _b[Post_avg]. 
H3: having daughters leads to higher outcomes, i.e. positive coefficient 
*/	
cd "$OUT"

csdid gender_score1, ivar( hid ) time(syear) gvar(firstgirl) notyet saverif(h3_rif_) replace

estat event
csdid_plot
graph export "gr_h3_att.png", replace width(1500)

sca num_obs= e(N)
qui describe using "$OUT\h3_rif_"
sca num_participants = r(N)

estat event, post
matrix b = r(table)
 
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		*local coefficient = _b[Post_avg]
		local coefficient = b[1,2]
		* (ii) standard error
		*local se = _se[Post_avg]
		local se = b[2,2]
		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "z"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		*local df_residual = e(df_r)
		*local df_model = e(df_m)

		* (v) p-value
		local p_value = b[4,2]
		
		* (vi) number of observations
		local num_obs = num_obs

		* (vii) number of participants
		local num_participants = num_participants

		* (viii) interpretation of directionality
		local interpretation "not linearly interpretable"
		/* excerpt from Wandschneider et al.: "the higher the gender score of a participant
		is, the higher the levels of gendered practices and attitudes
		associated with being female are. In line with the original works
		of Pelletier et al., we described the continuum ranging from
		“masculine” (towards zero) to “feminine” (towards 1) gendered
		social practices, with “androgynous” gendered practices inbetween
		the two poles indicating balanced levels of masculine
		and feminine gendered social practices (12)."*/
		

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
