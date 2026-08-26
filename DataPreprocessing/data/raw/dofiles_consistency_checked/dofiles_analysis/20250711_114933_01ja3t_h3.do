********************************************************************************
****************** Code H3: #ManyDaughters Project *****************************
********************************************************************************

* Authors: Authors: RT 01ja3t (using the template by Tina Wang, David Albrecht)

* ============================
* Setup * 
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:
ssc install outreg2

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

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	*keep if inlist(pop,1,2)
***Drop if No Answer to at least one children
drop if kidpnr01==-1
drop if kidpnr02==-1
drop if kidpnr03==-1

* GENERATE VARIABLES * 
	* Example: generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* For illustrative purposes, we will generate the variable randomly:
	*set seed 123
	*gen d_daughter = runiformint(0,1)
	
	forval i = 1/9 {
		replace kidpnr0`i'=. if kidpnr0`i' ==-2
		
	}
	
	forval i = 10/19 {
		replace kidpnr`i'=. if kidpnr`i' ==-2
		
	}

 
	
* GENERATE VARIABLES *
*^^Generate variable for the number of daughters
***Count the number of children	
	egen num_children=rownonmiss(kidpnr01 kidpnr02 kidpnr03 kidpnr04 kidpnr05 kidpnr06 kidpnr07 kidpnr08 kidpnr09 kidpnr10 kidpnr11 kidpnr12 kidpnr13 kidpnr14 kidpnr15 kidpnr16 kidpnr17 kidpnr18 kidpnr19)
	tab num_children
	label variable num_children "Number of children"
***Count the number of daughters	
	egen num_daughter = anycount(kidsex01-kidsex19), value(2)
	tab num_daughter
	label variable num_daughter "Number of daughters"
	
	
***Generate variable that is 1 if people have at least one daughter
    gen d_daughter=1 if num_daughter>=1 & num_daughter!=.
	replace d_daughter=0 if num_daughter==0
	tab d_daughter
	label variable d_daughter "1 if person has at least one daughter"
	
***Clean and generate control variables

		*Age
		gen age= syear-gebjahr
		tab age, mis
		label variable age "age that someone turns the year of the survey"
		drop if age<18
		
		*Female
		tab sex, mis
		gen female=1 if sex==2
		replace female=0 if sex==1
		label variable female "1 if person is female"
		
		*Years of education
		tab pgbilzeit
		replace pgbilzeit=. if pgbilzeit==-1
		replace pgbilzeit=. if pgbilzeit==-2
		rename pgbilzeit education
		
		* log household income
		tab hghinc
		replace hghinc=. if hghinc==-1
		replace hghinc=. if hghinc==-2
		replace hghinc=. if hghinc==-3
		gen lnhhincome=ln(hghinc)
		label variable lnhhincome "Ln household income"
				
		*Married
		tab pgfamstd
		gen married=.
		replace married=1 if pgfamstd==1|pgfamstd==2|pgfamstd==6|pgfamstd==7|pgfamstd==8
		replace married=0 if pgfamstd==3|pgfamstd==4|pgfamstd==5
		label variable married "Married or Registered Civil Partnership"

		*German citizens
		tab plj0014_v3, mis
		generate german=1 if plj0014_v3==1
		replace german=0 if plj0014_v3==2
		replace german=. if plj0014_v3==-5
		
		
*** Outcome variable: based on how equal housework is shared

*** STEP 1: Time spent on housework by individual
	
***Time spent housework for the full week

tab pli0012_h 
tab pli0016_h 
tab pli0043_h

gen housework_sat=.
replace housework_sat=pli0012_h if pli0012_h>=0
label variable housework_sat "hours spent on housework saturday"

gen housework_sun=.
replace housework_sun=pli0016_h if pli0016_h>=0
label variable housework_sun "hours spent on housework sunday"

gen housework_weekday=.
replace housework_weekday=pli0043_h if pli0043_h>=0
label variable housework_weekday "hours spent on housework weekdays"

gen housework_total=housework_weekday*5 + housework_sat + housework_sun
label variable housework_total "total time spent on housework"

tab housework_total 
drop if housework_total>100


*** We need a column that shows the partners time spent doing housework in the pid row. Create a separate partner dataset to merge partners to pids.

***save current dataset
save "$DATA/h3_dataset_hh.dta", replace

***keep cid hid pid parid houswork variable created above, partner id, number of children and number of daughters, and controls

global control female age education lnhhincome married german

keep cid pid hid parid partner syear housework_total num_daughter num_children $control

***rename variables for partner

foreach var of varlist housework_total num_daughter num_children female age education lnhhincome married german {
rename `var' `var'_partner
}

drop if parid==-2
*This drops respondents without a partner 

drop pid
rename parid pid

* Pid will be used to merge the partners (parid) to the pid in the hh dataset

save "$DATA/h3_dataset_partner.dta", replace

***merge the variable with housework data to original dataset with hid, pig syear.

use "$DATA/h3_dataset_hh.dta", replace

merge 1:1 hid pid syear using "$DATA/h3_dataset_partner.dta"

***Only keep if partners could be matched

drop if _merg!=3
*This drops anyone with a parid that was no match to the pids from hh dataset 


drop if female==female_partner
*This drops samesex couples, irrelevant as we analyze gender equality

save "$DATA/h3_dataset_hh.dta", replace

***Outcome variable, STEP 2: Calculate how equal housework is distributed

***time spend on housework by both partners

gen housework_total_couple=housework_total+housework_total_partner
tab housework_total_couple, mis
label variable housework_total_couple "total hours of housework a week both partners"
drop if housework_total_couple==0
*Drop couples that do not do any housework (might be other person in household or paid housework)

***equal allocation of housework

gen housework_propdiff = (abs(housework_total-housework_total_partner))/housework_total_couple
label variable housework_propdiff "Difference in housework proprotion between respondent and partner"
*difference in housework hours between partners divided by couple housework hours (absolute value)
*resulting variables takes value between 0 and 1
* 0 means both partners share housework equally, 1 means one partner does all the housework_proportion
* A value of 0.5 implies proportions of 75% to 25%

gen housework_equality = 1-housework_propdiff
*reverse code this variable, so that higher value means more equal housework (still between 0 and 1)

***Main explanatory variable on household level

*Potential issue: Partners have different number of daughters (and children)
*Solution: Use max. number between partners as also step-daughters might influence behavior

egen num_daughter_hh=rowmax(num_daughter num_daughter_partner)
egen num_children_hh=rowmax(num_children num_children_partner)

***Control variables on household level

gen mean_age_hh= (age+age_partner)/2
gen mean_education_hh = (education+education_partner)/2
gen german_share_hh = (german+german_partner)/2
*average age and education of couple, share of german citizens in household

*household income is already measured on household level

drop if married!=married_partner
*drop cases were current partner is married to someone else

***Reducing dataset to one observation per households

global control mean_age_hh mean_education_hh lnhhincome married german_share_hh

sum housework_equality num_daughter_hh num_children_hh $control if female==1
sum housework_equality num_daughter_hh num_children_hh $control if female==0
*the dataset as constructed contains a separate observation for each partner
*values for both partners are the same
drop if female==0
*we drop one partner (male partner) to remove these duplicates

* ============================
* Analysis
* ============================

*** HYPOTHESIS TEST FOR H3 ***
* In hypothesis 3 we are working on household level. This means that all variables are on the household level.
*For number of daughters and number of children we are using the maximum number that a couple has. So if a couple has two joint children and the wife has two additional children, we will use four as the number of children. The same applies to daughters.
*As before, there is a potentical endogenity problem that people with more children are more likly to have more daughters. There are probably unobserved factors that determine the number of children. We will therefore include number of children fixed effects. Here this is the maximum number of children fixed effect.
*Pooled panel regression with clustered SEs as households may have answered multiple times

	reg housework_equality num_daughter_hh $control i.num_children_hh i.syear, vce(cluster cid)
	

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[num_daughter_hh]

		* (ii) standard error
		local se = _se[num_daughter_hh]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[num_daughter_hh] / _se[num_daughter_hh]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_num_daughter = _b[num_daughter_hh]
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
