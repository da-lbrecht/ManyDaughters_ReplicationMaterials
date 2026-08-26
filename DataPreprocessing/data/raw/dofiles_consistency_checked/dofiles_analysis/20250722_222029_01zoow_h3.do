********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Blake H. [REDACTED] and [REDACTED] (adapted from template by Tina Wang and David Albrecht)
* Created: 8 July 2025
* Last Updated: 21 July 2025

* ============================
* Setup * 
* ============================


* clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * 
* install all packages you need here, for example:
* ssc install outreg2
* ssc install egenmore

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

disp "$IN"
disp "$DATA"
disp "$OUT"
*/

* ============================
* Data Preparation * 
* ============================

* LOAD AND MERGE DATA * 

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	* hgen contains generated variables on household level from the household interview; use for household net income and state of residence
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgnuts1)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS *
 
***FILTERS HAVE BEEN MOVED AFTER VARIABLES ARE GENERATED TO ACCOMODATE THE HOUSEHOLD STRUCTURE OF THE DATA***


* GENERATE VARIABLES * 

*generate housework outcome variable
foreach var of varlist pli0043_h pli0012_h pli0016_h pli0044_h pli0019_h pli0022_h{
	replace `var' = . if `var'<0
	
}

egen hrs_housework=rowtotal(pli0043_h pli0012_h pli0016_h)
egen hrs_childcare=rowtotal(pli0044_h pli0019_h pli0022_h)

*flag most recent measure of housework/childcare and drop other values
bys pid: egen max_yr_with_housework_data=max(syear) if !mi(hrs_housework)
replace hrs_housework=. if syear!=max_yr_with_housework_data
bys pid: egen max_yr_with_childcare_data=max(syear) if !mi(hrs_childcare)
replace hrs_childcare=. if syear!=max_yr_with_childcare_data

egen hrs_domestic_work=rowtotal(hrs_housework hrs_childcare) if !mi(hrs_housework)|!mi(hrs_childcare)

*clean up birthyear variable
foreach var of varlist kidgeb01-kidgeb19{
	replace `var'=. if `var'<0
}
*generate count of daughters
gen num_daughters=0
gen num_kids=0
foreach n in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19{
replace num_daughters=num_daughters+1 if kidsex`n'==2 & kidgeb`n'<=syear
replace num_kids=num_kids+1 if (kidsex`n'==1|kidsex`n'==2) & kidgeb`n'<=syear
}

*generate covariates

gen r_male = (sex==1)
gen r_age = syear- gebjahr

*generate educational attainment indicators (educ_*)
replace pgpsbil =. if pgpsbil<0
replace pgpbbil01 = . if pgpbbil01<0
replace pgpbbil02 = . if pgpbbil02<0

*impute "Intermediate School Degree" (the most common value of pgpsbil for individuals with non-missing pgpbbil01) to individuals with non-missing vocational degree data but missing educational attainment
replace pgpsbil=2 if mi(pgpsbil)&!mi(pgpbbil01)
*impute "Upper Secondary Degree" (the most common value of pgpsbil for individuals with non-missing pgpbbil02) to individuals with non-missing university degree data but missing educational attainment
replace pgpsbil=4 if mi(pgpsbil)&!mi(pgpbbil02)
tab pgpsbil, gen(r_educ_)
gen mi_educ=mi(pgpsbil)&mi(pgpbbil01)&mi(pgpbbil02)
gen r_educ_vocational=!mi(pgpbbil01)
gen r_educ_academic=!mi(pgpbbil02)
foreach var of varlist r_educ*{
	replace `var'=0 if mi(`var') & mi_educ==1
}

gen r_full_time = (pgemplst==1)
gen r_part_time = (pgemplst==2|pgemplst==4)
gen r_voc_training = (pgemplst==3|pgemplst==6)
gen r_unemployed = (pgemplst==5)
gen mi_work_status = (pgemplst==.|pgemplst<0)


replace pglabgro=. if  pglabgro<0
replace pgsndjob=. if pgsndjob<0
egen indiv_income = rowtotal(pglabgro pgsndjob)
bys hid syear: egen r_hhld_income = total(indiv_income)

replace pgfamstd=. if pgfamstd<0
gen r_married=(pgfamstd==1)
gen r_separated=(pgfamstd==2)
gen r_single=(pgfamstd==3)
gen r_divorced=(pgfamstd==4)
gen r_widowed=(pgfamstd==5)
gen r_spouse_abroad=(pgfamstd==6)
gen r_registered_partnership=(inlist(pgfamstd,7,8))
gen mi_marital_status=mi(pgfamstd)

replace plh0258_h=. if plh0258_h<0
gen r_catholic=(plh0258_h==1)
gen r_other_christian = (inlist(plh0258_h,2,3,7))
gen r_muslim = (inlist(plh0258_h,4,8,9,10))
gen r_other_non_christian = (plh0258_h==5)
gen r_no_affiliation=(plh0258_h==6)
gen mi_religion= (plh0258_h==.)


*identify hhlds with missing data on state of residence
replace hgnuts1=0 if mi(hgnuts1)

*keep partnered male respondenents with children who have non-missing values of hrs_domestic_work
preserve
keep if sex==1 & num_kids!=0 & parid!=-2 & !mi(hrs_domestic_work)
rename parid coupleid
rename hid hid_male
rename pid male_pid
rename hrs_* hrs_*_male
rename r_* rm_*
rename mi_* mim_*
rename num_daughters num_daughters_male
rename num_kids num_kids_male
rename corigin corigin_male
rename hgnuts1 hgnuts1_male
rename phrf phrf_male
tempfile male
save `male', replace
restore

*keep partnered female respondenents with children who have non-missing values of hrs_domestic_work
keep if sex==2 & num_kids!=0 & parid!=-2 & !mi(hrs_domestic_work)
gen double coupleid=pid
rename hid hid_female
rename pid female_pid
rename hrs_* hrs_*_female
rename r_* rf_*
rename mi_* mif_*
rename num_daughters num_daughters_female
rename num_kids num_kids_female
rename corigin corigin_female
rename hgnuts1 hgnuts1_female
rename phrf phrf_female
merge 1:1 coupleid syear using `male', gen(male_match)


egen num_daughters=rowmax(num_daughters_male num_daughters_female)
egen num_kids=rowmax(num_kids_male num_kids_female)




gen total_dom_work=hrs_domestic_work_female+hrs_domestic_work_male if !mi(hrs_domestic_work_female) & !mi(hrs_domestic_work_male)
gen male_share_dom_work=hrs_domestic_work_male/total_dom_work
gen fem_share_dom_work=hrs_domestic_work_female/total_dom_work
gen diff_share_dom_work=male_share_dom_work-fem_share_dom_work


* FILTERS *
 
***FILTERS HAVE BEEN MOVED AFTER VARIABLES ARE GENERATED TO ACCOMODATE THE HOUSEHOLD STRUCTURE OF THE DATA***

*keep only partnered respondenents with children who have non-missing values of hrs_domestic_work
*SEE LINES 161 and 178

*Keep only respondents representing households with one male parter and one female partner
keep if male_match==3 

*confirm that within the analytical sample, states of residence and household ids match within each couple
assert hgnuts1_female==hgnuts1_male
assert hid_female==hid_male

*Keep only coupled households who are not separated couples, divorced couples, widowed couples, or couples with one spouse living abroad
drop if rf_separated==1|rf_divorced==1|rf_widowed==1|rf_spouse_abroad==1|rm_separated==1|rm_divorced==1|rm_widowed==1|rm_spouse_abroad==1

*Keep only respondents with at least one child with known sex when survey was administered
drop if num_kids==0

*country of origin can't be negative for regression—replace negative values with 0 to represent all missing values
replace corigin_male = 0 if corigin_male<0
replace corigin_female = 0 if corigin_female<0


* HYPOTHESIS TEST FOR H3 * 


* Within heterosexual parterned couples with children (i.e., female-male parent pairs where the female's pid matches the male's parid), we calculate total hours of domestic work by summing reported hours allocated to housework (summing over pli0043_h, pli0012_h, and pli0016_h) and childcare (summing over pli0044_h, pli0019_h, and pli0022_h) in a typical week for each partner in a couple. We then convert this sum to a share by dividing by the total hours of domestic work of the couple. We then merge female and male partners into a single observation and calculate our dependent variable diff_share_dom_work by subtracting the female share from the male share of domestic work within each couple. We take this to be a good behavioral measure of gender equality in the household (H3). If H3 is true and having daughters leads to more gender equality in the household, couples will be more likely to have a more equal distribution of domestic work between male and female partners. We estimate the parameter of interest, the coefficient on NumDaughters, from a linear OLS regression analysis with diff_share_dom_work as dependent variable and the count variable num_daughters as independent variable, controlling for demographic characteristics, missing data, country of origin, state of residence, total family size, and survey wave.  Since female partners generally do a much larger share of domestic work within the overall sample (68.1% in our 5% sample), this means that a positive coefficient on num_daughters implies more gender equality until the male share exceeds the female share (which we take to include all or nearly all plausible values of the coefficient). We weight responses using the sum of individual weights within a couple (phrf_2) provided by the SOEP.

	reg diff_share_dom_work num_daughters rf_* rm_* mif_* mim_* i.corigin_male i.corigin_female i.syear i.hgnuts1_male num_kids if num_kids!=0, r


* EXTRACT AND EXPORT RESULTS * 

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[num_daughters]

		* (ii) standard error
		local se = _se[num_daughters]

		* (iii) family of the test statistic (one-sided t-test)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value from one-sided t-test		
		local coef_sign = sign(_b[num_daughters])
		disp `coef_sign'
		test num_daughters
		local p_value = ttail(r(df_r),`coef_sign'*sqrt(r(F)))
		display "H_1: coef<=0  p-value = `p_value'"
		
		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants (to test H3, each observation requires data from two participants)
		quietly levelsof coupleid if e(sample), local(participants)
		local num_participants : word count `participants'
		local num_participants=`num_participants'*2
		
		* (viii) interpretation 
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
