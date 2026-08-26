********************************************************************************
****************** Code H4: #ManyDaughters Project *****************************
********************************************************************************

* Authors: RT 01ja3t (using the template by Tina Wang, David Albrecht)

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
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

	save "$DATA/h4_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 


* GENERATE VARIABLES * (Please adapt according to your needs)
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
		

***Outcome variables for Hypothesis 4 uses the same outcome variable as H1

tab plh0308_v2

*Agreement to "It's best if the man and the woman work the same amount so they can share the responsibility for taking care of the family and household equally". This was only collected in 2018.
*Higher value indicates stronger support for gender equality.

***Generate new variable that only contains information

gen respshare=.
replace respshare=plh0308_v2 if plh0308_v2 >0
tab respshare
label variable respshare  "Share responsibilty"
label define agree 1 "Disagree completely" 7 "Agree completely"
label values respshare agree


* HYPOTHESIS TEST FOR H4 *
	*** We the effect of having daughters by including a variable that counts the number of daughters.
	***There is a potentical endogenity problem that people with more children are more likly to have more daughters. There are probably unobserved factors that determine the number of children. We will therefore include number of children fixed effects.
	*** We are adding to following control variables: gender, education, log household income, being german(to account for differences in cultural attitudes), age, being married
***For H4 we also include an interaction term 
	
	global control female age education lnhhincome married german

	reg respshare 1.female#c.num_daughter num_daughter $control i.num_children, vce(cluster hid)

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[1.female#c.num_daughter]

		* (ii) standard error
		local se = _se[1.female#c.num_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[1.female#c.num_daughter] / _se[1.female#c.num_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_num_fedaughter = _b[1.female#c.num_daughter]
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
	file open myfile using "$OUT/h4_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
