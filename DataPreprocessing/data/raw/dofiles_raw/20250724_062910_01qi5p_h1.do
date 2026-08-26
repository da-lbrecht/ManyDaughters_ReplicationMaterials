********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
* version 19 // indicate the version of stata you are using here //adapted by David Albrecht during reproducibility check
set maxvar 10000 // extend the maximum number of variables to 10,000 //set by David Albrecht during reproducibility check

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
ssc install outreg2

* CONFIGURE FILE PATHS * (Please do not change)
/*
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "C:/Users/ondra/Dropbox/Dokumenty/Věda/[REDACTED]/Many Daughters/29.5.2025 DATA SOEP UPDATE/#ManyDaughters_RT_AnalysisPackage"
}
else {
    global ROOT "`c(pwd)'"
}
global IN "$ROOT/data/raw" // folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" // folder for datasets
global OUT "$ROOT/out" // folder for output (results, tables)


* use "C:\Users\ondra\Dropbox\Dokumenty\Věda\[REDACTED]\Many Daughters\29.5.2025 DATA SOEP UPDATE\#ManyDaughters_RT_AnalysisPackage\data\derived\h1_dataset.dta" *

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
	
		* pl contains all variables from the individual questionnaire - can now use full version *
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches)
	
	* How to get only the key outcome variable *
	
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc)

	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	 * N/A -> would limit already low number of observations. *

* GENERATE VARIABLES * (Please adapt according to your needs)
	
	* Remove Manual Labour (outcome variable) missing values *
	* Job involves hard manual labor 1 = yes, 2, no and 3 -> Does not apply *
	* Result -> Dummy variable 1/0 if Job involves hard manual labor *
	replace plb0085 = . if inlist(plb0085, -1, -2, -8 , 3)
	label variable plb0085 "Job Involves Hard Manual Labor"
	tab plb0085
	replace plb0085 = 0 if plb0085 ==2
	tab plb0085
	
	* Number of Children *
	*  Generate variable counting all children with sex 1 or 2 *
	* Create a variable that is 1 if male or female, else 0, for each child
foreach var of varlist kidsex01-kidsex19 {
    gen temp_`var' = inlist(`var', 1, 2)
}

* Sum across all children to get the count
egen child_count = rowtotal(temp_kidsex*)

* Clean up temporary variables
drop temp_kidsex*
tab child_count
* If all kidsex variables are missing or not 1/2, child_count will be 0

 label variable child_count "Number of Children"
tab2 plb0085 child_count

* Having at least one Female Child * 

* Generate a variable indicating if at least one child is female (value 2) among kidsex01 to kidsex19
* Initialize the indicator variable as 0
gen at_least_one_female = 0

* Set to 1 if any kidsex variable is 2 (female)
foreach var of varlist kidsex01-kidsex19 {
    replace at_least_one_female = 1 if `var' == 2 & at_least_one_female == 0
}


label variable at_least_one_female "Having at least one Female Child"

tab2 kidsex01 at_least_one_female if !missing(plb0085) 
tab2 kidsex02 at_least_one_female if !missing(plb0085) 
tab2 kidsex03 at_least_one_female if !missing(plb0085) 
tab2 kidsex04 at_least_one_female if !missing(plb0085) 
tab2 kidsex05 at_least_one_female if !missing(plb0085) 
tab2 kidsex06 at_least_one_female if !missing(plb0085) 
tab2 kidsex07 at_least_one_female if !missing(plb0085) 
tab2 kidsex08 at_least_one_female if !missing(plb0085) 
tab2 kidsex09 at_least_one_female if !missing(plb0085) 
tab2 kidsex10 at_least_one_female if !missing(plb0085) 
tab2 kidsex11 at_least_one_female if !missing(plb0085) 
tab2 kidsex12 at_least_one_female if !missing(plb0085) 
tab2 kidsex13 at_least_one_female if !missing(plb0085) 
tab2 kidsex14 at_least_one_female if !missing(plb0085) 
tab2 kidsex15 at_least_one_female if !missing(plb0085) 
tab2 kidsex16 at_least_one_female if !missing(plb0085) 
tab2 kidsex17 at_least_one_female if !missing(plb0085) 
tab2 kidsex18 at_least_one_female if !missing(plb0085) 
tab2 kidsex19 at_least_one_female if !missing(plb0085) 

* Respondent gender *
tab sex
tab sex, gen(sex)
tab sex2
label variable sex2 "Female"

* Interaction Female * Respondent with at least one Daughter ---> Important for H4 as it is just modification of H1 *
tab sex2 
tab at_least_one_female
tab plb0085
gen interaction_F_D = .
replace interaction_F_D = sex2 * at_least_one_female 
tab interaction_F_D
label variable interaction_F_D "Female * At least One Daughter"

* VARIABLE FEMALE - RENAME *
gen d_daughter = .
replace d_daughter = at_least_one_female
label variable d_daughter "Having at least one Female Child"


* Respondent Years of Education *
tab pgbilzeit
gen education_years = .
replace education_years = pgbilzeit
replace education_years = . if inlist(pgbilzeit, -1, -2)
tab education_years
label variable education_years "Years of Education"

* Household Income *
tab hghinc if hghinc < 0
tabstat hghinc, stat(min sd max n)
gen household_income = .
replace household_income = hghinc
replace household_income = . if inlist(hghinc, -1, -2, -3)
label variable education_years "Household Income (EUR)"
tabstat household_income, stat(min sd max n)

* Respondent age *
tab bioage
gen age = .
replace age = bioage
replace age = . if inlist(bioage, -1, -2, -3)
label variable age "Respondent Age"
tabstat age, stat(min sd max n)

* Marital Status *
tab pgfamstd
replace pgfamstd = . if inlist(pgfamstd, -1, -2, -3)
tab pgfamstd , gen(pgfamstd)


tab at_least_one_female if !missing(plb0085)
* For our sample, the variable is clean, i.e., there are no missing values *
* Nationality *
* Remove missings, add a set of variables for the nationality of the respondent, controlling for the national and cultural values *
tab pgnation
replace plb0085 = . if pgnation == -1 
tab pgnation , gen(pgnation)


* We would love to test the presence of the parents/children in the household, searched for the SOEP relevant variable and the only relevant (which we do not have) was the bfkinhh – Member of Household. In this way we cannot control for the situation if the respondents live with their children in common household or not *

* HYPOTHESIS TEST FOR H1 * 

* Our model would test whether the the proxy identified in some of the studies (provided as example) would be deteremined by the controlled by the selected household and family characterstics, especially by the especially, income, age, education, nationality (and origin) and marital status. We also control for the survey year. 
* The perfect dependent variable would be according to our search of the prior literature and SOEP data variable from the plh set of data (https://paneldata.org/soep-core/datasets/bjp/bjp_166_04) plh0395i01, plh0395i02, plh0395i03, plh0395i04, plh0395i05, plh0395i06, especially "Sex assignment is not so important for a person’s development", which we do not have available - and we do not know why *      
* Alternatively, it would make sense to use of the proxy variables from Educational Goals (edgoal) set of variables (https://paneldata.org/soep-core/topics?open=topic_edg), specifically from the qualities and abilities, such as "behaves like a normal girl / boy, works toward achieving his/her goals and others. However, again this set of variables is not available in the dataset we got, so we cannot make use of those.  *    

	* Therefore, we have have to assume that having a job that requires hard manual labor (plb0085) is a good measure of support for gender equality (hypothesis 1), 
	* if having a job with less manual labor, the respondent is more likely to support gender equality.
	* Accordingly, we run a linear OLS regression analysis with plb0085 as dependent variable and the dummy variable d_daughter as independent variable *
	
	
* Correct Specification would be Logit/Probit, but the extraction and exporting code is set up to export and local OLS results, which is a reason, why we decided to stay with the OLS linear model * 
	*logit plb0085 age sex2 education_years household_income d_daughter pgfamstd1 pgfamstd2 pgfamstd3 pgfamstd4 pgfamstd5 pgfamstd6 pgfamstd7 pgfamstd8 pgnation2 pgnation3 pgnation4 pgnation5 pgnation6 pgnation7 pgnation8 pgnation9 pgnation10 pgnation11 pgnation12 pgnation13 pgnation14 pgnation15 pgnation16 pgnation17 pgnation18 pgnation19 pgnation20 pgnation21 pgnation22 pgnation23 pgnation24 pgnation25 pgnation26 pgnation27 pgnation28 pgnation29 pgnation30 pgnation31 pgnation32 pgnation33 pgnation34 pgnation35 pgnation36 pgnation37 pgnation38 pgnation39 pgnation40 pgnation41 pgnation42 pgnation43 pgnation44 pgnation45 pgnation46 pgnation47 pgnation48 pgnation49 pgnation50 pgnation51 pgnation52 pgnation53 pgnation54 pgnation55 pgnation56 pgnation57 pgnation58 pgnation59 pgnation60 pgnation61 pgnation62 pgnation63 pgnation64 pgnation65 pgnation66 pgnation67 pgnation68 pgnation69 pgnation70 pgnation71 pgnation72 pgnation73 pgnation74 pgnation75 pgnation76 pgnation77 pgnation78 pgnation79 pgnation80 pgnation81 pgnation82 *
	
	reg plb0085 d_daughter sex2 age education_years household_income i.pgnation i.pgfamstd i.syear

	
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

* reg plb0085 d_daughter i.syear *

* Extract statistics
local coefficient = _b[d_daughter]
local se = _se[d_daughter]
local test_stat_family "t"
local df_residual = e(df_r)
local df_model = e(df_m)
local tstat = _b[d_daughter]/_se[d_daughter]
local p_value = 2*ttail(e(df_r), abs(`tstat'))
local num_obs = e(N)

* Number of participants (assuming 'pid' is participant ID)
quietly levelsof pid if e(sample), local(participants)
local num_participants : word count `participants'

* Interpretation of directionality
local interpretation = "negative coefficient is in line with hypothesis"

* Display results
di "Coefficient: `coefficient'"
di "Standard Error: `se'"
di "Test Statistic Family: `test_stat_family'"
di "Residual Degrees of Freedom: `df_residual'"
di "Model Degrees of Freedom: `df_model'"
di "p-value: `p_value'"
di "Number of observations: `num_obs'"
di "Number of participants: `num_participants'"
di "Interpretation of directionality: `interpretation'"

* Export results to csv
file open myfile using "$OUT/h1_results.csv", write replace
file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
file write myfile "`coefficient',`se',`test_stat_family',`df_residual',`df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
file close myfile


********************************************************************************

exit
