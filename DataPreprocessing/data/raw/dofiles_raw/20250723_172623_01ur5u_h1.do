********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

*clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2
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

* LOAD AND MERGE DATA * (Please adapt according to your needs)

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset
	keep cid pid hid syear parid partner sex gebjahr psample sampreg germborn loc1989 germborninfo sexor birthregion birthregion_ew piyear
	//KW: person gender, nationality, location
	
	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) //Keep all variables
	//KW has kid id, gender, year of birth, month of birth for kids 1-19
	//To-do Could create "prior kids" var here, or after reshape
	
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	//merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)
	//KW - pgen has more convenient versions of pl variables.  Education, income.
	
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(plj0175 plj0014_v3 plh0395i04 plh0395i05 plh0333 pli0019_h pli0022_h pli0044_h pli0012_h pli0016_h pli0043_h plh0258_h plg0267 plb0471_h)
	//plj0175: Both Parents Born In Germany
    //plj0014_v3: German citizenship (yes/no)
	//plh0395i05 //Schools and kindergartens should encourage children to engage in activities that are not typical
	//plh0333  //Political Party - asked 2017 only
	//plh0395i04: "The assignment of gender is not important to the defvelopment of a person--2019 only"
	
		gen pol_party = . if plh0333 <0
		replace pol_party = . if plh0333==28|plh0333==29 //did not vote/ineleigible
		replace pol_party = 1 if plh0333==30|plh0333==27|plh0333==7|plh0333==8 //Far right
		replace pol_party = 2 if plh0333==2|plh0333==3|plh0333==10|plh0333==14|plh0333==15 //Center Right
		replace pol_party = 3 if plh0333==1|plh0333==4|plh0333==11 //Center Left
		replace pol_party = 4 if plh0333==5|plh0333==6|plh0333==9|plh0333==16 //Far Left
		egen pol_party_all = max(pol_party), by(pid)
		
	//pli0019_h pli0022_h pli0044_h pli0012_h pli0016_h pli0043_h House work variables
		foreach var in pli0019_h pli0022_h pli0044_h pli0012_h pli0016_h pli0043_h {
			replace `var' = . if `var' < 0 //replace missing with blank
			replace `var' = 16 if `var' > 16 //less than 1% of data reports > 16hrs/day.  
		}
		gen total_hours = pli0019_h + pli0022_h + pli0012_h +pli0016_h + 5*(pli0044_h + pli0043_h)
		
	//plh0258_h Religious affiliation
	//plg0267 Ed attainment 
	//plb0471_h prior gross income
		
		* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgnuts1)

	
	label language EN // change language of the labels to English
	//hghinc - monthly income.  Not listed on our document, but maybe more complete
	
	**Limit Data set.  Keep most recent response of any participant only //JH:: NOTE: Doing this removes most of the sample for questions that we care about. Removing.
	egen most_recent = rank(syear), by(pid) field
	//tab syear most_recent
	//tab syear
	//keep if most_recent==1
	//tab syear
	
	
	******=======
	**KW - Expand data set to 1 obs per-kids
	*****========
	expand sumkids, gen(multikids) //1 observation per kid (and 1 obs for no kids)
	egen kidorder = rank(pid), by(pid syear) unique //Variable to represent kid "order"
	replace kidorder = . if sumkids==0
	gen kidby = .
	gen kidgend = .
	forvalues i = 1(1)19 {
		if `i' < 10 {
			replace kidby = kidgeb0`i' if kidorder==`i'
			replace kidgend = kidsex0`i' if kidorder==`i'
		}
		else {
			replace kidby = kidgeb`i'  if kidorder==`i'
			replace kidgend = kidsex`i' if kidorder==`i'
		}
	}
	replace kidby = . if kidby<0
	replace kidgend =. if kidgend < 0
	
	**Older sibling FEs
	gen sib_only = (sumkids==1 & kidorder==1)
	gen sib_m = (sumkids==2 & kidsex01==1 & kidorder==2)
	gen sib_f = (sumkids==2 & kidsex01==2 & kidorder==2)
	gen sib_mm = (sumkids==3 & kidsex01==1 & kidsex02==1 & kidorder==3)
	gen sib_ff = (sumkids==3 & kidsex01==2 & kidsex02==2 & kidorder==3)
	gen sib_mf = (sumkids==3 & kidorder==3 & sib_mm==0 & sib_ff==0)
	gen sib_4p_nof = 0 //Dummy for 4th+ kids indicating any sisters. Could be a loop w/ a little effort.
		replace sib_4p_nof = 1 if (kidorder==4 & kidsex01==1 & kidsex02==1 & kidsex03==1)
		replace sib_4p_nof = 1 if (kidorder==5 & kidsex01==1 & kidsex02==1 & kidsex03==1 & kidsex04==1)
		replace sib_4p_nof = 1 if (kidorder==6 & kidsex01==1 & kidsex02==1 & kidsex03==1 & kidsex04==1 & kidsex05==1)
		replace sib_4p_nof = 1 if (kidorder==7 & kidsex01==1 & kidsex02==1 & kidsex03==1 & kidsex04==1 & kidsex05==1 & kidsex06==1)
		replace sib_4p_nof = 1 if (kidorder==8 & kidsex01==1 & kidsex02==1 & kidsex03==1 & kidsex04==1 & kidsex05==1  & kidsex06==1 & kidsex07==1)
		//In our 5% sample, 0 cases of 6+ all-male family.  In theory this code could go up to kidorder 19
	
	
	**Single "Prior Kids" variable for i.priorkids
	gen priorkids = 0 
	forvalues i = 2(1)9 {
		local i1 = `i' - 1
		forvalues j = 1(1)`i1' {
		if `j' < 10 {
		replace priorkids = priorkids + kidsex0`j'*10^`j' if kidsex0`j' > 0 & kidorder==`i'
		}
		else {
		replace priorkids = priorkids + kidsex`j'*10^`j' if kidsex`j' > 0 & kidorder==`i'	
		}
	}
	}
	replace priorkids = priorkids/10
	tab priorkids
	
**drop the 4 x 19 kids variables for cleanliness
	//drop kidmon* kidsex* kidgeb* kidpnr*
	
	save "$DATA/h1_jhkw_dataset.dta", replace // save merged dataset

* ============================
* Analysis
* ============================

use "$DATA/h1_jhkw_dataset.dta", replace

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	keep if kidby <= syear & priorkids > 0


* GENERATE VARIABLES * (Please adapt according to your needs)

	* Example: generate dummy variable for whether participant has at least one daughter (1) or not (0)
	* For illustrative purposes, we will generate the variable randomly:
	gen d_daughter = kidgend - 1 if kidgend >=1 //kidgend takes values of 1 for male, 2 for female.
	
	gen pos_gender_attitude = plh0395i05 if plh0395i05>0
	
	//Control variables:
	
	foreach var of varlist priorkids plh0258_h plg0267 plb0471_h hgnuts1 {
		
		tab `var'
		
		gen m_`var' = (`var'<0)
		
		tab syear m_`var'
		
		replace `var'=0 if `var'<0 //create category for missing rather than dropping observations
	}
	
	


* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Please add a verbal description of your model here
	
	*	Our measure for support for gender quality is agreement with the question: plh0395i04--The assignment of gender is not important for the development of a person. 
	
	*	Our data are at the child level, for child births after the first. We control for the gender of prior children, as a way of controlling for the likelihood of a child being a girl (due to propensity for a particular couple to produce female children). We also add a few controls to reduce error variance: plh0258_h – Categorial religious affiliation plg0267 – Categorical Educational Attainment plb0471_h – Gross Income prev year hgnuts1 -categorical federal state
	
	*	We cluster at the parent-level


	* As an example, and for illustrative purposes we consider a: linear OLS regression

	* For illustrative purposes, let's further assume that having a job that requires hard manual labor (plb0085) is a good measure of support for gender equality (hypothesis 1), 
	* if having a job with less manual labor, the respondent is more likely to support gender equality.
	* Accordingly, we run a linear OLS regression analysis with plb0085 as dependent variable and the dummy variable d_daughter as independent variable
	
	

	reg pos_gender_attitude d_daughter i.priorkids i.plh0258_h i.plg0267 i.plb0471_h i.hgnuts1,cluster(pid)
	
* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

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
