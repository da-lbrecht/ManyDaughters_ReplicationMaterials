********************************************************************************
********************* Do-File H2: #ManyDaughters Project ***********************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

//clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
* install all packages you need here, for example:

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

	save "$DATA/h2_dataset.dta", replace // save merged dataset

* LOAD AND MERGE DATA *

	use "$DATA/h2_dataset.dta", replace


* ============================
* Analysis
* ============================

* FILTERS *
	
	*plm0644: Fair society: equally distributed income and wealth
	
	*This will be the main dependent variable of this analysis. However, there are a considerable number of observations without answers for this variable 
	*due to various reasons such as the household not being asked the question in the wave of the survey they participated, data not being included in this
	*version of the dataset, or simply the household refusing to answer. Therefore, we screen out the observations with the following values for "plm0644"
	*via the modified version of this variable "plm0644_modified" which will be created in the next section.
	
	* plm0644==-8 | plm0644==-5 | plm0644==-1
	
	*We filter out the observations which do not have any data for this variables
	
	keep if missing(plm0644)==0
 
* GENERATE VARIABLES *

	*pld0026: Number Of Daughters
		*The problem is that the "pld0026" variable is not available for many observations
		*while the "kidsex01...kidsex19" variables are available for all observations.
		*Thus, instead of relying of the pld0026 variable, generating another variables
		*for the number of daughters based on the 19 child gender variables seems to better
		*the more reliable approach.
		
		*Based on the 19 kid gender variables above, create the following set of variables:
		*total_sons: the total number of sons in a household
		*total_daughters: the total number of daughters in a household
		*total_kids: the total number of kids in a household
		*has_sons: a binary variable showing whether there is at least a son in the household or not
		*has_daughters: a binary variable showing whether there is at least a daughter in the household or not
		*has_kids: a binary variable showing whether there is at list one kid in the household or not

		local kidcounts ""01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19""

		local kidcountno: word count `kidcounts'

		forvalues i=1/`kidcountno'{
    
			local x: word `i' of `kidcounts'
			*Generate the binary variable "kidmx" displaying whether the x'th kid is male
			gen kidm`x'=kidsex`x'
			replace kidm`x'=0 if kidm`x'!=1
			*Generate the binary variable "kidfx" displaying whether the x'th kid is female
			gen kidf`x'=kidsex`x'
			replace kidf`x'=0 if kidf`x'!=2
			replace kidf`x'=1 if kidf`x'==2
	
		}

		gen total_sons=kidm01+kidm02+kidm03+kidm04+kidm05+kidm06+kidm07+kidm08+kidm09+kidm10+kidm11+kidm12+kidm13+kidm14+kidm15+kidm16+kidm17+kidm18+kidm19

		gen total_daughters=kidf01+kidf02+kidf03+kidf04+kidf05+kidf06+kidf07+kidf08+kidf09+kidf10+kidf11+kidf12+kidf13+kidf14+kidf15+kidf16+kidf17+kidf18+kidf19

		gen total_kids=total_sons+total_daughters

		gen has_kid=.
		replace has_kid=1 if total_kids>0
		replace has_kid=0 if total_kids==0

		gen has_daughter=.
		replace has_daughter=1 if total_daughters>0
		replace has_daughter=0 if total_daughters==0

		gen has_son=.
		replace has_son=1 if total_sons>0
		replace has_son=0 if total_sons==0
		
	*plm0644: We use the modified version of this variable as the dependent variable of the second hypothesis	
	
		gen plm0644_modified=plm0644
		local invalid_answers -8 -5 -1
		
		foreach i in `invalid_answers'{
			
			replace plm0644_modified=. if plm0644_modified==`i'
			
		}
	
	*Since the responses given to plm0644 are inversely related to preferences for equality, we multiply the variable with "-1" to make it 
	*positively related to preferences for equality:
	
		replace plm0644_modified=-plm0644_modified
		
	*Control variables	
	*male: Derived varible based on "sex"
	
		gen male=.
		replace male=1 if sex==1
		replace male=0 if sex==2
		
	*age: Derived variable based on "bioage", non-responses and observations without data for other reasons filtered via "bioage!=-2"
	
		gen age=bioage if bioage!=-2
		
	*years_of_education: Derived variable based on "pgbilzeit", non-responses and non-applicable observations filtered via "pgbilzeit>-1"
	
		gen years_of_education=pgbilzeit if pgbilzeit>-1
		
	*household_income: Derived variable based on "hghinc", non-responses and observations without data for other reasons filtered via "hghinc>=0"
	
		gen household_income=hghinc if hghinc>=0
		
	*marital status: Derived variables based on "pgfamstd", non-responses and observations without data for other reasons filtered via "pgfamstd>0", used as "i.marital_status" in the analysis
	
		gen marital_status=pgfamstd if pgfamstd>0
		
	*employment status: Derived variables based on "pgemplst", non-applicable observations filtered via "pgemplst!=-2", used as "i.employment_status" in the analysis
	
		gen employment_status=pgemplst if pgemplst!=-2
		
	*migrant background, used as "i.migrant" in the analysis
	
		gen migrant=migback
	
	*The list of control variables stored as a local named "control_variables"
	
	local control_variables male age years_of_education household_income i.marital_status i.employment_status i.migrant
	
* HYPOTHESIS TEST FOR H2 *

	* Verbal description of the model:
		*Dependent variable: plm0644_modified: Fair society: equally distributed income and wealth
		*Independent variable: has_daughter (created above)
		
		*In this analysis, we assume that "plm0644" is a reasonable and straightforward proxy of preferences toward more equality - hence a suitable proxy for left-leaning
		*political preferences. We hypothesize that having a daughter correlates with having left leaning political preferences. In line with this hypothesis, we run a linear
		*regression of the "plm0644_modified" dependent variable on the "has_daughter" independent variable.  We use robust standard errors due to a potential relationship between
		*the variance of the dependent and the binary variable controlling the preserence of daughters in the household.
		*While we perform a causal hypothesis test and the main treatment variable "has_daughters" should not be correlated with the remaining determinants of the dependent variable
		*in the population, due to potential relationships arising between the treatment variable and other determinants as a result of (i) the small size of the sample, and (ii) the sample
		*selection procedure, we also control for the variables we believe to be among the primary determinants of the dependent variable.

	regress plm0644_modified has_daughter `control_variables', robust

* EXTRACT AND EXPORT RESULTS *

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[has_daughter]

		* (ii) standard error
		local se = _se[has_daughter]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[has_daughter] / _se[has_daughter]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		count
		local num_obs=r(N)

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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
