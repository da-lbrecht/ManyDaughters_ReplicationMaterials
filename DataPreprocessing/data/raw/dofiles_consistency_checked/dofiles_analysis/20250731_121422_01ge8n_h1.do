********************************************************************************
****************** Example Do-File H1: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup * (Please adapt according to your needs)
* ============================

//clear all
version 17 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* ssc install outreg2

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

	use "$IN/biol.dta", clear // use biol as a starting dataset

	* Merge biol with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* pl contains individual information 
	merge m:1 pid hid syear using "$IN/pl.dta", nogen keep(master matches)
	
	* bio_birth contains biographic information 
	merge m:1 pid cid using "$IN/biobirth.dta", nogen keep(master matches)
	
	keep pid syear hid cid pnr lb0288_v1 lb0290_h lb0287_h lb0290_h plj0620 plj0624 plj0625 gebjahr sex lb0063 plh0258_v10 plh0258_v11 plb0022_h plj0022 plj0023_v1 plb0471_h
	
	label language EN // change language of the labels to English

	save "$DATA/h1_dataset.dta", replace // save merged dataset	
	
* ============================
* Analysis
* ============================

* FILTERS * (Please adapt according to your needs)
 
	* Example: only keep participants from private households
	* keep if inlist(pop,1,2)

* GENERATE VARIABLES * (Please adapt according to your needs)

	* We use the variable lb0288_v1 to identify the sex of the firstborn child. To ensure comparability across respondents, we limit our analysis to individuals with exactly one child. This restriction is based on the variable lb0290_h, which indicates whether the respondent has a second child. A value of -2 for lb0290_h signifies that the question is not applicable, which we interpret as the respondent not having a second child. Therefore, we retain only those respondents for whom lb0290_h = -2, ensuring the sample includes only individuals with a single child.
	
	gen d_daughter = (lb0288_v1 == 2) if (lb0290_h == -2)&(lb0288_v1 >= 0)
	label variable d_daughter "1 if the responder has only one daughter and zero if he or she has only a son"
	
	tab d_daughter
	
	* Following the approach outlined in our pre-analysis plan (PAP), we construct a variable to identify which cases are included in the analysis. Our objective is to examine the parenting effect associated with raising a daughter. We assume that this effect requires sufficient exposure to the parenting experience, meaning that the firstborn daughter must be at least one year old at the time the gender equality attitudes are measured. To operationalize this, we create a variable named "sample", which takes the value 1 for individuals meeting this exposure condition. Specifically, we define the year of birth of the firstborn child—ensuring the respondent has no additional children—and compare it to the year the gender attitude questions were answered. Respondents whose firstborn child was born at least one year prior to the measurement of attitudes are included in the analysis (i.e., sample == 1).
	
	gen year_first_born = lb0287_h if (lb0290_h == - 2) & (lb0287_h >= 0)
    label variable year_first_born "Year of the firstborn when the individual has only one child"
	
	* The variable sample is defined as the difference between the year in which the dependent variable (gender equality attitudes) is measured and the year of birth of the firstborn child. We include in the analysis only cases where this difference is at least one year, ensuring that the respondent has been exposed to parenting the firstborn child for a minimum of one year. 
	
	gen sample = (syear - year_first_born >= 1) if!missing(year_first_born)
	label variable sample "1 if the individual's one child firstborn has at least one year wrt the dep. var.'"

* HYPOTHESIS TEST FOR H1 * (Please adapt according to your needs)

	* Our pre-analysis plan (PAP) proposed using five indicators to construct a composite measure of gender equality. The idea was to rely on measured attitudes that are plausibly correlated with the latent construct of gender equality. Initially, we intended to use principal component analysis (PCA) to combine the five SOEP variables into a single index. However, upon accessing a random sample of 5% of the data, we found that the indicators were not consistently measured across the same survey years. This lack of temporal overlap prevented the use of PCA. As an alternative, we selected the three variables most closely aligned with our conceptual definition of gender equality, which emphasizes equality in opportunities, responsibilities, and rights between women and men, and girls and boys. Specifically, we interpret variable plj0620 as capturing attitudes toward gender equality in rights, plj0624 as reflecting beliefs about educational equality (linked to opportunities), and plj0625 as indicating views on shared responsibilities. To construct the composite index, we recoded the three variables so that higher values represent more egalitarian attitudes, standardized them, and then averaged the resulting values. The remaining two indicators were excluded from the index due to their lack of temporal overlap. Nevertheless, we examined whether these discarded variables differed systematically between individuals raising daughters versus sons and found no significant differences. For the empirical analysis, we estimate the effect of having a daughter (versus a son) on the composite gender equality index using an Ordinary Least Squares (OLS) model. A positive and statistically significant coefficient would suggest that individuals with daughters express more egalitarian gender attitudes, thereby supporting H1. Finally, it is important to note that our identification strategy relies on the idea that the gender of the firstborn is a random event. Therefore, a simple OLS model would be sufficient to test the treatment effect. However, there may be systematic differences between the treated group (i.e., having a daughter) and the control group (i.e., having a son). To address this concern, we examine the balance of covariates across groups prior to conducting the regression analysis.
	
	* Recodification of the variable plj0620. Because higher values of plj0624 and plj0625 indicate more gender-equal attitudes, but not for plj0620, we codify plj0620 for consistency.
	
	gen plj0620_v1 = 10 - plj0620 
		
	recode plj0620_v1 (18 = -8) if plj0620 == -8 /*This restores the original category for the category: Question not part of the survey this year*/
	recode plj0620_v1 (15 = -5) if plj0620 == -5 /*This restores the original category for the category Not included in this version of the survey*/
	recode plj0620_v1 (11 = -1) if plj0620 == -1 /*This restores the original category for No answer*/
	
	label variable plj0620_v1 "High values mean equality rights not belonging to democracy"
	
	* Standardzation
	foreach x in plj0620_v1 plj0624 plj0625 {
		
		gen `x'_num = (1)*`x' /*To express the variable as a continuous variable*/
		label variable `x'_num "Numerical versions of `x'"
		
		egen mean_`x'= mean(`x'_num) if `x'_num >= 0 /*It is observed that each variable appears once. Thus, we have a cross-section design*/
		egen sd_`x' = sd(`x'_num) if `x'_num >= 0
		
		gen `x'_std = (`x'_num - mean_`x')/sd_`x' 
		label variable `x'_std "Standarized values of `x'"
	}
	
	sum plj0625_std plj0624_std plj0620_v1_std /*All the values are mean centered, with a sd == 1*/
	
	* Averaging the standardized variables to create a composite of gender equality
	gen  GE = (plj0625_std + plj0624_std + plj0620_v1_std)/3
	label variable GE "Composite for gender equality"
	
	* Balance analysis: According to our PAP, we check for balance in observable attributes across treated and control groups.
	tab sex d_daughter, chi2 /*We do not find differences in terms of the responders' gender*/
	
	gen age = 2025 - gebjahr
	label variable age "Responder's age at 2025"
	
	ttest age, by(d_daughter) /*We do not observe any difference*/
	
	ttest lb0063 if lb0063 > 0, by(d_daughter) /*We do not find differences in terms of the number of sisters of the responders*/
	
	tab plh0258_v10, gen(christian)
	
	forvalues t=4/7 {  /*We start at 4, the first category defining a religious affiliation*/
		
		tab christian`t' d_daughter, chi2 /*No differences are detected in terms of distinct catholic affiliation types*/
	} 
	
		tab plh0258_v11, gen(islamic)
	
	forvalues t=5/8 {  /*We start at 4, the first category defining a religious affiliation*/
		
		tab islamic`t' d_daughter, chi2 /*No differences are detected in terms of distinct muslim affiliation types*/
	} 
	
	tab plb0022_h, gen(employment_st) 
	
	forvalues t=1/10 {  
		
		tab employment_st`t' d_daughter, chi2 /*We observe statistically significant differences for employment_st3 == 1, that is for respondents under a in education/apprenticeship */
	} 
	
	tab plj0022 d_daughter if plj0022 > 0, chi2 /*No differences between german and non-german citizens*/
	
	tab plj0023_v1 d_daughter if plj0023_v1 > 0, chi2 /*No differences between responders having a second citizenship*/
	
	ttest plb0471_h if plb0471_h > 0, by(d_daughter) /*No differences in terms of responders' gross salary*/
	
	* Regression analysis
	reg GE d_daughter employment_st3 if (sample == 1)
	
	* Regression analysis with the discarded items: We do not observe statistically significant differences when these items are used.
	
	* reg lb1113 d_daughter if (lb1113>0)&(sample == 1)
	* reg lb1120 d_daughter if (lb1120>0)&(sample == 1)
	

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




