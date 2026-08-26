********************************************************************************
****************** Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: [REDACTED]

* ============================
* Setup *
* ============================

* clear all
version 14.2 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES *
* net install vlookup, from(http://www.stata.com/users/kcrow)

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

* LOAD AND MERGE DATA *

	use "$IN/ppathl.dta", clear // use ppathl as a starting dataset

	* Merge ppathl with all datasets, that you deem relevant for your analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(biovalid bioyear sumkids biokids kidsex* kidpnr* kidgeb*)
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches) keepusing(pgpiyear pgmonth pgbilzeit pgemplst pgfamstd pgpartnr pgpartz)
	* pl contains all variables from the individual questionnaire
	merge 1:1 pid syear using "$IN/pl.dta", nogen keep(master matches) keepusing(ple0040 pli0051 pli0036 pli0010)
	* hgen contains generated variables on household level from the household interview; use for household net income
	merge m:1 hid syear using "$IN/hgen.dta", nogen keep(master matches) keepusing(hid syear hghinc hgnuts1)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

* FILTERS *

	* Sort by pid and year
	sort pid syear

	* Only keep participants from private households
	keep if inlist(pop,1,2)
	* Only keep particiants with individual level data
	keep if netto>9 & netto<20
	* Only participants who have at least one child for whom there is information
	keep if biovalid == 31
	* Only participants aged 18+
	keep if (syear*12+pgmonth-gebjahr*12-gebmonat)>215 & (syear*12+pgmonth-gebjahr*12-gebmonat)<.
	* Only participants with valid household income
	keep if hghinc>=0 & hghinc<.
	* Only participants with valid number of years of education
	keep if pgbilzeit>=0 & pgbilzeit<.
	* Only participants with valid employment information
	keep if pgemplst>=0 & pgemplst<.
	* Only participants with valid disability information
	keep if ple0040>=0 & ple0040<.
	rename ple0040 disabled
	recode disabled 2=0
	* Only participants with valid marital status information
	keep if pgfamstd>=0 & pgfamstd<.
	rename pgfamstd marital_stat
	label define pgfamstd_EN 5 "Widowed" 2 "Separated" 3 "Single" 4 "Divorced" 5 "Widowed", modify
	* Only participants with valid sample region
	keep if sampreg>0 & sampreg<.
	rename sampreg east_germany
	recode east_germany 1=0 2=1
	label define sampreg_EN 0 "West-Germany" 1 "East-Germany", modify
// 	* Only participants with valid migration background information
// 	keep if migback~=.
// 	recode migback 1=0 2=1 3=2
// 	label define migback_EN 0 "None" 1 "Direct" 2 "Indirect", modify
	* Only participants with valid regional dummies
	keep if hgnuts1~=.
	* Only participants with valid information on the variables used to
	* construct the dependent variable
	keep if pli0051>-1 & pli0051~=.
	keep if pli0036>-1 & pli0036~=.
	keep if pli0010>-1 & pli0010~=.
	* The hours of leisure on weekends (Saturdays and Sundays) have been
	* measured only in odd years between 2000 and 2020, and also in 2012.
	* Only participants with valid partner IDs
	keep if pgpartnr>0 & pgpartnr~=.
	* Only participants who have a spouse, a partner, or a registered partner
	keep if inlist(pgpartz,1,2)
	* The partner should be alive.
	egen idyear = group(pid syear)
	egen partneridyear = group(pgpartnr syear)
	vlookup partneridyear, gen(p_todjahr) key(idyear) value(todjahr)
	keep if p_todjahr<syear
	* Look up partner's sex.
	sort pid syear
	vlookup pgpartnr, gen(p_sex) key(pid) value(sex)
	* Only participants with known sex and known partner's sex.
	keep if inlist(sex,1,2)
	keep if inlist(p_sex,1,2)
	* Only keep heterosexual couples.
	keep if (sex==1 & p_sex==2) | (sex==2 & p_sex==1)

	

* GENERATE VARIABLES *

	* Generate dummy for kidNN being present in the HH in the survey year
	forvalues i = 1/19 {
		local j : display %02.0f `i'
		gen d_kid`j' = .
	}
	forvalues i = 1/19 {
		local j : display %02.0f `i'
		replace d_kid`j' = cond(syear-kidgeb`j'>=0 & kidgeb`j'>0,1, ///
		cond(kidgeb`j'>0,0,.))
	}
	* Generate the number of kids respondent has in survey year
	egen total_kids = rowtotal(d_kid01-d_kid19), missing
	
	* Generate dummy for daughterNN being present in the HH in the survey year
	forvalues i = 1/19 {
		local j : display %02.0f `i'
		gen d_daughter`j' = .
	}
	forvalues i = 1/19 {
		local j : display %02.0f `i'
		replace d_daughter`j' = cond(syear-kidgeb`j'>=0 & kidgeb`j'>0 & ///
		kidsex`j'==2,1,cond(kidgeb`j'>0,0,.))
	}
	* Generate a dummy for having at least one daugther in survey year
	egen d_daughter = anymatch(d_daughter01-d_daughter19), values(1)
	
	* Generate a variable for the first year someone is the parent of a daughter
	by pid: egen first_daughter_year = min(cond(d_daughter==1, syear, .))
	
	* Generate a variable to distinguish those who have a daughter at some point
	* during the years prior to having any daughter
	gen pre_daughter = (syear < first_daughter_year)
	
	* Generate the number of daughters respondent has in survey year
	egen total_daughters = rowtotal(d_daughter01-d_daughter19), missing
	
	* Generate the age variables as they appear in regressions: squared and 
	* divided by 100
	gen age = (syear*12+pgmonth-gebjahr*12-gebmonat)/12
	gen age2_100 = (((syear*12+pgmonth-gebjahr*12-gebmonat)/12)^2)/100
	
	* Generate the log of household income
	gen loghinc=log(hghinc)
	
	* Generate dummy for being in full-time employment
	gen ft_employed = cond(pgemplst>1 & pgemplst~=.,0,cond(pgemplst==1,1,.))
	
	* Generate the variable for total no of hours per week for
	* hobbies/leisure
	gen week_h_hobbies = pli0051 + pli0036 + pli0010
	* Keep individuals for whom we have information about their leisure time
	keep if week_h_hobbies ~=.
	
	* Generate individual level independent variables for the partner.
	vlookup partneridyear, gen(p_age) key(idyear) value(age)
	vlookup partneridyear, gen(p_age2_100) key(idyear) value(age2_100)
	vlookup partneridyear, gen(p_pgbilzeit) key(idyear) value(pgbilzeit)
	vlookup partneridyear, gen(p_disabled) key(idyear) value(disabled)
	vlookup partneridyear, gen(p_ft_employed) key(idyear) value(ft_employed)
	vlookup partneridyear, gen(p_week_h_hobbies) key(idyear) value(week_h_hobbies)
	* Keep individuals whose partner's leisure time is recorded
	keep if p_week_h_hobbies~=.
	
	* Generate the dependent variable
	//keep if sex == 1
	gen share_h_hobbies = week_h_hobbies/(week_h_hobbies+p_week_h_hobbies)
	gen p_share_h_hobbies = p_week_h_hobbies/(week_h_hobbies+p_week_h_hobbies)
	replace share_h_hobbies = 0.5 if week_h_hobbies == 0 & p_week_h_hobbies == 0


* HYPOTHESIS TEST FOR H3 *

	* I run a linear OLS regression analysis with the share of the man's leisure
	* time of the total leisure time of the couple (share_h_hobbies) as dependent
	* variable and the total number of daughters as independent variable. The 
	* regression has individual fixed effects and standard errors clustered at
	* the individual level. This means that it examines the within-person change
	* in the dep var from having an additional daughter versus not having one 
	* (and having a son instead, since the total number of kids is held constant).
	* As mentioned in the PAP, there is one observation per heterosexual couple
	* who does have children at some point in their time on the panel.
	* The controls are the ones listed in the PAP, with adjustments as 
	* documented below.
	xtset pid syear
	
	* Check how much of leisure time in a couple was the man's for men who have 
	* daughters at some point, but for them before they father daughters.
	* This is needed to see if the effect of daughters is that they change the
	* intra-couple leisure time distribution in the direction of more gender 
	* equality, by providing a baseline to compare with.
	* I (quietly) run a regression first just to have access to the relevant sample.
	qui xtreg share_h_hobbies total_daughters i.total_kids age2_100 p_age p_age2_100 ///
	loghinc pgbilzeit p_pgbilzeit ft_employed p_ft_employed disabled p_disabled ///
	i.marital_stat east_germany i.syear ib3.hgnuts1 if sex==1, fe vce(cluster pid)
	su share_h_hobbies if pre_daughter==1 & sex==1 & e(sample)
	scalar mean_pre = r(mean)
	
	* Here I rerun the regression to get postestimation results.
	xtreg share_h_hobbies total_daughters i.total_kids age2_100 p_age p_age2_100 ///
	loghinc pgbilzeit p_pgbilzeit ft_employed p_ft_employed disabled p_disabled ///
	i.marital_stat east_germany i.syear ib3.hgnuts1 if sex==1, fe vce(cluster pid)
	quietly levelsof pid if e(sample), local(participants)
	
	* This tests H3.
	nlcom (abs(mean_pre + _b[total_daughters] - 0.5) - abs(mean_pre - 0.5))

	
	* Changes from PAP:
	* - The hypothesis test is slightly different: I had made a mistake when
	* specifying that the mean of the dep var in the non-treated sample was 
	* alpha i (the constant in the regression). It is wrong because the model uses individual 
	* fixed effects. So instead I calculated this mean separately (lines 445-446).
	* - I calculated age differently, using syear instead of pgpiyear, since 
	* pgpiyear was only recorded in some years.
	* - For years of education, I used pgbilzeit instead of pgbilzt, as the 
	* second variable wasn't in SOEP Core, but in SOEP IS (misunderstanding).
	* - For living in East/West Germany, I used sampreg instead of bgsampreg, 
	* as this was the correct variable in SOEP Core (misunderstanding).
	* - I restricted the estimation sample to those in private households, with
	* pop 1 or 2.
	* - I dropped the migration background as a control, because it cannot be used
	* in a regression with individual FEs (since it is constant within person). It 
	* was a mistake to include it in a FE model in the PAP.
	* - Unspecified in PAP: the baseline NUTS region is Berlin.
	
	
* EXTRACT AND EXPORT RESULTS *

* Extract the required statistics after nlcom

	* (i) coefficient estimate
	matrix define b = r(b)
	local coefficient = b[1,1]

	* (ii) standard error
	matrix V = r(V)
	local se = sqrt(V[1,1])

	* (iii) family of the test statistic (z for nlcom)
	local test_stat_family = "z"

	* (iv) residual and model degrees of freedom (not available for nlcom)
	local df_residual = "."
	local df_model = "."

	* (v) p-value
	local z = b[1,1]/`se'
	local p_value = 2*normal(-abs(`z'))

	* (vi) number of observations
	local num_obs = e(N)

	* (vii) number of participants
	local num_participants : word count `participants'

	* (viii) interpretation of directionality

	local interpretation "negative coefficient is in line with hypothesis"



	// Display the saved statistics
	di "Coefficient: `coefficient'"
	di "Standard Error: `se'"
	di "Test Statistic Family: `test_stat_family'"
	di "Residual Degrees of Freedom: `df_residual'"
	di "Model Degrees of Freedom: `df_model'"
	di "p-value: " `p_value'
	di "Number of observations: " `num_obs'
	di "Number of participants: " `num_participants'
	di "Interpretation of directionality: " "`interpretation'"

	* Export results to csv 
	file open myfile using "$OUT/h3_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit

