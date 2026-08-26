********************************************************************************
****************** Example Do-File H3: #ManyDaughters Project ******************
********************************************************************************

* Authors: Tina Wang, David Albrecht

* ============================
* Setup * (Please adapt according to your needs)
* ============================

* clear all
version 19.5 // BE-Basic Edition, indicate the version of stata you are using here
             // maxvar not settable in this version of Stata (maxvar is fixed at 2,048)

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
*ssc install outreg2
*ssc install estout

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
		
	* The full 'pl.dta' file contains ~4,000 variables, which exceeds Stata's
	* 2,048-variable limit (my version of stata). To avoid this, we create a pl subset
	* containing only the merge keys (pid, syear, hid, cid) and the three
	* questionnaire items needed for H3 (pli0043_v3, pli0040, pli0049_v3).
	
   * pl scontains all variables from the individual questionnaire
	use pid syear hid cid pli0043_v3 pli0040 pli0049_v3 using "$IN/pl.dta", clear
	save "$IN/pl_subset_h3.dta", replace
	
	* Merge pl with all datasets, that we deem relevant for our analysis. (Tip: use keepusing() to specify which variables to keep from the using dataset; the default is all.)

	* biobirth contains information on fertility history
	merge m:1 pid using "$IN/biobirth.dta", nogen keep(master matches) keepusing(kidsex01 kidgeb01)
	
	* ppathl 
	merge m:1 pid syear using "$IN/ppathl.dta", nogen keep(master matches)
	
	* pgen contains user-friendly data on the individual level that are consolidated from different sources
	merge 1:1 pid syear using "$IN/pgen.dta", nogen keep(master matches)

	label language EN // change language of the labels to English

	save "$DATA/h3_dataset.dta", replace // save merged dataset


* ============================
* Analysis
* ============================

use "$DATA/h3_dataset.dta", clear

* Analytic weights
* The fixed effects estimator in xtreg requires that the weight is constant within each panel (household, here "hid").
* Therefore, we create a household-level (panel-level) weight by averaging phrf within each household.
* This ensures phrf_panel is constant for all years of the same household and can be safely used in xtreg, fe.

bysort hid: egen phrf_panel = mean(phrf)


*1*Generate DEPENDENT VARIABLE gender_inequality_behavior 
foreach var in pli0043_v3 pli0040 pli0049_v3 {
        mvdecode `var', mv(-1/-8=.) 
           }

gen total_hours = pli0043_v3 + pli0040 + pli0049_v3

* For each observation, find partner's hours
preserve
keep pid syear total_hours
rename (pid total_hours) (pgpartnr partner_hours)
tempfile partner_hours_data
save `partner_hours_data'
restore

merge m:1 pgpartnr syear using `partner_hours_data', keep(1 3) nogen

*Absolute difference between my hours and partner hours: DEPENDENT VARIABLE gender_inequality_behavior 
gen gender_inequality_behavior = abs(total_hours - partner_hours) if !missing(partner_hours) & !missing(total_hours)


*2*Generate INDEPENDENT VARIABLE firstborn_child_female 
 *Code firstborn_child_female as 1 if firstborn is female (kidsex[01]=2), 0 if male (kidsex[01]=1)
    gen firstborn_child_female = .
    replace firstborn_child_female = 1 if kidsex01 == 2
    replace firstborn_child_female = 0 if kidsex01 == 1
	
**Generate EVENT TIME : year of the firstborn child
	gen firstborn_year_child = kidgeb01

	gen event_time = syear - firstborn_year_child
	
	keep if inrange(event_time, -3, 3)
	
	
*KEY CONTROLS
	
*AGE
gen age =  syear - gebjahr           // 
gen age_square = age^2
 
* For each observation, find partner's age
preserve
keep pid syear age
rename (pid age) (pgpartnr partner_age)
tempfile partner_age_data
save `partner_age_data'
restore

merge m:1 pgpartnr syear using `partner_age_data', keep(1 3) nogen

gen partner_age_square = partner_age^2

*SEX (to identify heterosexual couples)
preserve
keep pid syear sex
rename (pid sex) (pgpartnr partner_sex)
tempfile partner_sex_data
save `partner_sex_data'
restore

merge m:1 pgpartnr syear using `partner_sex_data', keep(1 3) nogen

*****
*SELECTION DES VARIABLES A GARDER
keep pid hid syear pgpartnr sex partner_sex age age_square partner_age partner_age_square gender_inequality_behavior event_time firstborn_child_female phrf phrf_panel

* Tout le reste sera supprimé de la base

****

* FILTERS *

* 2. Drop respondents with missing dependent variable
count
drop if missing(gender_inequality_behavior)
drop if gender_inequality_behavior < 0

inspect gender_inequality_behavior

* 3. Drop respondents with no firstborn information
count
drop if missing(firstborn_child_female)
drop if missing(event_time)

inspect firstborn_child_female
inspect event_time

* 4. Drop respondents missing key controls
count
drop if missing(sex)
drop if missing(partner_sex)

drop if missing(age)
drop if missing(partner_age)

drop if missing(age_square)
drop if missing(partner_age_square)

inspect sex
inspect partner_sex

inspect age
inspect partner_age

inspect age_square
inspect partner_age_square

* 4. Restrict age
count
keep if inrange(age, 18, 54)
keep if inrange(age_square, 324, 2916)

*5. Restrict to heterosexual couples
count
keep if (sex == 1 & partner_sex == 2) | (sex == 2 & partner_sex == 1)

*7. Restrict to one observation per year per household (keep arbitrarily men)
count
keep if sex == 1

*6. At least, 2 observations per household
* Keep only those households with at least 2 observations
* (Note: This is a modification from the pre-analysis plan (PAP), which originally required at least 3 observations per household. Due to sample size and power considerations, we are now including households with at least 2 observations.)
count
egen n_obs = count(pid), by(hid)
keep if n_obs >= 2


*7. 

*CHECK THE REMAINING SAMPLE SIMPLE
count  

**COUNT RESULTS : 418 OBSERVATIONS
* For our regression, assuming a small effect size of Cohen's d = 0.15. With a significance level of 𝛼 = 0.05 and desired power = 0.80, 
*We would require approximately 227 household-year observations per group (i.e., 3,184 total observations) to detect a statistically significant difference between respondents with firstborn daughters and those with firstborn sons, as well as to assess interaction effects with the variable event_time.

tabulate event_time firstborn_child_female


*           | firstborn_child_femal
*           |           e
*event_time |         0          1 |     Total
*-----------+----------------------+----------
*        -3 |        15          5 |        20 
*        -2 |        18          7 |        25 
*        -1 |        23         14 |        37 
*         0 |        42         25 |        67 
*         1 |        51         38 |        89 
*         2 |        54         42 |        96 
*         3 |        49         35 |        84 
*-----------+----------------------+----------
*     Total |       252        166 |       418 


*Except for event times -2 and -3 (with group sizes 7 and 5, respectively), all other groups exhibit sufficient extrapolated statistical power (eg. (15/5)*100=300). Given that the group sizes for firstborn_child_female == 0 are sufficient, we assume that statistical power will also be sufficient in the full dataset for all groups with firstborn_child_female ==1 to detect a significant effect.

* HYPOTHESIS TEST FOR H3 * (Please adapt according to your needs)
	* We estimate a household fixed effects panel linear OLS regression using an event-study approach.
	* Let's assume that the measure gender_inequality_behavior, constructed from pli0043_v3, pli0040, and pli0049_v3, provides a valid measure of gender inequality in the household based on behaviors (Hypothesis 3).
	* If the household's firstborn child is a female, we expect the household to exhibit lower gender inequality (higher values indicate greater inequality).
	* Accordingly, we run a linear panel regression with gender_inequality_behavior as the dependent variable, and firstborn_child_female, event_time (and their interactions), and age_square and partner_age_square as independent variables, using household fixed effects, analytic weights (phrf), and clustering standard errors by household ID (hid).
	
*Note, modification from the PAP: Since most individuals in our sample maintain stable partnerships, partner_age increases at thesame rate as individual age, creating perfect collinearity that prevents separate identification of their effects. Due to collinearity between age and partner_age in the fixed effects specification, we include only age, partner_age_square and age_square in the final model. Alternative approaches such as using age difference (age - partner_age) would also be problematic, as this variable becomes time-invariant for individuals with stable partnerships and would therefore be absorbed by the individual fixed effects. 
	
*Eliminate duplicates hid year
sort hid pid syear 
duplicates report 
duplicates list hid syear
duplicates tag hid syear, gen(isduplicate) 
duplicates drop hid syear, force

* Set panel structure
xtset hid syear

*Because the extraction of results requires a single summary statistic, we treat our event variable (event_time) as continuous in the analysis. Accordingly, we interpret the coefficient on event_time_adj (and its interaction) as the estimated effect of a one-unit increase in event time relative to the baseline, rather than estimating separate effects for each time period.
**Fixed-effects estimation absord the variable firstborn_child_female (single coefficient alone)
	* Panel regression with household fixed effects (xtreg), analytic weights, and clustered standard errors
xtreg gender_inequality_behavior c.event_time#1.firstborn_child_female age age_square partner_age_square [aweight=phrf_panel], fe cluster(hid)

	*Joint hypothesis test wald test
	test c.event_time#1.firstborn_child_female

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[c.event_time#1.firstborn_child_female]

		* (ii) standard error
		local se = _se[c.event_time#1.firstborn_child_female]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[c.event_time#1.firstborn_child_female] / _se[c.event_time#1.firstborn_child_female]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality
		scalar coef_firstborn_female_event = _b[c.event_time#1.firstborn_child_female]
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
	file open myfile using "$OUT/h3_results.csv", write replace // Renamed to original naming convention by David Albrecht on 03.07.2025
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


********************************************************************************

exit
