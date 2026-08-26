/*// Andrea
if c(username) == "replicator" {
    global folder "C:/Users/fazio/Dropbox/Many Daughters/#ManyDaughters_RT_AnalysisPackage/#ManyDaughters_RT_AnalysisPackage/"
    global data	"${folder}data"
    global out "${folder}out"
	disp in red	"This is Replicator's PC"
} 
else {
// David Albrecht
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global data "${folder}data"
global out "${folder}out"
disp in red	"This is David's PC"
}*/


***********************************************************************
**********THIS PART OF THE CODE PREPARE THE DATASET TO TEST HYP********
***********************************************************************

use "$IN/pl.dta", clear

keep plh0308_v1 plh0308_v2 plj0620 plj0621 plj0622 plj0623 plj0624 plj0625 plh0004 plh0017 plh0018 plh0019 plh0020 plh0021 plh0022 pli0043_h plh0204_h ple0008 pld0131_v1 pld0131_v2 pld0131_v3 plb0022_h syear pid hid cid

save "$DATA/pl_clean.dta", replace



use "$IN/ppathl.dta", clear

keep netto sex gebjahr pid syear

save "$DATA/ppathl_clean.dta", replace


use "$IN/biobirth.dta"

keep pid kidsex* kidgeb*

save "$DATA/biobirth_clean.dta", replace


use "$IN/pgen.dta", clear

keep pid syear pgpsbil pgbilzeit pglabgro

save "$DATA/pgen_clean.dta", replace

****************

use "$DATA/ppathl_clean.dta", clear

merge 1:1 pid syear using "$DATA/pl_clean.dta"

keep if _merge==3

drop _merge

merge 1:1 pid syear using "$DATA/pgen_clean.dta"

keep if _merge==3

drop _merge

merge m:1 pid using "$DATA/biobirth_clean.dta"

keep if _merge==3

drop _merge

save "$DATA/soep_clean.dta", replace

************************
use "$DATA/soep_clean.dta", clear

label lang EN

recode * (-9999/-1=.) //drop missings

ge age=syear- gebjahr //gen age

**understand year in which children are born**
foreach var in 01 02 03 04 05 06 07 08 09 10{
    
	

gen time`var'=syear-kidgeb`var'


recode time`var' -999999999999/-1=0 0/9999999999=1, gen(time_dic`var')


}

egen n_child=rsum(time_dic01 time_dic02 time_dic03 time_dic04 time_dic05 time_dic06 time_dic07 time_dic08 time_dic09 time_dic10 ) //# of children

br pid age syear time* kidgeb01 kidsex01 n_child // check data composition

**harmonize marital status**
recode pld0131_v1 1=1 2/6=0, gen(married_1)

recode pld0131_v2 8=1 2/7=0, gen(married_2)

recode pld0131_v3 1=1 2/7=0, gen(married_3)

gen married=1 if married_1==1 | married_2==1 | married_3==1
replace married=0 if married_1==0 | married_2==0 | married_3==0

ta married

**harmonize attitudes**
gen att=1 if plh0308_v1==1 | plh0308_v1==2
replace att=0 if plh0308_v1==3 | plh0308_v1==4
replace att=1 if plh0308_v2==5 | plh0308_v2==6
replace att=0 if plh0308_v2<=4

**hours work**
bys hid syear: egen av_housework=mean(pli0043_h)

gen diff_housework=1 if pli0043_h<av_housework & sex==1
replace diff_housework=2 if pli0043_h==av_housework & sex==1
replace diff_housework=3 if pli0043_h>av_housework & sex==1

**recode first Children var**
recode kidsex01 1=0 2=1

label def kidsex01 0 "Son" 1 "Daughter", replace
label val kidsex01 kidsex01

gen treatment=time_dic01*kidsex01

br pid age syear time* kidgeb01 kidsex01

gen age2=age*age

recode plb0022_h 1=1 2/12=0, gen(employed) //recode employment
recode ple0008 1/2=1 3/5=0, gen(good_health) //recode health

**fix labels**
label var att "Agree: Man and woman work the same amount"
label def diff_housework 1 "Most Work Women" 2 "Equality" 3 "Most Work Men"
label var age "Age"
label var married "Married"
label var treatment "Birth of Daughter"
label var age2 "Age Squared"
label var employed "Employed Full-Time"
label var good_health "Good Health"
label var treatment "1st Child Daughter"

save "$DATA/soep_estimates.dta", replace


***********************************************
****************H3*****************************
***********************************************

* Calculate mean of diff_housework for the analysis sample
su diff_housework
local mean_diff_housework = r(mean)
di "Mean of diff_housework: `mean_diff_housework'"

xtset pid syear

eststo clear

xtreg diff_housework i.time_dic01 treatment married age2 employed good_health pgbilzeit i.syear, fe cluster(pid)
	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[treatment]

		* (ii) standard error
		local se = _se[treatment]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[treatment] / _se[treatment]
		local p_value = 2 * ttail(e(df_r), abs(`p_value'))

		* (vi) number of observations
		local num_obs = e(N)

		* (vii) number of participants
		quietly levelsof pid if e(sample), local(participants)
		local num_participants : word count `participants'

		* (viii) interpretation of directionality // Adapted by DA during consistency check
		//local interpretation "not linearly interpretable"
		* Set interpretation based on mean
		if `mean_diff_housework' > 2 {
			local interpretation "negative coefficient is in line with hypothesis"
		}
		else if `mean_diff_housework' == 2 {
			local interpretation "not linearly interpretable"
		}
		else {
			local interpretation "positive coefficient is in line with hypothesis"
		}
		

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


