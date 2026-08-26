//clear all
set max_memory 16g
set maxvar 20000

/*
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global soepv38 "$ROOT/data/raw" // folder, in which the original datasets are stored
global outdata "$ROOT/data/derived" // folder for datasets
global outresults "$ROOT/out" // folder for output (results, tables)
*/

/* Commented by David Albrecht for reproducibility check
global soepv38 "C:\#ManyDaughters_RT_AnalysisPackage\data\raw"
global outdata "C:\#ManyDaughters_RT_AnalysisPackage\data\derived"
global outresults "C:\#ManyDaughters_RT_AnalysisPackage\out" 
*/

use hid pid syear parid sex gebjahr loc1989 migback *sampreg birthregion_ew phrf psample using "$IN/ppathl.dta"
merge 1:1 pid syear using "$IN/pl.dta",  keepus(plh0300_v2 plh0357 plh0358 plh0302_v2 plh0298_v2 plh0308_v2 plh0359 plh0360 plb0019_v2 plh0258_h pli0098_v2 plh0255 plh0215 plh0220 plh0225 plh0214 plh0217 plh0224 pld0030 pld0029 pld0031 pld0032 pla0011) nogen
merge 1:1 pid syear using "$IN/pgen.dta", keep(match master) keepus(pglabgro pgsiops08 pgcasmin pgbilzeit pgfamstd pgstib pgemplst pgmonth) nogen
merge m:1 hid syear using "$IN/hl.dta",  keep(match master) keepus(hlc0005_h) nogen
merge m:1 hid syear using "$IN/hgen.dta", keep(match master) keepus(hgtyp1hh) nogen
merge 1:1 pid syear using "$IN/biol.dta", keep(match master) keepus(lb1092_v1 lb1092_v2 lb1093_v1 lb1093_v2 lb1094_v1 lb1094_v2  lb1095_v1 lb1095_v2 lb1096_v1  lb1096_v2 lb1097_v1 lb1097_v2  lb1098_v1 lb1098_v2 lb1099_v1  lb1099_v2 lb1100_v1 lb1100_v2 lb1101_v1) nogen
merge m:1 pid  using "$IN/biobirth.dta", keep(match master)  keepus( kidgeb01 kidsex01 kidgeb02 kidsex02 kidmon01 kidmon02) nogen
merge m:1 pid using "$IN/bioparen.dta", keep(match master)  keepus(sibl nums numb siblup)
save "$DATA/subsetH1.dta", replace

recode hid pid syear parid sex gebjahr loc1989 migback *sampreg birthregion_ew phr plh0300_v2 plh0357 plh0358 plh0302_v2 plh0298_v2 plh0308_v2 plh0359 plh0360 plb0019_v2 plh0258_h pli0098_v2 plh0255 plh0215 plh0220 plh0225 plh0214 plh0217 plh0224 pglabgro pgsiops08 pgcasmin pgbilzeit pgfamstd pgstib pgemplst hlc0005_h  (min/-1=.)

recode lb1092_v1 lb1092_v2 lb1093_v1 lb1093_v2 lb1094_v1 lb1094_v2  lb1095_v1 lb1095_v2 lb1096_v1  lb1096_v2 lb1097_v1 lb1097_v2  lb1098_v1 lb1098_v2 lb1099_v1  lb1099_v2 lb1100_v1 lb1100_v2 lb1101_v1 (-8 -5 -1=.)

recode  pld0029 pld0031  (-8 -5 -1=.)
recode sibl nums numb siblup (-5 -1=.)
recode siblup (-5 -2 -1=.)

*** Recoding of key variables:

alpha plh0300_v2 plh0357 plh0358 plh0302_v2 plh0298_v2 plh0308_v2 plh0359 plh0360, item std gen(sge) 

*** First child born between 2014 and 2018 and no second child in that time
gen fc=.
replace fc=0 if ((syear<kidgeb01) | (syear==kidgeb01 & pgmonth<kidmon01)) & (kidgeb01>=2014 & kidgeb01<=2018)
replace fc=1 if ((syear>kidgeb01) | (syear==kidgeb01 & pgmonth>kidmon01)) & (kidgeb01>=2014 & kidgeb01<=2018)
replace fc=. if ((syear>kidgeb02) | (syear==kidgeb02 & pgmonth>kidmon02)) & (kidgeb02>=2014 & kidgeb02<=2018)

*First daughter variable (dvs1):
gen dvs1=.
replace dvs1=0 if fc==1 & kidsex01==1 // First son
replace dvs1=1 if fc==1 & kidsex01==2 // First daughter

*** Controls:
recode sex (1=1)(2=0) , gen(sex_new) //0= Female 1=Male
drop sex 
rename sex_new  sex
label define sex2 0 "Female" 1 "Male"
label val sex sex2

gen age=syear-gebjahr
gen age2=age^2

egen minyear=min(syear) if sampreg!=., by(pid)
gen minsampreg1=sampreg if syear==minyear
egen minsampreg2=mean(minsampreg1), by(pid) 
gen east=1 if loc1989==1
replace east=0 if loc1989==2
replace east=1 if birthregion_ew==22 & east==.
replace east=0 if birthregion_ew==21 & east==.
replace east=1 if east==. & minsampreg2==2
replace east=0 if east==. & minsampreg2==1

label define east 0 "Socialized/Living in West Germany" 1"Socialized/Living in East Germany"
label val east east

recode pgcasmin (0=.) (1/3=0) (4/7=1) (8/9=2), gen(educ)
labe define educ 0"Lower education" 1"Medium education" 2 "Higher education"
label val educ educ 

xtile hhinc_dec= hlc0005_h, n(10)

gen employment_status=.
replace employment_status=1 if  pgstib==10 | pgstib==12  // Not in occupation / unemployed
replace employment_status=3 if  pgstib==13 // Retired
replace employment_status=5 if  pgstib>=210 & pgstib <=340 & (pgemplst==2 | pgemplst==4) // Worker part-time 
replace employment_status=6 if  pgstib>=210 & pgstib <=340 & pgemplst==1 // Worker full-time
replace employment_status=7 if  pgstib>=410 & pgstib <=440 & (pgemplst==2 | pgemplst==4) // Self-employed // freelancer part-time
replace employment_status=8 if  pgstib>=410 & pgstib <=440 & pgemplst==1 // Self-employed // freelancer part-time full-time
replace employment_status=9 if  pgstib>=510 & pgstib <=560 & (pgemplst==2 | pgemplst==4) // Salaried employees  part-time
replace employment_status=10 if pgstib>=510 & pgstib <=560 & pgemplst==1 // Salaried employees  full-time
replace employment_status=11 if pgstib>=610 & pgstib <=640 & (pgemplst==2 | pgemplst==4) // Civil servant (pt)
replace employment_status=12 if pgstib>=610 & pgstib <=640 & pgemplst==1 // Civil cervant (ft)
replace employment_status=2 if (pgstib==11 | pgstib==110 | pgstib==120 | pgstib==130 | pgstib==140 |  pgstib==150 |  pgstib==15) | pgemplst==3 // In education / apprenticeship / internship / Wehr-/Zivildienst
replace employment_status=4 if (plb0019_v2==1 | plb0019_v2==2) // maternity/parental leave

label define employment_status 1"Umemployed" 2"In education/apprenticeship" 3"Retired" 4"Maternal/parental leave" 5"Worker (pt)" 6"Worker (ft)" 7"Self-employed/freelance (pt)" 8"Self-employed/freelance (ft)"  9"Employee (pt)" 	10"Employee (ft)" 11"Civil servant (pt)" 12 "Civil servant (ft)"
label val employment_status employment_status 

recode plh0258_h (6=0) (1=1) (2=2) (4 8 9 =3) (3 5 7 10=4), gen(confession)
label define confession 0 "No confession" 1  "Catholic" 2 "Protestant" 3  "Muslim" 4 "Other confession"
label val confession confession

recode pli0098_v2 (5=0) (4=1) (1 2 3=2), gen(attend_relig)

label define attend_relig 0  "Never" 1 "Less thaan once a month" 2 "At least once a month"
label val attend_relig attend_relig

recode  pgfamstd (3 4 5=0) (1 2 6 7 8=1), gen(legal_marstat)
label define legal_marstat 0  "Neither married nor civil partnership" 1  "Married or civil partnership"
label val legal_marstat legal_marstat

*** Generate variable for existing sibling relationships (There are different variables available with sibling information. We use the earliest information that is available) 

* First generate a variable sibling_v1 that is surveyed between 2014 and 2020 in a few subsamples
global SIBLINGS "lb1092_v1, lb1092_v2, lb1093_v1, lb1093_v2, lb1094_v1, lb1094_v2, lb1095_v1, lb1095_v2, lb1096_v1, lb1096_v2, lb1097_v1, lb1097_v2, lb1098_v1, lb1098_v2, lb1099_v1, lb1099_v2, lb1100_v1, lb1100_v2, lb1101_v1"

gen has_sister=.
replace has_sister=1 if inlist(2, $SIBLINGS) 

gen has_brother=.
replace has_brother=1 if inlist(1, $SIBLINGS)

gen no_siblings=.
replace no_siblings=1 if lb1092_v1==-2 | lb1092_v2 ==-2

gen siblings_v1= .
replace siblings_v1=1 if no_siblings==1
replace siblings_v1=2 if has_sister==1 & has_brother!=1
replace siblings_v1=3 if has_sister!=1 & has_brother==1
replace siblings_v1=4 if has_sister==1 & has_brother==1

* Attention: There is inconsistent availability of information in 2016:  Only pld0029 and pld0031 provide information for 2016 for brothers and sisters
* pld0030 and pld0032 offer 2016 information only for sisters but not for brothers.

gen siblings_v2=.
replace siblings_v2=1 if (pld0029==-2 & pld0031==-2)  // No siblings
replace siblings_v2=2 if (pld0029>0 & pld0029<.) & pld0031==-2 // Only sister(s)
replace siblings_v2=3 if (pld0031>0 & pld0031<.) & pld0029==-2 // Only brother(s)
replace siblings_v2=4 if (pld0031>0 & pld0031<.) & (pld0029>0 & pld0029<.) // Sister(s) and brother(s)

gen siblings_v3=.
replace siblings_v3=1 if (nums==0 & numb==0) | (nums==-2 & numb==-2) | (nums==0 & numb==-2) | (nums==-2 & numb==0) // No siblings
replace siblings_v3=2 if (nums>0 & nums<.) & (numb==0| numb==-2) // Only sister(s)
replace siblings_v3=3 if (numb>0 & numb<.) & (nums==0|numb==-2) // Only brother(s)
replace siblings_v3=4 if (numb>0 & numb<.) & (nums>0 & nums<.) // Sister(s) and brother(s)

xtset pid syear
sort pid syear

* Use the first value being available and replace all subsequent observations by it
forval t=1(1)21{
	replace siblings_v1 = l`t'.siblings_v1 if l`t'.siblings_v1!=. 
	replace siblings_v2 = l`t'.siblings_v2 if l`t'.siblings_v2!=. 
	replace siblings_v3 = l`t'.siblings_v3 if l`t'.siblings_v3!=. 
}

gen years_siblings_v1=.
bysort pid: replace years_siblings_v1=syear if siblings_v1!=. 

gen years_siblings_v2=.
bysort pid: replace years_siblings_v2=syear if siblings_v2!=. 

bysort pid: egen min_years_siblings_v1=min(years_siblings_v1)
bysort pid: egen min_years_siblings_v2=min(years_siblings_v2)

gen siblings=.
replace siblings=siblings_v1 if min_years_siblings_v1<=min_years_siblings_v2 & min_years_siblings_v1<=siblup
replace siblings=siblings_v2 if min_years_siblings_v2<min_years_siblings_v1  & min_years_siblings_v2<=siblup
replace siblings=siblings_v3 if siblup <min_years_siblings_v1 				 & siblup<min_years_siblings_v2

gen minus_year= -syear 
bys pid (minus_year): carryforward siblings, replace

replace siblings=siblings_v3 if siblings==. 

label define siblings 1"No siblings" 2 "Only sister(s)" 3"Only brother(s)" 4"Both" 
label val siblings siblings

recode hgtyp1hh (1 2 3 4 5 6 8=0) (7=1), gen(multigen_hh)
label define multigen_hh 0 "No multi-generation household" 1 "Multi-generation household"
label val multigen_hh multigen_hh

alpha plh0255 plh0215 plh0220 plh0225, item std gen(openness)
alpha plh0214 plh0217 plh0224, item std gen(agreeableness)

*******************
foreach var in openness agreeableness {
	clonevar `var'_temp=`var'
	replace `var'_temp=. if (kidgeb01==syear & ((kidmon01-pgmonth)<=6 & (kidmon01-pgmonth)>=0)) | (kidgeb01==syear+1 & ((kidmon01+12-pgmonth)<=6 & (kidmon01+12-pgmonth)>=0)) 
	xtset pid syear
	replace `var'_temp=l.`var' if ((l.kidgeb01==l.syear & ((l.kidmon01-l.pgmonth)<=6 & (l.kidmon01-l.pgmonth)>=0)) | (l.kidgeb01==l.syear+1 & ((l.kidmon01+12-l.pgmonth)<=6 & (l.kidmon01+12-l.pgmonth)>=0))) & l.`var'_temp==. & l.`var'!=. 
	rename `var' `var'_old
	bys pid (syear): carryforward `var'_temp, gen(`var')
	drop `var'_old `var'_temp
}

recode hgtyp1hh (1 2 4 5 6 7 8 =0) (3=1) , gen(single_parent)
label define single_parent 0 "No single parent household" 1 "Single parent household"
label val single_parent single_parent

********************************************************************************
*** Replace missings controls by 3 earlier waves
xtset pid syear

foreach x in sex east educ hhinc_dec employment_status confession attend_relig legal_marstat  multigen_hh single_parent{
	clonevar `x'_old=`x'
	forval tt=1/3 {
		replace `x'=l`tt'.`x'_old if `x'==.   
	}
}

foreach x in confession {
	forval tt=4/18 {
		replace `x'=l`tt'.`x'_old if `x'==.   
	}
}

replace confession=f1.confession_old if confession==.   
replace confession=f2.confession_old if confession==.   

********************************************************************************
* Sample restriction, selection of controls +  Models 
********************************************************************************

keep if syear==2018
keep if age>=18
keep if fc==1
keep if sge !=. & dvs1!=.

local dep_var="sge"
local key_indvar="dvs1"

regress `dep_var' `key_indvar' 
local obs_comp= e(N)
dis "`obs_comp'"

local controls sex age east educ hhinc_dec employment_status  confession legal_marstat  siblings multigen_hh openness agreeableness single_parent psample // forvalues below the K (=14) has to match number of variables

vl create remaining=(`controls')
vl create remainingtest=(`controls')
	
forvalues  i = 1(1)14{ 
		local max_loss = 0  	// Reset max_loss for each iteration
		local var`i'toremove ""  // Reset variable to be removed for each iteration

		* Identify which variable is the next candidate to be removed
		foreach var of varlist $remaining {
			quietly regress `dep_var' `key_indvar' `var'
			local obs`i' = e(N)
			local loss = `obs_comp' - `obs`i''
			// Check if the current loss is greater than the max_loss
				if `loss' > `max_loss' {
				// Update max_loss and var1toremove
					local max_loss = `loss'
					local var`i'toremove  `var'
				}
		display "`var`i'toremove'"
		}
		
		*Calculate the share of observations remaining in the sample after we remove the candidate  
		vl modify remainingtest=remaining - (`var`i'toremove')
		disp "$remainingtest"
		qui: regress `dep_var' `key_indvar' $remainingtest
		local obs_selvar`i'=e(N)
		local check10_loss`i'=`obs_comp'-`obs_selvar`i''
		local check10_share`i'=(`check10_loss`i''/`obs_comp')
		disp "`obs_comp'"
		disp "`obs_selvar`i''"
		display "`check10_loss`i''"
		display "`check10_share`i''"
		
		* If the loss is above 10 % remove the variable, else stop
		if (`check10_share`i''>0.10 ){
			local varlist `var`i'toremove'
			vl modify remaining=remaining - (`var`i'toremove')
			}
		else {
			local last_var `var`i'toremove'
			break
			display "Iteration stopped."	
			
			display "`last_var'"
			}
	}
	
*Last variable needs to removed manually since the last code breaks immediatelly before surpassing 10 % threshold, but we want variables just after surpassing the 10 % threshold 	
vl modify remaining=remaining - (`last_var')

disp "$remaining"
* Initialize the new varlist
local new_varlist

* Some variables don't go in as continous /dummys, but as categorical or via linear and quadratic terms
foreach var of varlist $remaining {
    if "`var'" == "educ" {
        local new_varlist `new_varlist' i.educ
    }
    else if "`var'" == "employment_status" {
        local new_varlist `new_varlist' i.employment_status
    }
	else if "`var'" == "psample" {
        local new_varlist `new_varlist' i.psample
    }
	else if "`var'" == "confession" {
        local new_varlist `new_varlist' i.confession
    }
	
	else if "`var'" == "siblings" {
        local new_varlist `new_varlist' i.siblings
    }
	
	else if "`var'" == "attend_relig" {
        local new_varlist `new_varlist' i.attend_relig
    }

    else if "`var'" == "age" {
        local new_varlist `new_varlist' age age2
    }
    else {
        local new_varlist `new_varlist' `var'
    }
}


********************************************************************************
*** Models:

regress `dep_var' `key_indvar' `new_varlist', vce(cluster hid)
disp r(table)[1,1] // effect size for H1
dis r(table)[4,1] // p-value for H1

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[dvs1]

		* (ii) standard error
		local se = _se[dvs1]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[dvs1] / _se[dvs1]
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
	file open myfileH1 using "$OUT/h1_results.csv", write replace //restored original naming convention
	file write myfileH1 "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfileH1 "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfileH1

********************************************************************************

exit

