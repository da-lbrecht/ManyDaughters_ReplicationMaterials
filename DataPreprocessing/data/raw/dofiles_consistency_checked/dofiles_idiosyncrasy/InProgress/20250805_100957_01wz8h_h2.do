

*********************************************************************************
** Setup * (Please adapt according to your needs)
*********************************************************************************

//clear all
version 18 // indicate the version of stata you are using here
set maxvar 10000 // extend the maximum number of variables to 10,000

* INSTALL PACKAGES * (Please adapt according to your needs)
* install all packages you need here, for example:
* Adapted by David Albrecht during idiosyncrasy index calculations
cap ssc install require, replace
cap ssc install egenmore, replace
cap ssc install gtools, replace
cap ssc install ereplace, replace
cap ssc install coefplot, replace
cap ssc install reghdfe, replace
cap ssc install ftools, replace


/*
* CONFIGURE FILE PATHS * (Please do not change)
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
/* Adapted by David Albrecht for reproducibility check
global data_in 		"$ROOT/00_data_in"
global data_out 	"$ROOT/00_data_out"
global outputs 		"$ROOT/00_outputs"
*/
global data_in 		"$ROOT/data/raw" // folder, in which the original datasets are stored
global data_out		"$ROOT/data/derived" // folder for output (results, tables)
global outputs 		"$ROOT/out"
*/

********************************************************************************
// Data preparation
********************************************************************************

	
	// Start with PPATH

	use "$IN/ppathl.dta"
	
	label language EN 
	
	
	// Select Reference Population
	* Example: only keep participants from private households
	keep if inlist(pop,1,2)

	
	// Create Minimal Dataset
	keep 	cid pid hid syear ///
			parid partner ///
			casemat /// 	
			/* eintritt erstbefr austritt psample letztbef */		/// Panel infos
			sex gebjahr	///
			immiyear germborn corigin gebmonat loc1989	///
			sexor	///
			birthregion birthregion_ew		///
			phrf	/// weight??
			
			
			
		
	// Merge Datasets we need:
	****************************************************************************

	// Info On Childrens
	merge m:1 pid using "$IN/biobirth.dta", keep(3) nogen 	///
									keepusing(sumkids kidpnr* kidgeb* kidsex*)
	
	// Info On Respondedns Parents
	merge m:1 pid using "$IN/bioparen.dta" , keep(3) nogen 	///
										keepusing(fnr mnr ///
											fybirth mybirth fydeath mydeath ///
											fsedu msedu ///
											fnat mnat freli mreli	///
											fisco88 misco88 fisco08 misco08 ///
											living1 sibl nums numb	///
										)
	
	// Generated Variables - Indivudals
	merge 1:1 pid syear using "$IN/pgen.dta" , keep(3) nogen 	///
										keepusing(pgfamstd ///
												pgisced97 pgisced11	///
												pglabgro pglabnet pgsndjob	///
												pgemplst pglfs pgisco88 pgisco08 ///
												pgtatzeit pgvebzeit pguebstd ///
												pgnace2 pgallbet pgexpue ///
												pgbilzeit pgpsbil pgpbbil01 pgpbbil02 pgpbbil03 pgdegree	///
											)
	
	// Generated HH info	
	merge m:1 hid syear using "$IN/hgen.dta" , keep(3) nogen 	///
										keepusing(hghinc hgnuts1)
	
	// Add info on religion
	merge 1:1 pid syear using "$IN/pl.dta" , keep(3) nogen 	///
										keepusing(pli0098_h plh0258_h)
			
	label language EN 
	
	
	
	
	
********************************************************************************
** Basic data harmonization
********************************************************************************
	
	// Core Covariates
	****************************************************************************
	gen year = syear
	
	gen age=syear-gebjahr
	
	gen female=(sex==2)
	
	gen married=(pgfamstd==1 | pgfamstd==2)

	gen forborn=(germborn==2)

	** education
	gen low_ed=(pgisced11>=0 & pgisced11<=2)
	replace low_ed=. if pgisced11==. | pgisced11 <=0

	gen med_ed=(pgisced11>=3 & pgisced11<=5)
	replace med_ed=. if pgisced11==. | pgisced11 <=0

	gen high_ed=(pgisced11>=6 & pgisced11<=8)
	replace high_ed=. if pgisced11==. | pgisced11 <=0
	
	** HH income Bins
	gcollapse (p25) p25=hghinc (p75) p75=hghinc , by(year) merge
	gen low_income=(hghinc<=p25)
	gen high_income=(hghinc>=p75)
	
	
	** 	Religion ** Impute over years - mnay missings even across sam pid!
	gen religion=1 		if plh0258_h==6					// Atei
	replace religion=2	if inlist(plh0258_h,1,2,3,7)	// Cristiani
	replace religion=3	if inlist(plh0258_h,4,8,9,10)	// Islam
	replace religion=4	if inlist(plh0258_h,5)			// Other
	
	*br pid year religion
	bys pid (year): egen min=min(religion)
	bys pid (year): egen max=max(religion)
	gen delta=(min!=max)
	bys pid (year): ereplace religion=max(religion) if delta!=1 

		** if delta==1 
		** --> take most recent religion as most of outcomes asked in recent waves
		*br pid year religion plh0258_h delta if delta==1
		bys pid (year): egen help=max(year) if delta==1 & plh0258_h!=. & plh0258_h>0
		gen help2=	religion if year==help
		bys pid (year): ereplace religion=max(help2) if delta==1 
		
		drop min max delta help2 help
			
	
	
	** Religion Frequency ** Impute over years  - mnay missings even across sam pid!
	*br pid year pli0098_h plh0258_h
	bys pid (year): egen relig_freq=min(pli0098_h) if pli0098_h>0 & pli0098_h!=.
	bys pid (year): ereplace relig_freq=min(relig_freq) 
	
	

	
	** At least 1 son or daughter
	gen parents=(sumkids>0)			// !!!
	
	
	
	** Sum N of Sons, N of Daughters
	ds kidsex* 
	local sex=r(varlist)
	foreach var of local sex {
		gen f_`var'=1 if `var'==2
		gen m_`var'=1 if `var'==1
	}
	ds f_kidsex* 
	local sex=r(varlist)
	egen N_daughters=rowtotal(`sex')		/// !!!
	
	ds m_kidsex* 
	local sex=r(varlist)
	egen N_sons=rowtotal(`sex')				/// !!!
	
		drop f_kidsex* m_kidsex*
	
	
	**************************************
	gen hlp=N_daughters+N_sons
	gen hlp2=hlp-sumkids
	drop if hlp!=sumkids
	**************************************
	tab hlp2 , m
	drop hlp hlp2 
	
	
	
	** At least 1,2,3 daughters
	gen at_least_1d=(N_daughters>=1)
	gen at_least_2d=(N_daughters>=2)
	gen at_least_3d=(N_daughters>=3)
	
	
	** At least 1,2,3 sons
	gen at_least_1s=(N_sons>=1)
	gen at_least_2s=(N_sons>=2)
	gen at_least_3s=(N_sons>=3)
	
	
	** n of daughters
	gen nd1=(N_daughters==1)
	gen nd2=(N_daughters==2)
	gen nd3=(N_daughters>=3)
	
	
	** n of sons
	gen ns1=(N_sons==1)
	gen ns2=(N_sons==2)
	gen ns3=(N_sons>=3)
	
	
	
	** Gen Age when first born
	ds kidgeb* 
	local year=r(varlist)
	egen help=rowmin(`year')
	gen age_at_first_born=help-gebjahr
		drop help
	
	** Age of last child
	ds kidgeb* 
	local year=r(varlist)
	egen help=rowmax(`year')
	gen age_of_last_born=year-help if help!=. & help>=0
	replace age_of_last_born=. if age_of_last_born<0
	sum age_of_last_born, d
	
	
	** Gender First born
	gen fstborn_d=(kidsex01==2)
	gen fstborn_s=(kidsex01==1)
	

	
	drop kid* help
	
	
// This is the core sample with key infos
save "$DATA/base_sample.dta", replace




********************************************************************************
** HP2 Testing - Political preferences
********************************************************************************
global plh0004 "Political tendency (left-right): Left–Right Self-Placement"
global set1 "plh0004" 
*"Political tendency (left-right): Left–Right Self-Placement"

use "$DATA/base_sample.dta", clear
	

** Gender Attitudes Variables
**********************************
merge 1:1 pid syear using "$IN/pl.dta", keep(3) nogen	///
				keepusing(	///
					$set1 	///
				)
				
label language EN 



********************************************************************************
keep if age>=25 & age<=55
keep if parents==1
******************************************************************************** 
	
		
	** Controls
	global X = "age female married forborn low_ed high_ed low_i high_i i.religion i.hgnuts1 i.year"

	** S.E.
	global vce=" vce(cluster hid)"
	global vce=" vce(robust)"
	*global vce=""
	
	** Abs
	global abs=" abs(hid)"
	global abs=""
	
	** Weight
	global w="[aw=phrf]"
	*global w=""



	
********************************************************************************
** Varialbes and Info
********************************************************************************
preserve
	foreach var in $set1 {
		levelsof year if `var'>=0 & `var'!=. 
		local year_`var'=r(levels)
		
		sum  `var' if  `var'>=0 & `var'!=. 
		local N_`var'=r(N)
	}

	clear
	set obs 100
	gen VAR=""
	gen INFO=""
	gen years=""
	gen N=""
	
	local x=1
	
	foreach var in $set1  {
		replace VAR="`var'" if _n==`x'
		replace INFO="${`var'}" if _n==`x'
		replace years="`year_`var''" if _n==`x'
		replace N="`N_`var''" if _n==`x'
	
		local x=`x'+1
	}
	drop if VAR==""
	gen sepr=(_n<=11)
	list VAR INFO years N, noo  sepby(sepr)
restore



** Invert Scale for set2
	foreach var in plh0004 {
		tab `var'
		sum `var'  if  `var'>=0 & `var'!=. 
		local max=r(max)
		replace `var'=-`var'+`max'+1  if  `var'>=0 & `var'!=. 
		tab `var'
	}

********************************************************************************
** Regression
********************************************************************************

foreach var in $set1   {
    
   
	
	// Model 1
	**************************************************************
	reghdfe `var' at_least_1d $X if `var'>=0 & `var'!=. $w ,  $vce $abs
	scalar N1_`var'=e(N)

		
		** Store Info
		lincom  at_least_1d , level(90)
		scalar b1_`var'=r(estimate)
		scalar ub1_`var'=r(ub)
		scalar lb1_`var'=r(lb)
		scalar P1_`var'=r(p)
		scalar se1_`var'=r(se)
		
	
	
}





// Generate Data bases on stored Estimates and Plot Graphs
********************************************************************************
clear
	
	
	
	// Set Dataset 
	****************************************************************************
	set obs 100
	gen VAR=""
	gen INFO=""

	
	// Model 1
	gen b1=.
	gen ub1=.
	gen lb1=.
	gen P1=.
	gen se1=.
	gen N1=.
	
	
	
	
	// Print stored results
	****************************************************************************
	local x=1
	
	foreach var in $set1 $set2 {			
		replace VAR="`var'" if _n==`x'
		
		// Model 1
		replace b1=b1_`var' if _n==`x'
		replace ub1=ub1_`var' if _n==`x'
		replace lb1=lb1_`var' if _n==`x'
		replace P1=P1_`var' if _n==`x'
		replace se1=se1_`var' if _n==`x'
		replace N1=N1_`var' if _n==`x'
		
		
		replace INFO="${`var'}" if _n==`x'
		*replace INFO=substr(INFO,1,4)
		local x=`x'+1
	}
	
	keep if b1!=.
	
	
	
	
	// Gen Stars
	**************************************************************************** 
	gen PV1=""
	replace PV1="***" 	if P1<=0.01	& P1>0
	replace PV1="**"  	if P1<=0.05 & P1>0.01
	replace PV1="*" 	if P1<=0.1 	& P1>0.05
	
	
	
	// Define Y-axis in Foigures and assign Variable Name or Infomration
	*****************************************************************************
	gen y = _n

	levelsof y, local(forlab)

	foreach xx of local forlab {
		levelsof INFO if y == `xx', c local(help)
		label define ylabel `xx' "`help'", modify
	}

	label values y ylabel

	
	// Grpahs
	****************************************************************************
	sum y
	local max=r(max)
	
	
	// Model 1
	twoway ///
		(rcap lb1 ub1 y if PV1=="", horizontal lcolor(black%50)) 		///
		(scatter y b1 	if PV1=="", msymbol(circle) mcolor(black%50) mlabel(PV1) mlabpos(3) mlabsize(big)) 	///
		///
		(rcap lb1 ub1 y if PV1!="" & b1>0, horizontal lcolor(stc3)) 		///
		(scatter y b1 	if PV1!="" & b1>0, msymbol(circle) mcolor(stc3) mlabel(PV1)  mlabpos(3) mlabcol(red) mlabsize(big)) 	///
		///
		(rcap lb1 ub1 y if PV1!="" & b1<0, horizontal lcolor(stc2)) 		///
		(scatter y b1 	if PV1!="" & b1<0, msymbol(circle) mcolor(stc2) mlabel(PV1) mlabpos(3) mlabcol(red) mlabsize(big)) 	///
		,	///
		xline(0, lc(red))	///
				ylab(,nolabels nogrid) ///
				xlab(, nogrid) ///
				ytitle("")	///
				xtitle("")	///
		title("$plh0004")	///
		legend( order(	2 "{&delta} at least 1 daughter " 1 "90% CI") pos(6) col(2))	///
		name(Model1, replace)  ysize(5) xsize(20)
		

	
graph combine Model1  , scale(0.5) ysize(15) xsize(20)
graph export "$OUT/h2_results.pdf", as(pdf) replace




** Print Regression Table
********************************************************************************
order VAR INFO 	///
		b1 se1 P1 N1	
		
keep VAR INFO 	///
		b1 se1 P1 N1	
	

export delimited using "$OUT/h2_results", replace

* EXTRACT AND EXPORT RESULTS * (Please check whether the suggested code automatically picks up the correct statistics for your chosen regression analysis and adapt if needed)

	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = b1

		* (ii) standard error
		local se = se1

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = ""
		local df_model = ""

		* (v) p-value
		local p_value = P1

		* (vi) number of observations
		local num_obs = N1

		* (vii) number of participants
		local num_participants = ""

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
	



	



