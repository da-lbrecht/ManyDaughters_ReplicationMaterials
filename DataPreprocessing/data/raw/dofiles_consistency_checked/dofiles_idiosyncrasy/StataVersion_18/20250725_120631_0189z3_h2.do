********************************************************************************
****************** Do-File H2: #ManyDaughters Project ******************
********************************************************************************
* Authors: [REDACTED]
* last modified: 25.07.2025

* ============================
* Setup 
* ============================

* clear all
version 18 
set maxvar 10000 

* INSTALL PACKAGES 
ssc install outreg2
ssc install fre
ssc install carryforward
ssc install distinct

/*
* CONFIGURE FILE PATHS * (Please do not change)
* the following code automatically sets paths based on where you saved the distributed file-package on your system *
if strpos("`c(pwd)'", "code") {
    global ROOT "`c(pwd)'/.."
}
else {
    global ROOT "`c(pwd)'"
}
global IN "$ROOT/data/raw" 			// folder, in which the original datasets are stored
global DATA "$ROOT/data/derived" 	// folder for datasets
global OUT "$ROOT/out" 				// folder for output (results, tables)
*/ 

*check paths
dir 


* ============================
* Data Preparation: Merge    * 
* ============================

	* (1) ppathl = starting dataset
	* ============================
	use pid hid syear parid partner 					/// 
		pop netto psample sampreg syear phrf netto		///
		gebjahr gebmonat sex migback 					///
		using "$IN/ppathl.dta", clear 

		
					
	* (2) + pgen 
	* ============================
		merge 1:1 pid syear using "$IN/pgen.dta"	, nogen keep(master matches) keepusing( /// full merge 
			pid syear 	///
			pgisced97 	/// ISCED 										
			pgfamstd 	/// Marital status 							
			pgpartz 	/// Cohabitation status							
			pgemplst 	/// Employment status							
			pglfs		/// Labor Force Status 											
			pgmode 		/// Mode of interview								
			pgmonth 	/// Month of interview 	
			pgpartnr	/// Partner Indikator annual survey
			)

			* no indicator interviewer sex 
			
	* (3) + pl-respondent 
	* ============================
		merge 1:1 pid syear using "$IN/pl.dta"		, nogen keep(master matches) keepusing( /// full merge
			pid syear	///
			plh0004 	/// Left–Right self-placement (political ideology)	
			pli0043_h 	/// HH Labour: Hours housework on weekday 					
			)

			// NOTE: Infos on Sisters is included through bioparen
			
		*prepare merge of individual variables for partner of each respondent 
			*outcomes observed for two partners in each household 
			rename pli0043_h ptuse3 // HH Labour: Hours housework on weekday 
			
			*check partner indicator  
			recode pgpartnr (-2=.) 			// no partner in survey year  
			tab partner if pgpartnr==.		// 80% no partner in hh 
			*br partner parid pgpartnr if pgpartnr==.	
			
			*check duplicates in partner indicator
			duplicates report pid syear
			duplicates report pgpartnr syear 
			duplicates report pgpartnr syear if pgpartnr !=. 	// missings == duplicates 
			distinct pid 										// 35.228 obs, 4508 indiv 
		
	
		* (3b) + pl-partner  
		* ============================
					
			// (1) prepare pl-partner infos (attitudes, H3)
			preserve 
			
			use pid syear sex using "$IN/ppathl.dta", clear 
			merge 1:1 pid syear using "$IN/pgen.dta"	, nogen keep (master matches) keepusing(pgpartnr) 		
			merge 1:1 pid syear using "$IN/pl.dta"		, nogen keep(master matches) keepusing( ///
				pid syear 	///
				pli0043_h 	/// // HH Labour: Hours housework on weekday 			
			)
			
			*reduce to those indicating partner - otherwise no match possible 
			drop if pgpartnr ==-2 
			drop pgpartnr
			
			*variables observed for partner
			rename sex psex_p
			rename pli0043_h ptuse3_p // HH Labour: Hours housework on weekday 
			
			*create partner-indicator based on pid 
			clonevar pgpartnr = pid
			drop pid
			duplicates report pgpartnr syear // no duplicates 
			
			distinct pgpartnr 				 // 26.554 (4201) potential partner observations 
			
			save "$DATA/h1_temp_1_partner.dta", replace 
			restore 
	
			// (2) prepare main data set for merge 
				
			*a: separate singles (stored - append later) and partnered respondents (master) for 1:1 merge 
			preserve
			keep if pgpartnr==.
			distinct pid 									
			save "$DATA/h1_temp_1_solo.dta", replace  // 16.497 (3708) without partner indicator 
			restore 
	
			*b: match remaining partnered individuals with pre-pared partner infos 
			drop if pgpartnr ==. // 16,497 dropped - otherwise no 1:1 match possible 
			merge 1:1  syear pgpartnr using "$DATA/h1_temp_1_partner.dta", keep (1 3) nogen // no match for 1083 master because of missing partner infos in SOEP
			
			
			*c append single infos again 
			append using "$DATA/h1_temp_1_solo.dta"
			distinct pid 									// 35,228       4508 - original sample again 
		
		
		* (4) + hgen 
		* ============================
			merge m:1 hid syear using "$IN/hgen.dta"	, nogen keep(master matches) keepusing( /// full merge 
			hid syear 	///
			hghinc		/// // HH Income										
			hgnuts1_ew	/// // NUTS: East vs WEst								
			hgowner		/// // Eigentuemer, Mieter, Heimbewohner, etc. 			
			)
		
		
		* (5) + bioparen 
		* ============================
			merge m:1 pid using "$IN/bioparen.dta"	, nogen keep(master matches) keepusing( /// full merge 
			pid 	///
			nums		/// // Number of sisters										
			numb		/// // Number of brothers										
			)	
	

		* (6) + Interview(er) Characteristics 
		* ============================
		
		*N.A.
		
			
		* (7) + childbirth infos (biobirth) 
		* ============================

		*pre-pare infos for those with births 
		preserve 
		
		use pid hid syear using "$IN/ppathl.dta", clear 	
		merge m:1 pid using "$IN/biobirth.dta"		, nogen keep(master matches) keepusing( ///
		biovalid bioyear bioage biokids sumkids kidpnr* kidgeb* kidmon* kidsex* biovalid bioinfo)  
		//structure = for each person-year same obs on child biography data - full merge 

		*gen flag for daugther among children 
		****************************************
		
		
		*1. Goal: Longformat - for each person multiple children: to keep youngest child for each year
		*****
		quietly: mvdecode kidpnr*, mv(-1=.k\-2=.\-3=.v)         
		forvalues x=1/9 {                                 		// reshape needs to have continues count - remove zeros 
			rename kidpnr0`x' kidpnr`x'
			rename kidsex0`x' kidsex`x'
			rename kidmon0`x' kidmon`x'
			rename kidgeb0`x' kidgeb`x'
		}

		reshape long kidpnr kidsex kidmon kidgeb , i(pid syear)  // 9 potential kids for each observation year 

		*clean
		rename _j kidnr 
		lab var kidnr "child rank"

		lab var kidpnr "child person number"
		lab var kidgeb "child birth year"
		lab var kidmon "child birth month"
		lab var kidsex "child sex"

		*check birthorder ranking according to date of birth  
		sort pid syear kidnr
		*br pid syear kidnr kidgeb biovalid // first line = first born 		
	
		*create help-indicator for observed rank 
		recode kidgeb (-2=.)
		bysort pid syear (kidgeb): gen rank_help=_n if kidgeb!=.

		gen flag = 1 if rank_help!=kidnr & kidgeb!=. // compare given rank with observed rank 
		tab flag, m 								 // no problems - in earlier SOEP waves this sometimes did not work

		drop flag rank_help 

	
		*2. Goal: keep only lines with children born after/at date of interview 
		****

		*based on clear syear vs. birth year distinction 
		recode kidgeb(-2=.)
		keep if kidgeb<=syear // drop children not yet born at time of interview 

		*same year -> decide based on month 
		count if kidgeb==syear
		
			*step: create interview date 
			merge m:1 pid syear using "$IN/pgen.dta", keepusing (pgmonth) keep (1 3) // using not fully merged 
			drop _merge
			
			mvdecode pgmonth, mv(-3) // missings in interview month across all years 
			tab pgmonth, m 			
			ge doiym=		syear+pgmonth/12 if pgmonth<.&pgmonth>=0
			replace doiym=	syear+6/12 if doiym==. 								// Assumption: June 
			lab var doiym "date of interview in year and month decimals" 

			*step: create birth date 
			mvdecode kidgeb kidmon, mv(-1 -3)
			ge kbirthym=		kidgeb+kidmon/12 if kidgeb!=. & kidmon!=. 
			replace kbirthym=	kidgeb+6/12 if kbirthym==. & kidgeb!=.	 		// Assumption: June 
			lab var kbirthym "date of child birth in year and month decimals" 

			*drop birth with birth before interview 
			keep if kbirthym<=doiym & kidgeb!=. & kidmon!=. 
			keep if kbirthym<=doiym & kidgeb!=. // check 
			
			count if kbirthym == doiym			// 49 gebmonths = interviewmonths  
			*br pid kbirthym doiym  if kbirthym == doiym // keine missings 

			
		*3. Goal: Identify first daughter
		
			*flag daugther 
			bys pid syear (kidnr): gen flag=1 if kidsex==2
			replace flag = 0 if flag==. 
			
				*indicator: daugther nr x
				bys pid syear kidsex (kidnr): gen daugther=_n if flag==1
				bys pid syear (kidnr): replace daugther=daugther[_n-1] if daugther==. & daugther[_n-1]!=. 
				lab var daugther "daugther(s) rank"
				
				*gebyear first daugther (later for "time with child")
				gen daugthfirst_gebyear = kbirthym if flag ==1 & daugther ==1
			
				
			*flag son 
			drop flag 
			bys pid syear (kidnr): gen flag=1 if kidsex==1
			replace flag = 0 if flag==. 
				
				bys pid syear kidsex (kidnr): gen son=_n if flag==1
				bys pid syear (kidnr): replace son=son[_n-1] if son==. & son[_n-1]!=. 
				lab var son "son(s) rank"		
					
				recode daugther son (.=0)
			
		*clean
		keep pid syear kidnr hid biokids sumkids kidpnr kidsex kbirthym kidgeb kidmon son daugther daugthfirst_gebyear biovalid


		*4 Goal: keep youngest child for each yearly observation -> possible to censor with new child
		bys pid syear (kbirthym): gen help=_n if kbirthym!=. 
		bys pid syear (kbirthym): gen help2=_N if kbirthym!=. 
		keep if help==help2 & kbirthym!=.
		drop help*

		duplicates report pid syear
		save "$DATA/h1_temp_3_child.dta", replace 
	
	restore 
	
	 merge 1:1 pid syear using "$DATA/h1_temp_3_child.dta", keep(1 3)	// normally perfekt match - keep all in ppath without biographie info

	 bysort pid: egen merge2 = max(_merge)
	 tab merge2
	 replace kidnr = 0 if kidnr ==.
	 replace sumkids = 0 if merge2 ==1
	 
	 misstable summarize 	// checked with table after easy merges - no increase in missings due to odd cases 
	 
	 
	*label language EN // change language of the labels to English - but then some labels missing 

	*CHECK
	tab syear // -> ab 2000 bis 2020! 
	
	order _all, alpha
	order pid hid syear, first

	
	
* ============================
* RECODE VARIABLES  				 
* ============================
distinct pid  // 35228 - 4508 


	// GENERATE MAIN INDEPENDENT VARIABLES * 			
	*********************************		

	*H1/H4 Firstborn child is a daugther (+ sample H2/H3)	
	*******************************
	fre daugther
	replace daugther = 0 if kidnr ==0 
	lab var daugther "Nr of daughter(s) at time of interview"
	tab syear daugther, m 	
	
	*new var: indpependent H1/H4
	recode daugther (1/max=1 "daugther") (0=0 "other"), gen (child_daugther)
	lab var child_daugther "Respondent has daughter (H1/H4)" 
	tab child_daugther, m 
	
	
	*H2/H3 First daughter born during observation					
	*******************************	
	tab kidsex, m 
	recode 	kidsex (.=.) ///
			(1=0 "male") ///
			(2=1 "female"), ///
	gen (k_sex)
	
	lab var k_sex "child's sex"
	tab k_sex kidsex, m 
	drop kidsex
	
	
	bys pid (syear): gen child_obs = 1 if 	(kidnr > kidnr[_n-1] & kidnr!=.) 
	gen child_obs2 = child_obs if k_sex ==1
	tab child_obs, m 						// 618 observed births
	tab child_obs2, m 						// 301 observed births to daughters
	
	bys pid (syear): gen help = 1 if _n ==1 & daugther ==1
	bys pid: egen daugther_before = max(help)
	lab var daugther_before "Has daughter before SOEP entry"
	bys pid (child_obs2 syear): gen count = _n if child_obs2 ==1
	replace count = . if daugther_before ==1
	sort pid syear 
	tab count child_obs2					//220 first daughters born
	
	gen child_obs_fem = 1 if count ==1 
	bys pid (syear): carryforward child_obs_fem, replace 
	replace child_obs_fem = 0 if child_obs_fem == . & kidnr!=.
	lab var child_obs_fem "first daugther born"
	
	drop help*


// GENERATE CONTROLS * 					
*********************************		
	
	* Respondent experiences birth of (any) son (H2/H3)
	*******************************		
	gen child_obs_son = 1 if child_obs==1 & k_sex==0 				// nr of birth not important! 
	bys pid (syear): carryforward child_obs_son, replace
	replace child_obs_son = 0 if kidnr !=. & child_obs_son ==. 
	lab var child_obs_son "son born"
	tab child_obs_son, m 
	
	
	* Date of Interview (Help variable)
	*******************************	
	fre pgmonth
	recode pgmonth (min/-1=.)
	ge doiym=syear+pgmonth/12 if pgmonth<.&pgmonth>=0
	replace doiym=syear+6/12 if doiym==. 							// Assumption: June if missing 
	lab var doiym "date of interview in year and month decimals" 
	drop pgmonth		
	
	
	* Age youngest Child at interview (H2 & H3)
	*******************************
	*in months 
	ge kagem=round((doiym-kbirthym)*12) if kbirthym>=0&kbirthym<.&kbirthym<=doiym&doiym<.&kidmon>0
	replace kagem=round(doiym-kidgeb) if kagem==. & kidgeb!=. 
	replace kagem=kagem+1 // add one to distinguish from those without child, i.e. actual month of life
	lab var kagem "youngest child age in months (rounded)"		
	fre kagem
	replace kagem=0 if kidpnr==. 
	
	*in years
	ge kageym=(doiym-kbirthym)
	replace kageym=round(doiym-kidgeb) if kageym==. & kidgeb!=. 
	lab var kageym "younget child age in years and months decimals"
	
	fre kageym 
	replace kageym=0 if kidpnr==. 
	
	*in categories
	recode kagem (0/36=0 "under 3") (37/120=1 "3- under 10") (121/max=2 "10 +"), gen (kagem_cat) 
	lab var kagem_cat "youngest child age in categories"
	
	
	* Respondent: Age at Interview (H1-H4)
	*******************************
	local vars gebjahr gebmonat
	recode `vars' (min/-1=.)
	tab1 `vars', m 						

	*replace info with previous / other waves 
		foreach v in `vars' {
		bys pid (syear): replace `v' = `v'[_n-1] if `v'== . & `v'[_n-1]!= .
		}
		
	* create new variable 	
	gen doibirth= gebjahr+gebmonat/12 if gebjahr<.&gebmonat>=0
	replace doibirth=gebjahr+6/12 if doibirth==. 					// Assumption: June 
	
	gen p_age = doiym - doibirth
	lab var p_age "Respondent age"

	tab p_age, m 
		
	* Clear negative values 
	list gebjahr gebmonat syear if p_age<0 							// "announced birth in syear before"
	replace p_age = . if p_age < 0

	drop gebjahr gebmonat doibirth 
	
	
	* Respondent: Gender (H1 - H4)
	**************
	*recode missings <0
	foreach var in sex psex_p {
    replace `var' = . if `var' < 0
	}																// no changes 
	
	recode sex (1 = 0 "male") ///
			   (2 = 1 "female"), ///
			   gen(p_sex)
	lab var p_sex "Respondent sex"
	tab p_sex sex, m 
	drop sex
	
	recode psex_p (1 = 0 "male") ///
			   (2 = 1 "female"), ///
			   gen(p_sexp)
	lab var p_sexp "Respondents' partner sex"
	tab p_sexp psex_p, m 
	drop psex_p
	
	
	* Respondent: Married (H1-H4)										
	**************		
	recode pgfamstd (min/-1 = .) ///
                (1 6 7 8 = 1 "married") ///	// verh+cohab, verh+ausl, lebenspartn
                (2 3 4 5 = 0 "not married") ///	// other 
			    , gen(p_married)		
				
	lab var p_married "respondent married"
	tab p_married pgfamstd, m 											// Missings 
	drop pgfamstd
	
	
	* Respondent: Cohabitation (H1-H4)
	**************	
	recode pgpartz (min/-1 = .) ///
						(0 = 0 "not cohabiting") 	 ///				// no partner 
						(1 2 3 4 5= 1 "cohabiting")	 ///				// verh+cohab, verh+ausl, lebenspartn 
						, gen(p_cohab)	
		
	lab var p_cohab "respondent cohabiting"
	tab p_cohab pgpartz, m 
	fre p_cohab															// Missings 
				
				
	* Respondent: Employment (H1-H4)
	**************	
	gen p_emp = 3
	replace p_emp = 0 if pglfs == 11 & pgemplst == 1 // Full-time
	replace p_emp = 1 if pglfs == 11 & pgemplst == 2 // Part-time
	replace p_emp = 2 if pglfs == 6

	lab def p_emp 0 "full-time" 1 "Part-time" 2 "unemployed" 3 "other"
	lab val p_emp p_emp
	lab var p_emp "employment status"
	tab p_emp pgemplst, m 
	tab p_emp pglfs, m 
		
	drop pgemplst pglfs 												// missings 
	
	
	* Household:  Income (H1-H4)									
	**************	
		
	*cpi - Consumer Price Index (2015 = 100) (source: destatis ) 
	gen cpi = .
	replace cpi = 59.0 if syear == 1984
	replace cpi = 60.2 if syear == 1985
	replace cpi = 60.2 if syear == 1986
	replace cpi = 60.3 if syear == 1987
	replace cpi = 61.1 if syear == 1988
	replace cpi = 62.8 if syear == 1989
	replace cpi = 64.4 if syear == 1990
	replace cpi = 65.5 if syear == 1991
	replace cpi = 68.8 if syear == 1992
	replace cpi = 71.9 if syear == 1993
	replace cpi = 73.8 if syear == 1994
	replace cpi = 75.1 if syear == 1995
	replace cpi = 76.1 if syear == 1996
	replace cpi = 77.6 if syear == 1997
	replace cpi = 78.3 if syear == 1998
	replace cpi = 78.8 if syear == 1999
	replace cpi = 79.9 if syear == 2000
	replace cpi = 81.5 if syear == 2001
	replace cpi = 82.6 if syear == 2002
	replace cpi = 83.5 if syear == 2003
	replace cpi = 84.9 if syear == 2004
	replace cpi = 86.2 if syear == 2005
	replace cpi = 87.6 if syear == 2006
	replace cpi = 89.6 if syear == 2007
	replace cpi = 91.9 if syear == 2008
	replace cpi = 92.2 if syear == 2009
	replace cpi = 93.2 if syear == 2010
	replace cpi = 95.2 if syear == 2011
	replace cpi = 97.1 if syear == 2012
	replace cpi = 98.5 if syear == 2013
	replace cpi = 99.5 if syear == 2014
	replace cpi = 100.0 if syear == 2015
	replace cpi = 100.5 if syear == 2016
	replace cpi = 102.0 if syear == 2017
	replace cpi = 104.0 if syear == 2018
	replace cpi = 105.5 if syear == 2019
	replace cpi = 105.8 if syear == 2020
	replace cpi = 109.2 if syear == 2021
	replace cpi = 118.7 if syear == 2022

	gen hhincome=(hghinc*(100/cpi)) //inflation-adjusted
	
	//top-coding at the 99th percentile
	sum syear
	local min=r(min)
	local max=r(max)
	forvalues i=`min'/`max' {
    _pctile hhincome if syear == `i' & pop < 3, p(99)
    replace hhincome = r(r1) if hhincome > r(r1) & syear == `i' & !missing(hhincome)
}
	//log-transform
	ge ln_hhincome=ln(hhincome+1)
	lab var ln_hhincome "nat. log of inflation-adjusted net monthly household income in Euros"


	* Context: East West (H1-H4)
	*******************************	
	 tab hgnuts1_ew, m 
	 recode hgnuts1_ew 	(min/-99 =.) ///
						(21=0 "West Germany") ///
						(22=1 "East Germany") ///
						, gen(p_east)
	tab p_east hgnuts1_ew,m 
	drop hgnuts1_ew
	
	
	* Context: Urban vs rural (H1-H4)
	*******************************		
	*N.A. 
		
	* Interview: Mode (H1-H4)
	*******************************		
	tab pgmode, m 
	
	recode pgmode (-1 -3 = .) ///
				(100 110 132 220 = 1 "Interviewer") 				 ///	
				(120 131 133 140 150 210= 0 "Self-admin/CAPI/CAWI")	 ///	
				, gen(p_mode)
	tab  pgmode p_mode, m 
	drop pgmode 																				// Missings 
		
	
	* Interview: Gender Interviewer (H1 - H4)
	*******************************	
	*N.A. as data set "Interview" is not provided 

	
	* Context: Policy periods (H2 & H3)
	*******************************		
	recode syear (2007/max = 0 "2007-20") (2000/2006=1 "2000-2006") (1984/1999=2 "1984-2000") (min/1983=.) (.=.), gen(period) 
	tab period, m
	
		
	* Weights
	*******************************	
	gen xrwght=.
	sum syear
	local min=r(min)
	local max=r(max)
	forvalues i=`min'/`max' {
	qui:sum phrf if syear==`i' & phrf>0
	replace xrwght=phrf/r(mean) if syear==`i'
	}

	lab var xrwght "X-sectional ind weight (adjusted for pop size)"	
	drop phrf

	
// GENERATE DEPENDENT VARIABLES * 	
*********************************				
			
	*H2: Pol Attitude 
	***************
	tab plh0004, m 
	replace plh0004 = . if plh0004 < 0
	
	* Reverse-code so that higher = more left-leaning (original: 0 = far left, 10 = far right)
	gen leftleaning = 10 - plh0004 if !missing(plh0004)
	lab var leftleaning "Respondents Left-Right Self-Placement: Higher=left"

	tab plh0004 leftleaning
	mean leftleaning, over(syear)		// 2005, 09, 14, 19, 4,563 obs 
	drop plh0004

		
	// recode all missings / 	*sampreg 
	************************************************
	
	keep 		pid hid pgpartnr kidpnr syear xrwght	 		///
				netto psample pop hgowner kidnr	child_daugther	///		sample
				p_sex p_sexp p_cohab p_married 					/// 	sample individual 
				leftleaning	 									///		DV: H2
				p_sex					 						/// 	IV: H1 - H4
				child_obs_fem   								///     IV: H2 / H3
				p_age p_emp ln_hhincome p_east p_mode			/// 	controls both (time varying)
 				child_obs_son kagem kageym kagem_cat period	   	// 		controls: H2 / H3  
	

* ============================
* SAMPLE RESTRICTIONS    * 
* ============================

	* Restrictions for H1 to H4
	****************************
	
	* only keep representative subsamples 
	fre psample
	keep if psample==1 | 				/// A West (B: Zuwanderer Gastarbeiter)
			psample==3 | 				/// C EAst (D Zuwanderer)
			(psample>=5 & psample<=6) |	/// refresh normal E & F  (G Hocheinkommen) 
			psample==8 |				/// refresh normal H ((I Incentive (bekommen die volle FRagen?, 17/18 N.A.) 
			psample==10 | psample==11 |	///	refresh J /K (M? Migrantensamples 24-31) 
			psample==12	|				/// representative FID subsamples (exclude low income, large families, singles)
			psample==20 				// Old PIAAC-L N (O-Q alle besondere Samples, Top Shareholder, LGBT, Refugees)
	
			// (dropped: 2 Migration, 4 Migration, G High Income; 9 Innovation, 13/14 selective FID; 15 to 19 Migration & Refugee; 21+ Satellites ); -11,722
			
	*keep  participants from private households 
	fre pop 
	keep if inlist(pop,1,2)		// -2.618 
	drop pop 
	
	*drop Gemeinschaftsunterkunft / Heimbewohner 
	tab hgowner,m  
	drop if hgowner==5 // 		// - 2
	drop  hgowner
	
	*successfull interviews
	tab netto, m 
	keep if netto<20			//-2078

	*Covid 
	keep if syear<2020 
		
	distinct pid				// 18112       2394
	
	
	* heterosexual couples 
	drop if p_sex==p_sexp & p_sex!=.  // - 13
	

	* age restriction: individuals 20-50 at first observation  
	fre p_age 
	keep if p_age >=18 
	
	distinct pid 		//  17724       2351
	
	
	* start folloing respondents once they have entered a partnership at least once 

		* flag first year in which observed with partners 
		bysort pid (syear): gen help = syear if pgpartnr != . & pgpartnr[_n-1] == .
		replace help=syear if pgpartnr != . & _n==1
		by pid: egen first_partner = min(help)

		*restrict 
		drop if first_partner == . // persons without a partner  
		drop if syear < first_partner // entfernt Jahre vor der ersten Partnerschaft 
		drop help first_partner 
		
		tab p_married, m 
		tab p_cohab p_married, m 
		//drop if  p_cohab==0 & p_married==0
		
		distinct pid 		//     13950       1759
			
		

	* additional H2 Restrictions
	****************************

	
	keep 		pid hid pgpartnr kidpnr syear 					///
				kidnr 											///		sample
				leftleaning	 									///		DV: H2
				child_obs_fem   								///     IV: H2 / H3
				p_age p_emp ln_hhincome p_east p_mode			/// 	controls both (time varying)
				p_married p_cohab								/// 	controls both (time varying)
 				child_obs_son kagem kageym kagem_cat period	   	/// 		controls: H2 / H3  
				child_daugther									// 		other

	distinct pid // 13950       1759		
	
	
	
	*restrict to waves with political attitudes information 2005, 2009, 2014, and 2019 
	keep if syear == 2005 | syear == 2009 | syear == 2014 | syear == 2019 
	
	*restrict to nonmissings 
	egen miss = rowmiss (child_obs_fem p_age p_emp ln_hhincome p_east p_mode child_obs_son kagem kageym kagem_cat period)
	tab miss,m 
	
	misstable summarize
	
	keep if miss==0 
	drop if leftleaning==. 
	drop if p_age>55	
	
	
	*Restrict to Childless or only son(s) at _n=1
	bys pid (syear): gen help=_n
	gen help2 = 1 if child_daugther == 1 & help ==1
	bys pid (syear): egen help3 = max(help2)
	drop if help3 == 1 
	
	drop help*
	
	*Observed at least twice
	bys pid: gen help = _N
	drop if help ==1
	
	*observations with daughters at least once before and once after. Check that that's the case
	bys pid: egen help_d = max(child_obs_fem)
	bys pid: egen help2 = min(child_obs_fem) if help_d ==1
	bys pid: egen help3 = max(child_obs_fem) if help_d ==1
	
	drop if help2 == help3 & help_d ==1
	
	drop help* 
	distinct pid 		//     3356 644
	
	
	xtset pid syear 
	
	
	
save "$DATA/h2_dataset_rec.dta", replace 
		
	
* ============================
* HYPOTHESIS TEST FOR 2 * 
* ============================

	/* To test Hypotheses 2,  
	assuming that having daughters leads to more left-leaning political preferences, 
	we estimate a linear fixed effects regression models with a reversed left–right self-placement question  
	'leftleaning' as dependent variable	and the variable 'child_obs_fem' as independent variable, indiciating the birth of a daugther since last observation. */

	*H2
	xtreg leftleaning child_obs_fem ///
		   p_age i.p_emp ln_hhincome p_east p_mode child_obs_son i.kagem_cat i.period ///
		   , fe vce(cluster pid)				

	
	* Extract the required statistics

		* (i) coefficient estimate
		local coefficient = _b[child_obs_fem]

		* (ii) standard error
		local se = _se[child_obs_fem]

		* (iii) family of the test statistic (e.g. z, t, F, Chi²)
		local test_stat_family = "t"

		* (iv) residual and model degrees of freedom (DFs) if applicable
		local df_residual = e(df_r)
		local df_model = e(df_m)

		* (v) p-value
		local p_value = _b[child_obs_fem] / _se[child_obs_fem]
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
	file open myfile using "$OUT/h2_results.csv", write replace
	file write myfile "coefficient,se,family,residual_df,model_df,p_value,num_obs,num_participants,interpretation" _n
	file write myfile "`coefficient', `se', `test_stat_family', `df_residual', `df_model',`p_value',`num_obs',`num_participants',`interpretation'" _n
	file close myfile


	
********************************************************************************

exit
