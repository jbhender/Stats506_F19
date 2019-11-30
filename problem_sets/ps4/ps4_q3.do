*------------------------------------------------------------------------------*
* Stats 506, Fall 2019
* Problem Set 4, Question 3
* 
* Examination of NHANES data looking at water consumption. 
*  
* Data: 2005 NHANES dietary recall data (DR1TOT_D.XPT, DR2TOT_D.XPT)
*       and demograhics (DEMO_D.XPT)
* 
* https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005
*
* notes:
*   + to use "outreg2" calls, first fun ssc install outreg2
*   + we ignore the sample weights in this script
*
* Dervived data sets: 
*   +
*
* Author: James Henderson
* Updated: November 17, 2019
*------------------------------------------------------------------------------*

* Set up: ----------------------------------------------------------------------
version 16.0
log using ps4_q3.log, text replace
cd ~/github/ps506/F19/ps4

// defines a mata function for summarizing regressions in a matrix that
// can be written to excel
do sum_reg.do

* (a) Import and wrangle: ------------------------------------------------------

// Import and save demographic data 
fdause DEMO_D.XPT, clear
quietly compress
gsort +seqn
rename (riagendr ridexmon ridageyr indfmpir) ///
       (gender winter age pir)
replace winter = 0 if winter == 2 /* examined May 1 - Oct 31 */
keep seqn gender winter age pir
save DEMO_D.dta, replace

// Import and save DR1TOT_D data
fdause DR1TOT_D.XPT, clear
keep seqn-wtdr2 dr1day dr1_320z
quietly compress
gsort +seqn 
save DR1TOT_D.dta, replace

// Import and save DR2TOT_D data
fdause DR2TOT_D.XPT, clear
keep seqn dr2day dr2_320z
quietly compress
gsort +seqn 
save DR2TOT_D.dta, replace

// Merge the two dietary recall data sets
merge 1:1 seqn using DR1TOT_D.dta
drop _merge

// Merge in the demographics
merge m:1 seqn using DEMO_D.dta

// Reshape to long
reshape long dr@day, i(seqn-wtdr2) j(day) 
label variable day "Day of Dietary Recall Interview"

// Plain water reponse variables
generate water = dr1_320z if day == 1
replace water = dr2_320z if day == 2
generate any_water = water != 0 if water != .
label variable any_water "Flag for whether water > 0"
drop water

// Weekday flag
generate weekday = 1
replace weekday = 0 if drday == 1 | drday == 7
label variable weekday "Weekday (M-F) Flag"
drop drday _merge

* (b) prepare data for analysis: -----------------------------------------------

// flag cases with any missing variables
generate missing = 0
foreach var of varlist any_water weekday winter age gender pir {
 replace missing = 1 if `var' == .
}

// center continuous variables at mean of day1 and not missing
foreach var of varlist age pir {
 quietly summarize `var' if missing == 0 & day == 1
 local `var'_mean = r(mean)
 display "`var' mean:",``var'_mean'
 generate `var'_c = `var' - ``var'_mean' /* Note: nested macro used here. */
}

// change age units to decades
replace age_c = age_c / 10

* (c) GLM analysis using day one only: -----------------------------------------

// fit the logistic model
logistic any_water ///
         i.weekday i.winter c.age_c##c.age_c i.gender c.pir_c ///
         if day == 1
*;
mata sum_reg("logit_coef")
		 
// use margins to get impact of each variable on probability scale
margins, dydx(weekday winter gender age_c pir_c) post
*outreg2 using logistic_margins.txt, replace
mata sum_reg("logit_me")

// write info to excel
putexcel set ps4_q3_results.xlsx, sheet("logit_coef") modify
putexcel A1=matrix(logit_coef), names
putexcel A1="group" B1="term"
putexcel set ps4_q3_results.xlsx, sheet("logit_me") modify
putexcel A1=matrix(logit_me), names
putexcel A1="variable"

* (d) Mixed GLM analysis using both days: --------------------------------------
meglm any_water ///
         i.weekday i.winter c.age_c##c.age_c i.gender c.pir_c || seqn:, ///
      family(binomial) link(logit) or
mata sum_reg("mixed_logit_coef")

// use margins to get impact of each variable on probability scale: ------------
*! Caution: this runs a bit slowly! ~ 1 minute
margins, dydx(weekday winter gender age_c pir_c) post
mata sum_reg("mixed_logit_me")

// write marginal effect estimates to a matrix
putexcel set ps4_q3_results.xlsx, sheet("mixed_logit_coef") modify
putexcel A1=matrix(mixed_logit_coef), names
putexcel A1="group" B1="term"
putexcel set ps4_q3_results.xlsx, sheet("mixed_logit_me") modify
putexcel A1=matrix(mixed_logit_me), names
putexcel A1="variable"

* clean up: --------------------------------------------------------------------
log close

*80: ---------------------------------------------------------------------------
