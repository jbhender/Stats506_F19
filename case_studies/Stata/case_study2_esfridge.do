*------------------------------------------------------------------------------*
* Stats 506, F19 RECS case study
*
* This script uses logistic regression to learn about the important 
* predictors of whether the primary fridge in a US home is Energy Star 
* compliant.
*
* Data: recs2009_public.csv 
* Source: https://www.eia.gov/consumption/residential/data/2009/csv/recs2009_public.csv 
*
* Author: James Henderson 
* Date:   Oct 31, 2019
*------------------------------------------------------------------------------*
* 80: --------------------------------------------------------------------------

* Script Setup: ----------------------------------------------------------------
version 16.0									// Stata version used
log using RECS_ES.log, text replace	 	   		// Generate a log
cd ~/github/Stats506_F19/case_studies/Stata 	// Working directory

// Windows path for MiDesktop
*cd \\afs\umich.edu\users\j\b\jbhender\Stats506_F19\case_studies

clear					// Start clean

* Data Prep: -------------------------------------------------------------------
import delimited recs2009_public.csv

// Label regions 
label define region_codes 1 "NE" 2 "MW" 3 "S" 4 "W", replace  
label values regionc region_codes 

// Decode missing values
mvdecode esfrig, mv(-2=.\-8=.\-9=.)
*mvdecode _, mv(-2=.\-8=.\-9=.) 

// Label outcome
label define estar 0 "Not Certified" 1 "Energy Star Certified"
label values esfrig estar
tabulate esfrig

//Exercise: write a foreach loop to apply estar to all applicable values.

// Logistic regression
logit esfrig totsqft i.regionc

// Use "logistic" to get estimates as odds ratios
logistic esfrig totsqft i.regionc, nolog

// Rescale house size
replace totsqft = totsqft/100
label variable totsqft "Total Square Feet (100s)"

// Repeat model
logistic esfrig totsqft i.regionc, nolog 

// In interaction, c. is necessary
logistic esfrig c.totsqft##i.regionc, nolog

* Margins: ---------------------------------------------------------------------

/* Adjusted predictions at the means */

// Regional probabilities at mean of totsqft
margins regionc, atmeans cformat("%4.3f")

// Specific values of totsqft for "average" region
margins, at(totsqft=(10 20 30)) atmeans 

/* Marginal Effects */

// marginal effect of region at mean of totsqft
margins, dydx(regionc) atmeans

// average marginal effect of region
margins, dydx(regionc)

// adjusted predictions at representative values
margins regionc, at(totsqft=(10 20 30)) cformat("%4.3f")

// marginal effects at rep. values
margins, dydx(regionc) at(totsqft=(10 20 30)) cformat("%4.3f")

* Script Cleanup: --------------------------------------------------------------
log close
exit

* 80: --------------------------------------------------------------------------
