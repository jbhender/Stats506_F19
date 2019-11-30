* ---------------------------------------------------------------------------- * 
* Problem Set 4, Question 2
* Stats 506, Fall 2019
* 
* Which US census division has the largest disparity in internet access 
* between its urban and rural areas?
*
* We provide an answer to this question using the RECS 2015 microdata from: 
*   https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv
*
* Updated: November 17, 2019
* Author: James Henderson
* ---------------------------------------------------------------------------- * 

* preliminaries: ---------------------------------------------------------------
version 16.0
log using ps4_q2.log, text replace
*cd ~/github/ps506/F19/ps4

* data prep: -------------------------------------------------------------------
import delimited recs2015_public_v4.csv, clear

generate urban = "urban"
replace urban = "rural" if uatyp10 == "R"

// division labels
label define divnames ///
  1 "New England" ///
  2 "Middle Atlantic" ///
  3 "East North Central" ///
  4 "West North Central" ///
  5 "South Atlantic" ///
  6 "East South Central" ///
  7 "West South Central" ///
  8 "Mountain North" ///
  9 "Mountain South" ///
  10 "Pacific" 

label values division divnames

* setup replicate weights in long form: ----------------------------------------
preserve
keep doeid brrwt*

reshape long brrwt, i(doeid) j(repl)
save recs2015_v4_brrwt_long, replace
restore

* point estimates: -------------------------------------------------------------
// minimal variables
keep doeid nweight urban division internet
preserve

// compute weighted sums and proportion
generate ninternet = internet * nweight
collapse (sum) p_int=ninternet N=nweight, by(division urban)
replace p_int=p_int / N
drop N

// reshape to wide for differencing
reshape wide p_int, i(division) j(urban) string
generate p_intdiff = p_inturban - p_intrural
save prop_internet, replace
restore

* replicate estimates: ---------------------------------------------------------
// merge in replicate weights
drop nweight
merge 1:m doeid using recs2015_v4_brrwt_long

// compute weighted sums and proportions
generate ninternet = internet * brrwt
collapse (sum) p_int=ninternet N=brrwt, by(division urban repl)
replace p_int = p_int / N
rename p_int repl_p_int
drop N

// reshape to wide
reshape wide repl_p_int, i(division repl) j(urban) string
generate repl_p_intdiff = repl_p_inturban - repl_p_intrural

* std error computations: ------------------------------------------------------
// merge in point estimates
merge m:1 division using prop_internet.dta

// compute squared differences
foreach var of varlist p_intrural-p_intdiff {
  generate se_`var' = (`var' - repl_`var')^2 
}

// compute the MSE and scale
collapse (mean) se_*, by(division)
foreach var of varlist se_* {
  replace `var' = 2 * sqrt(`var')
}

* merge with point estimates and compute CI bounds: ----------------------------
merge 1:1 division using prop_internet.dta
drop _merge

foreach var of varlist p_* {
  generate `var'_lwr = `var' - 1.96 * se_`var'
  generate `var'_upr = `var' + 1.96 * se_`var'
}

// reorder and drop se
drop se_*
order division *diff* *urban* *rural*

* export: ----------------------------------------------------------------------
export delimited ps4_q2_results.csv, replace

log close
* 80: --------------------------------------------------------------------------
