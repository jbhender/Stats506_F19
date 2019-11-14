*------------------------------------------------------------------------------*
* Stats 506, Fall 2018
* Problem Set 2, Question 2
* 
* Examination of NHANES data looking at the average age a tooth is lost.
*  
* Data: 2005 NHANES oral health data (OHX_D.XPT) and demograhics (DEMO_D.XPT)
* https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&CycleBeginYear=2005
* https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005
*
* Dervived data sets: OHX_D.dta, OHX_DEMO_merge.dta
*
* An alternt version of 
* this script utilizes the outreg2 command written by Oscar Torres-Reyna.
* If not previously installed, run: ssc install outreg2
*
* Author: James Henderson
# Updated: October 19, 2018 
*------------------------------------------------------------------------------*
*80: ---------------------------------------------------------------------------

* Set up: ----------------------------------------------------------------------
version 14.2
log using ps2_q2.log, text replace

* (a) Import and merge: --------------------------------------------------------

// Import and save OHX_D data
fdause OHX_D.XPT, clear
quietly compress
gsort +seqn
save OHX_D.dta, replace

//Import DEMO_D and merge OHX_D
fdause DEMO_D.XPT, clear
quietly compress
gsort +seqn
merge 1:1 seqn using OHX_D.dta

// Reduce to matched data
keep if _merge==3
save OHX_DEMO_merge.dta, replace

// Reduce to the variables of interest
keep seqn riagendr ridagemn ridreth1 indfmpir ohx04htc sdmvpsu sdmvstra wtmec2yr

// Clean up variable names and value labels
rename ohx04htc tooth4
rename indfmpir pir
rename riagendr gender
rename ridagemn age
rename ridreth1 race

// Generate indicators for race/ethnicity
// with (3) non-hispanic white as ref
generate mexamer = 0
replace mexamer = 1 if race == 1
generate black = 0
replace black = 1 if race == 4
generate other = 0
replace other = 1 if race == 5 | race == 2

// Label gender
label define labgender 1 male 2 female, replace
label values gender labgender

// Drop missing values
drop if tooth4 == 9
drop if tooth4 == .
drop if age == .

// Collapse the outcome variable
generate y = 1
replace y = 0 if tooth4==1
compress

* (b) logistic reression: ------------------------------------------------------
logit y age

// compute number of months
matrix b = e(b)
mata:
 b = st_matrix("b")
 p = (.25/.75, 1, .75/.25)
 o = ln(p)
 a = o - b*(0,0,0\1,1,1)
 a = a/b[1] 
// a = round(a) 
 st_matrix("a", a)
end
matrix list a

// Use multiples of 12 that just span this range
// 96, 108, 120, 132, 144

* (c) multivariable logistic regression: ---------------------------------------

putexcel set ps2_q2_c.xls, replace

// model 1: BIC = 1533.4
quietly logit y age
estat ic

putexcel A2="age"
putexcel B2=matrix(r(S))

// model 2: BIC = 1542.1, drop
quietly logit y age gender
estat ic
putexcel A3="age  gender"
putexcel B3=matrix(r(S))

// model 3a: BIC = 1529.3, keep
quietly logit y age i.black
estat ic
putexcel A4="age i.black"
putexcel B4=matrix(r(S))

// model 3b: BIC = 1533.1, drop
quietly logit y age i.black i.mexamer
estat ic
putexcel A5="age i.black i.mexamer"
putexcel B5=matrix(r(S))

// model 3c: BIC = 1536.2, drop 
quietly logit y age i.black i.other
estat ic
putexcel A6="age i.black i.other"
putexcel B6=matrix(r(S))

// model 4: BIC = 1460.9 vs 1462.9 drop

 *  Note: pir has additional missing values, we should compare BIC between
 *        the base model and pir using the same set of cases to be rigorous.
 *        We preserve the existing data set, then drop these cases. 

preserve
drop if pir == .

quietly logit y age i.black
estat ic
putexcel A7="age i.black (reduced)"
putexcel B7=matrix(r(S))

quietly logit y age i.black pir
estat ic
putexcel A8="age i.black pir"
putexcel B8=matrix(r(S))

restore

// model 5: BIC = 1538.1, drop
quietly logit y c.age##i.black // note the use of c.age!
estat ic
putexcel A9="c.age##i.black"
putexcel B9=matrix(r(S))

// refit model we wish to use
quietly logit y age i.black 

// Output results to Excel
// outreg2 using ps2_q2_c.txt, replace

matrix b = e(b)
matrix V = e(V)
mata:
 b = st_matrix("b")
 v = st_matrix("V")
 se = sqrt( diagonal(v) )
 se = se'
 st_matrix("se", se) 
end

putexcel set ps2_q2_c_coef.xls, replace
putexcel A2="term" A3="est" A4="se"
putexcel B1=matrix(b), colnames
putexcel B4=matrix(se)

* (d) margins: -----------------------------------------------------------------
// Because we have no additional variables, parts 2 and 3 are redundant.


putexcel set ps2_q2_d.xls, replace

// adjusted predictions at mean for "black"
margins, at(age=(96 108 120 132 144)) atmeans post
//outreg2 using ps2_q2_d_adjpred.txt

putexcel set ps2_q2_d.xls, sheet(adj_pred_est) replace
putexcel A2=matrix(e(b))

putexcel set ps2_q2_d.xls, sheet(adj_pred_v) modify
putexcel A2=matrix(e(V))

// Marginal effects for black at representative ages
quietly logit y age i.black
margins, dydx(black) at(age=(96 108 120 132 144)) post

//margins black, at(age=(96 108 120 132 144)) post
//outreg2 using ps2_q2_d_margins.txt, replace 

putexcel set ps2_q2_d.xls, sheet(me_black_est) modify
putexcel A2=matrix(e(b))

putexcel set ps2_q2_d.xls, sheet(me_black_v) modify
putexcel A2=matrix(e(V))

// Below are the "predictive margins" for black
// at representative ages. 

//quietly logit y age i.black
//margins black, at(age=(96 108 120 132 144)) post
//outreg2 using ps2_q2_d_margins.txt, replace

// The average marginal effect, in this case just the
// difference between the margins above. 

quietly logit y age i.black
margins, dydx(black) at(age=(96 108 120 132 144)) post
//outreg2 using ps2_q2_d_me.txt, replace 

putexcel set ps2_q2_d.xls, sheet(me_black_est) modify
putexcel A2=matrix(e(b))

putexcel set ps2_q2_d.xls, sheet(me_black_v) modify
putexcel A2=matrix(e(V))

* (e) survey weighted: ---------------------------------------------------------
svyset sdmvpsu [pweight=wtmec2yr], strata(sdmvstra) vce(linearized)

svy: logit y age i.black

// Output resuts to excel
// outreg2 using ps2_q2_e.txt, replace

matrix b = e(b)
matrix V = e(V)
mata:
 b = st_matrix("b")
 v = st_matrix("V")
 se = sqrt( diagonal(v) )
 se = se'
 st_matrix("se", se)
end

putexcel set ps2_q2_e_coef.xls, replace
putexcel A2="term" A3="est" A4="se"
putexcel B1=matrix(b), colnames
putexcel B4=matrix(se)


// You didn't need to refit margins but I've done so here.
margins, dydx(black) at(age=(96 108 120 132 144)) 
margins black, at(age=(96 108 120 132 144)) post 
//outreg2 using ps2_q2_e_adjpred.txt, replace   


// If including "post" above we have to refit the model
quietly logit y age i.black 

margins, dydx(black) at(age=(96 108 120 132 144)) post
outreg2 using ps2_q2_e_me.txt, replace 

log close
*80: ---------------------------------------------------------------------------