* ---------------------------------------------------------------------------- *
* Problem Set 4, Question 1 
* Stats 506, Fall 2019
* 
* In this problem we again analyze the Mouse Tracking data comparing
* conditions using four differnt measures of indecision.
*
* Updated: November 17, 2019
* Author: James Henderson
* ---------------------------------------------------------------------------- *

* preliminaries: ---------------------------------------------------------------
version 16.0
log using ps4_q1.log, text replace
*cd ~/github/ps506/F19/ps4/

* import and prepare data: -----------------------------------------------------

// import data
import delimited mousetrap_data.csv, clear

// encode factor variables
encode condition, generate(cond)
encode exemplar, generate(exemp)
generate condition_atypical = cond == 1

// create log curvature measures
foreach var of varlist tot_dist-auc {
 generate log_`var' = log(`var')
}

* fit models and compare outcomes: ---------------------------------------------

// models
foreach var of varlist log_tot_dist-log_auc {
	mixed `var' i.condition_atypical || _all: R.subject_nr || exemp:
    estimates store me_`var'
}

// define a mata function for obtaining estimates and CI's
*! Students could accomplish outputting results in any of a number of ways,
*  this is just one example using the concept of a mata function which was
*  not discussed in class.
* 
* Thanks to the post below for the pointer to mata functions:
* https://www.stata.com/statalist/archive/2011-01/msg00393.html
mata
 void sum_reg () {
   b  = st_matrix("e(b)")
   se = sqrt( diagonal( st_matrix("e(V)") ) )
   l = b' - 1.96 * se
   u = b' + 1.96 * se
   all = (b', se, l, u)
   st_matrix("reg_summary", all)
   st_matrixrowstripe("reg_summary", st_matrixcolstripe("e(b)") )
   cols = ("", "", "", "" \ "est", "se", "lwr", "upr")
   st_matrixcolstripe("reg_summary", cols')
  }
end

// loop over response variables, creating a sheet for each
foreach var of varlist log_tot_dist-log_auc {
  estimates restore me_`var'
  mata sum_reg()
  putexcel set ps4_q1_results.xlsx, modify sheet(`var')
  putexcel A1 = matrix(reg_summary), names
  putexcel A1 = "group"
  putexcel B1 = "term"
}

log close
* 80: --------------------------------------------------------------------------
