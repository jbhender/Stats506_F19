#'---
#' title: Problem Set 6, Solutions
#' date: "`r format(Sys.Date(), '%B %d, %Y')`"
#' author: Stats 506, Fall 2019
#' output:
#'   html_document:
#'     theme: united
#'     code_folding: hide
#'---
#'
#+ setup
suppressPackageStartupMessages({
  library(tidyverse); library(data.table)
})

#' ## About
#' This document is an example solution to problem set 6.  The source code for
#' this file and all SAS scripts and output can be found at the course 
#' [repo](https://github.com/jbhender/Stats506_F19/tree/master/problem_sets/ps6/).
#' Below, I've summarized the results from each question and provided some
#' further notes on the SAS scripts.
#' 
#' In addition to my own solution, I'd like to share an exemplary solution
#' from one of your peers, Jie Cao. The solution uses a number of interesting
#' techniques not featured in my solution.  You can find it at the repo under
#' [student_exemplar](https://github.com/jbhender/Stats506_F19/tree/master/problem_sets/ps6/student_exemplar).
#' 
#' ## Question 1
#' 
#' In this question, we re-analyze the mouse-tracking data from previous 
#' questions using linear mixed models for four log-curvature measures. Our
#' models has a fixed effect for "Condition" with "typical" as the reference 
#' value and a binar indicator for "atypical". In addition, we include 
#' variance components or random effects for "subject_nr" and "Exemplar".
#' 
#' Here is the `proc mixed` command used:
#'
#' ```
#' proc mixed data = mouse covtest;
#'  class Condition Exemplar subject_nr;
#'  model log_tot_dist = Condition / cl S;
#'  random subject_nr Exemplar / type=vc;
#' run;
#' ```
#'
#' A few comments:
#'  1. Condition, Exemplar and subject_nr all must appear in the `class`
#'  statement because we want to create indicators for them.
#'  1. The `model` statement specifies the response and "fixed" independent
#'   variables.
#'  1. The `random` statement specifies the variance components. Here, we
#'  ask for seperate effects for `subject_nr` and `Exemplar`. The `type=vc` 
#'  option ensures these two effects are not-correlated.
#'  1. The `covtest` option in the `proc mixed` statement instruct SAS to 
#'  include confidence intervals and test-statistics in the output for the
#'  random effects.
#'  
#' To keep the script succinct, we define a macro to setup, fit, and export key
#' values to be summarized. Here, we organize the values for presentation
#' using R. For an option on how to do this in SAS, please see
#' the exemplary student submission at the repo. 

#+ q1_results

## files to read
where = './q1_results/'
files = dir(where)

## fixed effects: read, name, and apppend
fe_results = lapply( files[grep('^fe', files)], function(fn) {
  df = fread( paste0(where, fn) )
  meas = stringr::str_sub(fn, 12, -5)
  df = df[ !is.na(DF),
           .( measure = meas, Term = paste(Effect, Condition, sep = c(' ', ':')),
             est = sprintf('%.2f (%.2f, %.2f)', 
                                      exp(Estimate), exp(Lower), exp(Upper) )
           )
  ]
  df
})
fe_results = rbindlist(fe_results)

## random effects: read, name, and apppend
re_results = lapply( files[grep('^re', files)], function(fn) {
  df = fread( paste0(where, fn) )
  meas = stringr::str_sub(fn, 12, -5)
  df[, .( measure = meas, Term = CovParm, 
          est = sprintf('%.2f', sqrt(Estimate)) ) ]
})
re_results = rbindlist(re_results)

cln_measure = c(AUC = 'AUC', 
                avg_abs_dev = 'Avg Abs Value',
                max_abs_dev = 'Max Abs Dev',
                tot_dist = 'Total Distance')
cap = 
  '**Table 1.** *Linear Mixed Models results for the log curvature measures.*'
merge( dcast(fe_results, measure ~ Term, value.var = 'est')[,c(1, 3, 2)],
       dcast(re_results, measure ~ Term, value.var = 'est'),
       by = 'measure' ) %>%
  mutate( measure = cln_measure[measure] ) %>%
  knitr::kable(format = 'html', caption = cap) %>%
  kableExtra::kable_styling("striped", full_width = TRUE) %>%
  kableExtra::add_header_above(
    header = c(' ' = 1, 'Independent Variables' = 2, 'Variance Components' = 3))

#' ## Questions 2
#' In this question we use the `quantile` function from SAS and the
#' csvexport macro from the case study. This solution also features 3 options
#' for implementing the aggregate by group pattern.
#' 
#' For this and the next question, part c is split into its own script. One
#' thing to note is that a data-step merge statement will not accomodate 
#' many-to-many or Cartesian joins.  
#' 
#' ## Question 3
#' There are three features of this solution I'd like to draw your attention to:
#' 
#'   1. The flexibility offered by `proc sql` makes for, in my opinion, more
#' succinct and easy to follow scripts.
#'   1. Cartesian joins can be done in `proc sql`.
#'   1. I show here how to reference a macro library to avoid repeating macro
#' definitions within every script.
#'
