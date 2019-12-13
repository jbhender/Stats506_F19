/* Problem Set 6, Question 1
 * Stats 506, Fall 2019
 *
 * In this problem, we re-analyze the (computer) mouse tracking data from
 * problem sets 1, 2, and 4. 
 *
 * Updated: December 2, 2019
 * Author: James Henderson
 */

/* 80: ---------------------------------------------------------------------- */

/* libnames:----------------------------------------------------------------- */
libname mylib '~/github/ps506/F19/ps6';
%let path = ~/github/ps506/F19/ps4;

/* data import: ------------------------------------------------------------- */

proc import
  datafile = "&path./mousetrap_data.csv"
  out = mouse
  replace;
 delimiter = ',';
 getnames = yes;
run;

/* prep for analysis: ------------------------------------------------------- */
data mouse;
  set mouse; 
  log_tot_dist = log(tot_dist);
  log_avg_abs_dev = log(avg_abs_dev);
  log_max_abs_dev = log(max_abs_dev);
  log_AUC = .;
  if AUC > 0 then
   log_AUC = log(AUC);

/* Uncomment to run
proc contents data=mouse;
run;
*/ 

/* fit first model: --------------------------------------------------------- */

/* uncomment to run
ods output
 SolutionF=fe_est_tot_dist
 CovParms=re_var_tot_dist;

proc mixed data = mouse covtest;
 class Condition Exemplar subject_nr;
 model log_tot_dist = Condition / cl S;
 random subject_nr Exemplar / type=vc;
run;

proc export 
 data = fe_est_tot_dist
 outfile = "./q1_results/fe_est_tot_dist.csv"
 dbms = dlm
 replace;
 delimiter=",";
run;

proc export
 data = re_var_tot_dist
 outfile = "./q1_results/re_var_tot_dist.csv"
 dbms = dlm
 replace;
 delimiter=",";
run;

*/

/* macro to fit multiple models: -------------------------------------------- */
%macro fit_model(var);

 /* ods setup */
 ods output
  SolutionF=fe_est_tot_dist
  CovParms=re_var_tot_dist;

 /* model fitting */
 proc mixed data = mouse covtest;
  class Condition Exemplar subject_nr;
  model &var = Condition / cl S;
  random subject_nr Exemplar / type=vc cl;
 run;

 /* export results */
 proc export
  data = fe_est_tot_dist
  outfile = "./q1_results/fe_est_&var..csv"
  dbms = dlm
  replace;
  delimiter=",";
 run;

 proc export
  data = re_var_tot_dist
  outfile = "./q1_results/re_var_&var..csv"
  dbms = dlm
  replace;
  delimiter=",";
 run;

%mend;

/* use the macro for each variable: ----------------------------------------- */
%fit_model(log_tot_dist)
%fit_model(log_avg_abs_dev)
%fit_model(log_max_abs_dev)
%fit_model(log_AUC)


/* 80: ---------------------------------------------------------------------- */
