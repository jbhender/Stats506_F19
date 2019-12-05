/* Case Study using the sleepstudy data from R's lme4 package
 * Stats 506, Fall 2019
 * 
 * This case study is meant to illusrtate some basic SAS usage with a focus
 * on fitting a linear mixed model.
 *
 * Updated: December 2, 2019
 * Author: James Henderson
 */

/* 80: ---------------------------------------------------------------------- */

/* libnames:----------------------------------------------------------------- */
libname mylib '~/github/stats506/F19/week12/';
%let path= ~/github/stats506/F19/week12;

/* data import: ------------------------------------------------------------- */

proc import 
  datafile = "&path./sleepstudy.csv" /*'~/github/stats506/F19/week12/sleepstudy.csv'*/
  out = mylib.sleep_study
  replace;
 delimiter = ','; 
 getnames = yes;

run; 

/** procs for inspecting data **/

proc contents data=mylib.sleep_study;
run;

proc print data=mylib.sleep_study(obs=10);
run;

/* fit the model: ----------------------------------------------------------- */

/* subject level slope and intercept are correlated */
ods output 
 SolutionF = m1_reml_coefs
 CovParms = m1_reml_revar;

proc mixed data=mylib.sleep_study;
 class Subject;
 model Reaction = Days / cl;
 random int days / type=un subject=Subject;
run;

/* refit the model above using maximum likelihood */
ods output
 SolutionF = m1_ml_coefs
 CovParms = m1_ml_revar;

proc mixed method=ml data=mylib.sleep_study;
 class Subject;
 model Reaction = Days / cl;
 random int days / type=un subject=Subject;
run;

/* subject level slope and intercept are uncorrelated */
proc mixed data=mylib.sleep_study;
 class Subject;
 model Reaction = Days; 
 random int days / type=vc subject=Subject;
run;

/* add Days^2 to a copy of the data in working memory */
data work.sleep_study;
 set mylib.sleep_study;
 days_sq = Days * Days;
run;

/* Model with fixed effect for Days squared */
proc mixed data=sleep_study;
 class Subject;
 model Reaction = Days days_sq;
 random int days / type=un subject=Subject;
run;

/* exporting data: ---------------------------------------------------------- */
proc export
  data = work.m1_reml_coefs
  outfile = "./m1_reml_coefs.csv"
  dbms = dlm
  replace;
 delimiter=",";
run;

proc export
  data = work.m1_ml_coefs
  outfile = "./m1_ml_coefs.csv"
  dbms = dlm
  replace;
 delimiter=",";
run;

/* macro for exporting to avoid code duplication: --------------------------- */
%macro csvexport(dataset, lib=work);
 proc export
   data = &lib..&dataset
   outfile = "./&dataset..csv"
   dbms = dlm
   replace;
  delimiter=",";
 run;
%mend;

%csvexport(m1_reml_coef)
%csvexport(m1_reml_revar)
%csvexport(m1_ml_coef)
%csvexport(m1_ml_revar)


/* 80: ---------------------------------------------------------------------- */