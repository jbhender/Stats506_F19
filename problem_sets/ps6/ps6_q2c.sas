/* Problem Set 6, Question 2 part c 
 * Stats 506, Fall 2019
 * 
 * Analyze home temperatures in winter among homes that use space heating
 * using the 2015 RECS data.
 *
 * Updated: December 11, 2019
 * Author: James Henderson
 * 
 */ 

/* 80: ---------------------------------------------------------------------- */ 

/* data library: ------------------------------------------------------------ */
libname lib "~/github/ps506/F19/ps6/"; 

/* macros and variables: ---------------------------------------------------- */
%let z = quantile('NORMAL', .975);
%macro csvexport(dataset, lib=work);
 proc export
   data = &lib..&dataset
   outfile = "./&dataset..csv"
   dbms = dlm
   replace;
  delimiter=",";
 run;
%mend;

/* (a) create long weights: ------------------------------------------------- */
/* This code is commented out as it has been previously run */
/***
proc transpose data=lib.recs2015_public_v4 out=lib.brrwt_long prefix=brrwt;
 by doeid;
 var brrwt1-brrwt96; 
run; 

data lib.brrwt_long(rename=(brrwt1=brrwt)); 
 set lib.brrwt_long;
 repl = _name_;
 drop _name_;

proc sort data=lib.brrwt_long out=lib.brrwt_long;
 by doeid repl;
run; 
***/

/* (c): avg temps by type and census division: ------------------------------ */

/** Transpose temp types to long **/
proc transpose data=lib.recs2015_public_v4 out=lib.temp_long prefix=temp;
 by doeid nweight division;
 var temphome tempgone tempnite;
run;

data lib.temp_long;
 set lib.temp_long;
 where temp1 > 0; 
 type = _name_;
 drop _name_ _label_; 
 rename temp1 = temp;
run;

proc sort data=lib.temp_long out=work.temp_long; 
 by division type;
run; 

/** point estimates **/
proc summary data = work.temp_long;
 weight nweight;
 by division type;
 var temp;
 output out = work.avg_temps
   mean(temp) = avg_temp;
run;

/** replicate estimates **/

/*** data step merge does not allow cartesian products in match merge***/
data work.temps_repl_wide;
 merge lib.recs2015_public_v4(keep = doeid division temphome tempgone tempnite)
       lib.brrwt_long(keep = doeid repl);
 by doeid;
run;

proc sort data=work.temps_repl_wide out=work.temps_repl_wide;
 by doeid repl; 

proc transpose data=work.temps_repl_wide out=work.temps_repl prefix=temp;
 by doeid repl division;
run;

data work.temps_repl;
 merge work.temps_repl lib.brrwt_long;
 by doeid repl; 

data work.temps_repl;
 set work.temps_repl;
 where temp1 > 0;
 type = _name_;
 drop _name_ _label_;
 rename temp1 = temp;
 
proc sort data=work.temps_repl out=work.temps_repl;
 by division type repl; 
run;

proc summary data = work.temps_repl;
 weight brrwt;
 by division type repl;
 var temp;
 output out = work.avg_temps_repl
  mean(temp) = avg_temp_repl;
run;

/** mean squared differences **/
data work.avg_temps_repl;
 merge work.avg_temps_repl work.avg_temps;
 by division type;
run;

data work.avg_temps_repl;
 set work.avg_temps_repl;
 sq_error = (avg_temp_repl - avg_temp)**2;
run;

proc summary data = work.avg_temps_repl;
 by division type;
 var sq_error;
 output out = work.avg_temp_se
   mean(sq_error) = mse;
run;

/** standard errors and confidence limits **/
data work.q2c_result;
  merge work.avg_temps work.avg_temp_se;
  by division type;
  se = 2 * sqrt(mse);
  lwr = avg_temp - &z.*se;
  upr = avg_temp + &z.*se;
  keep division type avg_temp se lwr upr;
run;

/** export **/
%csvexport(q2c_result, lib=work);

/* 80: ---------------------------------------------------------------------- */

