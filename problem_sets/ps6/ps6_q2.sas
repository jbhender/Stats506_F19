/* Problem Set 6, Question 2 
 * Stats 506, Fall 2019
 * 
 * Analyze home temperatures and heating types in winter 
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

proc transpose data=lib.recs2015_public_v4 out=lib.brrwt_long prefix=brrwt;
 by DOEID;
 var brrwt1-brrwt96; 
run; 

data lib.brrwt_long(rename=(brrwt1=brrwt)); 
 set lib.brrwt_long;
 repl = _name_;
 drop _name_;

proc sort data=lib.brrwt_long out=lib.brrwt_long;
 by doeid repl; 

/* (b) national average home temperature at night : -------------------------- */

/** point estimates **/
proc means data=lib.recs2015_public_v4;
  weight nweight; 
  var tempnite;
  output out=nighttemp mean=avg_night_temp; 
  where tempnite > 0 ; 
run;

/** replicate estimates **/

/*** merge ***/
data nighttemp_long;
  merge lib.recs2015_public_v4(keep=doeid tempnite)
        lib.brrwt_long(keep=doeid repl brrwt);
  by doeid;
run;

/*** limit and prep for option 1 ***/
data nighttemp_long;
  set nighttemp_long;
  where tempnite > 0;
  tempnite_weighted = brrwt * tempnite; /* For option 1 below. */
run;

/*** sort by replicate for use in by below ***/
proc sort data=nighttemp_long out=nighttemp_long;
 by repl; 

/*** option 1: use proc summary with weighted temps from previous step
proc summary data=nighttemp_long;
 by repl;
 output out=nighttemp_repl1
   sum(tempnite_weighted) = sum_temp
   sum(brrwt) = sum_weight; 
run;

data nighttemp_repl1;
 set nighttemp_repl1;
 avg_night_tmep_repl = sum_temp / sum_weight;
run;
***/

/*** option 2: use proc summary with a weight statement or option ***/
/**** preferred option ****/
proc summary data=nighttemp_long;
 by repl;
 var tempnite; /* / weight = brrwt */
 output out = nighttemp_repl2
   mean(tempnite) = avg_night_temp_repl;
 weight brrwt;
run;

/*** option 3: use proc means with a weight statement as above 
proc means data=nighttemp_long noprint;
 by repl;
 weight brrwt;
 var tempnite;
 output out=nighttemp_repl3 mean=avg_night_temp_repl;
run;
***/

/*** to check all 3 options are equal
proc print data=nighttemp_repl1 nobs(10);
proc print data=nighttemp_repl2 nobs(10);
proc print data=nighttemp_repl3 nobs(10);
run;
***/

/** standard errors for avg_night_temp **/

/*** merge replicate and point estimates ***/
data nighttemp_repl2;
 set nighttemp_repl2;
 help_merge = 1;

data nighttemp; 
 set nighttemp;
 help_merge = 1;

data nighttemp_repl;
 merge nighttemp_repl2 nighttemp;
 by help_merge;
run;

/*** compute squared errors ***/
data nighttemp_repl;
 set nighttemp_repl;
 sq_error = (avg_night_temp_repl - avg_night_temp)**2; 
run;

/*** compute unscaled MSE ***/
proc summary data=nighttemp_repl;
 by help_merge; /* so we can merge on it later */
 var sq_error;
 output out=nighttemp_se
   mean(sq_error) = mse; 
run;

/**merge with point estimate, scale standard error, and form CI **/
data q2b_result;
 merge nighttemp nighttemp_se;
 by help_merge;
run;

data q2b_result;
 set q2b_result;
 se = 2*sqrt(mse);
 lwr = avg_night_temp - &z.*se;
 upr = avg_night_temp + &z.*se;
run;

/* export results */
%csvexport(q2b_result, lib=work)
run;

/* (c): Please see ps6_q2c.sas */

/* 80: ---------------------------------------------------------------------- */

