/* Problem Set 6, Question 3
 * Stats 506, Fall 2019
 * 
 * Analyze home temperatures and heating types in winter 
 * using the 2015 RECS data. This is a repeat of part b
 * from question 2.  We make use of the long weights from part a of that 
 * quesiton.
 *
 * Updated: December 11, 2019
 * Author: James Henderson
 * 
 */ 
/* 80: ---------------------------------------------------------------------- */ 

/* data library: ------------------------------------------------------------ */
%let path = ~/github/ps506/F19/ps6/;
libname lib "&path."; 

/* macro library: ----------------------------------------------------------- */
filename autodir "&path./sasmacro"; /* csvexport.sas lives here */
options sasautos = (autodir, sasautos); 

/* macro variables: --------------------------------------------------------- */
%let z = quantile('NORMAL', .975);

/* (b) national average home temperature at night : ------------------------- */
proc sql; 

 /** point estimates **/
 create table nighttemp as
  select sum(tempnite * nweight) / sum(nweight) as avg_night_temp,
         1 as help_merge
  from lib.recs2015_public_v4
  where tempnite > 0;

 /** replicate estimates **/
 create table nighttemp_repl as
  select sum(r.tempnite * b.brrwt) / sum(b.brrwt) as avg_night_temp_repl,
         1 as help_merge 
  from lib.brrwt_long b
  left join lib.recs2015_public_v4 r
  on r.doeid = b.doeid
  where r.tempnite > 0
  group by repl;

 /** standard error **/
 create table q3b_result as
  select n0.avg_night_temp, 
         2*sqrt(mean( (n1.avg_night_temp_repl - n0.avg_night_temp)**2 )) as se
  from nighttemp_repl n1
  left join nighttemp n0
  on n1.help_merge = n0.help_merge
  group by n0.avg_night_temp;

quit;
run;

data q3b_result;
 set q3b_result;
 lwr = avg_night_temp - &z.*se;
 upr = avg_night_temp + &z.*se;
run;

/* export */
%csvexport(q3b_result);
run;

/* 80: ---------------------------------------------------------------------- */

