/* Problem Set 6, Question 3
 * Stats 506, Fall 2019
 * 
 * Analyze home temperatures and heating types in winter 
 * using the 2015 RECS data. This is a repeat of parts b and c
 * from question 2.  We make use of the long weights from there.
 * In part c, we also make use of temps_long from question 2, part c.
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

/* (c) avg temp by type (home, gone, night) and census division: ------------ */
proc sql;
 
 /** point estimates **/
 create table avg_temps as
  select division, type, sum(temp * nweight) / sum(nweight) as avg_temp
  from lib.temp_long
  group by division, type;

 /** replicate estiamtes **/
 create table avg_temps_repl as
  select division, type, repl, sum(temp*brrwt) / sum(brrwt) as avg_temp_repl
  from lib.temp_long t
  left join lib.brrwt_long b /* cartesian join */
  on t.doeid = b.doeid
  group by division, type, repl;

 /** standard errors **/
 create table q3c_result as
  select t.division, t.type, t.avg_temp, 
   2*sqrt(mean( (r.avg_temp_repl - t.avg_temp)**2 )) as se
  from avg_temps_repl r
  left join avg_temps t
  on r.division = t.division and r.type = t.type
  group by t.avg_temp, t.division, t.type
  order by division, type; 

quit;
run;

data q3c_result;
 set q3c_result;
 lwr = avg_temp - &z.*se;
 upr = avg_temp + &z.*se;
run;


/* export */
%csvexport(q3c_result);
run;

/* 80: ---------------------------------------------------------------------- */

