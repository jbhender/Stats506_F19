/* -------------------------------------------------------------------------- *
 * An example of "data step programming".
 * Stats 506, Fall 2019
 *
 * This analysis finds the % of single family homes 
 *  more than 1 sd above average in terms of electrical
 *  usage by cesnsus region.
 *
 * This script illusrates the following data step programming concepts:
 *   - merging tables using a data step
 *   - the 're-merging' technique in which we compute summary stats,
 *      then 're-merge' these into the original table, much like a 
 *      group_by / mutate.   
 *   
 *
 * Author: James Henderson
 * Upated: Dec 4, 2019
 * -------------------------------------------------------------------------- *
 */
/* 80: ---------------------------------------------------------------------- */

/* data library: ------------------------------------------------------------ */
libname mydata './data/';
run;

/* read full data and subset to single family homes: ------------------------ */
data recs1;
 set mydata.recs2009_public_v4;
 if typehuq = 2;
 keep doeid regionc kwh;
run;

/* Sort and then compute summary statistics: -------------------------------- */
proc sort data=recs1 out=recs2;
 by regionc;
run;

proc summary;
 class regionc;
 output out=meanstats1
  mean(kwh) = mean_kwh
   std(kwh) = std_kwh;

proc sort data=meanstats1 out=meanstats2;
    by regionc;
run;

/* "Remerge" summary stats into sorted recs data: --------------------------- */
data recs3;
    merge recs2 meanstats2(keep=regionc mean_kwh std_kwh);
    by regionc;

/* Filter to those homes at least 1 sd above mean kwh: ---------------------- */
data recs4;
    set recs3;
    high_kwh = mean_kwh + std_kwh;
    if kwh ge high_kwh;
run;

/* Number of homes above the threshold: ------------------------------------- */
proc summary;           /* Question: What data set is implicitly used ? */
    class regionc;
    output out=high_kwh;

proc sort data=high_kwh out=high_kwh2;
  by regionc;
run;

/* Print to see result: ------------------------------------------------------ */
title "high_kwh2"; 
proc print data=high_kwh2; 

/* Compute the total number of homes within each region:  -------------------- */
proc summary data=recs3;
 class regionc;
 output out=all_kwh;   /* Question: Why don't we need an aggregation statement?*/

title "all_kwh"; 
proc print data=all_kwh;
run;

data all_kwh2;
  set all_kwh;
  if _TYPE_ = 0 then delete; 
  N = _FREQ_;
  keep regionc N;

title "all_kwh2"; 
proc print data=all_kwh2; 
run;

/* Sort to faciltate merging: ------------------------------------------------- */
proc sort data=all_kwh2 out=all_kwh3;
  by regionc;

title "all_kwh3"; 
proc print data=all_kwh3;
run;
  
/* Merge total homes with number above threshold: ----------------------------- */
title "The % of single family homes with heating degree days more than one sd
        above the mean."; 
data pct_tab;
 merge all_kwh3 high_kwh2;
 by regionc;
 if _TYPE_ = 0 then delete;
 high = _FREQ_;
 pct = 100*high / N;
 keep regionc high N pct; 
 
proc print data=pct_tab;
  format pct 4.1;
run; 