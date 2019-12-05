/* -------------------------------------------------------------------------- *
 * An example SAS program for Stats 506.
 *
 * This file uses the 2009 RECS data from:
 *   http://www.eia.gov/consumption/residential/data/2009/index.cfm?view=microdata
 *  to Find the state with the highest proportion of wood shingles.
 *
 * This script illustrates:
 *   - simple data step programming
 *   - proc format to create a new variable format
 *   - how to save and link to formats in a sas library
 *   - use of proc summary for group_by / summarize style computations.
 * 
 * Author: James Henderson
 * Updated: Dec 4, 2019
 * -------------------------------------------------------------------------- *
*/
/* 80: ---------------------------------------------------------------------- */

/* data library for reading/writing data: ----------------------------------- */
libname mylib '~/github/Stats506_F19/examples/sas/data/';

/* create a short name for recs data: --------------------------------------- */
data recs;
 set mylib.recs2009_public_v4;
 n_rt2=0;
 if rooftype=2 then n_rt2=nweight;
 if rooftype=-2 then delete;
 keep n_rt2 rooftype reportable_domain nweight doeid regionc;

/* format statement for states: --------------------------------------------- */
proc format library=mylib.recs_formats;
 value state
       1="Connecticut, Maine, New Hampshire, Rhode Island, Vermont"
       2="Massachusetts"
       3="New York"
       4="New Jersey"
       5="Pennsylvania"
       6="Illinois"
       7="Indiana, Ohio"
       8="Michigan"
       9="Wisconsin"
       10="Iowa, Minnesota, North Dakota, South Dakota"
       11="Kansas, Nebraska"
       12="Missouri"
       13="Virginia"
       14="Delaware, District of Columbia, Maryland, West Virginia"
       15="Georgia"
       16="North Carolina, South Carolina"
       17="Florida"
       18="Alabama, Kentucky, Mississippi"
       19="Tennessee"
       20="Arkansas, Louisiana, Oklahoma"
       21="Texas"
       22="Colorado"
       23="Idaho, Montana, Utah, Wyoming"
       24="Arizona"
       25="Nevada, New Mexico"
       26="California"
       27="Alaska, Hawaii, Oregon, Washington";

/* Tell SAS where to find this format later */
options fmtsearch=( mylib.recs_formats work );

/* create grouped summary table: -------------------------------------------- */
proc summary data=recs;
 class reportable_domain;
 output out=totals
        sum(nweight) = total
        sum(n_rt2) = rt2_total; 

proc print data=totals;
title "Output table 'totals' from proc summary using a 'class' statement.";

/* alternately use a by statement: ------------------------------------------ */
proc sort data=recs; 
 by reportable_domain;

proc summary data=recs;
 by reportable_domain;
 output out=totals_by
   sum(nweight) = total
   sum(n_rt2) = rt2_total; 

proc print data=totals_by; 
title "Output table 'totals_by' from proc summary using a 'by' statement.";

/* compute the percentages in a data step: ---------------------------------- */
data pct_wood_shingles;
 set totals;
 if _TYPE_=1;			/* Why do we need this line ? */
 pct = 100*rt2_total/total;
 keep reportable_domain pct;
 label reportable_domain = 'State(s)' pct = '% Wood Shingled Roofs';

/* sort from highest to lowest: --------------------------------------------- */ 
proc sort data=pct_wood_shingles;
 by pct;

/* print results using format to control appearance of output: -------------- */
proc print data=pct_wood_shingles noobs label;
 format pct 4.1 reportable_domain state.;
title "Proportion of homes with wood shingles in each state (2009 RECS)."; 

run;
/* 80: ---------------------------------------------------------------------- */
