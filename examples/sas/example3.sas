/* -------------------------------------------------------------------------- *
 * An example SAS program for Stats 506.
 *
 * This file imports RECS data from:
 *   ./data/recs2009_public.csv
 *   http://www.eia.gov/consumption/residential/data/2009/index.cfm?view=microdata
 *
 * Then creates urban and rural subsets and writes as sas7bdat.
 *
 * Author: James Henderson
 * Updated: Dec 2, 2019
 * -------------------------------------------------------------------------- *
*/
/* 80: ---------------------------------------------------------------------- */

/* data library for reading/writing data: ----------------------------------- */
libname mylib '~/github/Stats506_F19/examples/sas/';

/* Create a rural subset: --------------------------------------------------- */
data rural;
 set mylib.recs2009_public_v4;
  if ur='U' then delete;

data mylib.recs_rural;
  set rural;
run;

/* Create and write an urban subset: ---------------------------------------- */
data urban;
 set mylib.recs2009_public_v4;
  if ur='R' then delete;
 
data mylib.recs_urban;
 set urban;
run;

/* Test final obs between CSV and sas7bdat: ---------------------------------- */
data saslast5;
  set mylib.recs2009_public_v4 nobs=obscount;
  if _n_ gt (obscount - 5); 

proc print data=saslast5;
  var DOEID;
 
data csvlast5; 
 set mylib.recs2009 nobs=obscount;
 if _n_ gt (obscount - 5);

proc print data=csvlast5;
 var DOEID; 

/* 80: ---------------------------------------------------------------------- */
