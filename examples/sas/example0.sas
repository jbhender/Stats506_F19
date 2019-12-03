/* -------------------------------------------------------------------------- *
 * An example SAS program for Stats 506.
 *
 * This file imports RECS data from:
 *   ./data/recs2009_public.csv
 *   http://www.eia.gov/consumption/residential/data/2009/index.cfm?view=microdata
 *
 * Then prints some basic information about the file.
 *
 * Author: James Henderson 
 * Updated: Dec 2, 2019
 * -------------------------------------------------------------------------- *
*/
/* 80: ---------------------------------------------------------------------- */

/* import delimited data in a data step: ------------------------------------ */
data test;
 infile './data/test_file.csv' dsd dlmstr=',' firstobs=2 truncover;
 input v1 :$5. v2 :$3. v3 :$3.;
run;

proc print data=test;
proc contents data=test;
run;


/* import delimited data with proc import: ---------------------------------- */
proc import datafile='./data/recs2009_public.csv' out=recs;

/* print the first 5 rows for a few variales: ------------------------------- */
proc print data=recs(obs=5);      /* (obs=5) is an 'option' */
    var DOEID NWEIGHT REGIONC;    /* This is a 'statement'  */

/* get some basic information about the imported data: ---------------------- */
proc contents data=recs;

run;

/* 80: ---------------------------------------------------------------------- */
