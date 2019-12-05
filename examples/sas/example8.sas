/* -------------------------------------------------------------------------- *
 * Compute the percent of wood shingled roofs by state 
 *  using PROC SQL and the 2009 RECS data.
 *
 * Author: James Henderson 
 * updated: December 4, 2019
 * -------------------------------------------------------------------------- *
 */ 
/* 80: ---------------------------------------------------------------------- */

/* Set a library: ----------------------------------------------------------- */ 
libname mylib '~/github/Stats506_F19/examples/sas/data/';

/* Tell sas where to find formats: ------------------------------------------ */ 
options fmtsearch=( mylib.recs_formats work ); 
run;

/* Use proc sql to find % wood shingle roof by "State": --------------------- */
proc sql;

  /* Count total homes by state */
  create table total as
    select sum(nweight) as n_total, reportable_domain
      from mylib.recs2009_public_v4
      where rooftype > 0
      group by reportable_domain;

  /* Count wood shingle roofs by state */
  create table wood_roof as
    select sum(nweight) as n_wood, reportable_domain
      from mylib.recs2009_public_v4
      where rooftype=2
      group by reportable_domain;

  /* Join these two tables */
  create table wood_roof_pct as
    select t.reportable_domain as state, n_wood, n_total, 
    	   100*n_wood/n_total as percent_wood
      from total t
      inner join wood_roof w
      on t.reportable_domain=w.reportable_domain
    order by -percent_wood;

  quit;
run;

/* Print the result: -------------------------------------------------------- */
proc print data=wood_roof_pct;
  var state percent_wood;
  format percent_wood 4.1
         state state.;
run;

/* Export to csv: ----------------------------------------------------------- */
proc export data=wood_roof_pct
  outfile = 'wood_roof_pct.csv'
  dbms=dlm replace; 
  delimiter  = ",";
run; 

/* 80: ---------------------------------------------------------------------- */
