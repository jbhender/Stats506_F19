/* Professor Shedden's Example of using PROC SQL.
 * Stats 506, F19
 * 
 * This script finds all single family homes with 
 * "heating degree days" above 2000.
 *
 * Updated: December 4, 2019
 */
/* 80: ---------------------------------------------------------------------- */

/* sas library: ------------------------------------------------------------- */
libname mydata './data';

/* create the desired table with proc sql: ---------------------------------- */
proc sql;

    create table recs as
        select doeid, reportable_domain, mean(hdd65) as mean_hdd65, cufeetng
        from mydata.recs2009_public_v4
        where typehuq = 2
        group by reportable_domain
        having mean(hdd65) ge 2000;

quit;

/* tables created in proc sql are regular sas tables/datasets: -------------- */
proc print data=recs;

run;

/* (SQL) Question: How would you modify this script to 
 * return only a single row per reportable_domain ?
 */

/* 80: ---------------------------------------------------------------------- */
