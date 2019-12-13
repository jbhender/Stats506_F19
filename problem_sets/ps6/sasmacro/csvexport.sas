/* Export a table from sas to csv
 * 
 * If specifying a library, you must do so by name using lib=.
 * 
 * Updated: December 11, 2019
 * Author: James Henderson
 */ 
%macro csvexport(dataset, lib=work);
 proc export
   data = &lib..&dataset
   outfile = "./&dataset..csv"
   dbms = dlm
   replace;
  delimiter=",";
 run;
%mend;
