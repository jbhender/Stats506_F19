/* -------------------------------------------------------------------------- *
 * An example SAS program for Stats 506.
 *
 * This file uses column input to read the fixed width file
 *   'ghcnd-stations.txt' from:
 * ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily
 * 
 * This file has format:
 *
 * ------------------------------
 * Variable   Columns   Type
 * ------------------------------
 * ID            1-11   Character
 * LATITUDE     13-20   Real
 * LONGITUDE    22-30   Real
 * ELEVATION    32-37   Real
 * STATE        39-40   Character
 * NAME         42-71   Character
 * GSN FLAG     73-75   Character
 * HCN/CRN FLAG 77-79   Character
 * WMO ID       81-85   Character
 * ------------------------------
 *
 * We create a permanent table as sas7bdat in our default library.
 *
 * Author: James Henderson
 * Upated: Dec 4, 2019
 * -------------------------------------------------------------------------- *
 */
/* 80: ---------------------------------------------------------------------- */

/* sas library: ------------------------------------------------------------- */
libname mylib './data';

/* import fixed width data using column format: ----------------------------- */
data mylib.stations;
 infile './data/ghcnd-stations.txt' dsd;
 input station $ 1-11 lat 13-20 lon 22-30 elev 32-37 state $ 39-40
       name $ 42-71 gsn $ 73-75 hcn  77-79 wmo  81-85;
run; 

/* 80: ---------------------------------------------------------------------- */


