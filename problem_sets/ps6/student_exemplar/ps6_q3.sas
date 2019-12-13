/******************************************************************************/
* Stats 506, Fall 2019														   ;
* Problem Set 6, Question 3													   ;
*------------------------------------------------------------------------------;
* This script solves question 3 for Problem Set 6:							   ;
* 3. Use 2015 RECS data to perform following analyses:						   ;
*	(Use proc SQL)														   	   ;
*	b. Estimate the national average home temperature at night, among homes	   ;
*	   that use space heating												   ;
*	c. By census division, estimate the average winter home temperatures	   ;
*	   at night, during the day with someone home, and during the day		   ;
*	   with no one home (when applicable)									   ;
*------------------------------------------------------------------------------;
* Author: Jie Cao (caojie@umich.edu)										   ;
* Last updated on: Dec 10, 2019												   ;
/******************************************************************************/


* 80: -------------------------------------------------------------------------;


/* Directories */
libname ps6 "M:\506\hw\hw6";


/* Formats */
proc format library = ps6.recs_format;
	value division
		1 = "New England"
		2 = "Middle Atlantic"
		3 = "East North Central"
		4 = "West North Central"
		5 = "South Atlantics"
		6 = "East South Central"
		7 = "West South Central"
		8 = "Mountain North"
		9 = "Mountain South"
		10 = "Pacific";
	/*
	value $uatyp
		"U" = "Urban Area"
		"C" = "Urban Cluster"
		"R" = "Rural";
	*/
	/*
	value fuelheat
		1 = "Natural gas from underground pipes"
		2 = "Propane (bottled gas)"
		3 = "Fuel oil/kerosene"
		5 = "Electricity"
		7 = "Wood (cordwood or pellets)"
		21 = "Some other fuel"
		other = "N/A";
	*/
	/*
	value equipmuse
		1 = "Set one temperature and leave it there most of time"
		2 = "Manually adjust the temperature at night or when no one is at home"
		3 = "Program the thermostat to automatically adjust the temperature during the day and night at certain times"
		4 = "Turn equipment on or off as needed"
		5 = "Our household does not have control over the equipment"
		9 = "Other"
		other = "N/A";
	*/
run;

/* Format catalog and search order */
options fmtsearch = (ps6.recs_format);

/* Import 2015 RECS data from web */
filename recs url "https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv";
proc import 
	file = recs 
	out = recs
	dbms = csv;
run;

/* Keep variables needed for this question */
proc sql noprint;
	/* List of BRRWT variables */
	select name into: brrwts separated by ','
	from dictionary.columns
	where upcase(libname) = "WORK" 
	and upcase(memname) = "RECS"
	and upcase(name) contains "BRRWT";

	create table recs_new as
	select doeid, 
		   division format division., 
		   heathome, 
		   temphome, 
		   tempgone, 
		   tempnite, 
		   nweight, 
		   &brrwts.
	from recs;
quit;


**********************************************************;
* b. Estimate national average home temperature at night, ;
*    among homes that use space heating					  ;
**********************************************************;
proc sql;
	create table recs_homeheat as
	select doeid, heathome, tempnite, nweight
	from recs_new
	where heathome = 1;
quit;

/* Point estimate */
proc sql;
	create table nat_avg_tempnite_pe as
	select "National Temperature at Night" as measure, 
		   sum(nweight * tempnite) / sum(nweight) as avg_tempnite
	from recs_homeheat;
quit;

/* Estimate standard error using replicate weights */
proc sql;
	* Get weight for each replicate;
	create table recs_homeheat_r as
	select a.doeid, 
		   a.tempnite, 
		   b.brrid, 
		   b.nweight_r
	from recs_homeheat a 
	left join ps6.brrwt_long b 
	on a.doeid = b.doeid;

	* Compute weighted mean temperature at night for each replicate;
	create table nat_avg_tempnite_r as
	select "National Temperature at Night" as measure, 
		   sum(nweight_r * tempnite) / sum(nweight_r) as avg_tempnite_r
	from recs_homeheat_r
	group by brrid;
	
	* Intermediate step to compute standard error;
	create table nat_avg_tempnite as
	select a.measure,
		   a.avg_tempnite, 
		   b.avg_tempnite_r, 
		   (avg_tempnite_r - avg_tempnite)**2 as diff2 
	from nat_avg_tempnite_pe a 
	left join nat_avg_tempnite_r b 
	on a.measure = b.measure;

	* Compute standard error;
	create table nat_avg_tempnite_se as
	select measure, 
		   2 * sqrt(mean(diff2)) as se
	from nat_avg_tempnite
	group by measure;
quit;

/* Final output */
proc sql;
	create table nat_avg_tempnite_final as
	select a.measure 
				label = "Measurement", 
		   a.avg_tempnite as avg_tempnite format f5.2
				label = "Average temperature at night" , 
		   b.se as se format f5.2
				label = "Standard Error", 
		   put(a.avg_tempnite - 1.96 * b.se, f5.2 -L) as lci 
				label = "Lower bound of 95% CI", 
		   put(a.avg_tempnite + 1.96 * b.se, f5.2 -L) as uci
				label = "Upper bound of 95% CI", 
		   cat("(", calculated lci, ", ", calculated uci, ")") as ci 
				label = "95% Confidence Interval"
	from nat_avg_tempnite_pe a 
	left join nat_avg_tempnite_se b
	on a.measure = b.measure;	
quit;

/* Export results to a csv file */
proc export data = nat_avg_tempnite_final dbms = csv
	outfile = "M:\506\hw\hw6\ps6_q3_b.csv" label replace;
run;


****************************************************************************;
* c. By census division, estimate the average winter home temperatures	    ;
*	 at night, during the day with someone home, and during the day			;
*	 with no one home (when applicable)										;
****************************************************************************;

/* Point estimates */
proc sql;
	create table recs_temp as
	select doeid, 
		   division format division., 
		   temphome, tempgone, tempnite, 
		   nweight
	from recs;
quit;
* Transpose to a long data to compute three temperature at once;
proc transpose data = recs_temp
	out = temp_long(rename = (_name_ = type col1 = temp));
	by doeid division nweight;
	var temphome tempgone tempnite;
run;
* Point estimate- average temperature for each type by census division;
proc sql;
	create table avg_temp_pe as
	select division, 
		   type, 
		   mean(temp) as avg_temp
	from temp_long
	group by division, type;
quit;

/* Standard errors */
proc sql;
	* Get weight for each replicate;
	create table temp_long_r as
	select a.doeid, 
		   a.division, 
		   a.type, 
		   a.temp, 
		   b.brrid, 
		   b.nweight_r
	from temp_long a 
	left join ps6.brrwt_long b 
	on a.doeid = b.doeid;

	* Compute weighted mean temperature for each type for each replicate, by census division;
	create table avg_temp_r as
	select brrid, 
		   division, 
		   type, 
		   sum(nweight_r * temp) / sum(nweight_r) as avg_temp_r
	from temp_long_r
	group by brrid, division, type;

	* Intermediate step to compute standard error;
	create table avg_temp as 
	select a.division, 
		   a.type, 
		   a.avg_temp, 
		   b.avg_temp_r, 
		   (avg_temp_r - avg_temp)**2 as diff2 
	from avg_temp_pe a 
	left join avg_temp_r b 
	on a.division = b.division and a.type = b.type;

	* Compute standard errors;
	create table avg_temp_se as
	select division, 
		   type, 
		   2 * sqrt(mean(diff2)) as se
	from avg_temp
	group by division, type;
quit;

/* Final output */
proc sql;
	create table avg_temp_final as
	select a.division 
				label = "Division", 
		   a.type
				label = "Temperature type", 
		   a.avg_temp 
		   		label = "Point Estimate (average temperature)", 
		   b.se as se format f5.2
				label = "Standard Error", 
		   put(a.avg_temp - 1.96 * b.se, f5.2 -L) as lci 
				label = "Lower bound of 95% CI", 
		   put(a.avg_temp + 1.96 * b.se, f5.2 -L) as uci
				label = "Upper bound of 95% CI", 
		   cat(put(a.avg_temp, f5.2 -L), 
			   " (", calculated lci, ", ", calculated uci, ")") as value
			    label = "Point Estimate (95% CI)"
	from avg_temp_pe a 
	left join avg_temp_se b
	on a.division = b.division and a.type = b.type;	
quit;

* Transpose to a wide table to present three temperatures separately;
proc transpose data = avg_temp_final out = avg_temp_wide(drop = _name_ _label_);
	by division;
	id type;
	var value;
run;
* Label three temperatures;
proc sql;
	alter table avg_temp_wide 
	modify tempnite label = "Average temperature (95% CI) at night", 
		   temphome label = "Average temperature (95% CI) during the day (someone home)", 
		   tempgone label = "Average temperature (95% CI) with no one home";
quit;

/* Export results to a csv file */
proc export data = avg_temp_wide dbms = csv
	outfile = "M:\506\hw\hw6\ps6_q3_c.csv" label replace;
run;
