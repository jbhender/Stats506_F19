/******************************************************************************/
* Stats 506, Fall 2019														   ;
* Problem Set 6, Question 2													   ;
*------------------------------------------------------------------------------;
* This script solves question 2 for Problem Set 6:							   ;
* 2. Use 2015 RECS data to perform following analyses:						   ;
*	(Use data steps)														   ;
*	a. Reshape the replicate weights to a longer format, save dataset		   ;
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
data recs;
	set recs;
	format division division. /*uatyp10 $uatyp. fuelheat fuelheat. equipmuse equipmuse.*/;
	keep doeid division /*uatyp10*/ heathome /*fuelheat equipmuse*/
		 temphome tempgone tempnite
		 nweight brrwt1-brrwt96;
run;



********************************************************;
* a. Reshape the replicate weights to a longer format 	;
********************************************************;
proc transpose data = recs 
	out = ps6.brrwt_long(rename = (_name_ = brrid col1 = nweight_r));
	by doeid;
	var brrwt1-brrwt96;
run;



**********************************************************;
* b. Estimate national average home temperature at night, ;
*    among homes that use space heating					  ;
**********************************************************;
/* Select homes that use space heating */
data recs_homeheat;
	set recs(keep = doeid heathome tempnite nweight);
	where heathome = 1;
run;

/* Point estimate */
proc means data = recs_homeheat mean noprint;
	weight nweight;
	var tempnite;
	output out = nat_avg_tempnite_pe(drop = _type_ _freq_) mean = avg_tempnite;
run;
data nat_avg_tempnite_pe;
	measure = "National Temperature at Night";
	set nat_avg_tempnite_pe;
run;

/* Estimate standard error using replicate weights */
* Get weight for each replicate;
data recs_homeheat_r;
	merge recs_homeheat(in = a) ps6.brrwt_long (in = b);
	by doeid;
	if a;
run;
* Compute weighted mean temperature at night for each replicate;
proc summary data = recs_homeheat_r;
	class brrid;
	var tempnite;
	weight nweight_r;
	output out = nat_avg_tempnite_r mean = avg_tempnite_r;
run;
data nat_avg_tempnite_r(drop = _type_ _freq_);
	measure = "National Temperature at Night";
	set nat_avg_tempnite_r;
	if _type_ = 0 then delete;
run;
* Compute standard error;
data nat_avg_tempnite;
	merge nat_avg_tempnite_pe nat_avg_tempnite_r;
	by measure;
	diff = avg_tempnite_r - avg_tempnite;
	diff2 = diff**2;
run;
proc means data = nat_avg_tempnite mean noprint;
	var diff2;
	output out = nat_avg_tempnite_se_tmp mean = mean_diff2;
run;
data nat_avg_tempnite_se;
	measure = "National Temperature at Night";
	set nat_avg_tempnite_se_tmp;
	se = 2 * sqrt(mean_diff2);
run;

/* Final output */
data nat_avg_tempnite_final(drop = _type_ _freq_ mean_diff2);
	merge nat_avg_tempnite_pe 
		  nat_avg_tempnite_se;
	by measure;
	/* Label variables */
	label measure = "Measurement"
		  avg_tempnite = "Average temperature at night"
		  se = "Standard Error"
		  lci = "Lower bound of 95% CI"
		  uci = "Upper bound of 95% CI"
		  ci = "95% Confidence Interval";
	format avg_tempnite f5.2 se f5.2;
	/* Calcualte 95% CI */
	lci = put(avg_tempnite - 1.96 * se, f5.2 -L);
	uci = put(avg_tempnite + 1.96 * se, f5.2 -L);
	ci = cat("(", lci, ", ", uci, ")");
run;

/* Export results to a csv file */
proc export data = nat_avg_tempnite_final dbms = csv
	outfile = "M:\506\hw\hw6\ps6_q2_b.csv" label replace;
run;



****************************************************************************;
* c. By census division, estimate the average winter home temperatures	    ;
*	 at night, during the day with someone home, and during the day			;
*	 with no one home (when applicable)										;
****************************************************************************;

/* Point estimates */
data recs_temp;
	set recs(keep = doeid division temphome tempgone tempnite nweight);
run;
* Transpose to a long data to compute three temperature at once;
proc transpose data = recs_temp
	out = temp_long(rename = (_name_ = type col1 = temp));
	by doeid division nweight;
	var temphome tempgone tempnite;
run;
* Average temperature for each type by census division;
proc means data = temp_long mean noprint;
	class division type;
	var temp;
	output out = avg_temp_pe mean = avg_temp;
run;
data avg_temp_pe(drop = _type_ _freq_);
	set avg_temp_pe;
	where _type_ = 3;
run;

/* Standard errors */
* Get weight for each replicate;
data temp_long_r;
	merge temp_long(drop = nweight) ps6.brrwt_long;
	by doeid;
run;
* Compute weighted mean temperature for each type for each replicate, by census division;
proc summary data = temp_long_r;
	class brrid division type;
	var temp;
	weight nweight_r;
	output out = avg_temp_r mean = avg_temp_r;
run;
data avg_temp_r(drop = _type_ _freq_);
	set avg_temp_r;
	where _type_ = 7;
run;
* Compute standard error;
proc sort data = avg_temp_pe;
	by division type;
run;
proc sort data = avg_temp_r;
	by division type brrid;
run;
data avg_temp;
	merge avg_temp_pe avg_temp_r;
	by division type;
	diff = avg_temp_r - avg_temp;
	diff2 = diff**2;
run;
proc means data = avg_temp mean noprint;
	class division type;
	var diff2;
	output out = avg_temp_se_tmp mean = mean_diff2;
run;
data avg_temp_se(drop = _type_ _freq_);
	set avg_temp_se_tmp;
	where _type_ = 3;
	se = 2 * sqrt(mean_diff2);
run;

/* Final output */
data avg_temp_final(drop = mean_diff2);
	merge avg_temp_pe 
		  avg_temp_se;
	by division type;
	/* Labels */
	label division = "Division"
		  type = "Temperature type"
		  avg_temp = "Point Estimate (average temperature)"
		  se = "Standard Error"
		  lci = "Lower bound of 95% CI"
		  uci = "Upper bound of 95% CI"
		  value = "Point Estimate (95% CI)";
	/* Format */
	format avg_temp f5.2 se f5.2;
	/* Calcualte 95% CI */
	lci = put(avg_temp - 1.96 * se, f5.2 -L);
	uci = put(avg_temp + 1.96 * se, f5.2 -L);
	/* Point estimate (95% CI) for presentation */
	value = cat(put(avg_temp, f5.2 -L), " (", 
			    lci, ", ", 
				uci, ")");
run;
* Transpose to a wide table to present three temperatures separately;
proc transpose data = avg_temp_final out = avg_temp_wide(drop = _name_ _label_);
	by division;
	id type;
	var value;
run;
* Label three temperatures;
data avg_temp_wide;
	set avg_temp_wide;
	label tempnite = "Average temperature (95% CI) at night"
		  temphome = "Average temperature (95% CI) during the day (someone home)"
		  tempgone = "Average temperature (95% CI) with no one home";
run;

/* Export results to a csv file */
proc export data = avg_temp_wide dbms = csv
	outfile = "M:\506\hw\hw6\ps6_q2_c.csv" label replace;
run;
