## Problem Sets

### Solutions

Solutions to problem set 1 are in the `ps1` folder. There you will find:

  1. `ps1_q1.sh` - a shell script answering question 1
  1. `cutnames.sh` - a shell program answering question 2 
  1. `ps1_q3.R` - an R script with solutions to question 3
  1. `ps1_solutions_pdf.Rmd` - the Rmd file used to produce the pdf version of the solution document.

Solutions to problem set 2 are in the `ps2` folder.  There you will find:

  1. `ps2_q1.R` - an R script with solutions to question 1
  1. `ps2_q2.R` - an R script with solutions to question 2
  1. `ps2_q2_funcs.R` - an R script with functions used by `ps2_q2.R`
  1. `PS2_Solutions.Rmd` - an Rmarkdown file that sources `ps2_q1.R` and `ps2_q2.R` and 
     			   creates `PS2_Soltuions.html`
  1. `PS2_Solutions.html` - an html file with answers (but note code) to problem set 2.

Solutions to problem set 3 are in the `ps3` folder. There you will find:

 1. `ps3_q1.R` - an R script with solutions to question 1.
 1. `ps3_q2.R` - an R script with solutions to question 2.
 1. `PS3_Solutions.Rmd` - the markdown file used to create the html solution.

Solutions to problem set 4 are in the `ps4` folder. There you will find:

 1. `ps4_q1.do`, `ps4_q1.log`, `ps4_q1_results.xlsx` - a Stata script, its log, and
    the results it outputs to answer question 1.
 1. `ps4_q2.do`, `ps4_q2.log`, `ps4_q2_results.csv` - a Stata script, its log, and 
    the results it outputs to answer question 2.
 1. `ps4_q3.do`, `ps4_q3.log`, `ps4_q3_results.xlsx` - a Stata script, its log, and
    the results it outputs to answer question 3. 
 1. `sum_reg.do` - a short Stata script defining a mata function, based on the same
     function defined in `ps4_q1.do`. This function is called by `ps4_q3.do`.  
 1. `ps4_q4.Rmd` - the R markdown file used to produce the solution summary document. 

### Data sets

#### Problem Set 1
Use the following three data sets for the final
part of question four, problem set 1:
  1. train_trajectories.csv
  1. train_measures.csv
  1. test_trajectories.csv.

The file `train_trajectories.csv` contains the raw trajectories
extracted from `mousetrap::KH2017_raw` for the first 6 subjects
and the subset of trials each answered correctly. This file has
columns:
  - `subject_nr` - a subject id,
  - `count_trial` - a trial id, nested within subject,
  - `xpos`, `ypos`, `tm` - the coordinates (x, y, t) of the trajectory.

The file `train_measures.csv` contains the measures your functions
should calculate for the trajectories corresponding to each unique 
combination of subject and trial.  It's columns are:
  - `subject_nr`, `count_trial` - the subject id and trial number,
  - `tot_dist` - the total (Euclidean) distance travelled,
  - `max_abs_dev` - the maximum absolute deviation from the secant,
  - `avg_abs_dev` - the average absolute deviation from the secant,
  - `AUC` - an area under the curve measure allowing cancellation in x
but not y. 

The file `test_trajectories.csv` contains additional trajectories whose
measures you should present in your solution.  

#### Problem Set 4
The dataset created in solution to Problem Set 2, question 2c and used
in problem set 4, question 1, is posted here as `mousetrap_data.csv`.