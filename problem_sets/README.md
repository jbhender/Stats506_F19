## Problem Sets

### Solutions

Solutions to problem set 1 are in the `ps1` folder. There you will find:

  1. `ps1_q1.sh` - a shell script answering question 1
  1. `cutnames.sh` - a shell program answering question 2 
  1. `ps1_q3.R` - an R script with solutions to question 3
  1. `ps1_solutions_pdf.Rmd` - the Rmd file used to produce the pdf version of the solution document.

### Data sets

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