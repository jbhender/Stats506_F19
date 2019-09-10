## Problem Sets

### Solutions

No solutions have been posted yet.

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