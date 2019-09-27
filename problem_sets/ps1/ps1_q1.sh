#!/bin/env bash
# Stats 506, Fall 2019
# This script contains solutions for
# problem set 1, question 1.
#
# Author: James Henderson
# Updated: September 25, 2019
# 80: --------------------------------------------------------------------------
# a - download data if not present
file="recs2015_public_v4.csv"
url="https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv"

if [ ! -f "$file" ]; then
  wget $url
fi

# b - extract header row and output to a file with one name per line
new_file="recs_names.txt"

# delete the file if it is present
if [ -f "$file" ]; then
  rm "$new_file"
fi

< $file head -n1 | tr , \\n > "$new_file"

# c - get column numbers for DOEID and the brr weights
# as a comma separated string
cols=$(
  < $new_file
  grep -n -E "DOEID|BRR" |
  cut -f1 -d: |
  paste -s -d, -  
)

# d - cut out the appropriate columns
<"$file" cut -f"$cols" -d, > recs_brrweights.csv

