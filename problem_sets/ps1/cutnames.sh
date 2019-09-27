#!/usr/bin/env bash
# Stats 506, Fall 2019
# This script contains solutions for problem set 1, question 2.

# Author: James Henderson
# Date: Sep 24, 2019

# Our solution is a command line utility for 
# extracting columns from a csv file by name.

# Usage: ./cutnames.sh file expression

# Note: to make the script executable, use
# chmod +x ./cutnames.sh
# Otherwise, call as bash ./cutnames.sh

# First argument is the file we are extracting columns from.
file=$1

## The second argument is an (extended) regular
## expression for the column names we want.
expr=$2

## Here is where we do the work
if [ ! -f "$file" ]; then
  # This line echos to stderr rather than stdout 
  echo "Could not find file $file." > /dev/stderr
else
  # get column numbers whose names match expr
  cols=$(
   <"$file" head -n1 |
    tr , \\n |   
    grep -n -E "$expr" |
    cut -f1 -d: |
    paste -s -d, -
  )

  # cut those columns out
  <"$file" cut -f"$cols" -d,
fi
