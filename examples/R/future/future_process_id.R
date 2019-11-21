## Examples comparing the "plan" strategies for concurrent
## execution using the future package.
##
## Updated: Nov 21, 2019
## Author: James Henderson

# libraries: ------------------------------------------------------------------
library(future)

# Functions to test: ----------------------------------------------------------
f = function(i) {  Sys.getpid() } 

f_wait = function(i){ 
  Sys.sleep(i)  
  Sys.getpid()
}

# Comparisons: ----------------------------------------------------------------
# Serial computations using `plan(sequential)`
plan(sequential)
results0 = list()
for( i in 1:12 ){
  results0[[i]] = future({f(i)})
}
unlist( lapply(results0, value) )

# Parallel computations using `plan(multisession)`
plan(multisession)
results1 = list()
for( i in 1:12 ){
  results1[[i]] = future({f(i)})
}

# Note the different default relative to mclapply
unique( unlist( lapply(results1, value) ) )
unique( unlist( parallel::mclapply(1:12, f)))
availableCores()

matrix( unlist( lapply(results1, value) ), 4, 3)

# Parallel computations using `plan(multisession)` with different wait pattern
results2 = list()
for( i in 1:12 ){
  results2[[i]] = future({f_wait( 13 - i + i %/% 4 )})
}
matrix( unlist( lapply(results2, value) ), 4, 3)

# Parallel computations using `plan(multicore)`
# ! Note the difference if run in RStudio vs a command line interface
plan(multicore)
results3 = list()
for( i in 1:12 ){
  results3[[i]] = future({f_wait( 1 / 12 )})
}
matrix( unlist( lapply(results3, value) ), 4, 3)

## Globals are exported and/or "frozen" as needed.
some_global = 'pid:'
plan(sequential)
plan_seq = future({paste(some_global, Sys.getpid())})
value(plan_seq)

# In "plan(multisession)", PID is the same each time because the processes are 
# created at the time the plan is declared. 
plan(multisession)
test_ms = function(i){
  plan_ms = future({paste(some_global, Sys.getpid())})
  value(plan_ms)
}
lapply(1:3, test_ms)

# Note the lag as we create new sessions each time.
test_ms2 = function(i){
  plan(multisession)
  plan_ms = future({paste(some_global, Sys.getpid())})
  value(plan_ms)
}
lapply(1:3, test_ms2)

# Use the most recent session
lapply(1:3, test_ms)

# If run from a CLI or in batch mode,
# PID changes each time because the current process is forked at the 
# instant the future is created.
plan(multicore)
test_mc = function(i){
  plan_mc = future({paste(some_global,Sys.getpid())})
  value(plan_mc)
}
lapply(1:3, test_mc)

# Depending on the plan, we excute futures as processes becomes available. 
plan(sequential)
results4 = list()
for( i in 1:12 ){
  results4[[i]] = future({
    cat(sprintf('Start %i: %s\n', i, Sys.time()))
    pid = f_wait(12/i)
    cat(sprintf('Finish %i: %s\n', i, Sys.time()))
    pid
    }) 
}

# PIDs we ran on
matrix( unlist( lapply(results4, value) ), 4, 3)
Sys.getpid()

plan(multisession)
results5 = list()
for( i in 1:12 ){
  results5[[i]] = future({
    cat(sprintf('Start %i: %s\n', i, Sys.time()))
    pid = f_wait(12/i)
    cat(sprintf('Finish %i: %s\n', i, Sys.time()))
    pid
  }) 
}
# PIDs we ran on
matrix( unlist( lapply(results5, value) ), 4, 3)

## Set up a file for printed messages
plan(multisession)
results5 = list()
cat('Plan Multission\n results5\n', file = 'future_results5.txt')
for( i in 1:12 ){
  results5[[i]] = future({
    cat( sprintf( 'Start %i on %s: %s\n', i, Sys.time(), Sys.getpid() ), 
         file = 'future_results5.txt', append = TRUE )
    pid = f_wait(12/i)
    cat( sprintf( 'Finish %i on %s: %s\n', i, Sys.time(), pid ), 
         file = 'future_results5.txt', append = TRUE )
    pid
  }) 
}
unlist(lapply(results5, value))
