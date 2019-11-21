# Temporal Experiment Number 2
# 
# Three tasks, all of which involve waiting ... but now 
# with (the potential for) concurrency.

# libraries: ------------------------------------------------------------------
library(future)

# Sequential code: ------------------------------------------------------------
wait = function(x) {
  # Wait x seconds
  Sys.sleep(x)
  cat( paste0('Waited ', x, ' seconds.\n') )

  # return the current time, invisibly
  invisible(Sys.time())
}
wait(3)
ex_time = wait(3)
ex_time

system.time({
  ## Task 1
  f1 = future({ wait(44) })
  
  ## Task 2
  f2 = future({ wait(29) })
  
  ## Task 3
  f3 = future({ wait(15)})
  
  f1_tm = value(f1)
  f2_tm = value(f2)
  f3_tm = value(f3)
})

# What order do you think the futures resolved in? 
lapply( list(f1_tm, f2_tm, f3_tm), print)

# Inspect the future object itself
f1

# Concurrent evaluation: ------------------------------------------------------
plan(multisession) # sets up a (local) PSOCK cluster of R sessions and
                   # instructs futures to be evaluated in them
system.time({
  ## Task 1
  f1 = future({ wait(44) })
  
  ## Task 2
  f2 = future({ wait(29) })
  
  ## Task 3
  f3 = future({ wait(15)})
  
  f1_tm = value(f1)
  f2_tm = value(f2)
  f3_tm = value(f3)
})

# We block when requesting the value, if there are sufficient workers: --------
system.time({
  ## Task 1
  f1 = future({ wait(44) })
  
  ## Task 2
  f2 = future({ wait(29) })
  
  ## Task 3
  f3 = future({ wait(15)})
})
sapply( list(f1, f2, f3), resolved)
f3_tm = value(f3)
f2_tm = value(f2)
f1_tm = value(f1)

# Read more about plans: ------------------------------------------------------
#?plan
plan(transparent) # sets up a (local) PSOCK cluster of R sessions and
# instructs futures to be evaluated in them
system.time({
  ## Task 1
  f1 = future({ r1 = 0; wait(44); r1=0 })
  
  ## Task 2
  f2 = future({ wait(29) })
  
  ## Task 3
  f3 = future({ wait(15)})
})
sapply( list(f1, f2, f3), resolved)

