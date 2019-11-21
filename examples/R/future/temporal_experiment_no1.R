# Temporal Experiment Number 1
# 
# Three tasks, all of which involve waiting.

# Sequential code: ------------------------------------------------------------
wait = function(x) {
  # Wait x seconds
  Sys.sleep(x)
  cat( paste0('Waited ', x, ' seconds.\n') )
}

system.time({
  ## Task 1
  wait(44)
  
  ## Task 2
  wait(29)
  
  ## Task 3
  wait(15)
})

# In sequential code, each task is blocking so the next can't start until the
# previous ones are finished.  

