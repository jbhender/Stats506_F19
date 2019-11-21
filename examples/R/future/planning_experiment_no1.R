# Experimenting with future plans

# libraries: ------------------------------------------------------------------
library(future)

# function telling us where we are waiting: -----------------------------------
wait_where = function(x){
  # Wait x seconds, then return the process id for the Rsession you waited in.
  id = Sys.getpid()
  Sys.sleep(x)
  cat( paste0('I waited ', x, ' seconds in process ', id,'.\n') )
  invisible(id)
}
wait_where(1.5)

# plan sequential: ------------------------------------------------------------
plan(sequential)
futures_seq = vector( mode = 'list', length = 9)
system.time({
 for ( i in 1:9 ) {
   results_seq[[i]] = future({ wait_where(i) })
 }
})
sapply(results_seq, resolved)
sapply(results_seq, value)
# Why do the messages print again? 
unique( sapply(results_seq, value) )

# plan multisession: ----------------------------------------------------------
plan(multisession)
futures_ms = vector( mode = 'list', length = 9)
system.time({
  for ( i in 1:9 ) {
    cat(i,'\n')
    futures_ms[[i]] = future({ wait_where(i) })
    print( Sys.time() )
  }
})
sapply(futures_ms, resolved)
results_ms = sapply(futures_ms, value)
# could repeate this multiple times, and always get the same ids
unique(results_ms) 

# plan multicore: -------------------------------------------------------------
plan(multicore)
futures_mc = vector( mode = 'list', length = 9)
system.time({
  for ( i in 1:9 ) {
    cat(i,'\n')
    futures_mc[[i]] = future({ wait_where(i) })
    print( Sys.time() )
  }
})
sapply(futures_mc, resolved)
results_mc = sapply(futures_mc, value)
unique(results_mc) # This will not be the same each time. 

# plan cluster, using an explicit cluster: ------------------------------------
## set up the cluster
cl = parallel::makeCluster(4)
parallel::clusterEvalQ(cl, Sys.getpid())

## set the plan
plan(cluster, workers = cl)

## work on the local cluster
futures_cl = vector( mode = 'list', length = 9)
system.time({
  for ( i in 1:9 ) {
    cat(i,'\n')
    futures_cl[[i]] = future({ wait_where(i) })
    print( Sys.time() )
  }
})
sapply(futures_cl, resolved)
results_cl = sapply(futures_cl, value)
unique(results_cl) 

# plan cluster, without an explicit cluster: ----------------------------------
unlist( clusterEvalQ(cl, Sys.getpid()) )

## set the plan
plan(sequential); plan(cluster)

## work on the local cluster
futures_cl2 = vector( mode = 'list', length = 9)
system.time({
  for ( i in 1:9 ) {
    cat(i,'\n')
    futures_cl2[[i]] = future({ wait_where(i) })
  }
})
sapply(futures_cl2, resolved)
results_cl2 = sapply(futures_cl2, value)
unique(results_cl2) 

# Explicitly close the cluser
stopCluster(cl)
