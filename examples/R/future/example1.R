# A simple illustrative example of using futures
# Stats 506, Fall 2019
#
# Updated: November 12, 2019
# Author: James Henderson

# libraries: -------------------------------------------------------------------
library(future)

## set the plan for evaluating futures
plan(multisession)

# example 1: -------------------------------------------------------------------
system.time(
  {
    a <- {
      Sys.sleep(5)
      2
    }
    
    b <- {
      Sys.sleep(3)
      3
    }
    result1 = a * b
  }
)

# implicit futures using %<-% 
system.time(
  {
    a %<-% {
      Sys.sleep(5)
      2
    }
    
    b %<-% {
      Sys.sleep(3)
      3
    }
    result2 = a * b
  }
)
result2

# explicit futures 
system.time(
  {
    a_future = future({
      Sys.sleep(5)
      2
    })
    
    b_future = future({
      Sys.sleep(3)
      3
    })
    result3 = value(a_future) * value(b_future)
  }
)
result3
