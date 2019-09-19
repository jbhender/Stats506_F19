# Vectorization, Example 1
#
# In this example, we carry out a small Monte Carlo study investigating the
# coverage of nominal confidence intervals.
#
# This example is based on Prof. Shedden's notes here: 
# http://dept.stat.lsa.umich.edu/~kshedden/Courses/Stat506/r_vectorization/
#
# Updated: September 18, 2019
# Author: James Henderson
# 80: --------------------------------------------------------------------------

# simulation parameters: -------------------------------------------------------
mcrep = 10000                 # Simulation replications
n = 30                        # Sample size we are studying

# simluate the data: -----------------------------------------------------------
target = 1                    # value we are estimating
xmat = rexp(n * mcrep)        # Simulate standard exponential data
dim(xmat) = c(mcrep, n)       # Each row is a dataset


# A step-by-step approach using "apply": ---------------------------------------

## Compute a Monte Carlo estimate
mn = apply(xmat, 1, mean)     # Sample mean of each row
std = apply(xmat, 1, sd)      # Sample SD of each row
se = std / sqrt(n)            # Standard errors of the mean
conf_level = .95              # Nominal confidence level
m = qt( 1 - {1 - conf_level} / 2, df = n - 1) # Multiplier for confidence level
lcb = mn - m*se               # lower confidence bounds
ucb = mn + m*se               # upper confidence bounds
cvrg_prob = mean( {lcb < target} & {ucb > target} ) # coverage probability

print(cvrg_prob)

## Estimate the Monte Carlo error
mc_se = sqrt(cvrg_prob * {1 - cvrg_prob} / mcrep)
cvrg_prob_str = sprintf("%4.2f (%5.3f, %5.3f)", 
                        cvrg_prob, cvrg_prob - m * mc_se, cvrg_prob + m * mc_se)

cvrg_prob_str

# The above approach as a function: -------------------------------------------

estimate_nominal_coverage = 
  function(rgen, target, mcrep=1e4, n=30, conf_level=.95, ...){
    ## Function to estimate nominal coverage probabilities
    # rgen       - a function generating a vector of simulated data, i.e rexp(),
    #              with length equal to its first argument.
    # target     - the actual expectation of rgen()
    # mcrep, n   - the number of Monte Carlo replications and sample size, respectively.
    # conf_level - the nominal coverage probability 
    # ...        - additional parameters to pass to rgen
    
    xmat = rgen(n * mcrep, ...)  # Simulate data
    dim(xmat) = c(mcrep, n)      # Each row is a dataset
    mn = apply(xmat, 1, mean)    # Sample mean of each row
    std = apply(xmat, 1, sd)     # Sample SD of each row
    se = std / sqrt(n)           # Standard errors of the mean 
    m = qt( 1 - {1 - conf_level} / 2, df = n - 1) # Multiplier for confidence level
    lcb = mn - m * se              # lower confidence bounds
    ucb = mn + m * se              # upper confidence bounds
    
    mean( (lcb < target) & (ucb > target)) # coverage probability
  }

# Exercise: use vectorization to improve the performance of the function above. 

# 80: --------------------------------------------------------------------------
