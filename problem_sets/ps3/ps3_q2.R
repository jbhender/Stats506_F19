# Solution to Question 1, Problem Set 3
# Stats 506, Fall 2019
#
# In this problem we write and use vectorized functions for 3 types of bootstrap # confidence intervals and also the jackknife.
#
# We will focus on the case where we have two groups, X ~ F_x and Y ~ F_y, and
# wish to estimate E[ X ] / E[ Y ] by \bar{x} / \bar{y}. 
# 
#
# Author: James Henderson
# Updated: October 19, 2019
# 80: --------------------------------------------------------------------------

# libraries: -------------------------------------------------------------------
library(tidyverse)

# flag, asking to refit results (when TRUE) even if the result_file exists: ----
result_file = './ps3_q2c_results.RData'
refit_results = FALSE

# (a) CIs using jackknife std errors: ------------------------------------------
confint_jackknife_mc = function(x, y, level = 0.95) {
  # This function computes a confidence interval for the ratio of expectations.
  # Inputs:
  #   x, y - numeric matrices, with each row representing a single Monte
  #          Carlo sample. They need not have the same number of columns.
  #  level - the desired confidence level, defaults to 95%
  # Returns:
  #   estimated standard errors of the estimators mean(x[i, ]) / mean(y[i, ])
  #   for all rows i.
  # 
  # Details:  the jackknife standard error is the rescaled standard deviation,
  #  of the estimates formed by leaving each observation out in turn. We compute
  #  confidence intervals for each pair of rows, x, y using this standard error.
  
  # error checking
  stopifnot( is.numeric(x) && is.numeric(y) )
  stopifnot( is.matrix(x) && is.matrix(y) )
  stopifnot( nrow(x) == nrow(y) ) # both samples need same mcreps
  stopifnot( all(!is.na(x)) && all(!is.na(y)))
  # We will assume we do not need to handle missing values.
  
  # Note, if n_x or n_y is large it may be beter to store rowMeans or rowSums
  # compute all the means of x[-i , j], xbarloo = 'xbar leave one out'
  xbarloo = { rowSums(x) - x } / {ncol(x) - 1} # mcrep by n_x
  
  # compute all the means of y[-i], ybarloo = 'ybar leave one out'
  ybarloo = { rowSums(y) - y } / {ncol(y) - 1} # mcrep by n_y
  
  # compute the ratio estimator for each loo mean, jk_ests is nx + ny by mcrep
  jke = cbind( xbarloo / rowMeans(y), rowMeans(x) / ybarloo )
 
  # compute the standard errors for each Monte Carlo replicate
  # should be equivalent to, but more efficient than: apply(jke, 1, sd)
  se_jack = sqrt( rowSums({jke - rowMeans(jke)}^2) * 
                            {ncol(jke) - 1} / {ncol(jke)} )
  
  # point estimates
  est = rowMeans(x) / rowMeans(y)
  
  # multiplier and data frame (tibble) with results
  m = qnorm(1 - {1 - level} / 2)
  tibble(
    method = 'jackknife',
    estimate = est, 
    lower = est - m * se_jack,
    upper = est + m * se_jack
  )
}

# (b) CIs using bootstrap methods: ---------------------------------------------
confint_boot_mc = function(x, y, R = 1e2, level = 0.95, debug = FALSE) {
  # Function to compute 
  # Inputs:
  #   x, y - numeric matrices, with each *column* representing a single Monte
  #          Carlo sample. They need not have the same number of columns.
  #  level - the desired confidence level, defaults to 95%
  #      R - the number of bootstrap replicates to use
  #  debug - if true, additional tests are performed to check intermediate output
  
  # Returns:
  #   estimated 95% CI ofs the estimators mean(x[i, ]) / mean(y[i, ])
  #   for all rows i.
  # 
  # Details: this function returns three types of bootstrap CIs constructed 
  #  using (1) the precentile method, (2) the basic bootstrap, and (3) a normal
  #  approximation using the bootstrap estimator for the standard devation. 
  
  # form the bootstrap samples
  xind = sample( 1:nrow(x), size = R * ncol(x) * nrow(x), replace = TRUE )
  yind = sample( 1:nrow(y), size = R * ncol(y) * nrow(y), replace = TRUE )

  # to index column j, we add nrow(x) * {j - 1} to each column "j" of indices
  # which occurs every R * n samples
  xind = xind + rep( nrow(x) * {1:ncol(x) - 1}, each = R * nrow(x))
  yind = yind + rep( nrow(y) * {1:ncol(y) - 1}, each = R * nrow(y))
  
  # the results are n x R x mcrep
  xb = x[xind]
  yb = y[yind]
  
  dim(xb) = c(nrow(x), R, ncol(x))
  dim(yb) = c(nrow(y), R, ncol(y))
  
  # Check that we have retained the structure
  if ( debug ) {
   stopifnot( all( xb[, , 1]  %in% x[, 1]) )
   stopifnot( all( yb[, , ncol(y)] %in% y[, ncol(y)] ) )
  }
  
  # compute the boostrap statistics for each MC sample
  xbar =  colMeans(xb, dims = 1)
  ybar =  colMeans(yb, dims = 1)
  
  # check the lengths here
  if ( debug ) {
    stopifnot( dim(xbar)[2] == ncol(x) && dim(ybar)[1] == R)
  }
  
  # compute the ratios
  theta = xbar / ybar
  
  # compute the quantiles, ok to use a loop here
  m = c( {1 - level} / 2, 1 - {1 - level} / 2)
  theta_q = apply(theta, 2, quantile, probs = m)
  
  # compute the bootstrap std errors
  se_boot = sqrt( R * colMeans( {theta - colMeans(theta)}^2 ) / { R - 1} )
 
  # point estimates
  est = colMeans(x) / colMeans(y)
  
  # final bootstrap CI's
  tibble( 
    method = rep(
     c('percentile bootstrap', 'basic bootstrap', 'normal bootstrap'), 
       each = ncol(x)),
    estimate = rep(est, 3),
    lower = c(theta_q[1, ], 2 * est - theta_q[2, ], est + qnorm(m[1])*se_boot),
    upper = c(theta_q[2, ], 2 * est - theta_q[1, ], est + qnorm(m[2])*se_boot)
  ) 
}

# (c) Monte Carlo study for exponential x and y: -------------------------------
nx = 30; mux = 1
ny = 20; muy = 2
theta =  muy / mux # target ratio, mux and muy are actually rate not mean (1/r)

# Monte Carlo sampling in blocks, to avoid allocating too long vectors
mcrep = 1e3 # per block * 10 blocks

if ( refit_results || !file.exists(result_file) ) {
  jk_time = boot_time = result_list = vector( mode = 'list', length = 10 )
  for ( block in 1:10 ) {
    x = matrix(rexp( mcrep * nx, mux), mcrep, nx)
    y = matrix(rexp( mcrep * ny, muy), mcrep, ny)
    
    
    jk_time[[block]] = system.time( {
      result_jk = confint_jackknife_mc(x, y)
    })
    
    boot_time[[block]] = system.time( {
      result_boot = confint_boot_mc( t(x), t(y), R = 1e4)
    })
    result_list[[block]] = bind_rows(result_jk, result_boot)
    
    cat('Finished block ', block, '\n')
    print(jk_time[[block]] + boot_time[[block]])
  }
  save( result_list, jk_time, boot_time, file = result_file)
} else {
  load( result_file )
}

## Jackknife results
# confint_jackknife(x[1,], y[1,])  ## quick check, fn defined in ps3_q1.R

## Bootstrap results
## combine and comptue key stats
result = bind_rows(result_list)
result_tab = result %>%
  group_by(method) %>%
  summarize( 
    cov_prob = mean( lower < theta & upper > theta),
    se_cov = sqrt( cov_prob * { 1 - cov_prob} / n() ),
    length = mean(upper - lower),
    se_length = sd( upper - lower ) / sqrt( n() ),
    shape = mean( {upper - estimate} / {estimate - lower} ),
    se_shape = sd(  {upper - estimate} / {estimate - lower}) / sqrt( n() )
  )
