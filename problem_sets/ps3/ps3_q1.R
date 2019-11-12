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

# (a) CI using jackknife estimator for std error: ------------------------------
confint_jackknife = function(x, y, level = .95){ 
 # This function constructs a confidence interval for the ratio of 
 #   expectations using the jackknife standard error.
 # Inputs:
 #   x, y - numeric vectors, they do not need to be the same length
 #  level - the desired confidence level, defaults to 95%
 # Returns:
 #   the estimated standard error of the estimator mean(x) / mean(y)
 #
 # Details: the jackknife standard error is the rescaled standard deviation,
 #  of the estimates formed by leaving each observation out in turn. Missing
 #  values are omitted with a warning. 
  
 # error checking
 stopifnot( is.numeric(x) && is.numeric(y) )

 # handle missing values
 xna = is.na(x)
 n_na_x = sum(xna)
 if ( n_na_x > 0 ) {
   warning( sprintf("Omitting %i missing values from x.", n_na_x))
   x = x[!xna]
 }
 
 yna = is.na(y)
 n_na_y = sum(yna)
 if ( n_na_y > 0 ) {
   warning( sprintf("Omitting %i missing values from y.", n_na_y))
   y = y[!yna]
 }
  
 # compute all the means of x[-i], xbarloo = 'xbar leave one out'
 xbarloo = { sum(x) - x } / {length(x) - 1}
 
 # compute all the means of y[-i], ybarloo = 'ybar leave one out'
 ybarloo = { sum(y) - y } / {length(y) - 1}

 # compute the std dev of the ratio estimators for each loo mean
 se_jack = sd( c( xbarloo / mean(y), mean(x) / ybarloo) )
 
 # rescale 
 n = length(x) + length(y)
 se_jack = se_jack * {n - 1} / sqrt(n)
 
 # return the confidence interval
 mean(x) / mean(y) + qnorm( c({1 - level} / 2, 1 - {1 - level} / 2) ) * se_jack
}

# (b) CIs using bootstrap resampling: -----------------------------------------
confint_boot = function(x, y, level = .95, nboot = 1e3 ) {
  # This function constructs confidence intervals for the ratio of 
  #   expectations using bootstrap resampling. 
  # Inputs:
  #   x, y - numeric vectors, they do not need to be the same length
  #  level - the desired confidence level, defaults to 95%
  #  nboot - the number of bootstrap samples to use
  # Returns:
  #   a tibble with 3 bootstrap CIs based on the estimator mean(x) / mean(y)
  #
  # Details: this function returns three types of bootstrap CIs constructed 
  #  using (1) the precentile method, (2) the basic bootstrap, and (3) a normal
  #  approximation using the bootstrap estimator for the standard devation. 
  #  Missing values are omitted with a warning. 
  
  # error checking
  stopifnot( is.numeric(x) && is.numeric(y) )
  
  # handle missing values
  xna = is.na(x)
  n_na_x = sum(xna)
  if ( n_na_x > 0 ) {
    warning( sprintf("Omitting %i missing values from x.", n_na_x))
    x = x[!xna]
  }
  
  yna = is.na(y)
  n_na_y = sum(yna)
  if ( n_na_y > 0 ) {
    warning( sprintf("Omitting %i missing values from y.", n_na_y))
    y = y[!yna]
  }
  
  # compute the point estimate
  est = mean(x) / mean(y)
  
  # draw the bootstrap samples
  xboot = sample(x, nboot * length(x), replace = TRUE)
  yboot = sample(y, nboot * length(y), replace = TRUE)
  dim(xboot) = c(length(x), nboot)
  dim(yboot) = c(length(y), nboot)
    
  # compute the bootstrapped estimates
  theta_boot = colMeans(xboot) / colMeans(yboot)
  
  # find the quantiles, bootstrap std error, and normal multipliers
  p =  c({1 - level} / 2, 1 - {1 - level} / 2) 
  theta_p = quantile(theta_boot, p)
  z = qnorm(p)
  sboot = sd(theta_boot)

  # construct the confidence intervals
  tibble(
    method = c('percentile bootstrap', 'basic bootstrap', 'normal bootstrap'),
    lower = c(theta_p[1], 2 * est - theta_p[2], est + z[1] * sboot),
    upper = c(theta_p[2], 2 * est - theta_p[1], est + z[2] * sboot)
  )
}

# (c) test the functions above on the "ToothGrowth" data: ----------------------
tooth_growth_cis =
  ToothGrowth %>%
  group_by(dose) %>%
  do( {
    with(.data, { 
      x = len[ supp == 'OJ' ]
      y = len[ supp == 'VC' ]
      ci_jk = confint_jackknife(x, y)
      bind_rows( 
        tibble( method = 'jackknife', lower = ci_jk[1], upper = ci_jk[2]),
        confint_boot(x, y, nboot = 1e4) 
      )
    })
  })



