# Bootstrap Example 1
# Stats 506, Fall 2019
#
# Updated: October 10, 2019
# Author: James Henderson

## Generate some example data: ------------------------------------------------
n = 50
x = rgamma(n, shape=3, rate=1)
boxplot(x, las=1, main='Boxplot of Data')
iqr_est = unname(diff(quantile(x, c(.25, .75))))
iqr_est

## Generate some bootstrap samples: -------------------------------------------
B = 1e3  ## Number of bootstrap samples
boot_samples = sample(x, n * B, replace=TRUE)  ## Sample with replacement
dim(boot_samples) = c(n, B)                    ## each column is a dataset

## Compute statistic for each bootstrap sample: -------------------------------
boot_iqr = apply(boot_samples, 2, 
                 function(b) unname( diff( quantile(b, c(.25, .75) ) ) ) 
)

# Investigate result: ---------------------------------------------------------
hist(boot_iqr, las=1, col='green4', xlab='Estimated IQR', 
     cex.axis=1.5, cex.lab=1.5, main = '')

boot_q = quantile(boot_iqr, c(.025, .975))
abline(v=boot_q, lty='dashed', lwd=2)

boot_ci = sprintf('%3.1f (%3.1f, %3.1f)', iqr_est, boot_q[1], boot_q[2])
cat(boot_ci,'\n')

# A functional approach: ------------------------------------------------------
iqr_ci = function(y, n_boot=1e3){
  # Return a bootstrap 95% confidence interval for the
  # q^th population quantile based on an iid sample.
  # 
  # Inputs: 
  #   y - a numeric vector representing an iid sample
  #   n_boot - the number of bootstrap samples to use in estimation
  # Returns:  
  #   a numeric vector representing the confidence bound, computed using
  #   the percentile method
  m = length(y)
  mat = sample(y, n_boot * m, replace=TRUE)
  dim(mat) = c(n_boot, m)
  f = function(x) diff( quantile(x, c(.25, .75) ) )
  v = apply(mat, 1, f)
  lcb = quantile(v, 0.025)
  ucb = quantile(v, 0.975)
  return( c(lcb, ucb) )
}
# iqr_ci(x)

# Similarly here is a function for bootstrapped CI's for quantiles. 
quant_ci = function(y, q=0.5, n_boot=1000) {
  # Return a bootstrap 95% confidence interval for the
  # q^th population quantile based on an iid sample.
  # 
  # Inputs: 
  #   y - a numeric vector representing an iid sample
  #   n_boot - the number of bootstrap samples to use in estimation
  # Returns:  
  #   a numeric vector representing the confidence bound, computed using
  #   the percentile method
  
  m = length(y)
  mat = sample(y, n_boot * m, replace=TRUE)
  dim(mat) = c(n_boot, m)
  f = function(x) quantile(x, q)
  v = apply(mat, 1, f)
  lcb = quantile(v, 0.025)
  ucb = quantile(v, 0.975)
  return( c(lcb, ucb) )
}
# Test the function
quant_ci(x)
