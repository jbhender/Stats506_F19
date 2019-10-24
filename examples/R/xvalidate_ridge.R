# Cross-validation in ridge regression 
# Stats 506, Fall 2019
# 
# Author: James Henderson
# Updated: October 23, 2019

# library for fitting penalized glms: -----------------------------------------
library(glmnet)

# Simulation parameters: -------------------------------------------------------
p = 100
n = 1.2*p
s = 4

# Generate a covariance matrix: -----------------------------------------------
sigma = rbeta( choose(p, 2), .3, .3)*{runif(choose(p, 2)) <= .1}

# Upweight the diagonal to ensure we have a positive definite matrix: ---------
Sigma = p*diag(p)/2

# Ensure it is symmetric, then rescale to give variance one: ------------------
Sigma[lower.tri(Sigma)] = sigma
Sigma = {Sigma + t(Sigma)} / p

# Cholesky decomposition will allow us to generate data from N(0, Sigma): -----
R = chol(Sigma)

# Here are some "true" coefficients for data generation: ----------------------
beta = runif(p, 0, 1)

# Generate training data: -----------------------------------------------------
X_train = matrix(rnorm(n*p), n, p) %*% R
Y_train = X_train %*% matrix(beta, ncol=1) + s*rnorm(n)

# Generate test data: ---------------------------------------------------------
## Here we use a "50-50" split, but "80-20" is more common in practice.
X_test = matrix(rnorm(n*p), n, p) %*% R
Y_test = X_test %*% matrix(beta) + s*rnorm(n)

# This fits a ridge regression choosing "beta-hat" to minimize
#  beta_hat = argmin_b .5*||Y - Xb||^2  + lambda*||b||^2
#
# It actually selects a sequence of lambda's for the fit, how is outside our
# scope.  
fit1 = glmnet(X_train, Y_train, alpha = 1)

# Here's how well each set of beta's does at fitting the test data: -----------
# Generally we don't get to do this as we need to choose lambda before evaluating
# on our test data. 
rmse_test = sqrt( colMeans( 
  { Y_test - coef(fit1)[1] - 
    X_test %*% coef(fit1)[2:{1+ncol(X_test)},] 
  }^2 ) )

plot(rmse_test ~ fit1$lambda)

# But, we want to choose lambda before using the test data: -------------------
# So we use cross validation to select "lambda" 

# Number of folds
n_folds = 10#nrow(X_train)

# Our data is arbitrarily ordered
folds = 0:{nrow(X_train)-1} %% n_folds

# Sequence of lambdas
lambda = fit1$lambda

# Function to compute the RMSE: -----------------------------------------------
rmse = function(y, yhat) {
  sqrt( mean( {y - yhat}^2 ) )
}

# Cycle through folds, using each in turn as the validation data and 
# other for training: ---------------------------------------------------------
leave_out_rmse = list()
for ( fold in unique(folds) ) {
    fit = glmnet(X_train[folds!=fold, ], Y_train[folds!=fold, ], 
                 alpha = 1, lambda = lambda)    
    leave_out_rmse[[fold+1]] = 
     sqrt( colMeans( {Y_train[fold==fold, ] - coef(fit)[1] - 
           X_train[fold==fold, ] %*% coef(fit)[2:{1+ncol(X_train)},] }^2 ) )
}

# Form a n_folds x length(lambda) matrix of estimated RMSE and 
# then average within each colum. 
leave_out_rmse = colMeans( do.call('rbind', leave_out_rmse) )

plot(leave_out_rmse ~ lambda)

# Estimate the hyperparameter "lambda": ---------------------------------------
lambda_hat = lambda[which.min(leave_out_rmse)]

# Get the prediction for the selected lambda: ---------------------------------
rmse(Y_test, predict(fit1, X_test)[, which.min(leave_out_rmse)])

# Compare to OLS: -------------------------------------------------------------
beta_ols = coef( lm(Y_train ~ X_train) )
rmse(Y_test, beta_ols[1] + X_train %*% beta_ols[-1])

## Exercise(s): ---------------------------------------------------------------
# 1. Modify this script for the lasso in the following ways:
#    a. Set all but a subset of "beta" to zero
#    b. Use alpha = 1 in glmnet
#
# 2. Modify the script to use the elastic net by cross-validating over a grid
#    of alpha and lambdas.
#
# These are suggested, not required. Use them as needed to solidify your
# understanding. 
