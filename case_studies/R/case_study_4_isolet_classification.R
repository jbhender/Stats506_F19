# Cross-validation case study using the "isolet". 
#
# We will learn to predict whehter an audio recording of a person speaking a
# single (English) letter is a vowel or consonant.
#
# Data comes from here:
# https://archive.ics.uci.edu/ml/machine-learning-databases/isolet/
#
# Author: James Henderson
# Updated: October 24, 2019

# libraries: ------------------------------------------------------------------
library(tidyverse); library(glmnet)

# read in the data: -----------------------------------------------------------
iso_train = read_delim('./isolet1+2+3+4.data', 
                       delim = ',', 
                       trim_ws = TRUE,
                       col_names = FALSE)

iso_test = read_delim('./isolet5.data', 
                       delim = ',', 
                       trim_ws = TRUE,
                       col_names = FALSE)

# separate features from repsonse: --------------------------------------------
resp_col = which( sapply(iso_train, function(x) length(unique(x)) ) == 26 )
resp = iso_train[[resp_col]] #? Why double brackets?

# label individuals based on apperance of 26 followed by 1,
iso_train = iso_train %>% 
  mutate( a = X618 == 1, 
          z = X618 == 26,
          id = c(1, 1 + cumsum( a[-1]*z[-n()] ))
  )

# one data subset appears to be organized in a different order
iso_train %>%
  group_by(id) %>%
  count() %>%
  filter(n < 52)

# explore how many "ids" appear in a particular range, expect 52*30 = 1560 per
#  "isolet"
iso_train %>% filter( id <= 30) %>% nrow()
iso_train %>% filter( id > 30 & id <= 60) %>% nrow()
iso_train %>% filter( id > 60 & id <= 120) %>% nrow()
iso_train %>% filter( id > 120 & id <= 120) %>% nrow()

# separate out the training data
Xtrain = as.matrix( iso_train[-resp_col] ) #? Why single brackets? 
#dim(Xtrain); Xtrain[1:5,1:5]

# simplify the task to distinguishing vowels or consonants: -------------------
vowels = which( letters %in% c('a', 'e', 'i', 'o', 'u'))
y = 1L * resp %in% vowels
length(y)

# fit a test model to work out details: ---------------------------------------
# ~10s to fit a model with ridge penaly (a = 0)
# ~40s to fit a model with lasso penalty (a = 1)
system.time({
  model_prep = glmnet(x = Xtrain, 
                      y = y, 
                      family = 'binomial',  
                      alpha = 0,
                      standardize = FALSE)
})
#names(model_prep)

# form predictions on the training data: --------------------------------------
# Note, the coef method returns a matrix of coefficients, with each column
#  corresponding to a value of the penalty parameter, lambda
# dim( coef(model_prep) )
beta_prep = coef(model_prep)
yhat_prep = plogis( as.matrix( cbind(1, Xtrain) %*% beta_prep ) )

# classification error on training data: --------------------------------------
error_prep = 1 - colMeans( {yhat_test > .5} == y )
tibble( lambda = model_prep$lambda, error_prep = error_prep) %>%
  ggplot( aes( x = lambda, y = error_prep) ) +
  geom_line() +
  theme_bw() +
  scale_x_log10()

# more prep, setting up 4-fold cross validation but using alpha = 0 still: ----
#! could we parallelize here?

# assign folds by isolet
iso_train = iso_train %>%
  group_by( id ) %>%
  mutate( fold = sum( unique(id) > c(0, 30, 60, 120) ) ) %>%
  ungroup() 
#iso_train %>% group_by(fold) %>% count()

# sequence of lambdas
lambda = model_prep$lambda

models = vector( mode = 'list', length = 4)
errors = vector( mode = 'numeric', length = length(lambda) )
for ( fold in 1:4 ) {
  
  # Indices for rows of Xt / y that are in this fold
  fold_ind = which(iso_train$fold == fold)
  
  # Training data
  Xt = Xtrain[-fold_ind, ]
  yt = y[-fold_ind]
  
  # Validation data
  Xv = Xtrain[fold_ind, ]
  yv = y[fold_ind]
  
  # Fit the model
  models[[fold]] =
    model_prep = glmnet(x = Xt, 
                        y = yt, 
                        family = 'binomial',  
                        alpha = 1, 
                        lambda = lambda
                 )
  
  # Evaluate the model on the hold out data for each lambda
  yvhat = plogis( as.matrix( cbind(1, Xv) %*% coef(models[[fold]]) ) )
  
  # Using sum, rather than mean to handle different fold sizes
  errors = errors + {colSums( {yvhat > .5} == yv )}
}

# Normalize errors to the number of predictions
errors = errors / nrow(Xtrain)

tibble( lambda = lambda, error = errors) %>%
  ggplot( aes( x = lambda, y = 1 - errors) ) +
  geom_line() +
  theme_bw() +
  scale_x_log10()

# Now, repeat the above process for a sequence of alphas: ---------------------
#! We will do this as a live demo if time permits.
