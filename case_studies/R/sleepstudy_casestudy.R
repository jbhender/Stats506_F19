# Case Study: sleepstudy
# Stats 506 Fall 2019
#
# This case study is intended to illustrate some of the functionality of the
# R package lme4 for fitting mixed effects models. This provides us an 
# opportunity to learn about R's S4 object-oriented system.
#
# This case study is based in part on the lme4 vignette:
# https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
#
# Date: October 3, 2019
# Author: James Henderson

# libraries: ------------------------------------------------------------------
library(tidyverse); library(tidyr); library(lme4); 
#library(lmerTest) # This package adds test statistics to lme4 methods.

# Read about lmerTest::lmer and its relation to lme4::lmer using help(lmer) 

# data: -----------------------------------------------------------------------
# reaction times for participants in a sleep deprivation study

# 18 subject, followed for 10 days
sleepstudy %>% 
  group_by(Subject) %>%
  tally()

sleepstudy %>% 
  group_by( Days ) %>%
  tally() 

# visualization: --------------------------------------------------------------
ggplot(sleepstudy, aes( x = Days, y = Reaction) ) +
  geom_point() +
  facet_wrap(~Subject, nrow = 3) +
  theme_bw()

# random intercept model: -----------------------------------------------------
model1 = lmer( Reaction ~ Days + (1 | Subject), data = sleepstudy,
               REML = FALSE)

summary(model1) # See the model results
summary         # Note the difference from prior to library(lmer) 


## Investigate the model object
class(model1) # How does this change if you load lmerTest? 
names(model1) # how we might investigate a "scalar" style S3 object 
isS4(model1)  # Verify it is an S4 object
names( attributes( model1 ) ) # Find out what slots are available

slot(model1, 'theta') # This is a parameter for the (relative) 
                      # covariance matrix of the random effects
model1@theta
attr(model1, 'theta')

slot(model1, 'beta')
slot(model1, 'pp')

# Use accessor functions (methods) to get specific objects of interest
coef(model1)
fixef(model1)
ranef(model1)

# relationship between the random intercepts and subject level random effects.
tibble( a0 = fixef(model1)[1] + ranef(model1)[['Subject']][,1], 
        a1 = coef(model1)[['Subject']][,1],
)
# For the above, ask yourself, why the comma in the final index? 
# What other syntax could we use to ensure a0 and a1 are numeric?

# Relation between subject level intercepts and lmer representation
tibble( re0 = slot(model1, 'theta') * slot(model1, 'u'),
        re1 = ranef(model1)[['Subject']][,1]
)

# Add predictions from model1 to the sleepstudy data set and visualize: -------

## approach 1
sleepstudy = sleepstudy %>%
  mutate( pred_model1_a = predict(model1))

## approach 2
#! There is an error here, can you diagnose it and provide a solution?
sleepstudy = sleepstudy %>%
  mutate( pred_model1_b = coef(model1)[['Subject']][,1] + 
            Days * fixef(model1)[2] )
head( sleepstudy )

## visualize the random intercept model
ggplot(sleepstudy, aes( x = Days, y = Reaction) ) +
  geom_point() +
  geom_line( aes(y = pred_model1_a), color = 'black' ) +
  facet_wrap(~Subject, nrow = 3) +
  ylab('Reaction Time (ms)') + 
  theme_bw()

# random intercept and slope model: -------------------------------------------
model2 = lmer( Reaction ~ Days + ( 1 + Days | Subject), data = sleepstudy )
summary(model2)

## b/c model is linear, fixed effects are the same
rbind( fixef(model2), fixef(model1) )

# Do you think the random intercepts will be the same? Why? 
tibble( model1 = coef(model1)$Subject[, 1], 
        model2 = coef(model2)$Subject[, 1]
) %>% head()

# Add predictions from model2 to the sleepstudy data set and visualize: -------
sleepstudy = sleepstudy %>%
  mutate( pred_model2_a = predict(model2) )

## visualize the random slope model
ggplot(sleepstudy, aes( x = Days, y = Reaction) ) +
  geom_point() +
  geom_line( aes(y = pred_model1_a), color = 'black' ) +
  geom_line( aes(y = pred_model2_a), color = 'red') + 
  facet_wrap(~Subject, nrow = 3) +
  ylab('Reaction Time (ms)') + 
  theme_bw()

# here's a trick to add a legend showing both lines, by transforming to longer
sleepstudy %>% 
  pivot_longer( ends_with('a'), names_to = 'model', values_to = 'pred',
                names_prefix = 'pred_' ) %>%
  ggplot(aes( x = Days, y = Reaction) ) +
  geom_point() +
  geom_line( aes(y = pred, color = model)) + 
  facet_wrap(~Subject, nrow = 3) +
  ylab('Reaction Time (ms)') + 
  scale_color_manual( values = c('black', 'red') ) +
  theme_bw() 

# Relation between subject level intercepts and lmer representation
tibble( ri0 = slot(model2, 'theta')[1] * slot(model2, 'u')[seq(1, 35, 2)],
        ri1 = ranef(model2)[['Subject']][, 1]
)

# Relation between subject level slopes and lmer representation
rs0 = 
  colSums( matrix( slot(model2, 'theta')[2:3] * slot(model2, 'u'), nrow = 2) )

tibble( rs0 = rs0,
        rs1 = ranef(model2)[['Subject']][, 2]
)

