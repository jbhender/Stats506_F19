# A short example of using futures to fit models in the background
# allowing us to continue to use the console to explore other models, 
# investigate data, or otherwise test ideas. 
# 
# Our example use the InstEval data set from the lme4 package
#
# Updated: November 21, 2019
# Author: James Henderson

# packages: -------------------------------------------------------------------
library(tidyverse); library(lme4); library(future)

# set the plan for evaluating futures: ----------------------------------------
plan(multisession)

# fit models of interest in the background: -----------------------------------

# A simple model, looking at the effect of service with three random effects. 
fit1_future = future({
  fit1 = lmer(y ~ service + (1 | dept/d) + (1|s), data = InstEval)
})

# A model with additional terms, using the studage orderd factor
InstEval$studage_f = with(InstEval, factor(studage, levels(studage)) )
fit2_future = future({
  fit2 = lmer(y ~ service + studage + (1 | dept/d) + (1 + studage|s),
            data = InstEval)
})  

# Fit a 3rd model in the background, adding "lectage" as a fixed effect. :-----
#! After kicking off the future, add it to the "resolved" list below and write
#  a code chunk to extract its value.


# Check which futures have resolved: ------------------------------------------
sapply( list(fit1_future, fit2_future), resolved )

# Extract the model fits after the futures resolve: ---------------------------
fit1 = value(fit1_future)
summary(fit1)

fit2 = value(fit2_future)
summary(fit2)

# Work on model 1 results, while waiting for model 2 to resolve: --------------
fit1_re = ranef(fit1)
length( fit1_re ); names(fit1_re)

fit1_re$dept %>%
  mutate( 
    dept = rownames(.),
    re_int = `(Intercept)`
  ) %>%
  arrange(`(Intercept)`) %>%
  mutate( d_ord = factor(dept, levels = unique(dept) ) ) %>%
  ggplot( aes(y = d_ord, x = re_int) ) +
  geom_point() +
  theme_bw()

# run confint method in background: -------------------------------------------
fit1_ci_future = future({
  confint(fit1)
})

sapply( list(fit1_ci_future), resolved)
#fit1_ci = value(fit1_ci_future)

