# A short example of using futures to run computations in parallel when
# doing "batch" processing of a script. 
# 
# Our example again uses the InstEval data set from the lme4 package. 
#
# Updated: November 21, 2019
# Author: James Henderson

# packages: -------------------------------------------------------------------
library(tidyverse); library(lme4); library(future)

# set the plan for evaluating futures: ----------------------------------------
plan(multisession)

# fit models of interest in the background: -----------------------------------

# A simple model, looking at the effect of service with three random effects. 
# Could use futures here, if we were using multiple models still.
fit1 = lmer(y ~ service + (1 | dept/d) + (1|s), data = InstEval)


# Check which futures have resolved: ------------------------------------------
sapply( list(fit1_future, fit2_future), resolved )

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

# Use bootMer to get CIs for the random effects: ------------------------------
get_dept_re = function(x){
  ranef(x)$dept$`(Intercept)`
}
test = bootMer(fit1, get_dept_re, nsim = 1)

# compute many bootstrap samples using the background sessions: ---------------
nboot = 12 
chunks = 4
chunk_size = 3
future_list = vector( mode = 'list', length = chunks)
for ( i in 1:chunks ) {
  future_list[[i]] = future({
    bootMer(fit1, get_dept_re, nsim = chunk_size)
  })
}

# For interactive use when testing: -------------------------------------------
#sapply(future_list, resolved)

# Get the values of the parallel chunks: --------------------------------------
boot_values = lapply(future_list, value)

# Do something useful with the values: ----------------------------------------
boot_samples = do.call("rbind", lapply(boot_values, function(x) x[['t']] ) )
